library(httr)
library(dplyr)
library(lubridate)
library(readr)

# Load API key from config file
if (!file.exists("config.R")) {
  stop("config.R not found. Please copy config.R.template to config.R and add your API key.")
}
source("config.R")

# Configuration
BASE_URL <- "https://www.metaculus.com/api2"
START_DATE <- as.Date("2022-01-01")
END_DATE <- as.Date("2023-12-31")
NUM_BINS <- 20

#' Fetch resolved binary questions within date range
fetch_resolved_binaries <- function(api_key, start_date, end_date) {
  questions <- list()
  offset <- 0
  limit <- 100

  cat("Fetching resolved binary questions from", as.character(start_date),
      "to", as.character(end_date), "...\n")

  repeat {
    url <- paste0(BASE_URL, "/questions/?limit=", limit, "&offset=", offset,
                  "&type=binary&has_group=false")

    response <- GET(url, add_headers(Authorization = paste("Token", api_key)))
    if (status_code(response) != 200) stop("API request failed")

    parsed <- content(response, "text", encoding = "UTF-8") %>% jsonlite::fromJSON(flatten = TRUE)
    if (length(parsed$results) == 0) break

    new_questions <- parsed$results %>%
      filter(resolved == TRUE,
             !is.na(question.resolution),
             !is.na(question.actual_resolve_time)) %>%
      mutate(resolve_time = as.POSIXct(question.actual_resolve_time,
                                       format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
             resolve_date = as.Date(resolve_time)) %>%
      filter(resolve_date >= start_date, resolve_date <= end_date)

    if (nrow(new_questions) > 0) questions <- append(questions, list(new_questions))

    cat("Fetched", offset + nrow(parsed$results), "questions (", nrow(new_questions), "in date range)...\n")

    if (is.null(parsed$`next`)) break

    if (length(questions) > 0 && nrow(new_questions) == 0) {
      no_results_count <- ifelse(exists("no_results_count"), no_results_count + 1, 1)
      if (no_results_count >= 5) break
    } else {
      no_results_count <- 0
    }

    offset <- offset + limit
    Sys.sleep(0.5)
  }

  if (length(questions) == 0) stop("No questions found")

  all_questions <- bind_rows(questions)
  cat("Total questions fetched:", nrow(all_questions), "\n")
  return(all_questions)
}

#' Download time-weighted CP data for a question
get_question_cp_timeseries <- function(post_id, api_key) {
  url <- paste0("https://www.metaculus.com/api/posts/", post_id,
                "/download-data/?minimize=false&aggregation_methods=recency_weighted")

  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  response <- GET(url, add_headers(Authorization = paste("Token", api_key)),
                  write_disk(tmp_zip, overwrite = TRUE))

  if (status_code(response) != 200) {
    unlink(tmp_zip)
    unlink(tmp_dir, recursive = TRUE)
    return(NULL)
  }

  unzip(tmp_zip, exdir = tmp_dir)
  forecast_file <- file.path(tmp_dir, "forecast_data.csv")

  if (!file.exists(forecast_file)) {
    unlink(tmp_zip)
    unlink(tmp_dir, recursive = TRUE)
    return(NULL)
  }

  forecasts <- read_csv(forecast_file, show_col_types = FALSE) %>%
    filter(`Forecaster Username` == "recency_weighted")

  unlink(tmp_zip)
  unlink(tmp_dir, recursive = TRUE)

  return(forecasts)
}

#' Process calibration data with time-weighted sampling
process_calibration_data <- function(questions, api_key, num_bins = 20) {
  cat("\nProcessing", nrow(questions), "questions with time-weighted CP sampling...\n")

  all_samples <- list()

  for (i in 1:nrow(questions)) {
    if (i %% 10 == 0) cat("Processing question", i, "of", nrow(questions), "\n")

    post_id <- questions$id[i]
    resolution <- questions$question.resolution[i]
    resolution_numeric <- ifelse(tolower(resolution) == "yes", 1,
                                  ifelse(tolower(resolution) == "no", 0, NA))

    if (is.na(resolution_numeric)) next

    forecasts <- get_question_cp_timeseries(post_id, api_key)

    if (is.null(forecasts) || nrow(forecasts) == 0) next

    # Calculate duration for each forecast
    forecasts <- forecasts %>%
      mutate(
        start_time = as.POSIXct(`Start Time`, tz = "UTC"),
        end_time = if_else(is.na(`End Time`),
                          as.POSIXct(questions$question.actual_close_time[i],
                                    format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
                          as.POSIXct(`End Time`, tz = "UTC")),
        duration_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
        cp_value = `Probability Yes`,
        resolution = resolution_numeric,
        question_id = post_id
      ) %>%
      filter(duration_seconds > 0, !is.na(cp_value)) %>%
      select(question_id, cp_value, duration_seconds, resolution)

    if (nrow(forecasts) > 0) {
      all_samples[[length(all_samples) + 1]] <- forecasts
    }

    Sys.sleep(0.15)  # Rate limiting
  }

  if (length(all_samples) == 0) stop("No valid samples collected")

  combined_samples <- bind_rows(all_samples)

  cat("\nTotal CP samples collected:", nrow(combined_samples), "\n")
  cat("Total time-weighted duration:", format(sum(combined_samples$duration_seconds) / (365.25 * 24 * 3600), digits=2), "years\n")

  # Create bins
  bin_width <- 1.0 / num_bins
  combined_samples <- combined_samples %>%
    mutate(
      bin = cut(cp_value, breaks = seq(0, 1, by = bin_width),
                include.lowest = TRUE, labels = FALSE),
      bin_center = (bin - 0.5) * bin_width
    ) %>%
    filter(!is.na(bin))

  # Calculate calibration statistics (weighted by duration)
  calibration_df <- combined_samples %>%
    group_by(bin, bin_center) %>%
    summarise(
      prob_assigned = first(bin_center),
      percent_resolved_pos = sum(resolution * duration_seconds) / sum(duration_seconds),
      num_markets = sum(duration_seconds),
      num_questions = n_distinct(question_id),
      .groups = "drop"
    ) %>%
    arrange(bin)

  return(calibration_df)
}

# Main execution
cat("Starting Metaculus time-weighted calibration data fetch...\n")
cat("Using download-data API endpoint\n\n")

questions <- fetch_resolved_binaries(API_KEY, START_DATE, END_DATE)
calibration_data <- process_calibration_data(questions, API_KEY, NUM_BINS)

cat("\nCalibration Data:\n")
print(calibration_data)

# Create file suffix from date range
start_str <- format(START_DATE, "%Y")
end_str <- format(END_DATE, "%Y")
output_suffix <- paste0(start_str, "-", end_str)

write.csv(calibration_data, paste0("metaculus_calibration_data_", output_suffix, ".csv"), row.names = FALSE)
saveRDS(calibration_data, paste0("metaculus_calibration_data_", output_suffix, ".rds"))
cat("\nData saved to files with suffix:", output_suffix, "\n")
