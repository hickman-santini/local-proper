#!/usr/bin/env Rscript
# Plot calibration data from RDS file
# Usage: Rscript plot_calibration.R <input.rds> [output.png]

library(dplyr)
library(ggplot2)
library(ggthemes)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript plot_calibration.R <input.rds> [output.png]\n")
  cat("Example: Rscript plot_calibration.R metaculus_calibration_data_5years.rds metaculus-calibration-5years.png\n")
  quit(status = 1)
}

input_file <- args[1]

# Auto-generate output filename if not provided
if (length(args) >= 2) {
  output_file <- args[2]
} else {
  # Extract basename and create output filename
  base_name <- tools::file_path_sans_ext(basename(input_file))
  base_name <- sub("metaculus_calibration_data_", "", base_name)
  output_file <- paste0("metaculus-calibration-", base_name, ".png")
}

# Check if input file exists
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

cat("Loading data from:", input_file, "\n")
df <- readRDS(input_file)

# Calculate 95% confidence intervals using Monte Carlo simulation
# The ribbon shows where the observed resolution rate would fall 95% of the time
# if the community predictions were perfectly calibrated
cat("Calculating 95% confidence intervals...\n")

interval <- function(N, p_gov, mw = 10000) {
  # Simulate mw trials of N binary outcomes with probability p_gov
  fracaligned <- numeric(mw)
  for (i in 1:mw) {
    outcomes <- runif(N) < p_gov
    fracaligned[i] <- sum(outcomes) / N
  }
  # Return the 2.5th and 97.5th percentiles (95% confidence interval)
  return(quantile(fracaligned, c(0.025, 0.975)))
}

df <- df %>%
  rowwise() %>%
  mutate(
    interval_values = list(interval(num_questions, prob_assigned)),
    interval_lower = interval_values[1],
    interval_upper = interval_values[2]
  ) %>%
  ungroup() %>%
  select(-interval_values)

# Extract title info from filename
title_suffix <- sub("metaculus_calibration_data_", "", tools::file_path_sans_ext(basename(input_file)))
title_suffix <- gsub("_", " ", title_suffix)

# Create calibration plot
cat("Creating calibration plot...\n")
p <- ggplot(df, aes(x = prob_assigned, y = percent_resolved_pos)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_ribbon(aes(ymin = interval_lower, ymax = interval_upper), alpha = 0.2, fill = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgreen") +
  labs(
    x = "Community Prediction",
    y = "Actual Resolution Rate",
    title = paste0("Metaculus Community Calibration (", title_suffix, ", time-weighted)")
  ) +
  theme_clean()

ggsave(output_file, plot = p, width = 8, height = 6, dpi = 300)
cat("Plot saved to:", output_file, "\n")

# Print summary statistics
cat("\nSummary Statistics:\n")
cat("Total questions:", sum(df$num_questions), "\n")
cat("Bins:", nrow(df), "\n")

# Calculate mean absolute calibration error
mae <- mean(abs(df$prob_assigned - df$percent_resolved_pos))
cat("Mean Absolute Calibration Error:", round(mae, 4), "\n")
