# Metaculus Calibration Analysis

This repository analyzes Metaculus community prediction calibration using time-weighted sampling methodology.

## Setup

### 1. Configure API Key

Before running any scripts, you need to set up your Metaculus API key:

```bash
# Copy the template file
cp config.R.template config.R

# Edit config.R and add your actual API key
# The config.R file is in .gitignore and will never be committed
```

Your `config.R` should look like:
```r
API_KEY <- "your_actual_api_key_here"
```

### 2. Install Required R Packages

```r
install.packages(c("httr", "dplyr", "lubridate", "readr", "ggplot2", "ggthemes", "shiny"))
```

## Usage

### Fetch Data by Time Period

```bash
# Fetch last 5 years (default)
Rscript fetch_metaculus_final.R

# Fetch last 6 months
# Edit YEARS_BACK <- 0.5 in fetch_metaculus_final.R first
Rscript fetch_metaculus_final.R
```

### Fetch Data by Date Range

```bash
# Edit START_DATE and END_DATE in fetch_metaculus_daterange.R
# Then run:
Rscript fetch_metaculus_daterange.R
```

### Generate Calibration Plots

```bash
# Render the R Markdown analysis
Rscript -e "rmarkdown::render('calibration.Rmd')"
```

### Run Interactive Shiny App

```bash
Rscript -e "shiny::runApp('app.R')"
```

## Files

- `config.R.template` - Template for API configuration (commit this)
- `config.R` - Your actual API key (DO NOT COMMIT - in .gitignore)
- `fetch_metaculus_final.R` - Fetch data by time period (e.g., last 5 years)
- `fetch_metaculus_daterange.R` - Fetch data by specific date range (e.g., 2022-2023)
- `calibration.Rmd` - R Markdown analysis and plotting
- `app.R` - Interactive Shiny application
- `.gitignore` - Ensures config.R is never committed

## Methodology

This analysis uses time-weighted sampling of Metaculus community predictions:

1. For each resolved binary question, fetch the complete time series of community predictions
2. Weight each prediction by how long it was active
3. Bin predictions into 20 probability bins (0-5%, 5-10%, ..., 95-100%)
4. Calculate actual resolution rates for each bin
5. Generate calibration plots with 95% confidence intervals

This matches Metaculus's internal track record methodology using the `recency_weighted` aggregation method.

## Security Note

**IMPORTANT:** Never commit `config.R` containing your API key. The `.gitignore` file is configured to prevent this, but always verify before pushing to a public repository.
