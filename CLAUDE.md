# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains calibration analysis tools for forecasting reliability, specifically analyzing how well predicted probabilities align with actual outcomes. It includes:

1. **R Markdown Analysis** (`calibration.Rmd`): Statistical analysis and visualization of calibration data from Manifold Markets
2. **Shiny Web App** (`reliability-app/`): Interactive "Instant Reliability Significance" (IRS) calculator

## Core Concepts

### Reliability Analysis
The codebase implements statistical methods from Bröcker & Smith (2007) "Increasing the reliability of reliability diagrams" to assess whether deviations from perfect calibration are statistically meaningful.

**Key Function: `exp_frac_aligned(N, p_gov, lessthan, mw)`**
- Simulates `mw` worlds where `N` forecasts are made with governing probability `p_gov`
- Calculates the probability that the fraction of positive outcomes is less than `lessthan`
- Returns statistical significance of calibration deviation
- Found in both `calibration.Rmd:20-37` and `reliability-app/app.R:31-46`

**Key Function: `interval(N, p_gov, mw)`**
- Computes 95% confidence intervals (2.5th and 97.5th percentiles) for fraction aligned
- Used to create uncertainty bands in calibration plots
- Found in `calibration.Rmd:39-47`

## Development Commands

### R Markdown Analysis
```r
# Open in RStudio and knit, or run from R:
rmarkdown::render("calibration.Rmd")
```

### Shiny App Development
```r
# Run locally from reliability-app/ directory:
shiny::runApp()

# Or from parent directory:
shiny::runApp("reliability-app")
```

### Deployment
```r
# Deploy to shinyapps.io (from reliability-app/ directory):
# First time: rsconnect::setAccountInfo(name='...', token='...', secret='...')
rsconnect::deployApp()
```

## Architecture

### Data Flow in calibration.Rmd
1. Load calibration data (20 bins of probability assignments)
2. For each bin:
   - Run Monte Carlo simulation (`exp_frac_aligned`) to test significance
   - Calculate confidence intervals (`interval`)
3. Generate calibration plot with uncertainty bands
4. Export as `manifold-calibration.png`

### Shiny App Structure (reliability-app/app.R)
- **UI**: Interactive form with 4 parameters (N, p_gov, lessthan, mw)
- **Server**: Reactive histogram generation using `exp_frac_aligned`
- **Assets**: Educational content and FiveThirtyEight calibration example in `www/`
- **Deployment**: Pre-configured for shinyapps.io (rsconnect/ contains deployment metadata)

## Key Dependencies
- **dplyr**: Data manipulation in analysis pipeline
- **ggplot2**: Calibration visualization
- **ggthemes**: Clean theme for plots
- **shiny**: Interactive web application framework
