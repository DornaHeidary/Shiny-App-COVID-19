# Health Dashboard (Shiny) — Category & Region Filters

A simple Shiny dashboard built with an open health dataset (COVID-19 Data Hub via the `COVID19` R package).
## Features
- **Filters (minimum 2):**
  - Category (metric): Cases / Deaths / Hospitalizations / Tests
  - Country
  - Region (state/province) — shown only when region-level data exists
  - Date range
- **Visualizations:**
  - Time-series chart (daily totals)
  - Bar chart (Top regions by total in selected date range)
  - Heatmap (Region × Month)

## Dataset
- Source: COVID-19 Data Hub (accessed through the `COVID19` R package)
- Notes:
  - Some countries do not provide region-level data at level 2.
  - Some metrics (e.g., tests/hospitalizations) may be missing for certain regions.

## Requirements
- R (recommended: R 4.x)
- Internet connection on the first run (data is downloaded)
- Packages are auto-installed by the app:
  - shiny, ggplot2, dplyr, tidyr, COVID19, lubridate

## How to Run
### Option 1 (RStudio)
1. Open `app.R`
2. Click **Run App*