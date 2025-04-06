# Postsecondary Outcomes Analysis for Deaf Veterans

This repository contains code and documentation for analyzing postsecondary education outcomes among deaf veterans using American Community Survey (ACS) datasets.

## Objective
To examine postsecondary education patterns and outcomes for deaf veteran populations using ACS 5-year survey data.

## Requirements
- R (v4.0+ recommended)
- RStudio (optional but recommended)
- ACS 5-year datasets:
  - Person Unit Survey (PUS)
  - Housing Unit Survey (HUS)

## 🛠️ Setup Instructions

1. **Clone Repository**

2. **Install R Packages**
Open R/RStudio and run this script `Scripts/Main.R`.

## 📁 Data Preparation

1. **Download ACS Data**
- Get latest 5-year datasets from [Census Bureau](https://www2.census.gov/programs-surveys/acs/data/pums/)
- Required files:
  - `ACS 5yr csv_pus.zip`
  - `ACS 5yr csv_hus.zip`

2. **Rename Files**
- Place in `data/` directory
- Rename to match script requirements:
  - `[year_n]_5y_csv_pus.zip` (example: "*2023_5y_csv_pus.zip*")
  - `[year_n]_5y_csv_hus.zip`

## ▶️ Running the Analysis

1. Open `Scripts/Main.R` in RStudio
2. Update year parameter:

2. **Rename Files**
- Place in `Raw_data/` directory
- Rename to match script requirements:
  - `[year_n]_5y_csv_pus.zip` (example: "*2023_5y_csv_pus.zip*")
  - `[year_n]_5y_csv_hus.zip`

## ▶️ Running the Analysis

1. Open `Scripts/Main.R` in RStudio
2. Update year parameter:

3. Run entire script (Ctrl+Shift+Enter)

## ⚠️ Important Notes

1. **Data Size Warning**
- ACS files are large (>1GB)
- Ensure adequate memory allocation
- Consider using `data.table` for large data:
  ```
  install.packages("data.table")
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("Hmisc")
  ```

2. **Variable Handling**
- Key variables analyzed:
  - `DEAR` (Hearing difficulty)
  - `VPS` (Veteran period of service)
  - `SCHL` (Educational attainment)
  - `ESR` (Employment rate)
- Weighting uses `PWGTP` variables

## 🐛 Troubleshooting

1. **Common Errors**
- `File not found`: Verify file names/locations
- `Memory issues`: Use `data.table::fread()` for import

2. **ACS Data Updates**
- Check for new variables annually
- Update variable dictionary references as needed

## 📊 Outputs
Analysis generates the weight summary statistics in the folder Assets. Currently, this folder is empty and this information will never appear in GitHub repository.

## 📜 License
CC BY-NC-ND 4.0 License - See [CC BY-NC-ND 4.0](LICENSE.md) file

