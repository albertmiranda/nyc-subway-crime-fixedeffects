# Subway Ridership and Crime in New York City
Master's Applied Project: Subway Ridership and Crime in New York City: A Fixed-Effects Analysis of Egohoods, 2020-2024

This repository contains the code for my Master's thesis analyzing the relationship between subway ridership and crime in New York City using quarter-mile egohoods and two-way fixed effects models.

## Abstract

This study asks whether crime levels vary with subway station availability across New York City. The unit of analysis is the quarter-mile "egohood," a buffer built around each 2020 Census block centroid, observed annually from 2020 to 2024. The dataset includes 37,984 egohoods and 189,920 egohood-years. Using two-way fixed effects models with egohood and year effects, the analysis finds that within-place increases in ridership are associated with modest increases in recorded crime (β ≈ 0.05, p < .001), while station presence alone is not a strong independent predictor once time-invariant local traits are held constant.

## Repository Structure
```
nyc-subway-crime-fixedeffects/
├── code/
│   ├── 01_data_cleaning.R
│   ├── 02_egohood_construction.R
│   ├── 03_visualization_and_eda.R
│   └── 04_regression_modeling.R
├── data_raw/
├── data_clean/
├── figs/
├── results/
└── README.md
```

## Scripts

| Script | Description |
|--------|-------------|
| `01_data_cleaning.R` | Loads and cleans NYPD complaint data and MTA ridership data (2020-2024) |
| `02_egohood_construction.R` | Creates quarter-mile egohoods from Census blocks, joins ACS demographics, and builds the panel dataset |
| `03_visualization_and_eda.R` | Generates maps and figures, computes Moran's I for spatial autocorrelation |
| `04_regression_modeling.R` | Estimates pooled OLS and two-way fixed effects models, runs spatial lag robustness check |

## Data Sources

The raw data files are not included in this repository due to size. To reproduce the analysis, download:

1. **NYPD Complaint Data Historic** - [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i)

2. **MTA Subway Hourly Ridership** - [data.ny.gov](https://data.ny.gov/Transportation/MTA-Subway-Hourly-Ridership-2020-2024/wujg-7c2s)

3. **Census Data** - Downloaded automatically via `tidycensus` package (requires API key)

Place the CSV files in the `data_raw/` folder before running the scripts.

## Requirements

The analysis was conducted in R. Required packages:
```r
install.packages(c(
  "data.table", "arrow", "sf", "units", "tigris", "tidycensus",
  "dplyr", "ggplot2", "viridis", "ggspatial", "ggrepel",
  "gridExtra", "cowplot", "patchwork", "ggplotify",
  "spdep", "spatialreg", "fixest", "modelsummary", "broom"
))
```

For Census data access, obtain a free API key from [census.gov](https://api.census.gov) and set it:
```r
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
```

## How to Run

1. Clone this repository
2. Download the raw data files and place them in `data_raw/`
3. Set your working directory to the repository root
4. Run the scripts in order: `01` → `02` → `03` → `04`

## Key Findings

- Raw crime is strongly clustered in space (Moran's I ≈ 0.92)
- In pooled OLS, higher ridership is associated with more crime (β ≈ 0.27)
- Under two-way fixed effects, this association shrinks but remains positive (β ≈ 0.05)
- Station presence alone does not carry an independent effect once stable local characteristics are held constant
- The results suggest that flows of people matter more than the fixed footprint of infrastructure

## Author

Alberto Miranda  
M.S. in Analytics
Harrisburg University of Science and Technology

## License

This project is for academic purposes. Please cite appropriately if using this code or methodology.
