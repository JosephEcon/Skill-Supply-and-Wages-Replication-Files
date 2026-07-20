# Replication Package for "Specification Searching in the Race between Education and Technology" by Joseph Francis

---

## Requirements

### Software

- **R** (4.0 or later)
- The following packages are installed automatically on first run:
  `tidyverse`, `readr`, `ipumsr`, `haven`, `dplyr`, `tseries`,
  `relaimpo`, `urca`, `sandwich`, `COINT`, `ARDL`, `AER`, `vars`

### Data

Two external data sources are required. Neither is included in this package.

#### 1. AGK replication archive (required)

Download the Autor, Goldin, and Katz (2020) replication package from openICPSR:

> https://www.openicpsr.org/openicpsr/project/120694/version/V2/view

Download **120694-V2.zip** and place it in this directory (alongside `master.R`).
Script `0_CPS_data_2014_2024.R` will unzip it automatically on first run into a
subfolder called `AGK_files/` and copy the required files to `Data/AGK/`.
This step only runs once; subsequent runs skip it.

Files extracted from the zip:
- `Data/AGK/CPS/` — 61 cleaned annual March CPS files (1962–2018)
- `Data/AGK/` — five processed series files (efficiency units, labor supply
  weights, wage gap series, march cells)
- `Data/colhs1405.dta` — AKK (2008) historical college/high school wage series

#### 2. Extended CPS data, 2014–2024 (required for extended analysis)

Script 0 downloads the 2014–2024 March CPS microdata from IPUMS CPS. You will
need a free IPUMS account and API key:

> https://cps.ipums.org/cps/

Set your API key as an environment variable before running:

```r
Sys.setenv(IPUMS_API_KEY = "your_key_here")
```

Or the script will prompt you to enter it interactively.

---

## Contents

| File | Description |
|------|-------------|
| `master.R` | Runs the full pipeline in sequence |
| `0_CPS_data_2014_2024.R` | Setup: unzips AGK archive, downloads and cleans 2014–2024 CPS data |
| `1_Demographic_cells_pt1.R` | Constructs demographic cells from cleaned CPS files (part 1) |
| `2_Demographic_cells_pt2.R` | Demographic cells (part 2); combines all years |
| `3_Efficiency_units.R` | Calculates efficiency unit supply by education and experience |
| `4_Wage_equations.R` | Estimates March CPS wage equations |
| `5_Labor_supply_weights.R` | Constructs labor supply weights |
| `6_Wages.R` | Aggregates wages using predicted wage equations |
| `7_Wage_premiums.R` | Computes college/high school wage premium series |
| `8_Analysis.R` | Main analysis: produces KM-style relative supply/wage plots |
| `9_Tests.R` | Replicates Tables 1–7 and Figures 1–3 from the paper |
| `10_Comparison_report.R` | Validates R outputs against AGK Stata reference files |
| `Data/CPIAUCSL.csv` | CPI deflator (FRED, base 2012) |
| `Data/PCEPI.csv` | PCE price index (FRED, base 2012) |

---

## Running the replication

1. Place `120694-V2.zip` in this directory.
2. Open `master.R` in R and set the working directory to this folder, or run:

```r
source("master.R")
```

The pipeline runs scripts 0 through 10 in sequence. Script 0 handles all setup
(unzipping AGK files, downloading extended CPS data) automatically. Each script
writes its outputs to `Data/` subdirectories; subsequent scripts read from there.

Total runtime is approximately 30–60 minutes, depending on download speed and
machine. The IPUMS download (script 0) dominates the first run.

---

## Output

Key outputs written to `Data/`:

- `Data/series/km-plot-*.csv` — relative supply and wage differential series
  for four sample periods (6387, 6305, 6318, 6323)
- `Data/series/clghsgwg-march-regseries-exp-*.csv` — college/high school wage
  premium by experience group
- `Replication_Results.txt` — Tables 1–7 (ECM regressions, cointegration tests,
  forecast comparisons)
- `Comparison_Report.txt` — Validation against AGK Stata reference files
