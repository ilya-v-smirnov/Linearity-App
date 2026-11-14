# Linearity App

<!-- badges: start -->
[![R-CMD-check](https://github.com/ilya-v-smirnov/Linearity-App/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ilya-v-smirnov/Linearity-App/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/ilya-v-smirnov/Linearity-App/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/ilya-v-smirnov/Linearity-App/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/ilya-v-smirnov/Linearity-App/actions/workflows/lint.yaml/badge.svg)](https://github.com/ilya-v-smirnov/Linearity-App/actions/workflows/lint.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
<!-- badges: end -->

## Overview
The **Linearity App** is an R package with a Shiny application that calculates analyte concentrations from laboratory assay data using a standard curve. It supports common assays like **sandwich ELISA, BCA assay, and Griess assay** and processes data from **Excel (.xlsx) and CSV (.csv) files**. The interface and reports are available in **English and Russian**.

## Features
- Generates structured data templates for easy data entry
- Imports and validates assay data
- Supports linear and quadratic regression models (log-log or linear scales)
- Provides standard curve and residual plots
- Flags samples outside the calibration range instead of extrapolating
- Exports results in Excel or CSV format
- Generates reports in Word (.docx) format
- English and Russian interface (`?lang=ru` in the URL or the language selector)

## Installation
Install the package from GitHub (R >= 4.1):

```r
install.packages("remotes")
remotes::install_github("ilya-v-smirnov/Linearity-App")
```

Run the app:

```r
LinearityApp::run_app()             # English
LinearityApp::run_app(lang = "ru")  # Russian
```

The calculations can also be used without the interface:

```r
library(LinearityApp)
path <- system.file("extdata", "griess.xlsx", package = "LinearityApp")
data <- format_input_table(check_input_file(read_table(path)))
res <- calculate(data, model = "LRM")
res$result                      # sample concentrations and flags
message_text(res$notes, "en")   # warnings
```

## How to Use - Brief Guideline

### 1. Generate a Data Template
- Set **template parameters**: start concentration, dilution step, number of standard levels and series, number of samples.
- Click **"Download Template"** to generate a CSV or Excel file.

### 2. Import Data
- Fill in the template with:
  - Sample names, dates, well types (`standard`, `sample`, `NC`)
  - Dilution factors and optical density (OD) values
- Click **"Browse"** to upload the modified file.
- The app validates the data before proceeding.

### 3. Analyze Standard Curve
- Navigate to the **Standard Curve** tab.
- View the **fitted curve** and the **residuals plot**.
- Choose between **linear regression** (LRM) or **quadratic regression** (QRM).
- Enable or disable **log transformation** with the "Linear scales" checkbox.

### 4. View and Export Results
- The **Results** table displays calculated analyte concentrations.
- Concentrations are calculated by inverting the fitted standard curve and multiplying by the dilution factor.
- Each sample well gets a **flag**. Only wells flagged `OK` receive a concentration; the others are left empty:
  - `< LLOQ` / `> ULOQ` - outside the range of standards used in the fit;
  - `OD <= NC` - optical density at or below the negative control;
  - `No solution` - the OD cannot be reached by the quadratic curve;
  - `Missing OD` - no optical density value.
- Warnings about excluded standards and unquantified samples are shown above the tables and in the report.
- Click **"Download Results"** to export as Excel or CSV.

### 5. Generate a Summary Report
- Navigate to the **Report** tab, enter an **experiment title** and click **"Download Report"**.

See the [User Manual](inst/app/www/user-manual.pdf) for detailed instructions.

## Sample Data Sets
Example files are installed with the package (`system.file("extdata", package = "LinearityApp")`):

| File | Assay |
|------|-------|
| `bca_semicolon.csv` | BCA assay, protein concentration (semicolon-separated CSV) |
| `elisa_smicb_comma.csv` | Sandwich ELISA, soluble MICB in plasma (comma-separated CSV) |
| `griess.xlsx` | Griess assay, nitrite production by immune cells (Excel) |
| `invalid/*.xlsx` | Files with typical input errors for testing the validation |

## CSV Format Options
- **Comma-separated (,)** uses a dot (.) as a decimal separator.
- **Semicolon-separated (;)** uses a comma (,) as a decimal separator.

## Development

```r
# install development tools
install.packages(c("roxygen2", "testthat", "covr", "lintr"))

roxygen2::roxygenise()        # update NAMESPACE and documentation
testthat::test_local()        # run tests
covr::package_coverage()      # test coverage
lintr::lint_package()         # static code analysis
```

Run the app from the source tree without installing: `shiny::runApp()` (uses `app.R`).

Continuous integration (GitHub Actions) runs `R CMD check` on Windows, macOS and Linux, measures test coverage (minimum 80 %) and runs `lintr` on every pull request.

`renv.lock` records package versions for reproducible deployment: `renv::restore()`.

### Project structure

| Path | Content |
|------|---------|
| `R/calibration.R` | background subtraction, model fitting, inverse prediction, flags |
| `R/io.R`, `R/validate.R`, `R/template.R` | reading, validation and templates |
| `R/aggregate.R`, `R/plots.R`, `R/report.R` | summary tables, plots, Word report |
| `R/i18n.R`, `inst/i18n/*.yml` | translations |
| `R/app_*.R`, `R/mod_*.R` | Shiny application and modules |
| `inst/extdata` | example data |
| `tests/testthat` | automated tests |

### Adding a language
Copy `inst/i18n/en.yml` to `inst/i18n/<code>.yml`, translate the texts (keep the keys and `{placeholders}`) and add the code to `supported_languages()` in `R/i18n.R`. The tests check that all dictionaries have the same keys and placeholders.

## Roadmap
See [ROADMAP.md](ROADMAP.md).

## License
This project is licensed under the MIT License.

## Contact
For questions or issues, open a GitHub issue or contact the developer at **smirnov.iv.mail@gmail.com**.
