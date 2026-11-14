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
- Plate format: paste the plate reader grid into an Excel template with the plate layout
- Simple table format with only two required columns (`type`, `OD`); old files keep working
- Validation lists every problem in the file with the well or row where it occurs
- Blank wells and quality controls (QC) with recovery
- Calibration models: linear and quadratic (log-log or linear scales), four- and five-parameter logistic (4PL, 5PL)
- Weighting: 1/y, 1/y², 1/x, 1/x² or a variance function estimated from replicates
- Concentrations with 95 % confidence intervals (delta method)
- Comparison of models (AIC/BIC on the OD scale, R², back-calculated standards, lack-of-fit test)
- Exclusion of standard points by clicking on the curve, with a mandatory reason
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
path <- system.file("extdata", "bca_plate.xlsx", package = "LinearityApp")
data <- check_assay(read_assay(path))   # plate workbook or table
res <- calculate(data, model = "4PL", weights = "1/y2")
res$result                      # sample concentrations and flags
res$qc                          # quality controls (if any)
message_text(res$notes, "en")   # warnings
```

## How to Use - Brief Guideline

### 1. Generate a Data Template
- Choose the **template format**: *Plate (Excel)* (recommended) or *Table*.
- Set the parameters: start concentration, dilution step, number of standard levels and series, number of samples (and, for plates, sample replicates and plate size 96/384).
- Click **"Download Template"**.

### 2. Fill In the Template

**Plate workbook** - one sheet per kind of information:

| Sheet | Content |
|-------|---------|
| `Layout` | well codes on the plate grid: `S1`...`Sn` standards, `B` blank, `NC` negative control, `QC-L`/`QC-M`/`QC-H` quality controls, `X1`...`Xn` samples; empty wells are not used; wells with the same code are replicates |
| `OD` | optical densities pasted from the plate reader at the same positions (decimal comma accepted) |
| `Standards` | `level`, `conc` - filled in from the template parameters |
| `Samples` | `id`, `name`, `dilution` and any extra columns (e.g. `date`) |
| `QC` | `id`, `conc` - nominal concentrations (optional) |

The template generated in Russian has Russian sheet names; both languages are recognised on import. Column names and well codes are the same in all languages.

**Table** - one row per well. Required columns: `type` (`standard`, `sample`, `NC`, `blank`, `QC`) and `OD`; `conc` for standards and QC; optional `dilution` (default 1), `id`, `sample.name`, `well` and any metadata columns. The six-column format of earlier versions is accepted as is.

The background is the mean OD of `NC` wells, or of blank wells if there are no `NC` wells.

### 3. Import Data
- Click **"Browse"** and choose the file; the format is recognised automatically.
- All problems are listed in a table with the well or row where they occur. Errors stop the analysis, warnings do not.

### 4. Analyze Standard Curve
- Navigate to the **Standard Curve** tab.
- View the **fitted curve** and the **residuals plot**.
- Choose the model:

  | Model | Equation | Scales |
  |-------|----------|--------|
  | LRM | y = b·x + c | log10 conc / log10 OD, or linear ("Linear scales") |
  | QRM | y = a·x² + b·x + c | log10 conc / log10 OD, or linear |
  | 4PL | y = d + (a − d) / (1 + (x/c)^b) | OD linear, concentration log |
  | 5PL | y = d + (a − d) / (1 + (x/c)^b)^g | OD linear, concentration log |

  In the logistic models `a` is the response at zero concentration, `d` at infinite concentration, `c` the inflection point (EC50), `b` the slope and `g` the asymmetry. They are fitted with the Levenberg-Marquardt algorithm (`minpack.lm`) from several starting points; the fits agree with the `drc` package.
- Choose the **weighting**. On log scales (LRM, QRM) weighting is usually not needed; for 4PL/5PL `1/y²` or the variance function estimated from replicates (`1/y^θ`) is recommended.
- The **comparison of models** shows, for the same standards and weighting, AIC and BIC (on the OD scale, so log-log and logistic models are comparable), R², the mean and maximum error of back-calculated standards and the lack-of-fit p-value; the model with the lowest AIC is marked.
- **Exclude a standard point** by clicking on it and entering a reason; excluded points are shown as red crosses, listed with their reasons and reported. Click a cross to restore the point.

### 5. View and Export Results
- The **Results** table displays calculated analyte concentrations.
- Concentrations are calculated by inverting the fitted standard curve and multiplying by the dilution factor; `conc.lower` and `conc.upper` give the 95 % confidence interval (uncertainty of the curve and of a single OD measurement).
- Each sample well gets a **flag**. Only wells flagged `OK` receive a concentration; the others are left empty:
  - `< LLOQ` / `> ULOQ` - outside the range of standards used in the fit;
  - `OD <= NC` - optical density at or below the negative control;
  - `No solution` - the OD cannot be reached by the quadratic curve;
  - `Missing OD` - no optical density value.
- Quality controls are shown in a separate table with nominal and measured concentration and recovery (%).
- Warnings about excluded standards and unquantified samples are shown above the tables and in the report.
- Click **"Download Results"** to export as Excel or CSV.

### 6. Generate a Summary Report
- Navigate to the **Report** tab, enter an **experiment title** and click **"Download Report"**.

See the [User Manual](inst/app/www/user-manual.pdf) for detailed instructions.

## Sample Data Sets
Example files are installed with the package (`system.file("extdata", package = "LinearityApp")`):

| File | Assay |
|------|-------|
| `bca_plate.xlsx` | BCA assay in the plate format (same measurements as `bca_semicolon.csv`) |
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
| `R/calibration.R` | background subtraction, standards, flags, `calculate()` |
| `R/models.R` | calibration models, weighting, inverse prediction with confidence intervals, model comparison |
| `R/io.R`, `R/plate.R`, `R/validate.R`, `R/template.R` | reading (table and plate formats), validation, templates |
| `R/aggregate.R`, `R/plots.R`, `R/report.R` | summary tables, plots, Word report |
| `R/i18n.R`, `inst/i18n/*.yml` | translations |
| `R/app_*.R`, `R/mod_*.R` | Shiny application and modules |
| `inst/extdata`, `data-raw` | example data and the script that creates them |
| `tests/testthat` | automated tests |

### Adding a language
Copy `inst/i18n/en.yml` to `inst/i18n/<code>.yml`, translate the texts (keep the keys and `{placeholders}`) and add the code to `supported_languages()` in `R/i18n.R`. The tests check that all dictionaries have the same keys and placeholders.

## Roadmap
See [ROADMAP.md](ROADMAP.md).

## License
This project is licensed under the MIT License.

## Contact
For questions or issues, open a GitHub issue or contact the developer at **smirnov.iv.mail@gmail.com**.
