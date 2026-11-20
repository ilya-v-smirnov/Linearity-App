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
- Calibration models: linear and quadratic on the original scale, log–log linear and log–log quadratic, four- and five-parameter logistic (4PL, 5PL)
- Weighting: 1/y, 1/y², 1/x, 1/x² or a variance function estimated from replicates
- Concentrations with 95 % confidence intervals (delta method)
- Comparison of models (AIC/BIC on the OD scale, R², back-calculated standards, lack-of-fit test)
- Exclusion of standard points by clicking on the curve, with a mandatory reason
- Quality assessment of each run (ICH M10 defaults, editable): back-calculated standards, LLOQ/ULOQ from accepted standard levels, replicate CV, QC bias with the 4-6-X rule, dilution linearity, run verdict
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
res$quality$accepted            # run verdict; see also res$quality$standards, $samples, $dilution
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

  | Model (code) | Equation | Scales |
  |--------------|----------|--------|
  | Linear (`LIN`) | OD = a + b·C | original |
  | Quadratic (`QUAD`) | OD = a + b·C + c·C² | original |
  | Log–log linear (`LRM`) | log10(OD) = a + b·log10(C) | log10 conc / log10 OD |
  | Log–log quadratic (`QRM`) | log10(OD) = a + b·log10(C) + c·[log10(C)]² | log10 conc / log10 OD |
  | 4PL | OD = d + (a − d) / (1 + (C/c)^b) | OD linear, concentration log |
  | 5PL | OD = d + (a − d) / (1 + (C/c)^b)^g | OD linear, concentration log |

  `C` is the concentration and `OD` the background-subtracted signal. The intercept of the Linear model is estimated freely. In R, `calculate(data, "LRM", linear = TRUE)` (and the same for `QRM`) still fits on the original scale, as `LIN` and `QUAD`.
- A **quadratic curve that turns within the standards** (vertex `C* = −b/(2c)` strictly inside the range; for the log–log quadratic on the log10 scale of `C`) gives two concentrations for one OD. The app then reports `Non-monotonic within calibration range` and calculates no concentrations; the curve and the residuals stay on view. Choose another model, or tick **Set a monotonic interval** and enter bounds on one side of the turning point: the curve is inverted on that branch only, the quantification range is limited to the interval, and standard levels outside it are marked `outside the interval` (`calculate(..., interval = c(1, 3))` in R). A zero quadratic coefficient gives the linear inverse; a horizontal line has no inverse.

  In the logistic models `a` is the response at zero concentration, `d` at infinite concentration, `c` the inflection point (EC50), `b` the slope and `g` the asymmetry. They are fitted with the Levenberg-Marquardt algorithm (`minpack.lm`) from several starting points; the fits agree with the `drc` package.
- Choose the **weighting**. For the log–log models weighting is usually not needed; for Linear, Quadratic, 4PL and 5PL `1/y²` or the variance function estimated from replicates (`1/y^θ`) is recommended.
- The **comparison of models** fits every model, with the chosen weighting, to the common set of calibration wells (background-subtracted standards after your exclusions), whichever model is selected. Each model uses all wells it can: the log–log models leave out wells with OD ≤ blank, the others keep them. The table shows AIC and BIC (on the OD scale, so log–log, original-scale and logistic models are comparable), R², the mean and maximum error of back-calculated standards (wells without a back-calculated concentration are counted in **Not back-calculated**) and the lack-of-fit p-value. While all models use the same wells, the model with the lowest AIC is marked; otherwise a **Wells used** column (e.g. `15/16`) and a note naming the left-out wells replace the mark, because AIC values on different data are not comparable.
- **Exclude a standard point** by clicking on it and entering a reason; excluded points are shown as red crosses, listed with their reasons and reported. Click a cross to restore the point. After an exclusion the model is refitted: the curve on all standards stays on the plot as a dashed line, and a tab **Original vs refitted** appears (it is removed when all points are restored). It compares the two calibrations - the coefficients (`Coef`, `Original`, `Refitted`), the back-calculated concentration and relative error of every standard under each model, and the mean and maximum absolute relative error and the number of standards meeting the acceptance criteria. The refitted model is the one accepted: it is used for the results and its parameters go to the report, together with the comparison (detailed report).

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

### 6. Check the Quality of the Run
The quality assessment is split between two steps:

- **Standard curve → Model parameters** shows the verdict of the run (accepted or rejected, LLOQ – ULOQ, number of accepted standard levels), the model coefficients, the back-calculated standards and the quality of the curve. The acceptance criteria of the standards are in the sidebar of this step.
- **Result Summary** assesses the samples. The columns ticked under **Group results by** define the replicates: each row of the summary is a replicate group, its CV is checked against the limit (flag `High CV`), and the dilutions of a sample are compared within the rows without the dilution column (flag `Dilution non-linear`). The **Table** tab shows the summary with flags, the dilution linearity and the quality controls; the **Infographics** tab shows summary tiles, the concentrations of each group with mean ± SD, the CV of the replicates against the limit, the wells by quantification status, the dilution linearity and the accuracy of the QC. The acceptance criteria of the samples, QC and dilutions are in the sidebar of this step.

| Check | How | Default limit (ICH M10, ligand binding assays) |
|-------|-----|-------------------------------------------------|
| Standards | bias of the mean back-calculated concentration of each level; CV of the back-calculated concentrations of the replicates | ±20 % (±25 % at the lowest and highest level); CV ≤ 20 % (25 %) |
| Back-calculation | each level counts its wells: planned, measured, fitted and back-calculated (`Back-calculated wells`, e.g. `1/2`). A fitted well without a concentration (e.g. OD outside the fitted asymptotes) makes the level `Incomplete back-calculation`, which is not accepted; the bias then uses the back-calculated wells and the CV is unavailable. A level planned in replicate that keeps one result (the others excluded or without an OD) is accepted by its bias, marked `single result`. A level planned as a single well is accepted by its bias if **Accept single-well standard levels** is ticked | — |
| Calibration | number of accepted standard levels | ≥ 75 % and ≥ 6 levels |
| LLOQ / ULOQ | lowest and highest accepted standard level; samples outside are flagged `< LLOQ` / `> ULOQ` | — |
| Quality controls | bias of each QC well; rule: ≥ 2/3 of all QC and ≥ 50 % of each level within limits | ±20 % (±25 % near LLOQ/ULOQ) |
| Samples | mean, SD and CV of the replicates of each row of the summary; flag `High CV` | CV ≤ 20 % (25 % near LLOQ/ULOQ) |
| Dilution linearity | for samples measured at several dilutions: deviation of each dilution from the mean and CV between dilutions; flag `Dilution non-linear` | ±20 %, CV ≤ 20 % |
| Curve | R², residual standard error, lack-of-fit test | warning if R² < 0.99 or lack of fit p < 0.05 |

The run is rejected if the calibration or the QC rule fails; replicate CV, dilution linearity and curve quality give warnings. For chromatographic assays use 15 % / 20 %; the quantification range can also be set to all standards used in the fit. From the console, `assess_samples(res$result, res$calibration_range, group_vars = c("sample.name", "dilution"))` assesses the samples with any grouping.

### 7. Generate a Summary Report
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
| `R/quality.R` | acceptance criteria, standards, replicates, QC, dilution linearity, run verdict |
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
