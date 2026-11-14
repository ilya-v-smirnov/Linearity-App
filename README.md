# Linearity App

## Overview
The **Linearity App** is a user-friendly tool designed to calculate analyte concentrations from laboratory assay data using standard titration methods. It supports common assays like **sandwich ELISA, BCA assay, and Griess assay** and processes data from **Excel (.xlsx) and CSV (.csv) files**.

## Features
- Generates structured data templates for easy data entry
- Imports and validates assay data
- Supports linear and quadratic regression models (log-log or linear scales)
- Provides detailed standard curve analysis and residual plots
- Flags samples outside the calibration range instead of extrapolating
- Exports results in Excel or CSV format
- Generates reports in Word (.docx) format

## Installation
Clone this repository and install required dependencies before running the app:

```bash
git clone https://github.com/ilya-v-smirnov/Linearity-App.git
cd Linearity-App
R -e 'install.packages(c("shiny", "dplyr", "ggplot2", "cowplot", "readxl", "openxlsx", "officer"))'
```

Run the app:

```bash
R -e 'shiny::runApp(launch.browser = TRUE)'
```

## How to Use - Brief Guideline

### 1. Generate a Data Template
- Set **template parameters** like start concentration, dilution step, and number of samples.
- Click **"Download Template"** to generate a CSV or Excel file.

### 2. Import Data
- Fill in the template with:
  - Sample names, dates, well types (standard, sample, NC)
  - Dilution factors and optical density (OD) values
- Click **"Browse"** to upload the modified file.
- The app automatically validates the data before proceeding.

### 3. Analyze Standard Curve
- Navigate to the **Standard Curve** tab.
- View the **fitted regression line** and **residuals plot**.
- Choose between **linear regression** (LRM) or **quadratic regression** (QRM).
- Enable or disable **log transformation** for better fit.

### 4. View and Export Results
- The **Results Table** displays calculated analyte concentrations.
- Concentrations are calculated by inverting the fitted standard curve and multiplying by the dilution factor.
- Each sample well gets a **flag**. Only wells flagged `OK` receive a concentration; the others are left empty:
  - `< LLOQ` / `> ULOQ` - outside the range of standards used in the fit;
  - `OD <= NC` - optical density at or below the negative control;
  - `No solution` - the OD cannot be reached by the quadratic curve;
  - `Missing OD` - no optical density value.
- Warnings about excluded standards and unquantified samples are shown above the tables and in the report.
- Adjust rounding options if needed.
- Click **"Download Results"** to export as Excel or CSV.

### 5. Generate a Summary Report
- Navigate to the **Report** tab.
- Enter an **experiment title**.
- Click **"Download Report"** to generate a detailed Word document.

  See the [User Manual](www/User%20Manual.pdf) for detailed instructions.

## Sample Data Sets
The app includes three real-world datasets for testing:
1. **BCA assay** - Measures protein concentration
2. **Sandwich ELISA** - Detects soluble MICB in plasma
3. **Griess assay** - Evaluates nitrite production in immune cells

## CSV Format Options
- **Comma-separated (,)** uses a dot (.) as a decimal separator.
- **Semicolon-separated (;)** uses a comma (,) as a decimal separator.
- Users can specify the desired format before exporting files.

## Dependencies
- R (version >= 4.1.3)
- shiny (version >= 1.7.1)
- dplyr (version >= 1.1.0)
- ggplot2 (version >= 3.4.4)
- cowplot (version >= 1.1.1)
- readxl (version >= 1.4.0)
- openxlsx (version >= 4.2.5)
- officer (version >= 0.4.2)

## Tests
Automated tests use [testthat](https://testthat.r-lib.org/):

```bash
R -e 'testthat::test_dir("tests/testthat")'
```

## Roadmap
See [ROADMAP.md](ROADMAP.md).

## License
This project is licensed under the MIT License.

## Contact
For questions or issues, open a GitHub issue or contact the developer at **smirnov.iv.mail@gmail.com**.

