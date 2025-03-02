# Linearity App

## Overview
The **Linearity App** is a user-friendly tool designed to calculate analyte concentrations from laboratory assay data using standard titration methods. It supports common assays like **sandwich ELISA, BCA assay, and Griess assay** and processes data from **Excel (.xlsx) and CSV (.csv) files**.

## Features
- Generates structured data templates for easy data entry
- Imports and validates assay data
- Supports linear and quadratic regression models
- Provides detailed standard curve analysis and residual plots
- Exports results in Excel or CSV format
- Generates reports in Word (.docx) format

## Installation
Clone this repository and install required dependencies before running the app:

```bash
git clone https://github.com/ilya-v-smirnov/LinearityApp.git
cd LinearityApp
R -e 'install.packages("shiny")'  # Install Shiny if not already installed
R -e 'install.packages("deming")' # Install deming package for Deming regression
R -e 'install.packages("ggplot2")' # Install ggplot2 package if not available
R -e 'install.packages("cowplot")' # Install cowplot package
R -e 'install.packages("xlsx")' # Install xlsx package if not already installed
R -e 'install.package("officer") # Install officer package to generate Word files with reports
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
- Choose between **linear regression** (LRM/QRM) or **quadratic regression**.
- Enable or disable **log transformation** for better fit.

### 4. View and Export Results
- The **Results Table** displays calculated analyte concentrations.
- Adjust rounding options if needed.
- Click **"Download Results"** to export as Excel or CSV.

### 5. Generate a Summary Report
- Navigate to the **Report** tab.
- Enter an **experiment title**.
- Click **"Download Report"** to generate a detailed Word document.

  See **User Manual** for detailed instructions.

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
- Shiny (version >= 1.71)
- deming (version >= 1.4)
- ggplot (version >= 3.4.4)
- cowplot (version >= 1.1.1)
- officer (version >= 0.4.2)

## License
This project is licensed under the MIT License.

## Contact
For questions or issues, open a GitHub issue or contact the developer at **smirnov.iv.mail@gmail.com**.

