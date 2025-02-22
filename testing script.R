
path <- 'D:/R projects/Linearity-App'
setwd(path)

source('./linearity.R')


############################ Reading Data ######################################

# Test for File Reading  
# The read_table() function should be able to read both .xlsx and .csv files.  
# It should automatically detect the delimiter in CSV files, whether comma-separated or semicolon-separated.  
# Expected output: A data frame containing the raw data.  


#   semicolon-delimited csv file
bca_assay <- read_table('./Sample Data/#01_BCA assay_semicolon-delimited.csv')
bca_assay


#   comma-delimited csv file
elisa_assay <- read_table('./Sample Data/#02_Sandwich ELISA (sMICB)_comma-delimited.csv')
elisa_assay


#   xlsx file
griess_assay <- read_table(path = './Sample Data/#03_Griess assay_Excel.xlsx')
griess_assay


###################### Checking  for Data Validity #############################

# Test for Input Validation Function  
# The check_input_file() function should detect and report common mistakes in the imported file.  
# If the file content is valid, it should return a data.frame.  

# Valid file checks: No error messages expected.  

griess_assay %>% check_input_file()
bca_assay %>% check_input_file()
elisa_assay %>% check_input_file()


# Invalid file checks: Error messages should be displayed.  

# List of invalid files:  
invalid_files <-
    list.files('./Sample Data/Incorrect input files/',
               pattern = '*\\.xlsx$', full.names = TRUE)


for (file in invalid_files) {
    tryCatch({
        file %>% read_table() %>% check_input_file()
    }, error = function(e) {
        message("Expected error for file ", basename(file), ": ", e$message)
    })
}



########################## Choosing one of the assays ##########################

# One of the assays is chosen for further tests.

assay_table <- bca_assay
# assay_table <- griess_assay
# assay_table <- elisa_assay


####################### Calculating model and results ##########################

# Test for Three Regression Models  
# Each fitting function is expected to return a list containing the following elements:  
# - $model_direct  
# - $model_reverse  
# - $standard_data  
# - $result  

# Linear Regression Model (LRM)
result_LRM <- calculate(assay_table, 'LRM')
result_LRM

# Quadratic Regression Model (QRM)
result_QRM <- calculate(assay_table, 'QRM')
result_QRM

# Deming Regression Model
result_deming <- calculate(assay_table, 'deming')
result_deming


################ Viewing Model Coefficient Tables ##############################

# Test for Coefficient Extraction Function  
# The get_coef_ci() function is expected to extract coefficients and 95% confidence  
# intervals from each model in the form of a data.frame.  

# LRM coefficients
get_coef_ci(result_LRM$model_direct)

# QRM coefficients
get_coef_ci(result_QRM$model_direct)

# Deming regression coefficients
get_coef_ci(result_deming$model_direct)


######################### Plotting Function ####################################

# Test for Plotting Function  
# The get_plots() function is expected to generate two plots for each model:  
# 1. Standard curve  
# 2. Fitted values vs. residuals plot  

# The X and Y axis titles of the first plot should be customizable.

# Plot LRM
get_plots(result_LRM,
          xtitle = 'X axis title',
          ytitle = 'Y axis title')

# Plot QRM
get_plots(result_QRM,
          xtitle = 'X axis title',
          ytitle = 'Y axis title')

# Plot Deming model
get_plots(result_deming,
          xtitle = 'X axis title',
          ytitle = 'Y axis title')


###################### Result Aggregation ######################################

# Test for Result Aggregation Function  
# The aggregate_result() function is expected to return a data.frame containing  
# the mean, minimum, and maximum concentration values for samples,  
# grouped by specified variables.  

aggregate_result(result_LRM$result, group_vars = c('sample.name', 'dilution'))

aggregate_result(result_QRM$result, group_vars = c('sample.name', 'dilution'))

aggregate_result(result_deming$result, group_vars = c('sample.name', 'dilution'))
