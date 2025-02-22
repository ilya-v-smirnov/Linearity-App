
path <- 'D:/R projects/Linearity-App'
setwd(path)

source('./linearity.R')


############################ Reading Data ######################################


#   semicolon-delimited csv file
bca_assay <- read_table('./Sample Data/#01_BCA assay_semicolon-delimited.csv')
bca_assay


#   comma-delimited csv file
elisa_assay <- read_table('./Sample Data/#02_Sandwich ELISA (sMICB)_comma-delimited.csv')
elisa_assay


#   xlsx file
griess_assay <- read_table(path = './Sample Data/#03_Griess assay_Excel.xlsx')
griess_assay


######################## Checking Data Validity ################################

# Correct files

griess_assay %>% check_input_file()
bca_assay %>% check_input_file()
elisa_assay %>% check_input_file()

# Incorrect files

'./Sample Data/Incorrect input files/#01_Mundatory column missing.xlsx' %>% 
    read_table() %>%
    check_input_file()

'./Sample Data/Incorrect input files/#02_Not allowed values in OD.xlsx' %>% 
    read_table() %>%
    check_input_file()

'./Sample Data/Incorrect input files/#02_Not allowed values in OD.xlsx' %>% 
    read_table() %>%
    check_input_file()

'./Sample Data/Incorrect input files/#03_Sample is missing in type.xlsx' %>% 
    read_table() %>%
    check_input_file()

'./Sample Data/Incorrect input files/#04 Dilution column not filled.xlsx' %>% 
    read_table() %>%
    check_input_file()

'./Sample Data/Incorrect input files/#05 Not all dilutions filled.xlsx' %>% 
    read_table() %>%
    check_input_file()

'./Sample Data/Incorrect input files/#06 0 dilution present.xlsx' %>% 
    read_table() %>%
    check_input_file()

'./Sample Data/Incorrect input files/#07 Only 3 standard wells.xlsx' %>% 
    read_table() %>%
    check_input_file()


#### Choosing one of the assays ###
assay_table <- bca_assay


# Calculating model and results
result_LRM <- calculate(assay_table, 'LRM')
result_LRM

result_QRM <- calculate(assay_table, 'QRM')
result_QRM

result_deming <- calculate(assay_table, 'deming')
result_deming


# Table of model coefficients
get_coef_ci(result_LRM$model_direct)
get_coef_ci(result_QRM$model_direct)
get_coef_ci(result_deming$model_direct)


# Plots
get_plots(result_QRM,
          xtitle = 'X axis title',
          ytitle = 'Y axis title')

# Aggregation of the result by sample.name and dilution
aggregate_result(result_QRM$result, aggr.pars = c('sample.name', 'dilution'))

