test_that('valid sample files pass validation', {
    for (name in names(sample_files)) {
        expect_s3_class(load_sample(name), 'data.frame')
    }
})

test_that('each incorrect input file gives its own error message', {
    expected <- c(
        '#01_Mandatory column missing.xlsx' = 'Mandatory sample.date column is missing',
        '#02_Not allowed values in OD.xlsx' = 'Non-numeric values in OD column',
        '#03_Sample is missing in type.xlsx' = 'Missing values in the type column: sample',
        '#04 Dilution column not filled.xlsx' = 'Non-numeric values in dilution column',
        '#05 Not all dilutions filled.xlsx' = 'Not all samples have dilution factors specified',
        '#06 0 dilution present.xlsx' = 'All dilution factors must be positive',
        '#07 Only 3 standard wells.xlsx' = 'Insufficient number of standard wells: 3'
    )
    files <- list.files(sample_path('Incorrect input files'))
    expect_setequal(files, names(expected))

    for (f in names(expected)) {
        data <- read_table(sample_path('Incorrect input files', f))
        expect_error(check_input_file(data), expected[[f]], fixed = TRUE, info = f)
    }
})

test_that('standards must have positive concentrations', {
    data <- load_sample('bca')
    bad <- data
    bad$conc[1] <- NA
    expect_error(check_input_file(bad), 'concentrations specified')
    bad <- data
    bad$conc[1] <- 0
    expect_error(check_input_file(bad), 'must be positive')
})

test_that('unknown type values are reported', {
    data <- load_sample('bca')
    data$type[1] <- 'blank'
    expect_error(check_input_file(data), 'Not allowed values in the type column: blank')
})
