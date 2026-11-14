test_that('valid sample files pass validation', {
    for (name in names(sample_files)) {
        expect_s3_class(load_sample(name), 'data.frame')
    }
})

test_that('each incorrect input file gives its own error message', {
    expected <- c(
        '01_mandatory_column_missing.xlsx' = 'Mandatory sample.date column is missing',
        '02_non_numeric_od.xlsx' = 'Non-numeric values in OD column',
        '03_sample_type_missing.xlsx' = 'Missing values in the type column: sample',
        '04_dilution_column_empty.xlsx' = 'Non-numeric values in dilution column',
        '05_dilution_missing.xlsx' = 'Not all samples have dilution factors specified',
        '06_zero_dilution.xlsx' = 'All dilution factors must be positive',
        '07_three_standard_wells.xlsx' = 'Insufficient number of standard wells: 3'
    )
    files <- list.files(sample_path('invalid'))
    expect_setequal(files, names(expected))

    for (f in names(expected)) {
        data <- read_table(sample_path('invalid', f))
        expect_error(check_input_file(data), expected[[f]], fixed = TRUE, info = f)
        expect_error(check_input_file(data), class = 'lin_error')
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
