test_that('valid sample files pass validation', {
    for (name in names(sample_files)) {
        expect_s3_class(load_sample(name), 'data.frame')
    }
})

test_that('each incorrect input file gives its own error message', {
    expected <- c(
        '01_mandatory_column_missing.xlsx' = 'Mandatory type column is missing',
        '02_non_numeric_od.xlsx' = 'Non-numeric values in OD column',
        '03_sample_type_missing.xlsx' = 'Missing values in the type column: sample',
        '04_dilution_column_empty.xlsx' = 'Not all samples have dilution factors specified',
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
    data$type[1] <- 'calibrator'
    expect_error(check_input_file(data), 'Not allowed values in the type column: calibrator (row 2)',
                 fixed = TRUE)
})

test_that('tables without the dilution or conc column are reported, not crashed on', {
    data <- load_sample('bca')
    keys <- function(tab) vapply(validate_assay(tab), function(p) p$message$key, '')

    expect_equal(keys(data[setdiff(names(data), 'dilution')]), 'error.dilution_missing')
    expect_equal(keys(data[setdiff(names(data), 'conc')]), 'error.std_conc_missing')
    expect_error(check_assay(data[c('type', 'OD')]), class = 'lin_error')

    # normalize_table() fills in the default dilution
    expect_s3_class(check_input_file(data[setdiff(names(data), 'dilution')]), 'data.frame')
})
