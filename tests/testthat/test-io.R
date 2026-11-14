test_that('sample files of all supported formats are read', {
    bca <- read_table(sample_path(sample_files[['bca']]))
    elisa <- read_table(sample_path(sample_files[['elisa']]))
    griess <- read_table(sample_path(sample_files[['griess']]))

    expect_equal(nrow(bca), 40)
    expect_true(is.numeric(bca$OD))        # semicolon + decimal comma
    expect_equal(bca$OD[1], 0.652199984)
    expect_equal(nrow(elisa), 96)
    expect_true(is.numeric(elisa$OD))      # comma + decimal dot
    expect_true(all(c('rep', 'type', 'OD') %in% names(griess)))
    expect_s3_class(griess, 'data.frame')
    expect_false(inherits(griess, 'tbl_df'))
})

test_that('unsupported file types are rejected', {
    path <- tempfile(fileext = '.txt')
    writeLines('a', path)
    expect_error(read_table(path), 'not supported')
})

test_that('CSV files written in both dialects are read back unchanged', {
    df <- data.frame(sample.name = c('A', 'B'), conc = c(1.5, NA), OD = c(0.123, 2))
    for (dialect in c('comma', 'semicolon')) {
        path <- tempfile(fileext = '.csv')
        save_csv(df, path, dialect)
        back <- read_table(path)
        expect_equal(back$conc, df$conc, info = dialect)
        expect_equal(back$OD, df$OD, info = dialect)
    }
    expect_error(save_csv(df, tempfile(), 'tab'), 'Unknown CSV dialect')
})

test_that('Excel files are written and read back', {
    df <- data.frame(sample.name = c('A', 'B'), conc = c(1.5, NA), OD = c(0.123, 2))
    path <- tempfile(fileext = '.xlsx')
    save_xlsx(df, path)
    back <- read_table(path)
    expect_equal(back$conc, df$conc)
    expect_equal(back$OD, df$OD)
})

test_that('date columns are formatted as dd.mm.yyyy', {
    df <- data.frame(sample.name = 1:2,
                     d = as.Date(c('2024-08-01', '2024-09-10')),
                     t = as.POSIXct(c('2024-08-01', '2024-09-10'), tz = 'UTC'))
    out <- format_input_table(df)
    expect_equal(out$d, c('01.08.2024', '10.09.2024'))
    expect_equal(out$t, c('01.08.2024', '10.09.2024'))
    expect_type(out$sample.name, 'character')
})
