test_that('template uses the dilution step and number of levels', {
    t <- make_template(start = 100, step = 3, n.standard = 2, n.sample = 5, n.levels = 5)

    expect_equal(nrow(t), 2 * (5 + 1) + 5)
    expect_equal(names(t), c('sample.name', 'sample.date', 'type', 'dilution', 'conc', 'OD'))
    expect_equal(t$conc[1:5], signif(100 / 3^(0:4), 3))
    expect_equal(t$type[1:6], c(rep('standard', 5), 'blank'))
    expect_equal(sum(t$type == 'sample'), 5)
    expect_true(all(t$dilution[t$type == 'sample'] == 1))
})

test_that('default template has 7 two-fold levels', {
    t <- make_template()
    expect_equal(t$conc[1:7], signif(100 / 2^(0:6), 3))
})

test_that('invalid template parameters are rejected', {
    expect_error(make_template(step = 1), 'greater than 1')
    expect_error(make_template(start = 0), 'positive')
    expect_error(make_template(n.levels = 2), 'At least 3')
})

test_that('a filled template passes validation after an Excel round trip', {
    t <- make_template(start = 10, step = 2, n.standard = 2, n.sample = 3, n.levels = 6)
    path <- tempfile(fileext = '.xlsx')
    save_xlsx(t, path)
    back <- read_table(path)
    expect_true(all(is.na(back$OD)))

    back$OD <- c(rep(c(1.6, 0.85, 0.45, 0.25, 0.15, 0.1, 0.05), 2), 0.3, 0.5, 0.7)
    back$sample.name[back$type == 'sample'] <- c('A', 'B', 'C')
    expect_s3_class(check_input_file(back), 'data.frame')
})

test_that('standard rows can follow the samples', {
    t <- make_template(n.standard = 1, n.sample = 3, n.levels = 3, standards = 'after')
    expect_equal(t$type, c(rep('sample', 3), rep('standard', 3), 'blank'))
})
