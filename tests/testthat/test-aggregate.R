test_that('aggregation counts flagged wells and summarises quantified ones', {
    res <- data.frame(sample.name = c('A', 'A', 'A', 'B', 'B'),
                      dilution = c(1, 1, 2, 1, 1),
                      conc = c(1, 3, NA, NA, NA),
                      flag = c('OK', 'OK', '< LLOQ', '< LLOQ', 'OD <= blank'))

    out <- aggregate_result(res, 'sample.name')
    expect_equal(out$sample.name, c('A', 'B'))
    expect_equal(out$n, c(3, 2))
    expect_equal(out$n.quantified, c(2, 0))
    expect_equal(out$mean, c(2, NA))
    expect_equal(out$min, c(1, NA))
    expect_equal(out$max, c(3, NA))

    out2 <- aggregate_result(res, c('sample.name', 'dilution'))
    expect_equal(nrow(out2), 3)

    expect_null(aggregate_result(res, NULL))
})

test_that('aggregation works on real results', {
    r <- calculate(load_sample('bca'), 'LRM')
    out <- aggregate_result(r$result, c('sample.name', 'dilution'))
    expect_equal(nrow(out), 8)
    expect_true(all(out$n == 3))
})
