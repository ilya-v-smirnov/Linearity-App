logistic <- function(x, a, b, c, d, g = 1) d + (a - d) / (1 + (x / c)^b)^g
levels_7 <- c(1, 2, 4, 8, 16, 32, 64)

# Background-subtracted standards in duplicate with optional noise
make_standards <- function(f, conc = levels_7, sd = 0, reps = 2, seed = 1) {
    set.seed(seed)
    x <- rep(conc, each = reps)
    data.frame(conc = x, OD = f(x) + stats::rnorm(length(x), 0, sd))
}

test_that('4PL and 5PL recover known parameters and invert exactly', {
    std <- make_standards(function(x) logistic(x, 0.02, 1.3, 12, 2.4))
    fit <- fit_curve(std, '4PL')
    expect_equal(unname(fit$par[c('a', 'b', 'd')]), c(0.02, 1.3, 2.4), tolerance = 1e-5)
    expect_equal(exp(fit$par[['lc']]), 12, tolerance = 1e-5)
    expect_equal(inverse_fun(fit, logistic(c(3, 20), 0.02, 1.3, 12, 2.4)), c(3, 20), tolerance = 1e-5)
    tab <- get_coef_ci(fit)
    expect_equal(tab$coef, c('a', 'b', 'c', 'd'))
    expect_equal(tab$est[tab$coef == 'c'], 12, tolerance = 1e-5)

    std5 <- make_standards(function(x) logistic(x, 0.05, 1.1, 10, 2.5, g = 0.5),
                           conc = c(0.5, 1, 2, 4, 8, 16, 32, 64, 128))
    fit5 <- fit_curve(std5, '5PL')
    expect_equal(unname(fit5$par[['g']]), 0.5, tolerance = 1e-3)
    expect_equal(inverse_fun(fit5, logistic(5, 0.05, 1.1, 10, 2.5, 0.5)), 5, tolerance = 1e-4)
})

test_that('4PL agrees with drc', {
    skip_if_not_installed('drc')
    std <- make_standards(function(x) logistic(x, 0.05, 1.2, 10, 2), conc = c(0.5, levels_7, 128, 256),
                          sd = 0.02, seed = 3)
    ours <- fit_curve(std, '4PL')
    ref <- drc::drm(OD ~ conc, data = std, fct = drc::LL.4())
    cf <- stats::coef(ref)
    expect_lte(ours$rss, sum(stats::residuals(ref)^2) * (1 + 1e-6))
    # drc: c = lower, d = upper, e = EC50, b = -slope
    expect_equal(ours$par[['a']], unname(cf['c:(Intercept)']), tolerance = 1e-3)
    expect_equal(ours$par[['d']], unname(cf['d:(Intercept)']), tolerance = 1e-3)
    expect_equal(exp(ours$par[['lc']]), unname(cf['e:(Intercept)']), tolerance = 1e-3)
    expect_equal(ours$par[['b']], -unname(cf['b:(Intercept)']), tolerance = 1e-3)

    # A curve whose upper asymptote is not reached: parameters are not identifiable,
    # but our fit must not be worse than drc
    bca <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), '4PL')$standard_data
    ref <- drc::drm(OD ~ conc, data = bca, fct = drc::LL.4())
    expect_lte(fit_curve(bca, '4PL')$rss, sum(stats::residuals(ref)^2) * (1 + 1e-6))
})

test_that('logistic fits survive parameters that are not identifiable', {
    # flat curve: the slope is zero, so the inflection point does not affect the fit
    v <- LinearityApp:::logistic_vcov('4PL', c(a = 0.1, b = 0, lc = 1, d = 1), levels_7,
                                      rep(1, 7), sigma = 0.01)
    expect_false(v$identifiable)
    expect_false(v$exact)
    expect_true(all(is.finite(v$vcov)))
    expect_equal(dimnames(v$vcov)[[1]], c('a', 'b', 'lc', 'd'))

    std <- make_standards(function(x) logistic(x, 0.05, 1.2, 10, 2), sd = 0.02)
    v <- LinearityApp:::logistic_vcov('4PL', fit_curve(std, '4PL')$par, std$conc, rep(1, 14), 0.02)
    expect_true(v$identifiable)

    # a curve far from its upper asymptote (BCA example): concentrations still work
    bca <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), '4PL')
    expect_equal(sum(bca$result$flag == 'OK'), 24)
    expect_true(all(is.finite(bca$result$conc.lower)))
    expect_match(message_text(bca$notes), 'not determined by the standards', all = FALSE)

    # with a pseudo-inverse the parameter intervals are not shown
    fake <- bca$fit
    fake$vcov_exact <- FALSE
    expect_true(all(is.na(get_coef_ci(fake)$ci.lower)))

    # the log-likelihood matches logLik() of the equivalent nls fit
    std <- make_standards(function(x) logistic(x, 0.05, 1.2, 10, 2), sd = 0.02, seed = 3)
    ours <- fit_curve(std, '4PL')
    ref <- stats::nls(OD ~ d + (a - d) / (1 + exp(b * (log(conc) - lc))), data = std,
                      start = as.list(ours$par))
    expect_equal(ours$loglik, as.numeric(stats::logLik(ref)), tolerance = 1e-6)
    expect_equal(unname(sqrt(diag(ours$vcov))), unname(sqrt(diag(stats::vcov(ref)))), tolerance = 1e-3)
})

test_that('confidence intervals of concentrations cover the true value', {
    f <- function(x) logistic(x, 0.05, 1.2, 10, 2)
    covered <- vapply(1:40, function(seed) {
        std <- make_standards(f, sd = 0.02, seed = seed)
        fit <- fit_curve(std, '4PL')
        set.seed(1000 + seed)
        od <- f(8) + stats::rnorm(1, 0, 0.02)
        ci <- inverse_predict_ci(fit, od)
        ci$lower <= 8 && 8 <= ci$upper
    }, logical(1))
    expect_gte(mean(covered), 0.85)

    fit <- fit_curve(make_standards(f, sd = 0.02), '4PL')
    ci <- inverse_predict_ci(fit, c(f(8), -1, NA))
    expect_true(ci$lower[1] < ci$conc[1] && ci$conc[1] < ci$upper[1])
    expect_gt(ci$cv[1], 0)
    expect_true(all(is.na(ci[2:3, ])))
})

test_that('weights follow the chosen scheme', {
    std <- make_standards(function(x) 0.1 * x, conc = c(1, 2, 4, 8, 16))
    w <- function(scheme) fit_curve(std, 'LRM', linear = TRUE, weights = scheme)$w
    expect_equal(mean(w('1/y2')), 1)
    expect_equal(w('1/y2') / w('1/y2')[1], (0.1 / (0.1 * std$conc))^2)
    expect_equal(w('1/x') / w('1/x')[1], 1 / std$conc)
    expect_equal(w('none'), rep(1, nrow(std)))
    expect_error(fit_curve(std, 'LRM', weights = '1/z'), 'Unknown weighting')

    # variance proportional to mean^2: theta close to 2
    set.seed(5)
    x <- rep(c(1, 2, 4, 8, 16, 32), each = 6)
    y <- 0.05 * x * exp(stats::rnorm(length(x), 0, 0.1))
    fit <- fit_curve(data.frame(conc = x, OD = y), 'LRM', linear = TRUE, weights = 'power')
    expect_equal(fit$theta, 2, tolerance = 0.5)

    single <- data.frame(conc = c(1, 2, 4, 8), OD = c(0.1, 0.2, 0.4, 0.8))
    fit <- fit_curve(single, 'LRM', linear = TRUE, weights = 'power')
    expect_equal(fit$theta, 0)
    expect_match(message_text(fit$notes), 'Too few replicates')
})

test_that('lack-of-fit test detects a wrong model', {
    std <- make_standards(function(x) 0.002 * x^2 + 0.05 * x, sd = 0.002)
    expect_lt(lack_of_fit(fit_curve(std, 'LRM', linear = TRUE))$p, 0.001)
    expect_gt(lack_of_fit(fit_curve(std, 'QRM', linear = TRUE))$p, 0.01)
    no_reps <- make_standards(function(x) 0.1 * x, reps = 1)
    expect_true(is.na(lack_of_fit(fit_curve(no_reps, 'LRM', linear = TRUE))$p))

    data <- synthetic_data(function(x) 0.002 * x^2 + 0.05 * x, conc = levels_7)
    data$OD[1:7] <- data$OD[1:7] + 0.003
    r <- calculate(data, 'LRM', linear = TRUE)
    expect_match(message_text(r$notes), 'lack-of-fit test is significant', all = FALSE)
})

test_that('models are compared on a common scale', {
    std <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), '4PL')$standard_data
    cmp <- compare_models(std)
    expect_equal(cmp$model, calibration_models())
    expect_equal(sum(cmp$recommended), 1)
    expect_true(all(cmp$R2 > 0.99))
    expect_true(all(cmp$mean.abs.error < 10))

    lrm <- fit_curve(std, 'LRM')
    shift <- 2 * sum(log(lrm$y * log(10)))
    expect_equal(information_criteria(lrm)[['AIC']], stats::AIC(lrm$object) + shift)

    few <- std[std$conc %in% unique(std$conc)[1:5], ]
    cmp <- compare_models(few)
    expect_true(is.na(cmp$AIC[cmp$model == '5PL']))
    expect_false(is.na(cmp$AIC[cmp$model == '4PL']))

    tab <- LinearityApp:::comparison_table(compare_models(std), 'ru')
    expect_equal(names(tab)[1], tr('compare.model', 'ru'))
})

test_that('standard points can be excluded with a reason', {
    data <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    keys <- well_keys(data)
    expect_equal(keys[1:2], c('A1', 'A2'))
    expect_equal(well_keys(data.frame(x = 1:2)), c('row 2', 'row 3'))

    ex <- data.frame(key = c('A1', 'A2'), reason = c('pipetting error', 'bubble'))
    r <- calculate(data, 'QRM', exclude = ex)
    expect_false(any(r$standard_data$.key %in% c('A1', 'A2')))
    expect_equal(r$exclusions$key, c('A1', 'A2'))
    expect_equal(r$exclusions$reason, c('pipetting error', 'bubble'))
    expect_equal(r$calibration_range[2], 1)       # the top level (2) is gone
    expect_match(message_text(r$notes), 'Excluded (2)', fixed = TRUE, all = FALSE)
    expect_match(message_text(r$notes, 'ru'), tr('flag.excluded', 'ru'), fixed = TRUE, all = FALSE)

    expect_equal(nrow(r$standard_data), nrow(calculate(data, 'QRM')$standard_data) - 2)
    expect_equal(nrow(r$excluded_standards), 2)
    expect_s3_class(get_plots(r), 'ggplot')
})

test_that('logistic curves have no solution beyond the asymptotes', {
    f <- function(x) logistic(x, 0, 1.2, 10, 2)
    data <- synthetic_data(f, conc = levels_7, sample_od = c(f(5), 2.5, 0.5))
    r <- calculate(data, '4PL', round_fun = function(x, ...) x)
    expect_equal(r$result$conc[1], 5, tolerance = 1e-4)
    expect_equal(r$result$flag[2], 'No solution')
    expect_true(r$result$conc.lower[1] <= 5 && 5 <= r$result$conc.upper[1])
    expect_equal(back_calculate(r$fit)$recovery, rep(100, 14), tolerance = 1e-4)
})

test_that('every model works on the example data in both scales', {
    data <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    for (m in calibration_models()) {
        for (w in c('none', '1/y2', 'power')) {
            r <- calculate(data, m, weights = w)
            expect_equal(sum(r$result$flag == 'OK'), 24, info = paste(m, w))
            expect_true(all(r$result$conc.lower <= r$result$conc & r$result$conc <= r$result$conc.upper),
                        info = paste(m, w))
        }
    }
})
