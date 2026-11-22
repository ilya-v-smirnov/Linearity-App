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

test_that('a 5PL gives no concentration for an OD beyond its asymptotes, whatever g', {
    std <- make_standards(function(x) logistic(x, 0.05, 1.1, 10, 2.5, g = 0.5),
                          conc = c(0.5, 1, 2, 4, 8, 16, 32, 64, 128))
    fit <- fit_curve(std, '5PL')
    # OD between 0 and 1; with 1/g = 2 the power would turn the negative ratio of OD 1.5 positive
    fit$par[c('a', 'b', 'lc', 'd', 'g')] <- c(0, 1, log(10), 1, 0.5)
    expect_equal(LinearityApp:::curve_fun(fit, 30), 0.5)
    expect_equal(inverse_fun(fit, 0.5), 30)
    expect_true(is.na(inverse_fun(fit, 1.5)))
    calc <- inverse_fun(fit, c(-0.2, 0, 1, 1.5, 0.5))
    expect_equal(calc, c(NA, NA, NA, NA, 30))
    expect_equal(LinearityApp:::inverse_reason(fit, c(1.5, -0.2, 0.5)),
                 c('outside_asymptotes', 'outside_asymptotes', NA))
})

test_that('Linear and Quadratic models are fitted on the original scale', {
    f <- function(x) 0.08 + 0.02 * x
    std <- make_standards(f, conc = c(1, 2, 5, 10, 20, 50))
    fit <- fit_curve(std, 'LIN')
    expect_false(fit$log_scale)
    # the intercept is estimated freely
    expect_equal(unname(fit$par), c(0.08, 0.02), tolerance = 1e-10)
    tab <- get_coef_ci(fit)
    expect_equal(tab$coef, c('a', 'b'))
    expect_equal(tab$est, c(0.08, 0.02), tolerance = 1e-10)
    expect_equal(back_calculate(fit)$calc, std$conc, tolerance = 1e-10)
    # the former call of LRM on linear scales gives the same fit
    expect_equal(fit_curve(std, 'LRM', linear = TRUE)$par, fit$par)

    # Power is a separate log-log model
    pow <- fit_curve(std, 'LRM')
    expect_true(pow$log_scale)
    expect_gt(max(abs(pow$par - fit$par)), 0.01)

    # OD = a + b*C + c*C^2
    g <- function(x) 0.05 + 0.03 * x - 2e-4 * x^2
    std2 <- make_standards(g, conc = c(1, 2, 5, 10, 20, 50))
    quad <- fit_curve(std2, 'QUAD')
    expect_false(quad$log_scale)
    expect_equal(unname(quad$par[c('a', 'b', 'c')]), c(0.05, 0.03, -2e-4), tolerance = 1e-8)
    expect_equal(inverse_fun(quad, g(c(3, 30))), c(3, 30), tolerance = 1e-8)
    expect_equal(get_coef_ci(quad)$coef, c('a', 'b', 'c'))

    expect_equal(LinearityApp:::scale_key('LIN'), 'scale.linear')
    expect_equal(LinearityApp:::scale_key('QRM'), 'scale.log')
    expect_equal(LinearityApp:::scale_key('LRM', linear = TRUE), 'scale.linear')
    expect_equal(LinearityApp:::scale_key('5PL'), 'scale.logistic')
})

test_that('calculate() inverts the Linear model and keeps standards below the blank', {
    f <- function(x) 0.08 + 0.02 * x
    data <- synthetic_data(f, sample_od = f(c(3, 10, 25)))
    r <- calculate(data, 'LIN')
    expect_equal(r$fit$model, 'LIN')
    expect_true(r$linear_scale)
    expect_equal(r$result$conc, c(3, 10, 25), tolerance = 1e-10)
    expect_equal(r$back_calc$recovery, rep(100, 12), tolerance = 1e-8)

    # a standard below the blank stays in the Linear fit, not in the Power fit
    h <- function(x) -0.01 + 0.02 * x
    low <- synthetic_data(h, conc = c(0.25, 1, 2, 4, 8, 16), sample_od = 0.1)
    expect_equal(nrow(calculate(low, 'LIN')$standard_data), 12)
    expect_equal(nrow(calculate(low, 'LRM')$standard_data), 10)
    cmp <- compare_models(calculate(low, 'LIN')$standard_data)
    expect_equal(cmp$model, calibration_models())
    # an exact straight line has no plateau: the logistic fits drift and do not converge
    logistic <- cmp$model %in% c('4PL', '5PL')
    expect_false(any(is.na(cmp$AIC[!logistic])))
    expect_equal(cmp$status[logistic], rep('not_converged', 2))
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

test_that('power weights are estimated on the scale of the residuals of the model', {
    set.seed(5)
    x <- rep(c(1, 2, 4, 8, 16, 32), each = 6)
    level_ratio <- function(fit) {
        w <- tapply(fit$w, fit$x, mean)
        max(w) / min(w)
    }
    # constant CV: the variance grows as OD^2 on the OD scale, the log10(OD) scatter is constant
    cv <- data.frame(conc = x, OD = 0.05 * x * exp(stats::rnorm(length(x), 0, 0.05)))
    log_fit <- fit_curve(cv, 'LRM', weights = 'power')
    expect_lt(abs(log_fit$theta), 0.5)
    # levels 32-fold apart: nearly equal weights, not the ~1000-fold of 1/OD^2
    expect_lt(level_ratio(log_fit), 3)
    od_fit <- fit_curve(cv, 'LRM', linear = TRUE, weights = 'power')
    expect_equal(od_fit$theta, 2, tolerance = 0.5)
    expect_gt(level_ratio(od_fit), 100)

    # constant SD: equal weights on the OD scale, the high levels weigh more on log scales
    sd <- data.frame(conc = x, OD = 0.05 * x + stats::rnorm(length(x), 0, 0.005))
    expect_lt(abs(fit_curve(sd, 'LRM', linear = TRUE, weights = 'power')$theta), 0.5)
    log_fit <- fit_curve(sd, 'LRM', weights = 'power')
    expect_equal(log_fit$theta, -2, tolerance = 0.5)
    expect_gt(log_fit$w[x == 32][1], log_fit$w[x == 1][1])

    # too few replicates: unweighted on both scales
    single <- data.frame(conc = c(1, 2, 4, 8), OD = c(0.1, 0.2, 0.4, 0.8))
    for (linear in c(FALSE, TRUE)) {
        fit <- fit_curve(single, 'LRM', linear = linear, weights = 'power')
        expect_equal(fit$theta, 0, info = linear)
        expect_match(message_text(fit$notes), 'Too few replicates', info = linear)
    }
})

test_that('lack-of-fit test detects a wrong model', {
    std <- make_standards(function(x) 0.002 * x^2 + 0.05 * x, sd = 0.002)
    expect_lt(lack_of_fit(fit_curve(std, 'LRM', linear = TRUE))$p, 0.001)
    expect_gt(lack_of_fit(fit_curve(std, 'QRM', linear = TRUE))$p, 0.01)
    no_reps <- make_standards(function(x) 0.1 * x, reps = 1)
    expect_true(is.na(lack_of_fit(fit_curve(no_reps, 'LRM', linear = TRUE))$p))
    expect_equal(lack_of_fit(fit_curve(no_reps, 'LRM', linear = TRUE))$reason, 'no_replicates')
    expect_true(is.na(lack_of_fit(fit_curve(std, 'QRM', linear = TRUE))$reason))

    data <- synthetic_data(function(x) 0.002 * x^2 + 0.05 * x, conc = levels_7)
    data$OD[1:7] <- data$OD[1:7] + 0.003
    r <- calculate(data, 'LRM', linear = TRUE)
    expect_match(message_text(r$notes), 'lack-of-fit test is significant', all = FALSE)
})

test_that('lack-of-fit test is unavailable with too few levels or identical replicates', {
    f <- function(x) 0.05 + 2 / (1 + (8 / x)^1.2)
    # as many levels as parameters of the 4PL
    four <- make_standards(f, conc = c(1, 4, 16, 64), sd = 0.01)
    lof <- lack_of_fit(fit_curve(four, '4PL'))
    expect_true(is.na(lof$p))
    expect_equal(lof$reason, 'too_few_levels')

    # rounded OD: replicates read the same OD in every level; the pure error is zero up to rounding
    # errors, which must not turn into a huge F and a significant lack of fit
    rounded <- make_standards(f, reps = 3)
    rounded$OD <- round(rounded$OD, 3)
    for (w in c('none', '1/y2', 'power')) {
        for (model in c('LRM', '4PL')) {
            lof <- lack_of_fit(fit_curve(rounded, model, weights = w))
            expect_true(is.na(lof$p))
            expect_equal(lof$reason, 'zero_pure_error')
        }
    }
    # one level with scatter: the test is done
    rounded$OD[1] <- rounded$OD[1] + 0.002
    expect_false(is.na(lack_of_fit(fit_curve(rounded, 'LRM'))$p))
})

test_that('models are compared on a common scale', {
    std <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), '4PL')$standard_data
    cmp <- compare_models(std)
    expect_equal(cmp$model, calibration_models())
    expect_equal(sum(cmp$lowest.aic), 1)
    expect_true(all(cmp$R2 > 0.99))
    # the BCA curve bends: a straight line on the original scale misses the low standards
    curved <- cmp$model %in% c('LRM', 'QRM', '4PL', '5PL')
    expect_true(all(cmp$mean.abs.error[curved] < 10))
    expect_gt(cmp$mean.abs.error[cmp$model == 'LIN'], cmp$mean.abs.error[cmp$model == '4PL'])

    lrm <- fit_curve(std, 'LRM')
    shift <- 2 * sum(log(lrm$y * log(10)))
    expect_equal(information_criteria(lrm)[['AIC']], stats::AIC(lrm$object) + shift)

    few <- std[std$conc %in% unique(std$conc)[1:5], ]
    cmp <- compare_models(few)
    expect_true(is.na(cmp$AIC[cmp$model == '5PL']))
    expect_false(is.na(cmp$AIC[cmp$model == '4PL']))
    # a model that cannot be fitted says why, with the same requirement as the calibration
    five <- cmp[cmp$model == '5PL', ]
    expect_equal(c(five$status, five$n.levels, five$n.levels.required), c('too_few_levels', '5', '6'))
    expect_equal(cmp$status[cmp$model == '4PL'], 'ok')
    tab <- LinearityApp:::comparison_table(cmp, 'en')
    expect_equal(names(tab)[ncol(tab)], 'Note')
    expect_equal(tab$Note[cmp$model == '5PL'], 'Insufficient levels: 5 available, 6 required')
    expect_equal(tab$Note[cmp$model == '4PL'], '')
    expect_equal(LinearityApp:::comparison_status(data.frame(status = 'fit_failed', n.levels = 7,
                                                             n.levels.required = 6), 'ru'),
                 'Подгонка не удалась')
    expect_equal(LinearityApp:::comparison_status(data.frame(status = 'not_converged', n.levels = 7,
                                                             n.levels.required = 6), 'ru'),
                 'Подгонка не сошлась')

    tab <- LinearityApp:::comparison_table(compare_models(std), 'ru')
    expect_equal(names(tab)[1], tr('compare.model', 'ru'))
    # every model fitted: no column of notes
    expect_false(tr('compare.note_column', 'ru') %in% names(tab))
    expect_true(tr('compare.lowest_aic', 'ru') %in% names(tab))
})

test_that('only converged starts of the logistic fits are compared', {
    # The Griess standards do not reach the upper plateau: the fits need a few hundred iterations
    std <- calculate(load_sample('griess'), 'LRM')$calibration_pool
    std <- std[!is.na(std$OD), ]
    shape <- list(model = '4PL', log_scale = FALSE)
    # the stop codes taken as convergence
    converged <- LinearityApp:::lm_converged
    expect_true(all(vapply(c(1:4, 6:8), converged, logical(1))))
    expect_false(any(vapply(c(-1, 0, 5, 9, NA), converged, logical(1))))

    # with the default limits every model and weighting converges
    for (weights in weighting_schemes()) {
        cmp <- compare_models(std, weights = weights)
        expect_true(all(cmp$status == 'ok'), info = weights)
    }

    # the iterations are limited to two: the estimates are finite, but no start converges
    local_mocked_bindings(logistic_control = function(n_par) minpack.lm::nls.lm.control(maxiter = 2))
    res <- suppressWarnings(minpack.lm::nls.lm(
        par = LinearityApp:::start_4pl(std$conc, std$OD), control = LinearityApp:::logistic_control(4),
        fn = function(p) std$OD - LinearityApp:::curve_fun(shape, std$conc, p)))
    expect_true(all(is.finite(unlist(res$par))))
    expect_true(is.finite(sum(res$fvec^2)))
    expect_false(converged(res$info))
    for (model in c('4PL', '5PL')) {
        w <- rep(1, nrow(std))
        expect_equal(LinearityApp:::fit_logistic(model, std$conc, std$OD, w), list(status = 'not_converged'))
        expect_error(fit_curve(std, model), 'did not converge', class = 'lin_error')
    }
    expect_error(calculate(load_sample('griess'), '4PL'), 'did not converge')

    # in the comparison: no statistics, not marked, and the reason in the note
    expect_no_warning(cmp <- compare_models(std))
    logistic <- cmp$model %in% c('4PL', '5PL')
    expect_equal(cmp$status[logistic], rep('not_converged', 2))
    expect_true(all(is.na(cmp$AIC[logistic])))
    expect_true(all(is.na(cmp$BIC[logistic])))
    expect_false(any(cmp$lowest.aic[logistic]))
    expect_equal(sum(cmp$lowest.aic), 1)
    expect_equal(cmp$status[!logistic], rep('ok', sum(!logistic)))
    for (lang in supported_languages()) {
        tab <- LinearityApp:::comparison_table(cmp, lang)
        expect_equal(tab[[tr('compare.note_column', lang)]][logistic], rep(tr('compare.not_converged', lang), 2))
        expect_equal(tab[[tr('compare.aic', lang)]][logistic], c('', ''))
        expect_equal(tab[[tr('compare.bic', lang)]][logistic], c('', ''))
    }
    expect_equal(LinearityApp:::comparison_table(cmp, 'en')$Note[logistic], rep('Fit did not converge', 2))
})

test_that('the common set of wells does not depend on the model', {
    data <- load_sample('bca')
    keys <- well_keys(data)[data$type == 'standard']
    excl <- data.frame(key = keys[1], reason = 'Outlier')
    r <- calculate(data, 'LIN', exclude = excl, at_blank = 'exclude')
    pool <- LinearityApp:::calibration_pool(data, r$nc_value, excl, 'exclude')
    expect_equal(pool, r$calibration_pool)
    expect_false(keys[1] %in% pool$.key)
    # the pool is there even when the chosen model cannot be fitted
    few <- data[!(data$type == 'standard' & data$conc %in% c(0.031, 0.063)), ]
    expect_error(calculate(few, '5PL'), 'At least 6')
    cmp <- compare_models(LinearityApp:::calibration_pool(few, calculate(few, 'LRM')$nc_value))
    expect_equal(cmp$status[cmp$model == '5PL'], 'too_few_levels')
    expect_false(is.na(cmp$AIC[cmp$model == '4PL']))
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
    # points excluded by the user are listed in `exclusions`, not repeated as a warning
    expect_false(any(grepl('excluded from the fit', message_text(r$notes), fixed = TRUE)))

    expect_equal(nrow(r$standard_data), nrow(calculate(data, 'QRM')$standard_data) - 2)
    expect_equal(nrow(r$excluded_standards), 2)
    expect_s3_class(get_plots(r), 'ggplot')
})

test_that('excluded points are compared with the fit on all standards', {
    data <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    full <- calculate(data, 'QRM')
    expect_null(full$original_fit)
    expect_null(full$exclusion_effect)
    expect_null(LinearityApp:::exclusion_effect_tables(full$exclusion_effect))

    r <- calculate(data, 'QRM', exclude = data.frame(key = 'B1', reason = 'bubble'))
    # the original fit is the fit on all standards; the results use the refitted one
    expect_equal(r$original_fit$par, full$fit$par)
    expect_false(isTRUE(all.equal(r$fit$par, full$fit$par)))
    e <- r$exclusion_effect
    expect_equal(e$coefs$coef, c('a', 'b', 'c'))
    expect_equal(e$coefs$original, coef_table_fit(full$fit)$est)
    expect_equal(e$coefs$refitted, coef_table_fit(r$fit)$est)

    bc <- e$back_calc
    expect_equal(nrow(bc), nrow(full$standard_data))
    expect_equal(bc$key[bc$excluded], 'B1')
    # levels are named from the highest concentration; the position is the well
    expect_equal(bc$standard[bc$key %in% c('A1', 'B1', 'G1')], c('S1', 'S2', 'S7'))
    expect_equal(r$exclusions$standard, 'S2')
    expect_equal(bc$error.original, 100 * (bc$calc.original - bc$conc) / bc$conc)
    expect_equal(bc$calc.original, back_calculate(full$fit)$calc)

    s <- e$summary
    expect_equal(s$statistic, c('mean_error', 'max_error', 'n_wells_within', 'n_levels_accepted'))
    expect_equal(s$original[1], mean(abs(bc$error.original)))
    expect_equal(s$refitted[2], max(abs(bc$error.refitted[!bc$excluded])))
    expect_equal(s$n_total.original[3], nrow(bc))
    expect_equal(s$n_total.refitted[3], nrow(bc) - 1)
    # the refitted levels are those of the standards table
    cal <- r$quality$calibration
    expect_equal(c(s$refitted[4], s$n_total.refitted[4], s$n_needed.refitted[4]),
                 c(cal$n_pass, cal$n_levels, cal$n_needed))
    expect_equal(c(s$original[4], s$n_total.original[4]),
                 c(full$quality$calibration$n_pass, full$quality$calibration$n_levels))

    tabs <- LinearityApp:::exclusion_effect_tables(e, 'en')
    expect_equal(names(tabs$coefs), c('Coef', 'Original', 'Refitted'))
    expect_equal(names(tabs$back_calc), c('Standard', 'Position', 'Nominal', 'Original calculated',
                                          'Refitted calculated', 'Original error, %', 'Refitted error, %',
                                          'Excluded'))
    expect_equal(tabs$summary[[1]], c('Mean absolute relative error, %', 'Maximum absolute relative error, %',
                                      'Wells within individual bias limits', 'Accepted calibrator levels'))
    expect_match(tabs$summary$Refitted[3], paste0(' / ', nrow(bc) - 1), fixed = TRUE)
    expect_equal(tabs$summary$Refitted[4], sprintf('%d / %d (%d required)', cal$n_pass, cal$n_levels, cal$n_needed))
    ru <- LinearityApp:::exclusion_effect_tables(e, 'ru')$summary
    expect_equal(ru[[1]][3:4], c(tr('effect.n_wells_within', 'ru'), tr('effect.n_levels_accepted', 'ru')))
    expect_match(ru[[3]][4], sprintf('(требуется %d)', cal$n_needed), fixed = TRUE)
    expect_equal(tabs$back_calc$Position[tabs$back_calc$Excluded == 'Yes'], 'B1')
    expect_equal(sum(tabs$back_calc$Excluded == ''), nrow(bc) - 1)
    ex <- exclusions_table(r$exclusions, 'ru')
    expect_equal(names(ex)[1:2], c(tr('exclusion.standard', 'ru'), tr('exclusion.key', 'ru')))
    expect_equal(unname(unlist(ex[1, 1:2])), c('S2', 'B1'))
})

test_that('the effect of the exclusions tells the wells within their limits from the levels accepted', {
    # Duplicates of 10 read 8.1 and 11.9: each well is within ±20 %, the mean is exact, but the CV is 26.9 %
    f <- function(x) 0.01 * x
    conc <- c(1, 2.5, 5, 10, 20, 40, 80)
    data <- synthetic_data(f, conc = conc)
    ten <- which(data$type == 'standard' & data$conc == 10)
    data$OD[ten] <- f(c(8.1, 11.9)) + 0.05
    # a well of another level is excluded by the user
    keys <- well_keys(data)
    out <- keys[data$type == 'standard' & data$conc == 40][1]
    r <- calculate(data, 'LIN', exclude = data.frame(key = out, reason = 'bubble'))
    st <- r$quality$standards
    expect_equal(st$calc.cv[st$conc == 10], 100 * stats::sd(c(8.1, 11.9)) / 10, tolerance = 1e-6)
    expect_equal(st$status[st$conc == 10], 'fail')

    s <- r$exclusion_effect$summary
    bc <- r$exclusion_effect$back_calc
    expect_equal(bc$error.refitted[bc$conc == 10], c(-19, 19), tolerance = 1e-6)
    # both wells of 10 are within their limits...
    expect_equal(s$refitted[s$statistic == 'n_wells_within'], sum(!bc$excluded))
    # ...but the level is not accepted: as in the standards table
    levels <- s[s$statistic == 'n_levels_accepted', ]
    expect_equal(c(levels$refitted, levels$n_total.refitted), c(length(conc) - 1, length(conc)))
    expect_equal(levels$refitted, r$quality$calibration$n_pass)
    # the original model, on all wells, rejects the same level
    expect_equal(c(levels$original, levels$n_total.original), c(length(conc) - 1, length(conc)))
    tab <- LinearityApp:::exclusion_effect_tables(r$exclusion_effect, 'en')$summary
    row <- function(label) unlist(tab[tab$Parameter == label, c('Original', 'Refitted')], use.names = FALSE)
    expect_equal(row('Wells within individual bias limits'), c('14 / 14', '13 / 13'))
    expect_equal(row('Accepted calibrator levels'), rep('6 / 7 (6 required)', 2))
})

test_that('a standard level keeps its ID when another level is lost', {
    data <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    keys <- well_keys(data)
    std <- data$type == 'standard'
    expect_equal(sort(unique(data$conc[std]), decreasing = TRUE)[1:2], c(2, 1))
    # no OD in any well of S1 (2); a well of S2 (1) excluded by the user
    data$OD[std & data$conc == 2] <- NA
    s2 <- keys[std & data$conc == 1]
    r <- calculate(data, 'QRM', exclude = data.frame(key = s2[1], reason = 'bubble'))

    # one map from the whole plan
    expect_equal(r$standard_levels$id[r$standard_levels$conc == 1], 'S2')
    expect_equal(r$standard_levels$id[r$standard_levels$conc == 2], 'S1')
    expect_equal(nrow(r$standard_levels), 7)
    # the individual points, the exclusions and the effect of the exclusions
    pts <- LinearityApp:::standard_points_table(r, 'en')
    expect_equal(unique(pts$Standard[pts$Nominal == '1']), 'S2')
    expect_equal(unique(pts$Standard[pts$Nominal == '0.031']), 'S7')
    expect_false('S1' %in% pts$Standard)
    expect_equal(r$exclusions$standard, 'S2')
    bc <- r$exclusion_effect$back_calc
    expect_equal(unique(bc$standard[bc$conc == 1]), 'S2')
    # the background step: the labels of the plot and the lowest level of the finding on the blanks
    raw <- data[std, c('conc', 'OD')]
    roles <- intersect(background_roles(), unique(data$type))
    expect_true(length(roles) > 0)
    bg <- LinearityApp:::background_summary(data, roles)
    labels <- attr(LinearityApp:::background_points(bg, raw, 'en'), 'labels')
    expect_true('S2\n1' %in% labels)
    expect_false(any(grepl('^S1\n', labels)))
    expect_equal(bg$scatter$level, 'S7')
    expect_equal(LinearityApp:::blank_scatter(bg$od, bg$value, raw)$level, 'S7')
    # the names of a run without losses are the same
    full <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), 'QRM')
    expect_equal(full$standard_levels, r$standard_levels)
})

test_that('a standard meets the criteria when its error is within the limit of its level', {
    std <- data.frame(.key = paste0('w', 1:4), conc = c(1, 2, 4, 8), OD = c(1, 2, 4, 8))
    fit <- fit_curve(std, 'LRM', linear = TRUE)
    # an exact line: every error is zero
    e <- exclusion_effect(fit, fit, std, character(0), qc_criteria())
    expect_equal(e$summary$original, c(0, 0, 4, 4), tolerance = 1e-8)
    expect_equal(e$back_calc$key, std$.key)
    expect_equal(e$back_calc$standard, c('S4', 'S3', 'S2', 'S1'))
    expect_equal(LinearityApp:::standard_names(c(1, 8), levels = c(1, 2, 4, 8, 16)), c('S5', 'S2'))
})

test_that('the original curve is dashed on the plot after exclusions, not in the report', {
    data <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    r <- calculate(data, 'QRM', exclude = data.frame(key = 'B1', reason = 'bubble'))
    linetypes <- function(p) {
        built <- ggplot2::ggplot_build(p)
        unlist(lapply(built$data, function(d) if ('linetype' %in% names(d)) unique(d$linetype)))
    }
    curve <- LinearityApp:::plot_standard_curve(r, lang = 'ru')
    expect_setequal(linetypes(curve), c('dashed', 'solid'))
    expect_true(tr('plot.curve_original', 'ru') %in% ggplot2::ggplot_build(curve)$plot$scales$get_scales('linetype')$get_labels())
    report <- LinearityApp:::plot_standard_curve(r, show_original = FALSE)
    expect_false('dashed' %in% linetypes(report))
    expect_false('dashed' %in% linetypes(LinearityApp:::plot_standard_curve(calculate(data, 'QRM'))))
})

test_that('logistic curves have no solution beyond the asymptotes', {
    f <- function(x) logistic(x, 0, 1.2, 10, 2)
    data <- synthetic_data(f, conc = levels_7, sample_od = c(f(5), 2.5, 0.5))
    r <- calculate(data, '4PL')
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

test_that('models are compared on the common set of calibration wells', {
    truth <- function(x) 0.01 + 1.8 / (1 + (8 / x)^1.2)
    data <- synthetic_data(truth, conc = c(0.25, 0.5, 1, 2, 4, 8, 16, 32), sample_od = 0.3)
    # 16 measured standard wells; one of them reads 0.003 below the blank
    low <- which(data$type == 'standard' & data$conc == 0.25)[1]
    data$OD[low] <- data$OD[data$type == 'blank'][1] - 0.003
    key <- well_keys(data)[low]
    label <- function(m) tr(paste0('model_short.', m), 'en')

    # the chosen model changes neither the wells nor the statistics of the comparison
    cmps <- lapply(c('LRM', '4PL', 'LIN'), function(m) {
        r <- calculate(data, m)
        expect_equal(nrow(r$calibration_pool), 16)
        expect_equal(r$calibration_pool$OD[r$calibration_pool$.key == key], -0.003, tolerance = 1e-12)
        compare_models(r$calibration_pool)
    })
    expect_identical(cmps[[2]], cmps[[1]])
    expect_identical(cmps[[3]], cmps[[1]])
    cmp <- cmps[[1]]

    loglog <- cmp$model %in% c('LRM', 'QRM')
    expect_equal(cmp$n.used[loglog], c(15, 15))
    expect_equal(cmp$n.used[!loglog], rep(16, 4))
    expect_equal(cmp$n.available, rep(16, 6))
    expect_equal(cmp$dropped[[which(cmp$model == 'LRM')]], key)
    expect_false(key %in% cmp$wells[[which(cmp$model == 'LRM')]])
    expect_true(key %in% cmp$wells[[which(cmp$model == '4PL')]])
    expect_false(LinearityApp:::same_wells(cmp))
    expect_false(any(cmp$lowest.aic))

    tab <- LinearityApp:::comparison_table(cmp, 'en')
    expect_true('Wells used' %in% names(tab))
    expect_false('Lowest AIC' %in% names(tab))
    expect_equal(tab[['Wells used']][tab$Model == '4PL'], '16/16')
    expect_equal(tab[['Wells used']][tab$Model == label('LRM')], '15/16')
    expect_match(attr(tab, 'note'), 'not comparable', fixed = TRUE)
    expect_match(attr(tab, 'note'), paste0(label('LRM'), ', ', label('QRM'), ' leave out ', key, ':'),
                 fixed = TRUE)

    # once the user excludes that well all models share the same 15 wells
    r <- calculate(data, '4PL', exclude = data.frame(key = key, reason = 'bubble'))
    cmp <- compare_models(r$calibration_pool)
    expect_equal(cmp$n.used, rep(15, 6))
    expect_true(LinearityApp:::same_wells(cmp))
    expect_equal(sum(cmp$lowest.aic), 1)
    tab <- LinearityApp:::comparison_table(cmp, 'en')
    expect_false('Wells used' %in% names(tab))
    expect_true('Lowest AIC' %in% names(tab))
    expect_null(attr(tab, 'note'))
})

test_that('the sets of wells are compared, not their sizes', {
    cmp <- data.frame(model = c('LIN', 'LRM', '4PL'))
    cmp$wells <- list(c('A1', 'A2'), c('A2', 'A1'), NULL)
    expect_true(LinearityApp:::same_wells(cmp))
    cmp$wells <- list(c('A1', 'A2'), c('A1', 'A3'), NULL)
    expect_false(LinearityApp:::same_wells(cmp))
})

test_that('a duplicate with one failed inversion is an incomplete back-calculation', {
    f <- function(x) logistic(x, 0.05, 1.3, 10, 2.5)
    std <- make_standards(f, conc = c(0.5, 1, 2, 4, 16, 32, 64))
    fit <- fit_curve(std, '4PL')
    # level 10 in duplicate: one well reads 10.1, the other lies above the upper asymptote
    fit$x <- c(fit$x, 10, 10)
    fit$y <- c(fit$y, f(10.1), 2.6)
    fit$keys <- c(paste0('W', seq_len(nrow(std))), 'C5', 'C6')
    st <- assess_standards(fit, wells = data.frame(conc = fit$x, OD = fit$y))
    lv <- st[st$conc == 10, ]
    expect_equal(c(lv$n.planned, lv$n.measured, lv$n, lv$n.calc), c(2, 2, 2, 1))
    expect_equal(lv$status, 'incomplete')
    expect_false(lv$pass)
    expect_true(is.na(lv$calc.cv))
    # the bias belongs to the single back-calculated replicate
    expect_equal(lv$calc.mean, 10.1, tolerance = 1e-6)
    expect_equal(lv$bias, 1, tolerance = 1e-4)
    expect_equal(lv$failed[[1]], data.frame(key = 'C6', reason = 'outside_asymptotes'))

    tab <- quality_tables(list(standards = st), 'en')$standards
    row <- tab[tab$Concentration == '10', ]
    expect_equal(row[['CV of back-calculated, %']], 'CV unavailable')
    expect_equal(row[['Bias, %']], '1.0')
    expect_equal(row$Accepted, 'Incomplete back-calculation')
    # the wells against the plan are in a table of their own
    expect_false('Lost wells' %in% names(tab))
    wells <- quality_tables(list(standards = st), 'en')$wells
    w <- wells[wells$Concentration == '10', ]
    expect_equal(c(w$Planned, w$Fitted, w[['Back-calculated']]), c(2, 2, 1))
    expect_equal(w[['Lost wells']], 'C6: OD outside fitted asymptotes')
    expect_equal(wells[['Back-calculated']][wells$Concentration == '4'], 2)
})

test_that('incomplete back-calculation is reported in the notes, the points and the comparison', {
    truth <- function(x) 0.05 + 2 / (1 + (1 / x)^3)
    data <- synthetic_data(truth, conc = c(0.25, 0.5, 1, 2, 4, 8, 16, 32), sample_od = 0.5)
    # one well of the top level reads above the plateau of the curve
    top <- which(data$type == 'standard' & data$conc == 32)
    data$OD[top[2]] <- 2.15
    key <- well_keys(data)[top[2]]
    r <- calculate(data, '4PL')
    lv <- r$quality$standards[r$quality$standards$conc == 32, ]
    expect_equal(lv$status, 'incomplete')
    expect_equal(lv$n.calc, 1)
    expect_match(message_text(r$notes, 'en'),
                 paste0('S1 (32): 1 of 2 wells in the fit back-calculated; ', key, ': OD outside fitted asymptotes'),
                 fixed = TRUE, all = FALSE)
    pts <- LinearityApp:::standard_points_table(r, 'en')
    # the reason is in the wells deviating from the plan, not repeated in the points
    expect_false('Not back-calculated' %in% names(pts))
    expect_equal(pts[['Back-calculated']][pts$Position == key], '')

    # the errors of the comparison use the back-calculated standards; the failures are counted
    cmp <- compare_models(r$calibration_pool)
    i <- which(cmp$model == '4PL')
    expect_equal(cmp$n.failed[i], 1)
    bc <- back_calculate(r$fit)
    expect_equal(cmp$mean.abs.error[i], mean(abs(bc$recovery - 100), na.rm = TRUE))
    tab <- LinearityApp:::comparison_table(cmp, 'en')
    expect_equal(tab[['Not back-calculated']][tab$Model == '4PL'], '1')
    expect_equal(tab[['Not back-calculated']][tab$Model == tr('model_short.LIN', 'en')], '')

    # every model inverts all standards of a straight line (4PL may not fit it at all)
    clean <- calculate(synthetic_data(function(x) 0.1 * x, sample_od = 0.5), 'LIN')
    expect_false('Not back-calculated' %in% names(LinearityApp:::comparison_table(
        compare_models(clean$calibration_pool), 'en')))
    expect_false('Not back-calculated' %in% names(LinearityApp:::standard_points_table(clean, 'en')))
})

test_that('a comparison without any fitted model recommends nothing and does not warn', {
    few <- data.frame(conc = c(1, 1, 2, 2), OD = c(0.1, 0.1, 0.2, 0.2))
    expect_no_warning(cmp <- compare_models(few))
    expect_true(all(is.na(cmp$AIC)))
    expect_false(any(cmp$lowest.aic))
})

test_that('a flat calibration line has no inverse', {
    std <- data.frame(conc = rep(c(1, 2, 4, 8), 2), OD = rep(c(0.1, 0.2, 0.4, 0.8), 2))
    for (linear in c(TRUE, FALSE)) {
        fit <- fit_curve(std, 'LRM', linear = linear)
        expect_equal(LinearityApp:::inverse_fun(fit, 0.3), 3)
        fit$par[['b']] <- 0
        expect_true(all(is.na(LinearityApp:::inverse_fun(fit, c(0.2, 0.5, 0.9)))), info = linear)
        cls <- LinearityApp:::classify_wells(fit, 0.5, LinearityApp:::inverse_fun(fit, 0.5), c(1, 8))
        expect_equal(cls$flag, FLAGS$no_solution)
        expect_equal(cls$inverse, 'flat')
    }
})

test_that('weights are defined when all standards equal the blank', {
    x <- rep(c(1, 2, 4), 2)
    for (w in weighting_schemes()) {
        expect_no_warning(wf <- LinearityApp:::weight_function(w, x, rep(0, 6)))
        expect_true(all(is.finite(wf$fun(x, wf$level_mean))), info = w)
    }
})

test_that('decreasing (competitive) curves are inverted', {
    truth <- function(x) 0.2 + 1.8 / (1 + (x / 5)^1.2)
    conc <- c(0.5, 1, 2, 4, 8, 16, 32)
    data <- synthetic_data(truth, conc = conc, sample_od = truth(c(3, 10)))
    for (m in c('4PL', '5PL')) {
        r <- calculate(data, m)
        expect_equal(r$result$conc, c(3, 10), tolerance = 1e-4, info = m)
        expect_equal(r$result$flag, rep(FLAGS$ok, 2), info = m)
    }
    r <- calculate(data, '4PL')
    expect_equal(r$back_calc$recovery, rep(100, 14), tolerance = 1e-4)
})
