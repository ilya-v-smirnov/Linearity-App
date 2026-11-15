scenarios <- expand.grid(dataset = names(sample_files),
                         model = c('LRM', 'QRM'),
                         linear = c(FALSE, TRUE),
                         stringsAsFactors = FALSE)

no_rounding <- function(x, ...) x

test_that('sample concentrations lie on the plotted calibration curve', {
    for (i in seq_len(nrow(scenarios))) {
        s <- scenarios[i, ]
        info <- paste(s, collapse = ' / ')
        data <- load_sample(s$dataset)
        r <- calculate(data, s$model, linear = s$linear, round_fun = no_rounding)
        ok <- r$result$flag == 'OK'
        expect_gt(sum(ok), 0)

        od_on_curve <- curve_fun(r$fit, r$result$conc[ok] / r$result$dilution[ok])
        net_od <- subtract_nc(data, r$nc_value)
        net_od <- net_od$OD[net_od$type == 'sample'][ok]
        expect_equal(od_on_curve, net_od, tolerance = 1e-8, info = info)
    }
})

test_that('only concentrations within the calibration range are reported', {
    for (i in seq_len(nrow(scenarios))) {
        s <- scenarios[i, ]
        info <- paste(s, collapse = ' / ')
        r <- calculate(load_sample(s$dataset), s$model, linear = s$linear,
                       round_fun = no_rounding)
        res <- r$result
        ok <- res$flag == 'OK'

        expect_true(all(res$flag %in% unlist(FLAGS)), info = info)
        expect_true(all(is.na(res$conc[!ok])), info = info)
        expect_false(any(is.na(res$conc[ok])), info = info)
        undiluted <- res$conc[ok] / res$dilution[ok]
        expect_true(all(undiluted >= r$calibration_range[1] * (1 - 1e-9)), info = info)
        expect_true(all(undiluted <= r$calibration_range[2] * (1 + 1e-9)), info = info)
    }
})

test_that('samples below the background are flagged, not reported as zero', {
    r <- calculate(load_sample('griess'), 'LRM')
    m0 <- r$result[r$result$sample.name == 'M0', ]
    expect_equal(unique(m0$flag), 'OD <= blank')
    expect_true(all(is.na(m0$conc)))
    expect_false(any(r$result$conc == 0, na.rm = TRUE))
    expect_match(message_text(r$notes), 'OD <= blank (4)', fixed = TRUE, all = FALSE)
})

test_that('standards with missing OD are excluded from the fit and reported', {
    r <- calculate(load_sample('elisa'), 'LRM')
    expect_equal(nrow(r$excluded_standards), 1)
    expect_equal(r$excluded_standards$reason, 'Missing OD')
    expect_equal(nrow(r$standard_data), 13)
    expect_match(message_text(r$notes), 'excluded from the fit: Missing OD (1)', fixed = TRUE,
                 all = FALSE)
})

test_that('linear model is inverted exactly', {
    f <- function(x) 0.1 * x
    data <- synthetic_data(f, sample_od = c(0.25, 1, 2.5, 0.05, 5, -0.01))
    r <- calculate(data, 'LRM', linear = TRUE, round_fun = no_rounding)
    expect_equal(r$result$conc[1:3], c(2.5, 10, 25))
    expect_equal(r$result$flag, c('OK', 'OK', 'OK', '< LLOQ', '> ULOQ', 'OD <= blank'))
})

test_that('quadratic model uses the branch that contains the standards', {
    # Parabola with vertex at x = 25, standards on the rising branch (1..16)
    f <- function(x) -0.002 * x^2 + 0.1 * x + 0.02
    data <- synthetic_data(f, conc = c(1, 2, 4, 8, 12, 16), sample_od = f(c(3, 10, 15)) - 0.02)
    data$OD[data$type == 'sample'] <- f(c(3, 10, 15)) + 0.05
    r <- calculate(data, 'QRM', linear = TRUE, round_fun = no_rounding)
    expect_equal(r$result$conc, c(3, 10, 15), tolerance = 1e-8)
    expect_true(is_monotonic(r$fit))
})

test_that('dilution is applied after inverse prediction', {
    f <- function(x) 0.1 * x
    data <- synthetic_data(f, sample_od = c(1, 1))
    data$dilution[data$type == 'sample'] <- c(1, 7)
    r <- calculate(data, 'LRM', linear = TRUE, round_fun = signif, round_par = 3)
    expect_equal(r$result$conc, c(10, 70))
})

test_that('background is the unrounded geometric mean of NC wells', {
    bg <- background_value(c(0.0784, 0.0753))
    expect_equal(bg$value, sqrt(0.0784 * 0.0753))
    expect_length(bg$notes, 0)

    bg <- background_value(c(0.01, -0.01, 0.03))
    expect_equal(bg$value, mean(c(0.01, -0.01, 0.03)))
    expect_match(message_text(bg$notes), 'arithmetic mean')

    expect_error(background_value(c(NA, NA)), 'No valid OD')
})

test_that('unsupported models and too few levels are rejected', {
    data <- load_sample('bca')
    expect_error(calculate(data, 'deming'), 'Unknown model')

    f <- function(x) 0.1 * x
    few <- synthetic_data(f, conc = c(1, 2, 4))
    expect_s3_class(calculate(few, 'LRM', linear = TRUE)$model_direct, 'lin_fit')
    expect_error(calculate(few, 'QRM', linear = TRUE), 'At least 4')
})

test_that('coefficient table labels match the model terms', {
    r <- calculate(load_sample('bca'), 'QRM')
    tab <- get_coef_ci(r$model_direct)
    cf <- coef(r$fit$object)
    expect_equal(tab$coef, c('a', 'b', 'c'))
    expect_equal(tab$est[tab$coef == 'c'], unname(cf['(Intercept)']))
    expect_equal(tab$est[tab$coef == 'a'], unname(cf[grepl('^2', names(cf), fixed = TRUE)]))
    expect_equal(tab$est[tab$coef == 'b'], unname(cf['u']))
    expect_true(all(tab$ci.lower < tab$est & tab$est < tab$ci.upper))
})

test_that('residual plot uses fitted ODs, not concentrations', {
    r <- calculate(load_sample('bca'), 'LRM')
    fr <- fit_residuals(r$fit)
    expect_equal(fr$fitted, unname(10^fitted(r$fit$object)))
    expect_equal(fr$resid, unname(resid(r$fit$object)))
    expect_false(isTRUE(all.equal(fr$fitted, r$standard_data$conc)))
})

test_that('plots are built for all scenarios', {
    for (i in seq_len(nrow(scenarios))) {
        s <- scenarios[i, ]
        r <- calculate(load_sample(s$dataset), s$model, linear = s$linear)
        p <- get_plots(r, xtitle = 'x', ytitle = 'y')
        expect_s3_class(p, 'ggplot')
    }
})

test_that('blank and NC wells are equivalent background wells', {
    conc <- c(1, 2, 4, 8, 16)
    make <- function(bg_types) {
        data.frame(type = c(rep('standard', 10), bg_types, 'sample'),
                   conc = c(conc, conc, NA, NA, NA),
                   dilution = c(rep(NA, 12), 1),
                   OD = c(0.1 * conc + 0.04, 0.1 * conc + 0.04, 0.02, 0.08, 0.84))
    }
    mixed <- calculate(make(c('NC', 'blank')), 'LRM', linear = TRUE, round_fun = function(x, ...) x)
    blank <- calculate(make(c('blank', 'blank')), 'LRM', linear = TRUE, round_fun = function(x, ...) x)
    nc <- calculate(make(c('NC', 'NC')), 'LRM', linear = TRUE, round_fun = function(x, ...) x)
    expect_equal(mixed$result$conc, blank$result$conc)
    expect_equal(nc$result$conc, blank$result$conc)
    # the background is the geometric mean of both kinds of wells
    expect_equal(mixed$result$conc, (0.84 - 0.04) / 0.1, tolerance = 1e-8)
    expect_equal(normalize_table(data.frame(type = c('NC', 'nc', 'Blank'), OD = 1))$type, rep('blank', 3))
    expect_equal(LinearityApp:::classify_codes(c('NC', 'B')), c('blank', 'blank'))
})

test_that('residuals are plotted against the concentrations of the standard curve', {
    r <- calculate(load_sample('bca'), 'QRM')
    curve <- LinearityApp:::plot_standard_curve(r, xtitle = 'Conc')
    resid <- LinearityApp:::plot_fitted_resid(r, xtitle = 'Conc')
    expect_equal(resid$labels$x, 'Conc')
    expect_equal(rlang::as_label(resid$mapping$x), 'conc')
    breaks <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$breaks
    expect_equal(breaks(resid), breaks(curve))
})

test_that('samples outside the range can be extrapolated and below-blank samples set to zero', {
    data <- load_sample('griess')
    plain <- calculate(data, 'LRM', round_fun = function(x, ...) x)
    expect_true(all(is.na(plain$result$conc[plain$result$flag != 'OK'])))

    below <- calculate(data, 'LRM', round_fun = function(x, ...) x, extrapolate = 'below')
    low <- below$result$flag == '< LLOQ'
    expect_true(all(below$result$conc[low] > 0))
    expect_true(all(below$result$conc[low] < min(below$standard_data$conc)))
    expect_true(all(below$result$conc.lower[low] < below$result$conc.upper[low]))
    expect_equal(below$result$flag, plain$result$flag)       # flags are kept

    # only wells left without a concentration are reported as not quantified
    expect_match(message_text(plain$notes), '8 of 12 sample wells', fixed = TRUE, all = FALSE)
    expect_match(message_text(below$notes), '4 of 12 sample wells', fixed = TRUE, all = FALSE)

    zero <- calculate(data, 'LRM', blank_zero = TRUE)
    expect_equal(unique(zero$result$conc[zero$result$flag == 'OD <= blank']), 0)
    expect_true(all(is.na(zero$result$conc[zero$result$flag == '< LLOQ'])))

    # above the ULOQ: the top standards are excluded so that samples exceed the range
    bca <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    keys <- well_keys(bca)
    top <- data.frame(key = keys[bca$type == 'standard' & bca$conc >= 0.5], reason = 'test')
    r_plain <- calculate(bca, 'LRM', exclude = top)
    r_above <- calculate(bca, 'LRM', exclude = top, extrapolate = 'above')
    high <- r_plain$result$flag == '> ULOQ'
    expect_true(any(high))
    expect_true(all(is.na(r_plain$result$conc[high])))
    expect_true(all(r_above$result$conc[high] > 0.25 * r_above$result$dilution[high]))
})

test_that('sample and QC results show the OD as measured, before the blank is subtracted', {
    data <- load_sample('griess')
    r <- calculate(data, 'LRM')
    expect_equal(r$result$OD, round(data$OD[data$type == 'sample'], 4))
    expect_gt(r$nc_value, 0)
    expect_false('.od_raw' %in% c(names(r$result), names(r$standard_data)))
})
