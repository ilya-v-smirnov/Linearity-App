scenarios <- expand.grid(dataset = names(sample_files),
                         model = c('LIN', 'QUAD', 'LRM', 'QRM'),
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

test_that('a quadratic curve that turns within the range gives no concentration', {
    # OD = 1 + (C - 3)^2: OD 2 matches C = 2 and C = 4
    f <- function(x) 1 + (x - 3)^2
    data <- synthetic_data(f, conc = c(1, 2, 3, 4, 5), sample_od = c(2, 10))
    r <- calculate(data, 'QUAD', round_fun = no_rounding)
    expect_equal(unname(r$fit$par), c(10, -6, 1), tolerance = 1e-8)
    expect_false(r$monotonic)
    expect_false(is_monotonic(r$fit))
    expect_true(all(is.na(r$result$conc)))
    expect_equal(r$result$flag, rep(FLAGS$no_solution, 2))
    notes <- message_text(r$notes)
    expect_match(notes, 'Non-monotonic within calibration range: the quadratic curve turns at 3, inside 1\u20135',
                 fixed = TRUE, all = FALSE)
    # one warning for the curve, not one per standard level
    expect_false(any(grepl('Incomplete back-calculation', notes, fixed = TRUE)))
    expect_equal(LinearityApp:::inverse_reason(r$fit, 2), 'non_monotonic')
    expect_true(all(r$quality$standards$status == 'incomplete'))
    expect_false(r$quality$calibration$accepted)
    # the curve and the residuals can still be viewed
    expect_s3_class(LinearityApp:::plot_standard_curve(r), 'ggplot')
    expect_s3_class(LinearityApp:::plot_fitted_resid(r), 'ggplot')

    # an explicit interval on one side of the vertex chooses the branch
    left <- calculate(data, 'QUAD', round_fun = no_rounding, interval = c(1, 3))
    expect_true(left$monotonic)
    expect_equal(left$result$conc[1], 2, tolerance = 1e-8)
    expect_equal(left$calibration_range, c(1, 3))
    st <- left$quality$standards
    expect_equal(st$status[st$conc > 3], rep('outside_interval', 2))
    expect_match(message_text(left$notes), 'within the monotonic interval 1\u20133', fixed = TRUE, all = FALSE)

    right <- calculate(data, 'QUAD', round_fun = no_rounding, interval = c(3, 5))
    expect_equal(right$result$conc[1], 4, tolerance = 1e-8)
    # the bounds also apply to the range check: OD 10 is C = 6 on this branch
    expect_equal(right$result$flag, c(FLAGS$ok, FLAGS$above))
    expect_equal(right$calibration_range, c(3, 5))

    expect_error(calculate(data, 'QUAD', interval = c(3, 1)), 'monotonic interval')
    expect_error(calculate(data, 'QUAD', interval = c(10, 20)), 'monotonic interval')
    # the other models ignore the interval
    expect_null(calculate(synthetic_data(function(x) 0.1 * x), 'LIN', interval = c(1, 3))$interval)
})

test_that('the log-log quadratic curve is checked on the log scale of the concentration', {
    # log10(OD) = -0.5 + 1 * u - 1 * u^2 with u = log10(C): vertex at C = 10^0.5
    f <- function(x) 10^(-0.5 + log10(x) - log10(x)^2)
    data <- synthetic_data(f, conc = c(1, 2, 4, 8, 10), sample_od = f(1.5))
    r <- calculate(data, 'QRM', round_fun = no_rounding)
    expect_false(r$monotonic)
    expect_equal(LinearityApp:::quad_vertex(r$fit)$conc, sqrt(10), tolerance = 1e-6)
    expect_true(is.na(r$result$conc))
    low <- calculate(data, 'QRM', round_fun = no_rounding, interval = c(1, sqrt(10)))
    expect_equal(low$result$conc, 1.5, tolerance = 1e-6)
})

test_that('a quadratic coefficient of zero gives the linear inverse', {
    std <- data.frame(conc = rep(c(1, 2, 4, 8, 16), 2), OD = rep(0.05 + 0.1 * c(1, 2, 4, 8, 16), 2))
    fit <- fit_curve(std, 'QUAD')
    # a tiny fitted c must not lose precision
    expect_lt(abs(fit$par[['c']]), 1e-10)
    expect_equal(inverse_fun(fit, 0.05 + 0.1 * c(3, 12)), c(3, 12), tolerance = 1e-8)
    fit$par[['c']] <- 0
    expect_true(is_monotonic(fit))
    expect_equal(inverse_fun(fit, 0.05 + 0.1 * c(3, 12)), c(3, 12), tolerance = 1e-8)
    # a horizontal line has no unique inverse
    fit$par[['b']] <- 0
    expect_true(all(is.na(inverse_fun(fit, c(0.2, 0.5)))))
    expect_equal(LinearityApp:::inverse_reason(fit, 0.5), 'flat')
})

test_that('dilution is applied after inverse prediction', {
    f <- function(x) 0.1 * x
    data <- synthetic_data(f, sample_od = c(1, 1))
    data$dilution[data$type == 'sample'] <- c(1, 7)
    r <- calculate(data, 'LRM', linear = TRUE, round_fun = signif, round_par = 3)
    expect_equal(r$result$conc, c(10, 70))
})

test_that('background is the unrounded arithmetic mean of the background wells', {
    bg <- background_value(c(0.0784, 0.0753))
    expect_equal(bg$value, (0.0784 + 0.0753) / 2)
    expect_length(bg$notes, 0)
    # the same rule with a zero or negative value
    expect_equal(background_value(c(0.01, -0.01, 0.03))$value, mean(c(0.01, -0.01, 0.03)))
    expect_equal(background_value(c(0, 0.02))$value, 0.01)
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
    # log10(OD) = a + b*log10(C) + c*log10(C)^2
    expect_equal(tab$coef, c('a', 'b', 'c'))
    expect_equal(tab$est[tab$coef == 'a'], unname(cf['(Intercept)']))
    expect_equal(tab$est[tab$coef == 'b'], unname(cf['u']))
    expect_equal(tab$est[tab$coef == 'c'], unname(cf[grepl('^2', names(cf), fixed = TRUE)]))
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

test_that('the background is the mean of the chosen background wells', {
    # standards OD = 0.02 * conc + 0.10 as measured; reagent blanks 0.04 and 0.16
    conc <- c(1, 2, 4, 8, 16)
    make <- function(bg_types, bg_od) {
        data.frame(type = c(rep('standard', 10), bg_types, 'sample'),
                   conc = c(conc, conc, rep(NA, length(bg_types)), NA),
                   dilution = c(rep(NA, 10 + length(bg_types)), 1),
                   OD = c(0.02 * conc + 0.10, 0.02 * conc + 0.10, bg_od, 0.30))
    }
    no_round <- function(x, ...) x
    blank <- calculate(make(c('blank', 'blank'), c(0.04, 0.16)), 'LIN', round_fun = no_round)
    expect_equal(blank$nc_value, 0.10)
    expect_equal(blank$background$roles, 'blank')
    expect_equal(blank$background$n, 2)
    expect_equal(blank$background$sd, stats::sd(c(0.04, 0.16)))
    # the standard of 1 measured at 0.12 is corrected to 0.02; both values are kept
    s1 <- blank$standard_data[blank$standard_data$conc == 1, ]
    expect_equal(s1$OD.raw, c(0.12, 0.12))
    expect_equal(s1$OD, c(0.02, 0.02))
    expect_equal(blank$result$OD, 0.30)
    expect_equal(blank$result$OD.corrected, 0.20)
    expect_equal(blank$result$conc, 10, tolerance = 1e-8)

    # a negative control does not change the result until it is chosen
    with_nc <- make(c('blank', 'blank', 'NC'), c(0.04, 0.16, 0.40))
    plain <- calculate(with_nc, 'LIN', round_fun = no_round)
    expect_equal(plain$nc_value, 0.10)
    expect_equal(plain$result$conc, blank$result$conc)
    expect_equal(plain$background$unused, c(NC = 1))
    both <- calculate(with_nc, 'LIN', round_fun = no_round, background = c('blank', 'NC'))
    expect_equal(both$nc_value, 0.20)
    expect_equal(both$background$n, 3)
    nothing <- calculate(with_nc, 'LIN', round_fun = no_round, background = character(0))
    expect_equal(nothing$nc_value, 0)
    expect_match(message_text(nothing$notes), 'No background is subtracted', all = FALSE)
    expect_error(calculate(with_nc, 'LIN', background = 'buffer'), 'Unknown background roles')

    # without a reagent blank the zero calibrator is the default; negative controls only alone
    expect_equal(default_background(c('standard', 'zero', 'NC')), 'zero')
    expect_equal(default_background(c('standard', 'NC')), 'NC')
    expect_equal(default_background('standard'), character(0))
})

test_that('files of earlier versions with NC wells are analysed visibly', {
    expect_equal(normalize_table(data.frame(type = c('NC', 'nc', 'Blank', 'Negative control', 'zero'), OD = 1))$type,
                 c('NC', 'NC', 'blank', 'NC', 'zero'))
    expect_equal(LinearityApp:::classify_codes(c('NC', 'B', 'S0', 'Z', 'S1')), c('NC', 'blank', 'zero', 'zero', 'standard'))

    # only NC wells (the examples before they were relabelled): they are the background, as before, with a note
    data <- load_sample('bca')
    expect_equal(sum(data$type == 'blank'), 2)
    data$type[data$type == 'blank'] <- 'NC'
    r <- calculate(data, 'LRM')
    expect_equal(r$background$roles, 'NC')
    expect_match(message_text(r$notes), 'its 2 negative control well(s) (NC) are subtracted as background', fixed = TRUE,
                 all = FALSE)
    html <- as.character(LinearityApp:::background_import_ui(data, 'en'))
    expect_match(html, 'Background by default: Negative control (2 well(s)).', fixed = TRUE)
    expect_match(html, 'as in earlier versions', fixed = TRUE)

    # the mean differs from the geometric mean of earlier versions by more than 1 %: noticed
    odd <- data
    odd$OD[odd$type == 'NC'] <- c(0.02, 0.10)
    changed <- calculate(odd, 'LRM')
    expect_equal(changed$background$legacy, sqrt(0.02 * 0.10))
    expect_match(message_text(changed$notes), 'differs from that of earlier versions of the app (0.04472',
                 fixed = TRUE, all = FALSE)
    # the value of earlier versions is not repeated under the background table
    expect_false(any(grepl('Earlier versions', LinearityApp:::background_notes(changed$background, 'en'))))
    # a difference below 1 % is shown with the background only
    expect_false(any(grepl('earlier versions', message_text(r$notes), fixed = TRUE) &
                     grepl('differs', message_text(r$notes), fixed = TRUE)))
})

test_that('single background wells are excluded from the background', {
    # blanks 0.075 and 0.175 (one too high) on the BCA example
    data <- load_sample('bca')
    data$OD[data$type == 'blank'] <- c(0.075, 0.175)
    keys <- well_keys(data)[data$type == 'blank']
    both <- calculate(data, 'LRM')
    expect_equal(both$nc_value, 0.125)
    excl <- data.frame(key = keys[2], reason = 'Outlier')
    r <- calculate(data, 'LRM', exclude_background = excl)
    expect_equal(r$nc_value, 0.075)
    bg <- r$background
    expect_equal(c(bg$n, bg$n_roles), c(1, 2))
    expect_equal(LinearityApp:::background_table(bg, 'en')$Used, '1/2')
    # the excluded well is listed in the table of the wells only
    expect_length(LinearityApp:::background_notes(bg, 'en'), 0)
    wells <- LinearityApp:::background_wells_table(bg, 'en')
    expect_equal(wells$Status, c('\u2713', 'excluded: Outlier'))
    expect_true(endsWith(LinearityApp:::background_text(bg, 'en'), paste0('; excluded by the user: ', keys[2], ' (Outlier)')))
    # an excluded background well is not a standard exclusion; with the high blank five low standards
    # fell to OD <= 0 and left the log-log fit, now they are back
    expect_equal(nrow(r$exclusions), 0)
    expect_equal(c(nrow(both$standard_data), nrow(r$standard_data)), c(9, 14))

    # all chosen wells excluded: nothing is subtracted, with a warning
    none <- calculate(data, 'LRM', exclude_background = data.frame(key = keys, reason = 'Outlier'))
    expect_equal(none$nc_value, 0)
    expect_match(message_text(none$notes), 'all chosen background wells are excluded', fixed = TRUE, all = FALSE)

    # the plot shows the calibrators as measured from the lowest level, to the right of the blanks
    std <- data[data$type == 'standard', c('conc', 'OD')]
    pts <- LinearityApp:::background_points(bg, std, 'en')
    expect_equal(attr(pts, 'n_roles'), 1)
    expect_equal(attr(pts, 'labels')[1:2], c('Reagent\nblank', 'S7\n0.031'))
    expect_equal(sort(unique(pts$kind)), c('excluded', 'standard', 'used'))
    expect_equal(sum(pts$kind == 'standard'), 14)
    expect_s3_class(LinearityApp:::plot_background(bg, std, 'ru'), 'ggplot')
})

test_that('a scatter of the blanks above the signal of the lowest calibrator is reported', {
    data <- load_sample('bca')
    # the blanks of the example agree well: no warning
    ok <- calculate(data, 'LRM')
    expect_false(ok$background$scatter$warn)
    expect_false(any(grepl('scatter', message_text(ok$notes), fixed = TRUE)))

    # blanks 0.075 and 0.175: a range of 0.1 OD, while S7 (0.031) reads about 0.0885
    data$OD[data$type == 'blank'] <- c(0.075, 0.175)
    r <- calculate(data, 'LRM')
    sc <- r$background$scatter
    expect_equal(sc$range, 0.1)
    expect_equal(sc$conc, 0.031)
    expect_equal(sc$level, 'S7')
    s7 <- mean(data$OD[data$type == 'standard' & data$conc == 0.031])
    expect_equal(sc$signal, s7 - 0.125)
    expect_true(sc$warn)
    expect_match(message_text(r$notes, 'en'),
                 sprintf('The background wells scatter by 0.1000 OD (maximum minus minimum), more than the lowest calibrator S7 (0.031) rises above the background (%.4f OD)',
                         s7 - 0.125), fixed = TRUE, all = FALSE)
    expect_match(message_text(r$notes, 'ru'), 'Разброс лунок фона 0.1000 OD', fixed = TRUE, all = FALSE)

    # with the high blank excluded a single well is left: nothing to compare
    keys <- well_keys(data)[data$type == 'blank']
    one <- calculate(data, 'LRM', exclude_background = data.frame(key = keys[2], reason = 'Outlier'))
    expect_null(one$background$scatter)

    # saturation and a signal at the background level are no reasons for a background well
    expect_equal(LinearityApp:::background_exclusion_reasons(), c('outlier', 'pipetting', 'bubble'))
})

test_that('the background correction is shown and reported', {
    data <- synthetic_data(function(x) 0.1 * x)
    data <- rbind(data, data.frame(sample.name = NA, sample.date = NA, type = 'NC', dilution = NA, conc = NA,
                                   OD = 0.3))
    r <- calculate(data, 'LIN')
    tab <- LinearityApp:::background_table(r$background, 'en')
    expect_equal(names(tab), c('Background wells', 'Used', 'Mean OD', 'SD', 'Subtracted'))
    expect_equal(unlist(tab[1, ], use.names = FALSE), c('Reagent blank', '2/2', '0.0500', '0.0000', '0.0500'))
    expect_equal(LinearityApp:::background_notes(r$background, 'en'), 'Not used: Negative control (1 well(s)).')
    expect_equal(LinearityApp:::background_text(r$background, 'en'),
                 'Reagent blank, wells used 2/2, mean OD 0.0500, SD 0.0000; subtracted 0.0500')
    wells <- LinearityApp:::background_wells_table(r$background, 'en')
    expect_equal(names(wells), c('Role', 'Position', 'OD', 'Deviation', 'Status'))
    expect_equal(wells$Role, c('Reagent blank', 'Reagent blank', 'Negative control'))
    expect_equal(wells$Status, c('\u2713', '\u2713', '\u2014'))
    expect_equal(wells$Deviation, c('+0.0000', '+0.0000', '+0.2500'))
    expect_s3_class(LinearityApp:::plot_background(r$background, lang = 'en'), 'ggplot')
    expect_null(LinearityApp:::background_wells_table(list(wells = r$background$wells[0, ]), 'en'))
    pts <- LinearityApp:::standard_points_table(r, 'en')
    expect_equal(pts[['OD as measured']][1], sprintf('%.4f', 0.1 * 32 + 0.05))
    expect_equal(pts[['Corrected OD']][1], sprintf('%.4f', 0.1 * 32))
    blocks <- LinearityApp:::report_blocks('short', date = '2026-11-20', title = 't', method = 'm',
                                           background = LinearityApp:::background_text(r$background, 'en'),
                                           model_table = get_coef_ci(r$fit), result_table = r$result)
    lines <- Filter(function(b) b$type == 'line', blocks)
    expect_true(any(vapply(lines, function(b) identical(b$label, 'Background:'), logical(1))))
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

test_that('axis breaks of replicated standards are not repeated', {
    breaks <- LinearityApp:::design_x_scale(c(1, 1, 2, 2, 4, 4, NA))
    expect_equal(breaks, c(1, 2, 4))
    expect_equal(LinearityApp:::design_x_scale(c(5, 5)), 5)

    r <- calculate(load_sample('bca'), '4PL')
    axis <- LinearityApp:::conc_axis(r)
    expect_false(anyDuplicated(axis$breaks) > 0)
})
