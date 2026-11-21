linear_7 <- c(1, 2, 4, 8, 16, 32, 64)

# Synthetic run: OD = 0.02 * conc, NC = 0.05; `bias` multiplies the ODs of
# selected standard levels; samples given as data.frame(name, dilution, od)
run_data <- function(bias = c(), samples = data.frame(name = 'A', dilution = 1, od = 0.4),
                     qc = NULL, nc = 0.05) {
    od_std <- 0.02 * linear_7
    if (length(bias) > 0) od_std[as.integer(names(bias))] <- od_std[as.integer(names(bias))] * bias
    rows <- rbind(
        data.frame(type = 'standard', sample.name = NA, dilution = NA, conc = rep(linear_7, 2),
                   OD = rep(od_std, 2) + nc),
        data.frame(type = 'blank', sample.name = NA, dilution = NA, conc = NA, OD = c(nc, nc)),
        data.frame(type = 'sample', sample.name = samples$name, dilution = samples$dilution, conc = NA,
                   OD = samples$od + nc)
    )
    if (!is.null(qc)) {
        rows <- rbind(rows, data.frame(type = 'QC', sample.name = qc$id, dilution = 1, conc = qc$nominal,
                                       OD = qc$od + nc))
        rows$id <- ifelse(rows$type == 'QC', rows$sample.name, NA)
    }
    check_assay(normalize_table(rows))
}

no_round <- function(x, ...) x

test_that('criteria have ICH M10 defaults and are validated', {
    cr <- qc_criteria()
    expect_s3_class(cr, 'lin_criteria')
    expect_equal(c(cr$std_bias, cr$std_bias_edge, cr$cv, cr$min_levels), c(20, 25, 20, 6))
    expect_error(qc_criteria(cv = -1), 'Invalid acceptance criteria: cv')
    expect_error(qc_criteria(range = 'some'), 'range')
    expect_equal(c(cr$sample_cv, cr$sample_cv_edge), c(20, 25))
    expect_equal(qc_criteria(cv = 15)$sample_cv, 15)
    expect_equal(qc_criteria(cv = 15, sample_cv = 30)$sample_cv, 30)
})

test_that('standard levels are accepted with tighter limits inside the range', {
    fit <- calculate(run_data(), 'LRM', linear = TRUE)$fit
    st <- assess_standards(fit)
    expect_equal(nrow(st), 7)
    expect_true(all(st$pass))
    expect_equal(st$bias.limit, c(25, rep(20, 5), 25))

    # level 4 (conc 8) reads 40 % high: fails; the curve (log-log) still accepts 6 of 7
    r <- calculate(run_data(bias = c('4' = 1.4)), 'LRM')
    st <- r$quality$standards
    expect_false(st$pass[st$conc == 8])
    expect_equal(r$quality$calibration$n_pass, 6)
    expect_true(r$quality$accepted)
    expect_equal(r$calibration_range, c(1, 64))   # an interior level does not change the range
})

test_that('all standard levels of the run count for the acceptance of the calibration', {
    data <- run_data()
    keys <- calculate(data, 'LRM')$standard_data
    keys <- keys[order(keys$conc), ]
    exclude <- function(k) data.frame(key = k, reason = 'test')

    # 10 of 14 points excluded: 4 levels with one point each remain of 7
    first <- keys$.key[!duplicated(keys$conc)]
    kept <- first[c(1, 3, 5, 7)]
    r <- calculate(data, 'LRM', exclude = exclude(setdiff(keys$.key, kept)))
    cal <- r$quality$calibration
    expect_equal(c(cal$n_levels, cal$n_pass, cal$n_needed), c(7, 4, 6))
    expect_false(cal$accepted)
    expect_equal(cal$status, 'rejected')
    st <- r$quality$standards
    expect_equal(st$n, c(1, 0, 1, 0, 1, 0, 1))
    expect_false(any(st$pass[st$n == 0]))
    # duplicates left with one well are accepted by their bias, not as plain passes
    expect_equal(st$status, rep(c('pass_no_cv', 'no_points'), length.out = 7))
    tab <- quality_tables(r$quality, 'en')$standards
    empty <- quality_tables(r$quality, 'en')$wells$Fitted == 0
    expect_equal(sum(empty), 3)
    expect_equal(tab[['Bias, %']][empty], rep('—', 3))
    expect_equal(tab[['CV of back-calculated, %']][empty], rep('—', 3))
    html <- as.character(LinearityApp:::verdict_ui(r, 'en'))
    expect_match(html, 'Calibration not accepted', fixed = TRUE)
    expect_match(html, '4 of 7 (6 required)', fixed = TRUE)

    # fewer levels than the minimum: not accepted even if all of them pass
    few <- calculate(data, 'LRM', criteria = qc_criteria(min_levels = 8))
    expect_true(all(few$quality$standards$pass))
    expect_false(few$quality$calibration$accepted)
})

test_that('a calibration without replicates is accepted with the CV not assessed', {
    data <- run_data()
    keys <- calculate(data, 'LRM')$standard_data
    single <- keys$.key[duplicated(keys$conc)]
    r <- calculate(data, 'LRM', exclude = data.frame(key = single, reason = 'test'))
    cal <- r$quality$calibration
    expect_true(cal$accepted)
    expect_equal(c(cal$n_no_cv, cal$n_cv, cal$status), c('7', '0', 'accepted_no_cv'))
    expect_true(r$quality$accepted)
    expect_match(message_text(r$quality$warnings), 'checked for only 0 accepted levels (6 required)',
                 fixed = TRUE, all = FALSE)
    for (lang in supported_languages()) {
        html <- as.character(LinearityApp:::verdict_ui(r, lang))
        expect_match(html, 'alert-warning', fixed = TRUE)
        expect_match(html, tr('quality.calibration_accepted_no_cv', lang), fixed = TRUE)
    }
    # with replicates the status is a plain acceptance
    expect_equal(calculate(data, 'LRM')$quality$calibration$status, 'accepted')

    # levels without replicates do not matter while enough levels have a CV:
    # 2 of 7 levels without replicates, 5 with a CV
    cr <- qc_criteria(min_levels = 5, min_fraction = 0.8)
    r5 <- calculate(data, 'LRM', criteria = cr, exclude = data.frame(key = single[1:2], reason = 'test'))
    cal <- r5$quality$calibration
    expect_equal(c(cal$n_pass, cal$n_no_cv, cal$n_cv), c(7, 2, 5))
    expect_equal(cal$status, 'accepted')
    expect_match(as.character(LinearityApp:::verdict_ui(r5, 'en')), 'alert-success', fixed = TRUE)
    expect_false(any(grepl('was checked for only', message_text(r5$quality$warnings), fixed = TRUE)))
    # 3 without replicates, 4 with a CV: accepted with the exception
    r4 <- calculate(data, 'LRM', criteria = cr, exclude = data.frame(key = single[1:3], reason = 'test'))
    expect_equal(r4$quality$calibration$status, 'accepted_no_cv')
    expect_match(as.character(LinearityApp:::verdict_ui(r4, 'en')),
                 'checked for only 4 accepted standard levels (5 required): 3 accepted levels', fixed = TRUE)
})

test_that('failing end levels narrow the quantification range', {
    data <- run_data(bias = c('1' = 2), samples = data.frame(name = c('lo', 'mid'), dilution = 1,
                                                             od = c(0.025, 0.4)))
    r <- calculate(data, 'LRM', linear = TRUE, round_fun = no_round)
    expect_equal(r$calibration_range[1], 2)
    expect_equal(r$standard_range, c(1, 64))
    expect_equal(r$result$flag, c('< LLOQ', 'OK'))
    # the edges found on this plate are not called LLOQ and ULOQ (limits of a method)
    notes <- message_text(r$notes)
    expect_match(notes, 'lowest accepted calibrator 2, highest accepted calibrator 64', fixed = TRUE, all = FALSE)
    expect_false(any(grepl('LLOQ|ULOQ', notes)))
    expect_false(any(grepl('LLOQ|ULOQ|НПКО|ВПКО', c(message_text(r$notes, 'ru'),
                                                     LinearityApp:::quality_summary(r, 'en')$range,
                                                     LinearityApp:::quality_summary(r, 'ru')$range))))
    # R² gives no warning: no criterion of the methods is set on it
    low_r2 <- calculate(load_sample('griess'), 'LRM', subtract_background = FALSE)
    expect_lt(low_r2$quality$curve$r2, 0.99)
    expect_false(any(grepl('R²', message_text(low_r2$quality$warnings), fixed = TRUE)))

    r_all <- calculate(data, 'LRM', linear = TRUE, criteria = qc_criteria(range = 'all'))
    expect_equal(r_all$calibration_range, c(1, 64))
    expect_equal(r_all$result$flag[1], 'OK')
})

test_that('a calibration with too few accepted levels rejects the run', {
    r <- calculate(run_data(bias = c('3' = 1.5, '5' = 0.6)), 'LRM')
    cal <- r$quality$calibration
    expect_equal(cal$n_needed, 6)
    expect_lt(cal$n_pass, 6)
    expect_false(r$quality$accepted)
    expect_match(message_text(r$quality$reasons), 'calibration is not accepted', all = FALSE)

    lenient <- calculate(run_data(bias = c('3' = 1.5, '5' = 0.6)), 'LRM',
                         criteria = qc_criteria(min_levels = 3, min_fraction = 0.5))
    expect_true(lenient$quality$accepted)
})

test_that('replicates are summarised and high CV is flagged', {
    samples <- data.frame(name = c('A', 'A', 'B', 'B'), dilution = 1, od = c(0.40, 0.42, 0.30, 0.60))
    r <- calculate(run_data(samples = samples), 'LRM', linear = TRUE, round_fun = no_round)
    s <- r$quality$samples
    expect_equal(s$sample.name, c('A', 'B'))
    expect_equal(s$n, c(2, 2))
    expect_equal(s$mean, c(20.5, 22.5))
    expect_equal(s$cv[1], 100 * stats::sd(c(20, 21)) / 20.5)
    expect_equal(s$flag, c('OK', 'High CV'))
    expect_match(message_text(r$notes), 'Replicate CV above the limit in 1', all = FALSE)
    expect_equal(translate_flags(s$flag, 'ru')[2], tr('flag.high_cv', 'ru'))
})

test_that('dilution linearity is checked across dilutions of a sample', {
    samples <- data.frame(name = c('A', 'A', 'B', 'B'), dilution = c(2, 4, 2, 4),
                          od = c(0.4, 0.2, 0.4, 0.3))
    r <- calculate(run_data(samples = samples), 'LRM', linear = TRUE, round_fun = no_round)
    d <- r$quality$dilution
    expect_equal(nrow(d), 4)
    expect_equal(d$pass, c(TRUE, TRUE, FALSE, FALSE))
    expect_equal(d$deviation[1:2], c(0, 0))
    # wells behind each mean, also in the table
    expect_equal(d$n, c(1, 1, 1, 1))
    expect_equal(d$n.quantified, c(1, 1, 1, 1))
    expect_equal(names(quality_tables(r$quality, 'en')$dilution)[3:4], c('n', 'Quantified'))
    s <- r$quality$samples
    expect_equal(s$flag[s$sample.name == 'B'], c('Dilution non-linear', 'Dilution non-linear'))
    expect_match(message_text(r$notes), 'Non-linear dilutions in 1', all = FALSE)

    expect_null(dilution_linearity(s[s$sample.name == 'A' & s$dilution == 2, ]))

    # values outside the limits are highlighted in the app, not in the exports
    plain <- quality_tables(r$quality, 'en')$dilution
    expect_false(any(grepl('crit-fail', unlist(plain), fixed = TRUE)))
    marked <- quality_tables(r$quality, 'en', highlight = TRUE)$dilution
    b <- marked$Sample == 'B'
    expect_true(all(grepl('crit-fail', marked[['CV between dilutions, %']][b], fixed = TRUE)))
    # sample B fails by its CV; with a strict limit its deviations are highlighted too
    strict <- quality_tables(list(dilution = d, criteria = qc_criteria(dilution_bias = 1)), 'en',
                             highlight = TRUE)$dilution
    expect_equal(grepl('crit-fail', strict[['Deviation from mean, %']], fixed = TRUE), abs(d$deviation) > 1)
    expect_true(any(abs(d$deviation) > 1))
    expect_true(all(grepl('crit-fail', marked[['Accepted']][b], fixed = TRUE)))
    expect_false(any(grepl('crit-fail', unlist(marked[!b, ]), fixed = TRUE)))
    # the texts from the data are escaped in the marked table
    odd <- d
    odd$sample.name <- '<b>A</b>'
    expect_equal(quality_tables(list(dilution = odd), 'en', highlight = TRUE)$dilution$Sample[1],
                 '&lt;b&gt;A&lt;/b&gt;')
})

test_that('each sample of the dilution plot has its own colour', {
    samples <- data.frame(name = rep(c('A', 'B', 'C'), each = 2), dilution = rep(c(2, 4), 3),
                          od = c(0.4, 0.2, 0.4, 0.3, 0.3, 0.15))
    r <- calculate(run_data(samples = samples), 'LRM', linear = TRUE, round_fun = no_round)
    p <- plot_dilution_linearity(r$quality$dilution)
    built <- ggplot2::ggplot_build(p)
    colours <- unique(built$data[[3]]$colour)
    expect_length(colours, 3)
    # the non-linear sample is dashed, also in the legend
    expect_equal(sort(unique(as.character(built$data[[3]]$linetype))), c('dashed', 'solid'))
    expect_equal(unname(p$guides$guides$colour$params$override.aes$linetype), c('solid', 'dashed', 'solid'))
    expect_equal(dilution_samples(r$quality$dilution), 3)
    expect_gt(dilution_plot_height(20), dilution_plot_height(2))
    expect_length(unique(dilution_palette(12)), 12)
})

test_that('quality controls follow the 4-6-X rule', {
    qc <- data.frame(id = c('QC-L', 'QC-L', 'QC-H', 'QC-H'), nominal = c(4, 4, 40, 40),
                     od = 0.02 * c(4, 4.6, 40, 41))
    r <- calculate(run_data(qc = qc), 'LRM', linear = TRUE, round_fun = no_round)
    q <- r$quality$qc
    expect_equal(q$levels$id, c('QC-L', 'QC-H'))
    expect_equal(q$levels$n.pass, c(2, 2))     # 4.6 / 4 = +15 %, within the 20 % limit
    expect_true(q$accepted)

    qc$od[1:2] <- 0.02 * c(6, 6)               # both QC-L wells +50 %
    r <- calculate(run_data(qc = qc), 'LRM', linear = TRUE)
    expect_false(r$quality$qc$levels$pass[1])
    expect_false(r$quality$qc$accepted)
    expect_false(r$quality$accepted)
    expect_match(message_text(r$quality$reasons), 'quality controls do not meet', all = FALSE)
})

test_that('quality tables and summary are translated', {
    qc <- data.frame(id = 'QC-M', nominal = 10, od = 0.2)
    samples <- data.frame(name = c('A', 'A'), dilution = c(1, 2), od = c(0.4, 0.2))
    r <- calculate(run_data(samples = samples, qc = qc), 'LRM', linear = TRUE)
    for (lang in supported_languages()) {
        tabs <- quality_tables(r$quality, lang)
        expect_equal(names(tabs$standards)[1], tr('col.conc', lang))
        expect_equal(nrow(tabs$standards), 7)
        expect_false(is.null(tabs$qc))
        expect_false(is.null(tabs$dilution))
        expect_equal(nrow(tabs$curve), 3)
        s <- LinearityApp:::quality_summary(r, lang)
        expect_equal(s$title, tr('quality.accepted', lang))
    }
})

test_that('example plate passes the quality assessment', {
    r <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), 'QRM')
    expect_true(r$quality$accepted)
    expect_equal(r$quality$calibration$n_pass, 7)
    expect_equal(nrow(r$quality$samples), 8)
    expect_true(all(r$quality$dilution$pass))
})

test_that('aggregated results include SD and CV', {
    res <- data.frame(sample.name = c('A', 'A', 'B'), conc = c(1, 3, NA))
    out <- aggregate_result(res, 'sample.name')
    expect_equal(out$sd, c(signif(sqrt(2), 3), NA))
    expect_equal(out$cv, c(round(100 * sqrt(2) / 2, 1), NA))
})

test_that('criteria module reads both parts of the criteria', {
    shiny::testServer(LinearityApp:::mod_criteria_server, args = list(lang = 'en'), {
        session$setInputs(preset = 'custom', preset_samples = 'custom', std_bias = 15, std_bias_edge = 20,
                          cv = 15, cv_edge = 20,
                          min_fraction = 75,
                          min_levels = 6, sample_cv = 10, sample_cv_edge = 12, qc_bias = 15, qc_bias_edge = 20,
                          qc_fraction = 67, qc_level_fraction = 50, dilution_bias = 20, dilution_cv = 20,
                          range = 'all')
        cr <- session$returned()
        expect_equal(cr$std_bias, 15)
        expect_equal(cr$min_fraction, 0.75)
        expect_equal(c(cr$sample_cv, cr$sample_cv_edge), c(10, 12))
        expect_equal(cr$range, 'all')
        session$setInputs(cv = -5)
        expect_error(session$returned(), 'Invalid acceptance criteria')
        session$setInputs(reset_standards = 1, reset_samples = 1)
    })
    expect_setequal(unique(LinearityApp:::criteria_fields()$part), c('standards', 'samples'))
})

test_that('fixed presets of the criteria of the standards ignore the inputs', {
    presets <- criteria_presets()
    expect_equal(presets$custom, presets$ich_m10)
    d <- qc_criteria()
    expect_equal(presets$ich_m10, d[names(presets$ich_m10)])
    expect_equal(unlist(presets$colorimetric), c(std_bias = 15, std_bias_edge = 20, cv = 15, cv_edge = 20,
                                                 min_fraction = 0.8, min_levels = 5))
    shiny::testServer(LinearityApp:::mod_criteria_server, args = list(lang = 'en'), {
        session$setInputs(std_bias = 50, cv = 50, min_levels = 2, sample_cv = 12, preset_samples = 'custom')
        expect_equal(session$returned()$std_bias, 20)        # ICH M10 by default
        session$setInputs(preset = 'colorimetric')
        cr <- session$returned()
        expect_equal(c(cr$std_bias, cr$std_bias_edge, cr$cv, cr$cv_edge, cr$min_fraction, cr$min_levels),
                     c(15, 20, 15, 20, 0.8, 5))
        expect_equal(cr$sample_cv, 12)                        # the samples keep their own inputs
        session$setInputs(preset = 'custom')
        expect_equal(session$returned()$std_bias, 50)
    })
    ui <- as.character(LinearityApp:::mod_criteria_ui('criteria', 'en', 'standards'))
    expect_match(ui, 'Colorimetric assay', fixed = TRUE)
    expect_match(ui, 'Bias at boundary calibrators', fixed = TRUE)
    expect_false(grepl('LLOQ', ui, fixed = TRUE))
})

test_that('the criteria of the samples have their own presets that follow the standards', {
    presets <- criteria_presets('samples')
    expect_equal(presets$custom, presets$ich_m10)
    d <- qc_criteria()
    expect_equal(presets$ich_m10, d[names(presets$ich_m10)])
    expect_setequal(names(presets$ich_m10),
                    LinearityApp:::criteria_fields()$name[LinearityApp:::criteria_fields()$part == 'samples'])
    expect_equal(unlist(presets$colorimetric[c('sample_cv', 'qc_bias', 'dilution_bias', 'dilution_cv')]),
                 c(sample_cv = 15, qc_bias = 15, dilution_bias = 15, dilution_cv = 15))
    shiny::testServer(LinearityApp:::mod_criteria_server, args = list(lang = 'en'), {
        session$setInputs(sample_cv = 50, dilution_bias = 50, std_bias = 50)
        cr <- session$returned()
        expect_equal(c(cr$sample_cv, cr$dilution_bias, cr$qc_fraction), c(20, 20, 2 / 3))   # ICH M10
        session$setInputs(preset_samples = 'colorimetric')
        cr <- session$returned()
        expect_equal(c(cr$sample_cv, cr$sample_cv_edge, cr$qc_bias, cr$dilution_cv), c(15, 20, 15, 15))
        expect_equal(cr$std_bias, 20)                          # the standards keep their preset
        session$setInputs(preset_samples = 'custom')
        expect_equal(c(session$returned()$sample_cv, session$returned()$dilution_bias), c(50, 50))
    })
    ui <- as.character(LinearityApp:::mod_criteria_ui('criteria', 'en', 'samples'))
    expect_match(ui, 'Colorimetric assay', fixed = TRUE)
    expect_match(ui, 'CV near boundary calibrators', fixed = TRUE)
    expect_match(ui, '66.7', fixed = TRUE)                     # fractions shown rounded
    expect_false(grepl('LLOQ|ULOQ', ui))
    expect_false(grepl('LLOQ|ULOQ|НПКО|ВПКО',
                       as.character(LinearityApp:::mod_criteria_ui('criteria', 'ru', 'samples'))))
})

test_that('the verdict of the calibration is shown with the model parameters', {
    r <- calculate(run_data(bias = c('3' = 1.5, '5' = 0.6)), 'LRM')
    html <- as.character(LinearityApp:::verdict_ui(r, 'ru'))
    expect_match(html, tr('quality.calibration_rejected', 'ru'), fixed = TRUE)
    expect_match(html, 'alert-danger', fixed = TRUE)
    ok <- calculate(run_data(), 'LRM')
    html <- as.character(LinearityApp:::verdict_ui(ok, 'en', unit = '\u00b5M'))
    expect_match(html, 'Calibration accepted', fixed = TRUE)
    expect_match(html, 'Accepted calibration range: 1\u201364 \u00b5M', fixed = TRUE)
    expect_match(as.character(LinearityApp:::verdict_ui(ok, 'en')), 'range: 1\u201364</p>', fixed = TRUE)

    # a level not accepted inside an accepted range is named
    expect_false(grepl('Not accepted', html, fixed = TRUE))
    gap <- calculate(run_data(bias = c('4' = 1.4)), 'LRM')
    expect_true(gap$quality$calibration$accepted)
    expect_match(as.character(LinearityApp:::verdict_ui(gap, 'en')), '<p>Not accepted inside the range: 8</p>',
                 fixed = TRUE)

    # failing quality controls reject the run, not the calibration
    qc <- calculate(run_data(qc = data.frame(id = 'QC-M', nominal = 10, od = 0.9)), 'LRM')
    expect_false(qc$quality$accepted)
    expect_match(as.character(LinearityApp:::verdict_ui(qc, 'en')), 'Calibration accepted', fixed = TRUE)
})

test_that('the bias of the back-calculated standards is plotted along the concentration', {
    r <- calculate(run_data(bias = c('4' = 1.4)), 'LRM')
    p <- LinearityApp:::plot_bias(r, xtitle = 'x', lang = 'en')
    expect_s3_class(p, 'ggplot')
    b <- ggplot2::ggplot_build(p)
    # wells, limits and level means
    bc <- back_calculate(r$fit)
    expect_equal(nrow(p$data), sum(!is.na(bc$calc)))
    expect_equal(p$data$bias, (bc$recovery - 100)[!is.na(bc$calc)])
    means <- b$data[[5]]
    expect_equal(nrow(means), sum(!is.na(r$quality$standards$bias)))
    # the level of 8 is not accepted: a red diamond
    expect_true('#c62828' %in% means$fill)
    # the limits applied: wider at the ends
    upper <- b$data[[2]]$y
    expect_equal(max(upper), 25)
    expect_equal(min(upper), 20)
    expect_equal(p$labels$y, 'Bias, %')
    expect_equal(LinearityApp:::plot_bias(r, lang = 'ru')$labels$y, 'Смещение, %')
})

test_that('the equation of the model is written in the reports', {
    eq <- function(model) {
        LinearityApp:::model_equation_text(list(model = model, log_scale = LinearityApp:::model_log_scale(model)))
    }
    expect_equal(eq('LRM'), 'log₁₀(OD) = a + b · log₁₀(C)')
    expect_equal(eq('QRM'), 'log₁₀(OD) = a + b · log₁₀(C) + c · [log₁₀(C)]²')
    expect_equal(eq('LIN'), 'OD = a + b · C')
    expect_equal(eq('QUAD'), 'OD = a + b · C + c · C²')
    expect_equal(eq('4PL'), 'OD = d + (a − d) / (1 + (C / c)ᵇ)')
    expect_equal(eq('5PL'), 'OD = d + (a − d) / [1 + (C / c)ᵇ]ᵍ')
    r <- calculate(run_data(), 'LRM')
    for (type in c('short', 'detailed')) {
        blocks <- LinearityApp:::report_blocks(type, date = '2026-11-21', title = 't', method = 'm',
                                               equation = LinearityApp:::model_equation_text(r$fit),
                                               model_table = get_coef_ci(r$fit), result_table = r$result)
        i <- which(vapply(blocks, function(x) identical(x$label, 'Model equation:'), logical(1)))
        expect_length(i, 1)
        expect_equal(blocks[[i]]$value, 'log₁₀(OD) = a + b · log₁₀(C)')
        # right before the coefficients
        expect_equal(blocks[[i + 1]]$type, 'table')
    }
})

test_that('a rejected calibration has no range, and its concentrations are not reportable', {
    # Griess without background subtraction: only 3.12 and 50 of seven levels are accepted
    r <- calculate(load_sample('griess'), 'LRM', subtract_background = FALSE)
    cal <- r$quality$calibration
    expect_false(cal$accepted)
    expect_equal(c(cal$lloq, cal$uloq), c(3.125, 50))
    html <- as.character(LinearityApp:::verdict_ui(r, 'en', unit = 'nM'))
    expect_match(html, 'Calibration not accepted', fixed = TRUE)
    expect_false(grepl('Accepted calibration range', html, fixed = TRUE))
    expect_match(html, '<p>Calibration range: not established</p>', fixed = TRUE)
    expect_match(html, '<p>Accepted standard levels: 2 of 7 (6 required)</p>', fixed = TRUE)
    expect_match(html, '<p>Not accepted: 1.56, 6.25, 12.5, 25, 100</p>', fixed = TRUE)
    expect_match(html, '<p>Range applied to the samples: 3.12–50 nM (not reportable)</p>', fixed = TRUE)
    # the levels are not repeated as a bullet
    expect_false(grepl('<li>', html, fixed = TRUE))
    ru <- as.character(LinearityApp:::verdict_ui(r, 'ru'))
    expect_match(ru, 'Диапазон калибровки: не установлен', fixed = TRUE)

    # the concentrations are kept for inspection, with a note instead of a narrowed range
    notes <- message_text(r$notes)
    expect_false(any(grepl('quantification range is limited', notes, fixed = TRUE)))
    expect_match(notes, 'shown for inspection only and are not reportable (range applied to the samples: 3.12–50)',
                 fixed = TRUE, all = FALSE)
    expect_true(any(!is.na(r$result$conc)))
    # the report says the same
    s <- LinearityApp:::quality_summary(r, 'en')
    expect_equal(s$range, 'Quantification range: not established (range applied to the samples: 3.12 – 50, not reportable)')
    expect_false(any(grepl('standard levels meet the criteria', s$reasons, fixed = TRUE)))

    # an accepted calibration keeps its range and the note of a narrowed range
    ok <- calculate(load_sample('griess'), 'LRM')
    expect_false(any(grepl('not reportable', message_text(ok$notes), fixed = TRUE)))
    expect_match(LinearityApp:::quality_summary(ok, 'en')$range, '^Quantification range \\(lowest')
})

test_that('a planned singlet is told apart from a replicate left alone', {
    data <- run_data()
    # level 16 is planned as a single well
    data <- data[-which(data$type == 'standard' & data$conc == 16)[1], ]
    std <- calculate(data, 'LIN')$standard_data
    # one of the two wells of level 8 is excluded by the user
    exclude <- data.frame(key = std$.key[std$conc == 8][1], reason = 'bubble')
    r <- calculate(data, 'LIN', exclude = exclude)
    st <- r$quality$standards
    expect_equal(st$status[st$conc == 16], 'pass_singlet')
    expect_equal(st$status[st$conc == 8], 'pass_no_cv')
    expect_equal(c(st$n.planned[st$conc == 8], st$n.measured[st$conc == 8], st$n[st$conc == 8]), c(2, 2, 1))
    expect_true(all(st$pass))
    expect_equal(r$quality$calibration$n_no_cv, 2)

    tab <- quality_tables(r$quality, 'en')$standards
    row <- function(conc) tab[tab$Concentration == conc, ]
    expect_equal(row('16')[['CV of back-calculated, %']], '\u2014')
    expect_equal(row('8')[['CV of back-calculated, %']], 'CV unavailable')
    expect_equal(row('16')$Accepted, '\u2713 singlet')
    expect_equal(row('8')$Accepted, '\u2713 single result')
    expect_equal(names(tab), c('Concentration', 'Mean OD', 'Back-calculated', 'CV of back-calculated, %', 'Bias, %',
                               'Limit, \u00b1%', 'Accepted'))
    # the wells against the plan: all levels in the app and the detailed report, the deviating ones in the short report
    tabs <- quality_tables(r$quality, 'en')
    expect_equal(names(tabs$wells), c('Concentration', 'Planned', 'Fitted', 'Back-calculated', 'Lost wells'))
    expect_equal(nrow(tabs$wells), nrow(tab))
    w <- function(conc) tabs$wells[tabs$wells$Concentration == conc, ]
    expect_equal(unlist(w('16')[2:4], use.names = FALSE), c(1, 1, 1))
    expect_equal(unlist(w('8')[2:4], use.names = FALSE), c(2, 1, 1))
    expect_equal(w('8')[['Lost wells']], paste0(exclude$key, ': excluded \u2014 bubble'))
    expect_equal(w('16')[['Lost wells']], '')
    expect_equal(tabs$wells_deviating$Concentration, '8')
    # no deviation from the plan: no table
    plain <- quality_tables(calculate(run_data(), 'LIN')$quality, 'en')
    expect_null(plain$wells)
    expect_null(plain$wells_deviating)

    # the reports: the deviating levels in the short one, all levels in the detailed one
    blocks <- function(type) {
        LinearityApp:::report_blocks(type, date = '2026-11-21', title = 't', method = 'm',
                                     model_table = get_coef_ci(r$fit), result_table = r$result,
                                     quality = list(summary = LinearityApp:::quality_summary(r, 'en'), tables = tabs))
    }
    wells_table <- function(b) {
        i <- which(vapply(b, function(x) identical(x$text, 'Wells deviating from the plan'), logical(1)))
        if (length(i) == 0) NULL else b[[i + 1]]$data
    }
    expect_equal(nrow(wells_table(blocks('short'))), 1)
    expect_equal(nrow(wells_table(blocks('detailed'))), nrow(tab))
    none <- LinearityApp:::report_blocks('detailed', date = '2026-11-21', title = 't', method = 'm',
                                         model_table = get_coef_ci(r$fit), result_table = r$result,
                                         quality = list(summary = LinearityApp:::quality_summary(r, 'en'),
                                                        tables = plain))
    expect_null(wells_table(none))

    # singlets concern the plan only; the replicate left alone follows its own rule
    strict <- calculate(data, 'LIN', exclude = exclude, criteria = qc_criteria(singlets = FALSE))
    st <- strict$quality$standards
    expect_equal(st$status[st$conc == 16], 'singlet')
    expect_false(st$pass[st$conc == 16])
    expect_equal(st$status[st$conc == 8], 'pass_no_cv')
    expect_error(qc_criteria(singlets = NA), 'singlets')
    expect_error(qc_criteria(replicate_loss = 'allow'), 'replicate_loss')
})

test_that('a standard level that lost a replicate follows an explicit rule', {
    data <- run_data()
    keys <- well_keys(data)
    eight <- keys[data$type == 'standard' & data$conc == 8]
    status <- function(r) r$quality$standards$status[r$quality$standards$conc == 8]
    reject <- qc_criteria(replicate_loss = 'reject')
    # excluded by the user with a reason: accepted by default, not with the strict rule
    excl <- data.frame(key = eight[1], reason = 'Outlier')
    expect_equal(status(calculate(data, 'LIN', exclude = excl)), 'pass_no_cv')
    strict <- calculate(data, 'LIN', exclude = excl, criteria = reject)
    expect_equal(status(strict), 'replicate_lost')
    expect_false(strict$quality$standards$pass[strict$quality$standards$conc == 8])
    tab <- quality_tables(strict$quality, 'en')$standards
    expect_equal(tab$Accepted[tab$Concentration == '8'], '\u2717 replicate lost')
    wells <- quality_tables(strict$quality, 'en')$wells
    expect_equal(unlist(wells[wells$Concentration == '8', 2:4], use.names = FALSE), c(2, 1, 1))
    # no OD: not a documented loss, whatever the rule
    missing <- data
    missing$OD[keys == eight[1]] <- NA
    r <- calculate(missing, 'LIN')
    expect_equal(status(r), 'replicate_lost')
    tab <- quality_tables(r$quality, 'en')$wells
    expect_equal(tab[['Lost wells']][tab$Concentration == '8'], paste0(eight[1], ': no OD'))
    expect_equal(status(calculate(missing, 'LIN', criteria = reject)), 'replicate_lost')
    # OD <= blank left out automatically (log-log): not a documented loss either
    low <- data
    low$OD[keys == eight[1]] <- 0.01
    r <- calculate(low, 'LRM')
    expect_equal(status(r), 'replicate_lost')
    tab <- quality_tables(r$quality, 'ru')$wells
    expect_match(tab[[tr('col.lost', 'ru')]][tab[[1]] == '8'], 'OD \u2264 \u0431\u043b\u0430\u043d\u043a\u0430, \u0438\u0441\u043a\u043b\u044e\u0447\u0435\u043d\u0430 \u0438\u0437 \u043f\u043e\u0434\u0433\u043e\u043d\u043a\u0438', fixed = TRUE)
    # both replicates kept: the rule does not apply
    expect_equal(status(calculate(data, 'LIN', criteria = reject)), 'pass')
    # with the same data a planned singlet and a replicate left alone are told apart by the rules, not silently
    single <- data[-which(keys == eight[1]), ]
    expect_equal(status(calculate(single, 'LIN', criteria = qc_criteria(singlets = FALSE))), 'singlet')
    expect_equal(status(calculate(missing, 'LIN', criteria = qc_criteria(singlets = FALSE))), 'replicate_lost')
})

test_that('the CV criterion of the standards uses back-calculated concentrations', {
    fit <- calculate(run_data(), 'LRM')$fit
    st <- assess_standards(fit)
    bc <- back_calculate(fit)
    expect_equal(st$calc.cv[1], LinearityApp:::cv_percent(bc$calc[bc$conc == st$conc[1]]))
    strict <- assess_standards(fit, qc_criteria(cv = 0.001, cv_edge = 0.001))
    expect_equal(strict$pass, is.na(st$calc.cv) | st$calc.cv <= 0.001)
    tab <- quality_tables(list(standards = st), 'en')$standards
    expect_true('CV of back-calculated, %' %in% names(tab))
    expect_false(any(grepl('OD, %', names(tab), fixed = TRUE)))
})

test_that('standards are sorted by decreasing concentration and failures are highlighted', {
    r <- calculate(run_data(bias = c('4' = 1.4)), 'LRM')
    plain <- quality_tables(r$quality, 'en')$standards
    expect_equal(plain[[1]], as.character(rev(linear_7)))
    expect_false(any(grepl('crit-fail', unlist(plain), fixed = TRUE)))
    marked <- quality_tables(r$quality, 'en', highlight = TRUE)$standards
    failed <- grepl('crit-fail', marked[['Bias, %']], fixed = TRUE)
    expect_equal(marked[[1]][failed], '8')
    expect_match(marked[['Accepted']][failed], '<span class="crit-fail">', fixed = TRUE)
    expect_false(any(grepl('crit-fail', marked[['CV of back-calculated, %']], fixed = TRUE)))
})

test_that('individual standard points are listed from the highest concentration', {
    r <- calculate(run_data(), 'LRM')
    for (lang in supported_languages()) {
        tab <- LinearityApp:::standard_points_table(r, lang)
        expect_equal(nrow(tab), 14)
        expect_equal(names(tab)[c(3, 4, 5, 8)], c(tr('col.nominal', lang), tr('col.od_raw', lang),
                                                  tr('col.od_corrected', lang), tr('col.error', lang)))
    }
    tab <- LinearityApp:::standard_points_table(r, 'en')
    expect_equal(tab$Standard[1:2], c('S1', 'S1'))
    expect_equal(tab$Nominal[1], '64')
    bc <- back_calculate(r$fit)
    top <- bc[bc$conc == 64, ][1, ]
    expect_equal(tab[['Bias, %']][1], sprintf('%.1f', 100 * (top$calc - 64) / 64))
    expect_false(any(grepl('e-', tab[['Abs. error']], fixed = TRUE)))
    expect_false(any(grepl('crit-fail', unlist(tab), fixed = TRUE)))

    # level 4 (conc 8) reads 40 % high: both of its points deviate, the others do not
    r <- calculate(run_data(bias = c('4' = 1.4)), 'LRM')
    marked <- LinearityApp:::standard_points_table(r, 'en', highlight = TRUE)
    failed <- grepl('crit-fail', marked[['Bias, %']], fixed = TRUE)
    expect_equal(marked$Nominal[failed], c('8', '8'))
})

test_that('the equation of each model uses the names of its coefficients', {
    d <- run_data()
    eq <- function(model, linear = FALSE) {
        as.character(LinearityApp:::model_equation_ui(calculate(d, model, linear = linear)$fit))
    }
    expect_match(eq('LRM'), 'log<sub>10</sub>(<i>OD</i>) = <i>a</i> + <i>b</i>', fixed = TRUE)
    expect_false(grepl('<i>c</i>', eq('LRM'), fixed = TRUE))
    expect_match(eq('QRM'), '<i>c</i> \u00b7 [log<sub>10</sub>(<i>C</i>)]<sup>2</sup>', fixed = TRUE)
    expect_match(eq('LRM', linear = TRUE), '<i>OD</i> = <i>a</i>', fixed = TRUE)
    expect_match(eq('LIN'), '^<div[^>]*><i>OD</i> = <i>a</i> \\+ <i>b</i> \u00b7 <i>C</i></div>$')
    expect_match(eq('QUAD'), '<i>OD</i> = <i>a</i> + <i>b</i> \u00b7 <i>C</i> + <i>c</i> \u00b7 <i>C</i><sup>2</sup>',
                 fixed = TRUE)
    expect_match(eq('4PL'), 'eq-frac', fixed = TRUE)
    expect_match(eq('5PL'), ']<sup><i>g</i></sup>', fixed = TRUE)
    for (m in calibration_models()) {
        coefs <- get_coef_ci(calculate(d, m)$fit)$coef
        html <- eq(m)
        for (k in coefs) expect_match(html, paste0('<i>', k, '</i>'), fixed = TRUE)
    }
})

test_that('the report contains the quality section', {
    r <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), 'QRM')
    path <- tempfile(fileext = '.docx')
    save_report(path, date = '2026-11-14', title = 'Q', plot = get_plots(r), method = 'QRM',
                model_table = get_coef_ci(r$fit), result_table = r$result,
                quality = list(summary = LinearityApp:::quality_summary(r, 'en'),
                               tables = quality_tables(r$quality, 'en')))
    text <- officer::docx_summary(officer::read_docx(path))$text
    expect_true('Quality of the run' %in% text)
    expect_true('Run accepted' %in% text)
    expect_true('Dilution linearity' %in% text)
})

test_that('criteria of the standards are in the curve step, those of the samples in the summary', {
    curve <- as.character(LinearityApp:::mod_curve_ui('curve', 'en'))
    expect_match(curve, 'id="criteria-std_bias"', fixed = TRUE)
    expect_match(curve, 'id="criteria-range"', fixed = TRUE)
    expect_false(grepl('id="criteria-dilution_cv"', curve, fixed = TRUE))
    expect_match(curve, 'id="curve-standards"', fixed = TRUE)
    expect_match(curve, 'id="curve-verdict"', fixed = TRUE)

    summary <- as.character(LinearityApp:::mod_summary_ui('summary', 'en'))
    for (id in c('sample_cv', 'sample_cv_edge', 'qc_bias', 'dilution_cv')) {
        expect_match(summary, paste0('id="criteria-', id, '"'), fixed = TRUE, info = id)
    }
    expect_false(grepl('id="criteria-std_bias"', summary, fixed = TRUE))
    for (key in c('crit.group_replicates', 'crit.group_qc', 'crit.group_dilution', 'summary.tab_table',
                  'summary.tab_infographics')) {
        expect_match(summary, tr(key, 'en'), fixed = TRUE, info = key)
    }
})

test_that('replicates follow the grouping chosen by the user', {
    # A: two dilutions that agree; B: two dilutions that do not; one well each
    samples <- data.frame(name = c('A', 'A', 'B', 'B'), dilution = c(2, 4, 2, 4), od = c(0.4, 0.2, 0.4, 0.3))
    r <- calculate(run_data(samples = samples), 'LRM', linear = TRUE, round_fun = no_round)

    by_sample <- assess_samples(r$result, r$calibration_range, group_vars = 'sample.name')
    expect_equal(by_sample$samples$sample.name, c('A', 'B'))
    expect_equal(by_sample$samples$n, c(2, 2))
    expect_equal(by_sample$samples$min, c(40, 40))
    # dilutions are compared within the groups even when they are averaged together
    expect_equal(nrow(by_sample$dilution), 4)
    expect_match(by_sample$samples$flag[2], 'Dilution non-linear', fixed = TRUE)
    expect_match(by_sample$samples$flag[2], 'High CV', fixed = TRUE)
    expect_equal(by_sample$samples$flag[1], 'OK')
    expect_equal(vapply(by_sample$warnings, function(m) m$key, ''), c('quality.warn_cv', 'quality.warn_dilution'))

    by_dilution <- assess_samples(r$result, r$calibration_range, group_vars = c('sample.name', 'dilution'))
    expect_equal(nrow(by_dilution$samples), 4)
    expect_equal(by_dilution$samples$dilution, c(2, 4, 2, 4))
    expect_equal(nrow(by_dilution$dilution), 4)

    # without the sample name, there are no samples to compare dilutions in
    only_dilution <- assess_samples(r$result, r$calibration_range, group_vars = 'dilution')
    expect_null(only_dilution$dilution)
    expect_equal(only_dilution$samples$n, c(2, 2))
})

test_that('the CV limit of samples near the ends of the range follows the concentration in the well', {
    samples <- data.frame(name = c('lo', 'lo', 'mid', 'mid'), dilution = c(10, 10, 1, 1),
                          od = c(0.022, 0.026, 0.40, 0.42))
    r <- calculate(run_data(samples = samples), 'LRM', linear = TRUE, round_fun = no_round)
    s <- assess_samples(r$result, r$calibration_range, qc_criteria(sample_cv = 10, sample_cv_edge = 30),
                        group_vars = 'sample.name')$samples
    expect_equal(s$cv.limit, c(30, 10))
})

test_that('quality controls without a code are grouped by their nominal concentration', {
    qc <- data.frame(id = c('QC-L', NA, NA, NA), nominal = c(4, 4, 40, 40), od = 0.02 * c(4, 4, 40, 40))
    data <- run_data(qc = qc)
    data$id[data$type == 'QC'] <- qc$id
    r <- calculate(data, 'LRM', linear = TRUE, round_fun = no_round)
    lv <- r$quality$qc$levels
    expect_equal(lv$id, c('QC-L', 'QC 4', 'QC 40'))
    expect_equal(lv$n, c(1, 1, 2))
    expect_true(r$quality$qc$accepted)
    expect_false(anyNA(quality_tables(r$quality)$qc))
})

test_that('the flags of the summary are split into range, precision and linearity', {
    samples <- data.frame(sample.name = c('A', 'A', 'B', 'C'), dilution = c(2, 4, 2, 1), cv = c(5, NA, 30, 4),
                          flag = c('Dilution non-linear; < LLOQ', 'Dilution non-linear', 'High CV; > ULOQ; < LLOQ',
                                   'OK'))
    dilution <- data.frame(sample.name = c('A', 'A', 'B', 'B'), dilution = c(2, 4, 2, 4), n = 2, n.quantified = 2,
                           mean = 1, deviation = 0, cv = 0, pass = c(FALSE, FALSE, TRUE, TRUE))
    checks <- summary_checks(samples, dilution, 'en')
    expect_equal(checks$range, c('Below', 'OK', 'Below; Above', 'OK'))
    expect_equal(checks$precision, c('OK', '', 'High CV', 'OK'))
    expect_equal(checks$linearity, c('Non-linear', 'Non-linear', 'Linear', ''))
    expect_equal(summary_checks(samples, NULL, 'ru')$linearity, c('Нелинейно', 'Нелинейно', '', ''))
    out <- summary_table(cbind(samples, mean = 1, sd = 0, min = 1, max = 1), 'sample.name',
                         stats = c('cv', 'range', 'linearity'), dilution = dilution)
    expect_equal(names(out), c('sample.name', 'cv', 'range', 'linearity'))
})
