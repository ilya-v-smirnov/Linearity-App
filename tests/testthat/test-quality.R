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
        data.frame(type = 'NC', sample.name = NA, dilution = NA, conc = NA, OD = c(nc, nc)),
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

test_that('failing end levels narrow the quantification range', {
    data <- run_data(bias = c('1' = 2), samples = data.frame(name = c('lo', 'mid'), dilution = 1,
                                                             od = c(0.025, 0.4)))
    r <- calculate(data, 'LRM', linear = TRUE, round_fun = no_round)
    expect_equal(r$calibration_range[1], 2)
    expect_equal(r$standard_range, c(1, 64))
    expect_equal(r$result$flag, c('< LLOQ', 'OK'))
    expect_match(message_text(r$notes), 'LLOQ = 2', fixed = TRUE, all = FALSE)

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
    s <- r$quality$samples
    expect_equal(s$flag[s$sample.name == 'B'], c('Dilution non-linear', 'Dilution non-linear'))
    expect_match(message_text(r$notes), 'Non-linear dilutions in 1', all = FALSE)

    expect_null(dilution_linearity(s[s$sample.name == 'A' & s$dilution == 2, ]))
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

test_that('quality and criteria modules work', {
    shiny::testServer(LinearityApp:::mod_criteria_server, args = list(lang = 'en'), {
        session$setInputs(std_bias = 15, std_bias_edge = 20, cv = 15, cv_edge = 20, min_fraction = 75,
                          min_levels = 6, qc_bias = 15, qc_bias_edge = 20, qc_fraction = 67,
                          qc_level_fraction = 50, dilution_bias = 20, dilution_cv = 20, range = 'all')
        cr <- session$returned()
        expect_equal(cr$std_bias, 15)
        expect_equal(cr$min_fraction, 0.75)
        expect_equal(cr$range, 'all')
        session$setInputs(cv = -5)
        expect_error(session$returned(), 'Invalid acceptance criteria')
        session$setInputs(reset = 1)
    })

    r <- calculate(run_data(bias = c('3' = 1.5, '5' = 0.6)), 'LRM')
    curve <- list(result = shiny::reactive(r))
    shiny::testServer(LinearityApp:::mod_quality_server, args = list(lang = 'ru', curve = curve), {
        expect_match(output$verdict$html, tr('quality.rejected', 'ru'), fixed = TRUE)
        expect_match(output$standards, tr('quality.fail', 'ru'), fixed = TRUE)
        expect_null(output$qc)
    })
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

test_that('the quality step has grouped criteria in the sidebar and two tabs', {
    html <- as.character(LinearityApp:::mod_quality_ui('quality', 'en'))
    expect_match(html, 'bslib-sidebar-layout', fixed = TRUE)
    expect_match(html, 'id="quality-criteria-std_bias"', fixed = TRUE)
    expect_match(html, 'id="quality-criteria-dilution_cv"', fixed = TRUE)
    for (key in c('crit.group_standards', 'crit.group_replicates', 'crit.group_qc', 'crit.group_dilution',
                  'quality.tab_standards', 'quality.tab_samples')) {
        expect_match(html, tr(key, 'en'), fixed = TRUE, info = key)
    }
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
