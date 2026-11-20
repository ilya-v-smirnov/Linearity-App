make_report <- function(lang, ...) {
    r <- calculate(load_sample('griess'), 'QRM')
    path <- tempfile(fileext = '.docx')
    save_report(path,
                date = '2026-11-14',
                title = 'Test report',
                ...,
                plot = get_plots(r, plot.theme = report_theme(), lang = lang),
                method = tr('model.QRM', lang),
                scale = tr('scale.log', lang),
                model_table = get_coef_ci(r$model_direct, signif, 3),
                result_table = localize_result(r$result, lang),
                means_table = aggregate_result(r$result),
                notes = message_text(r$notes, lang),
                lang = lang)
    officer::docx_summary(officer::read_docx(path))$text
}

test_that('Word report is generated with a warnings section', {
    text <- make_report('en')
    expect_true('Warnings' %in% text)
    expect_true(any(grepl('OD <= blank', text, fixed = TRUE)))
    expect_true(any(grepl(tr('model.QRM', 'en'), text, fixed = TRUE)))
})

test_that('the head of the report has the date, the title, the operator and the plate', {
    text <- make_report('en', operator = 'I. Smirnov', plate = 'P-07')
    expect_equal(text[1:4], c('14.11.2026', 'Test report', 'Operator: I. Smirnov', 'Plate: P-07'))
    # empty fields are left out
    text <- make_report('ru', operator = '  ', plate = 'P-07')
    expect_equal(text[3], paste0(tr('report.plate_label', 'ru'), 'P-07'))
    expect_false(any(grepl(tr('report.operator_label', 'ru'), text, fixed = TRUE)))
})

test_that('Word report headings follow the language', {
    text <- make_report('ru')
    expect_true(tr('report.section_warnings', 'ru') %in% text)
    expect_true(any(grepl(tr('model.QRM', 'ru'), text, fixed = TRUE)))
    expect_true(any(grepl(tr('flag.below', 'ru'), text, fixed = TRUE)))
})

report_args <- function(lang = 'en') {
    r <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), 'QRM')
    list(date = '2026-11-14', title = 'Plate 1\nOperator: A. B.',
         method = tr('model.QRM', lang), scale = tr('scale.log', lang), weights = 'None',
         model_table = get_coef_ci(r$fit), result_table = localize_result(r$result, lang),
         means_table = aggregate_result(r$result), qc_table = NULL,
         exclusions = data.frame(key = character(0)), comparison = comparison_table(compare_models(r$standard_data), lang),
         quality = list(summary = LinearityApp:::quality_summary(r, lang), tables = quality_tables(r$quality, lang)),
         notes = c('note one'), lang = lang)
}

test_that('short and detailed reports have different sections', {
    args <- report_args()
    short <- do.call(LinearityApp:::report_blocks, c(list('short'), args))
    detailed <- do.call(LinearityApp:::report_blocks, c(list('detailed'), args))
    headings <- function(blocks) {
        vapply(Filter(function(b) b$type %in% c('heading', 'subheading'), blocks), `[[`, '', 'text')
    }
    expect_equal(headings(short), c('Standard curve', 'Result', 'Means'))
    expect_true(all(c('Analysis parameters', 'Standard curve', 'Comparison of models', 'Quality of the run',
                      'Standards (level means)', 'Dilution linearity', 'Result', 'Means', 'Warnings') %in% headings(detailed)))
    # the empty exclusions table is left out
    expect_false('Excluded standard points' %in% headings(detailed))
    expect_equal(sum(vapply(short, function(b) b$type == 'plot', TRUE)), 1)

    path <- tempfile(fileext = '.docx')
    LinearityApp:::write_report(short, path, get_plots(calculate(load_sample('bca'), 'QRM')))
    text <- officer::docx_summary(officer::read_docx(path))$text
    expect_true('Means' %in% text)
    expect_false('Quality of the run' %in% text)
    # the title keeps its lines
    expect_true(any(grepl('Plate 1', text, fixed = TRUE) & grepl('Operator', text, fixed = TRUE)))
})

test_that('the report states that the model was refitted and compares it with the original', {
    data <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    r <- calculate(data, 'QRM', exclude = data.frame(key = 'B1', reason = 'bubble'))
    args <- report_args()
    args$model_table <- get_coef_ci(r$fit)
    args$exclusions <- exclusions_table(r$exclusions, 'en')
    args$exclusion_effect <- LinearityApp:::exclusion_effect_tables(r$exclusion_effect, 'en')
    texts <- function(blocks) unlist(lapply(blocks, function(b) b$text))
    short <- do.call(LinearityApp:::report_blocks, c(list('short'), args))
    expect_true(any(grepl('refitted without 1 standard point', texts(short), fixed = TRUE)))
    expect_false('Effect of the excluded points' %in% texts(short))

    detailed <- do.call(LinearityApp:::report_blocks, c(list('detailed'), args))
    expect_true(all(c('Excluded standard points', 'Effect of the excluded points', 'Back-calculated standards',
                      'Back-calculation errors') %in% texts(detailed)))
    tables <- Filter(function(b) b$type == 'table', detailed)
    expect_true(any(vapply(tables, function(b) identical(names(b$data), c('Coef', 'Original', 'Refitted')), TRUE)))

    path <- tempfile(fileext = '.docx')
    LinearityApp:::write_report(detailed, path, get_plots(r, show_original = FALSE))
    text <- officer::docx_summary(officer::read_docx(path))$text
    expect_true('Effect of the excluded points' %in% text)
})

test_that('the preview shows the blocks and cuts long tables', {
    args <- report_args()
    blocks <- do.call(LinearityApp:::report_blocks, c(list('short'), args))
    html <- as.character(LinearityApp:::report_preview(blocks, shiny::div(id = 'plot-here'), 'en', max_rows = 5))
    expect_match(html, 'report-page', fixed = TRUE)
    expect_match(html, 'plot-here', fixed = TRUE)
    expect_match(html, '<div>Operator: A. B.</div>', fixed = TRUE)
    expect_match(html, sprintf('... and %d more rows', nrow(args$result_table) - 5), fixed = TRUE)
})
