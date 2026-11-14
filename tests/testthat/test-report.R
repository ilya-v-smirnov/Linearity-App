make_report <- function(lang) {
    r <- calculate(load_sample('griess'), 'QRM')
    path <- tempfile(fileext = '.docx')
    save_report(path,
                date = '2026-11-14',
                title = 'Test report',
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
    expect_true(any(grepl('OD <= NC', text, fixed = TRUE)))
    expect_true(any(grepl('Quadratic regression model', text, fixed = TRUE)))
})

test_that('Word report headings follow the language', {
    text <- make_report('ru')
    expect_true(tr('report.section_warnings', 'ru') %in% text)
    expect_true(any(grepl(tr('model.QRM', 'ru'), text, fixed = TRUE)))
    expect_true(any(grepl(tr('flag.below', 'ru'), text, fixed = TRUE)))
})
