test_that('Word report is generated with warnings section', {
    suppressMessages(source(file.path(project_root, 'report.R')))
    r <- calculate(load_sample('griess'), 'QRM')
    path <- tempfile(fileext = '.docx')
    save_report(path,
                date = '2026-11-14',
                title = 'Test report',
                plot = get_plots(r, plot.theme = report_theme),
                method = MODELS[['QRM']],
                scale = 'log10',
                model_table = get_coef_ci(r$model_direct, signif, 3),
                result_table = r$result,
                means_table = aggregate_result(r$result),
                notes = r$notes)
    expect_true(file.exists(path))

    text <- officer::docx_summary(officer::read_docx(path))$text
    expect_true('Warnings' %in% text)
    expect_true(any(grepl('OD <= NC', text, fixed = TRUE)))
    expect_true(any(grepl('Quadratic regression model', text, fixed = TRUE)))
})
