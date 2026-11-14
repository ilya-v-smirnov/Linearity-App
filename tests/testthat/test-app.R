rounding_3 <- function() shiny::reactive(list(fun = signif, par = 3))

test_that('import module reads, validates and reports errors', {
    shiny::testServer(LinearityApp:::mod_import_server, args = list(lang = 'en'), {
        session$setInputs(file = list(datapath = sample_path('griess.xlsx')))
        expect_equal(nrow(session$returned()), nrow(load_sample('griess')))

        session$setInputs(file = list(datapath = sample_path('invalid', '05_dilution_missing.xlsx')))
        err <- tryCatch(session$returned(), error = identity)
        expect_s3_class(err, 'validation')
        expect_match(conditionMessage(err), 'Import error: Not all samples')
    })
})

test_that('import errors are shown in Russian', {
    shiny::testServer(LinearityApp:::mod_import_server, args = list(lang = 'ru'), {
        session$setInputs(file = list(datapath = sample_path('invalid', '06_zero_dilution.xlsx')))
        err <- tryCatch(session$returned(), error = identity)
        expect_match(conditionMessage(err), tr('error.dilution_positive', 'ru'), fixed = TRUE)
        expect_match(conditionMessage(err), tr('common.import_error', 'ru', message = ''), fixed = TRUE)
    })
})

test_that('curve module recalculates when the model changes', {
    data <- shiny::reactive(load_sample('bca'))
    shiny::testServer(LinearityApp:::mod_curve_server,
                      args = list(lang = 'en', data = data, rounding = rounding_3()), {
        session$setInputs(model = 'LRM', linear_scale = FALSE,
                          xtitle = 'Concentration', xunits = ', ng/ml', ytitle = 'OD')
        expect_equal(session$returned$result()$model, 'LRM')
        expect_equal(nrow(session$returned$coef_table()), 2)
        expect_equal(session$returned$plot_args()$xtitle, 'Concentration, ng/ml')

        session$setInputs(model = 'QRM', linear_scale = TRUE)
        expect_equal(session$returned$result()$model, 'QRM')
        expect_true(session$returned$result()$linear_scale)
        expect_equal(nrow(session$returned$coef_table()), 3)
        expect_match(output$plots$src, '^data:image/png')
    })
})

test_that('curve module shows calculation errors as validation messages', {
    few <- synthetic_data(function(x) 0.1 * x, conc = c(1, 2, 4))
    shiny::testServer(LinearityApp:::mod_curve_server,
                      args = list(lang = 'en', data = shiny::reactive(few), rounding = rounding_3()), {
        session$setInputs(model = 'QRM', linear_scale = TRUE)
        err <- tryCatch(session$returned$result(), error = identity)
        expect_s3_class(err, 'validation')
        expect_match(conditionMessage(err), 'Calculation error: At least 4')
    })
})

fake_curve <- function(lang = 'en') {
    r <- calculate(load_sample('griess'), 'LRM')
    list(result = shiny::reactive(r),
         coef_table = shiny::reactive(get_coef_ci(r$model_direct, signif, 3)),
         notes = shiny::reactive(message_text(r$notes, lang)),
         plot_args = shiny::reactive(list(xtitle = 'x', ytitle = 'y')),
         model = shiny::reactive('LRM'),
         linear = shiny::reactive(FALSE))
}

test_that('results module translates flags and downloads tables', {
    shiny::testServer(LinearityApp:::mod_results_server,
                      args = list(lang = 'ru', curve = fake_curve('ru')), {
        expect_true(tr('flag.od_le_nc', 'ru') %in% session$returned()$flag)
    })
})

test_that('summary module keeps the selection and aggregates', {
    shiny::testServer(LinearityApp:::mod_summary_server,
                      args = list(lang = 'en', curve = fake_curve(), rounding = rounding_3()), {
        session$setInputs(group_vars = c('sample.name', 'sample.date'))
        out <- session$returned()
        expect_true(all(c('sample.name', 'sample.date', 'n', 'n.quantified') %in% names(out)))
        expect_equal(sum(out$n), 12)
    })
})

test_that('download modules write files', {
    shiny::testServer(LinearityApp:::mod_template_server, args = list(lang = 'en'), {
        session$setInputs(start = 100, step = 3, levels = 5, replicates = 2, samples = 4)
        expect_equal(nrow(session$returned()), 2 * 6 + 4)
    })

    tab <- shiny::reactive(data.frame(a = 1:2, b = c(0.5, NA)))
    shiny::testServer(LinearityApp:::mod_table_download_server,
                      args = list(table = tab, prefix = 'x'), {
        session$setInputs(filetype = 'csv', dialect = 'semicolon')
        path <- output$download
        expect_equal(read_table(path)$b, c(0.5, NA))
    })

    shiny::testServer(LinearityApp:::mod_report_server,
                      args = list(lang = 'en', curve = fake_curve(),
                                  means = shiny::reactive(NULL)), {
        session$setInputs(title = 'Test')
        expect_true(file.exists(output$download))
    })
})

test_that('rounding module switches defaults', {
    shiny::testServer(LinearityApp:::mod_rounding_server, {
        session$setInputs(fun = 'signif', par = 4)
        expect_identical(session$returned()$fun, signif)
        session$setInputs(fun = 'round', par = 2)
        expect_identical(session$returned()$fun, round)
    })
})

test_that('UI is built in every language and the language comes from the URL', {
    for (lang in supported_languages()) {
        ui <- LinearityApp:::app_ui(list(QUERY_STRING = paste0('?lang=', lang)))
        html <- as.character(ui)
        expect_match(html, tr('tab.results', lang), fixed = TRUE)
        expect_match(html, 'language_selector', fixed = TRUE)
    }
    expect_equal(LinearityApp:::request_lang(list(QUERY_STRING = '?lang=ru')), 'ru')
    expect_equal(LinearityApp:::request_lang(list(QUERY_STRING = '?lang=xx')), 'en')
    expect_equal(LinearityApp:::request_lang(list(), default = 'ru'), 'ru')
    expect_s3_class(run_app(lang = 'ru'), 'shiny.appobj')
})

test_that('unit choices use the micro sign', {
    units <- LinearityApp:::unit_choices('en')
    expect_equal(units[['ug/ml']], ', μg/ml')
    expect_equal(units[['uM']], ', μM')
    expect_equal(names(LinearityApp:::unit_choices('ru'))[1], tr('curve.unit_none', 'ru'))
})

test_that('server takes the language from the page URL', {
    langs <- character(0)
    local_mocked_bindings(mod_template_server = function(id, lang) langs <<- c(langs, lang))
    local_mocked_bindings(
        mod_import_server = function(...) NULL, mod_rounding_server = function(...) NULL,
        mod_curve_server = function(...) NULL, mod_results_server = function(...) NULL,
        mod_summary_server = function(...) NULL, mod_report_server = function(...) NULL
    )
    fake_session <- function(search) list(clientData = list(url_search = search))
    LinearityApp:::app_server(list(), list(), fake_session('?lang=ru'))
    LinearityApp:::app_server(list(), list(), fake_session(''), default_lang = 'ru')
    LinearityApp:::app_server(list(), list(), fake_session(''))
    expect_equal(langs, c('ru', 'ru', 'en'))
})

test_that('import module reads plate workbooks and shows problems', {
    shiny::testServer(LinearityApp:::mod_import_server, args = list(lang = 'en'), {
        session$setInputs(file = list(datapath = sample_path('bca_plate.xlsx')))
        expect_true('well' %in% names(session$returned()))
        expect_null(output$problems)
        expect_match(output$table, 'A1', fixed = TRUE)

        session$setInputs(file = list(datapath = sample_path('invalid', '05_dilution_missing.xlsx')))
        expect_match(output$problems$html, 'Problems found in the file', fixed = TRUE)
        expect_match(output$problems_table, 'Not all samples have dilution factors specified', fixed = TRUE)
        expect_match(output$problems_table, 'rows 26', fixed = TRUE)
    })
})

test_that('template module downloads a plate workbook', {
    shiny::testServer(LinearityApp:::mod_template_server, args = list(lang = 'ru'), {
        session$setInputs(format = 'plate', size = '96', start = 100, step = 2, levels = 7,
                          replicates = 2, samples = 10, sample_reps = 2)
        path <- output$download_plate
        expect_true(LinearityApp:::is_plate_workbook(path))
        expect_true(tr('plate.sheet_samples', 'ru') %in% readxl::excel_sheets(path))
    })
})

test_that('results module shows quality controls', {
    curve <- fake_curve()
    qc_result <- shiny::isolate(curve$result())
    qc_result$qc <- data.frame(id = 'QC-L', nominal = 2, conc = 2.2, recovery = 110, OD = 0.3, flag = 'OK')
    curve$result <- shiny::reactive(qc_result)
    shiny::testServer(LinearityApp:::mod_results_server, args = list(lang = 'en', curve = curve), {
        expect_match(output$qc$html, 'Quality controls', fixed = TRUE)
        expect_match(output$qc_table, 'QC-L', fixed = TRUE)
    })
})
