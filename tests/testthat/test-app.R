rounding_3 <- function() shiny::reactive(list(fun = signif, par = 3))

test_that('import module reads, validates and reports errors', {
    shiny::testServer(LinearityApp:::mod_import_server, args = list(lang = 'en'), {
        session$setInputs(file = list(datapath = sample_path('griess.xlsx')))
        expect_equal(nrow(session$returned$data()), nrow(load_sample('griess')))

        session$setInputs(file = list(datapath = sample_path('invalid', '05_dilution_missing.xlsx')))
        err <- tryCatch(session$returned$data(), error = identity)
        expect_s3_class(err, 'validation')
        expect_match(conditionMessage(err), 'Import error: Not all samples')
    })
})

test_that('import errors are shown in Russian', {
    shiny::testServer(LinearityApp:::mod_import_server, args = list(lang = 'ru'), {
        session$setInputs(file = list(datapath = sample_path('invalid', '06_zero_dilution.xlsx')))
        err <- tryCatch(session$returned$data(), error = identity)
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
        expect_match(output$curve$src, '^data:image/png')
        expect_match(output$resid$src, '^data:image/png')
        expect_equal(nrow(session$returned$comparison()), 4)

        session$setInputs(model = '4PL', weights = '1/y2')
        expect_equal(session$returned$result()$model, '4PL')
        expect_equal(session$returned$weights(), '1/y2')
        expect_false(session$returned$linear())
        expect_equal(nrow(session$returned$coef_table()), 4)
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
         linear = shiny::reactive(FALSE),
         weights = shiny::reactive('none'),
         exclusions = shiny::reactive(data.frame(key = character(0), reason = character(0))),
         comparison = shiny::reactive(compare_models(r$standard_data)))
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
        session$setInputs(start = 100, step = 3, levels = 5, replicates = 2, samples = 4, export = 'long')
        path <- output$download
        long <- read_table(path)
        expect_equal(nrow(long), 2 * 6 + 4)
        expect_equal(names(long), c('well', 'sample.name', 'sample.date', 'type', 'dilution', 'conc', 'OD'))
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
        mod_import_server = function(...) list(data = NULL), mod_rounding_server = function(...) NULL,
        mod_curve_server = function(...) NULL, mod_results_server = function(...) NULL,
        mod_summary_server = function(...) NULL, mod_report_server = function(...) NULL,
        mod_criteria_server = function(...) NULL, mod_quality_server = function(...) NULL
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
        expect_true('well' %in% names(session$returned$data()))
        expect_null(output$problems)
        expect_null(output$empty)
        expect_match(output$table, 'A1', fixed = TRUE)
        expect_match(output$plate$html, 'well-standard', fixed = TRUE)
        expect_match(output$summary$html, 'stat-tile', fixed = TRUE)

        session$setInputs(file = list(datapath = sample_path('invalid', '05_dilution_missing.xlsx')))
        expect_match(output$problems$html, 'Problems found in the file', fixed = TRUE)
        expect_match(output$problems_table, 'Not all samples have dilution factors specified', fixed = TRUE)
        expect_match(output$problems_table, 'rows 26', fixed = TRUE)
    })
})

test_that('template module downloads plate maps', {
    shiny::testServer(LinearityApp:::mod_template_server, args = list(lang = 'ru'), {
        session$setInputs(size = '96', by = 'column', position = 'before', export = 'maps',
                          start = 100, step = 2, levels = 7, replicates = 2, samples = 10)
        path <- output$download
        expect_true(LinearityApp:::is_plate_maps_workbook(path))
        expect_true(all(c('type', 'conc', 'OD', 'sample.name') %in% readxl::excel_sheets(path)))
        expect_match(output$plate$html, '>S7<', fixed = TRUE)
        expect_match(output$plate$html, 'editable', fixed = TRUE)
        expect_match(output$standards, '<td> 1.56 </td>', fixed = TRUE)
        expect_equal(session$returned$conc()[7], 1.56)
    })
})

test_that('template layout follows the parameters and is edited by the user', {
    shiny::testServer(LinearityApp:::mod_template_server, args = list(lang = 'en'), {
        session$setInputs(size = '96', by = 'row', position = 'after',
                          start = 100, step = 2, levels = 7, replicates = 2, samples = 10)
        grid <- session$returned$layout()
        expect_equal(unname(grid['A', 1:10]), paste0('X', 1:10))
        expect_equal(unname(grid['B', 1:8]), c(paste0('S', 1:7), 'B'))

        session$setInputs(edit = list(action = 'sample', wells = list('H11', 'H12'), nonce = 1))
        expect_equal(unname(session$returned$layout()['H', 11:12]), c('X11', 'X12'))
        # the browser receives the new number of samples; it does not change the layout
        session$setInputs(samples = 12)
        expect_equal(LinearityApp:::count_samples(session$returned$layout()), 12)
        expect_equal(session$returned$layout()[['H', 12]], 'X12')

        session$setInputs(edit = list(action = 'blank', wells = list('A1'), nonce = 2))
        grid <- session$returned$layout()
        expect_equal(grid[['A', 1]], 'B')
        expect_equal(unname(grid['A', 2:10]), paste0('X', 1:9))
        # standards are not added by hand
        session$setInputs(edit = list(action = 'standard', wells = list('D1'), nonce = 3))
        expect_true(is.na(session$returned$layout()[['D', 1]]))

        # the number of samples changes the edited layout instead of discarding it
        session$setInputs(samples = 11)
        expect_equal(LinearityApp:::count_samples(session$returned$layout()), 11)
        session$setInputs(samples = 10)
        grid <- session$returned$layout()
        expect_equal(grid[['A', 1]], 'B')
        expect_equal(LinearityApp:::count_samples(grid), 10)
        expect_equal(grid[['H', 11]], 'X10')
        expect_true(is.na(grid[['H', 12]]))

        # a new start concentration keeps the edits, a new structure discards them
        session$setInputs(start = 50)
        expect_equal(session$returned$layout()[['A', 1]], 'B')
        session$setInputs(levels = 6)
        expect_equal(session$returned$layout()[['A', 1]], 'X1')

        session$setInputs(edit = list(action = 'empty', wells = list('A1'), nonce = 4))
        expect_true(is.na(session$returned$layout()[['A', 1]]))
        session$setInputs(reset = 1)
        expect_equal(session$returned$layout()[['A', 1]], 'X1')

        session$setInputs(export = 'long')
        path <- output$download
        expect_equal(sum(read_table(path)$type == 'sample'), 10)
    })
})

test_that('template layout is shifted along the filling direction', {
    shiny::testServer(LinearityApp:::mod_template_server, args = list(lang = 'en'), {
        session$setInputs(size = '96', by = 'column', position = 'before',
                          start = 100, step = 2, levels = 7, replicates = 2, samples = 10)
        expect_match(output$shift$html, 'shift_right', fixed = TRUE)
        expect_match(output$shift$html, '<button[^>]*disabled[^>]*shift_left')
        session$setInputs(shift_right = 1)
        expect_equal(session$returned$layout()[['A', 2]], 'S1')
        expect_true(all(is.na(session$returned$layout()[, 1])))
        session$setInputs(shift_left = 1)
        expect_equal(session$returned$layout()[['A', 1]], 'S1')

        session$setInputs(by = 'row')
        expect_match(output$shift$html, 'shift_down', fixed = TRUE)
        session$setInputs(shift_down = 1)
        expect_equal(session$returned$layout()[['B', 1]], 'S1')
    })
})

test_that('template preview reports a plate that is too small', {
    shiny::testServer(LinearityApp:::mod_template_server, args = list(lang = 'en'), {
        session$setInputs(size = '96', start = 100, step = 2, levels = 7, replicates = 2, samples = 90)
        err <- tryCatch(output$plate, error = identity)
        expect_s3_class(err, 'validation')
        expect_match(conditionMessage(err), 'Cannot build the template', fixed = TRUE)
    })
})

test_that('plate grids and counts describe imported data', {
    d <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    grids <- LinearityApp:::data_plate_grids(d)
    expect_equal(dim(grids$codes), c(8, 12))
    expect_equal(grids$codes['A', '1'], 'S1')
    expect_equal(grids$types['A', '3'], 'sample')
    expect_equal(unname(grids$values['A', '1']), d$OD[d$well == 'A1'])
    counts <- LinearityApp:::data_counts(d)
    expect_equal(counts[['wells']], nrow(d))
    expect_gt(counts[['levels']], 0)
    expect_null(LinearityApp:::data_plate_grids(load_sample('griess')))
    expect_equal(LinearityApp:::data_counts(load_sample('griess'))[['qc']], 0)
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

test_that('standard points are excluded and restored by clicking on the curve', {
    data <- shiny::reactive(check_assay(read_assay(sample_path('bca_plate.xlsx'))))
    clicked_point <- NULL
    local_mocked_bindings(nearPoints = function(df, ...) df[df$key == clicked_point, ], .package = 'shiny')
    shiny::testServer(LinearityApp:::mod_curve_server,
                      args = list(lang = 'en', data = data, rounding = rounding_3()), {
        session$setInputs(model = 'QRM', weights = 'none', linear_scale = FALSE,
                          xtitle = 'x', xunits = '', ytitle = 'y')
        n_used <- nrow(session$returned$result()$standard_data)

        clicked_point <<- 'B1'
        session$setInputs(curve_click = list(x = 1, y = 1))
        session$setInputs(reason = '   ', confirm_exclude = 1)
        expect_equal(nrow(session$returned$exclusions()), 0)

        session$setInputs(reason = 'bubble in the well', confirm_exclude = 2)
        expect_equal(session$returned$exclusions()$key, 'B1')
        expect_equal(nrow(session$returned$result()$standard_data), n_used - 1)
        expect_equal(session$returned$result()$exclusions$reason, 'bubble in the well')
        expect_match(output$exclusions_table, 'bubble in the well', fixed = TRUE)

        session$setInputs(curve_click = list(x = 2, y = 2))
        session$setInputs(confirm_restore = 1)
        expect_equal(nrow(session$returned$exclusions()), 0)

        clicked_point <<- 'C2'
        session$setInputs(curve_click = list(x = 3, y = 3))
        session$setInputs(reason = 'outlier', confirm_exclude = 3)
        session$setInputs(restore_all = 1)
        expect_equal(nrow(session$returned$exclusions()), 0)
    })
})
