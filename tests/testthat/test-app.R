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
        session$setInputs(model = 'LRM',
                          xtitle = 'Concentration', xunits = ', ng/ml', ytitle = 'OD')
        expect_equal(session$returned$result()$model, 'LRM')
        expect_equal(nrow(session$returned$coef_table()), 2)
        expect_equal(session$returned$plot_args()$xtitle, 'Concentration, ng/ml')
        # with the units chosen, the plot can be saved and no message asks for them
        expect_true(session$returned$units_ok())
        expect_match(output$save_plot_ui$html, 'shiny-download-link', fixed = TRUE)

        session$setInputs(model = 'QRM')
        expect_equal(session$returned$result()$model, 'QRM')
        expect_false(session$returned$result()$linear_scale)
        expect_equal(nrow(session$returned$coef_table()), 3)
        expect_match(output$curve$src, '^data:image/png')
        expect_match(output$resid$src, '^data:image/png')
        expect_equal(nrow(session$returned$comparison()), 4)

        session$setInputs(model = '4PL', weights = '1/y2')
        expect_equal(session$returned$result()$model, '4PL')
        expect_equal(session$returned$weights(), '1/y2')
        expect_equal(nrow(session$returned$coef_table()), 4)
    })
})

test_that('curve module shows calculation errors as validation messages', {
    few <- synthetic_data(function(x) 0.1 * x, conc = c(1, 2, 4))
    shiny::testServer(LinearityApp:::mod_curve_server,
                      args = list(lang = 'en', data = shiny::reactive(few), rounding = rounding_3()), {
        session$setInputs(model = 'QRM')
        err <- tryCatch(session$returned$result(), error = identity)
        expect_s3_class(err, 'validation')
        expect_match(conditionMessage(err), 'Calculation error: At least 4')
    })
})

fake_curve <- function(lang = 'en', units_ok = TRUE) {
    r <- calculate(load_sample('griess'), 'LRM')
    list(result = shiny::reactive(r),
         units_ok = shiny::reactive(units_ok),
         coef_table = shiny::reactive(get_coef_ci(r$model_direct, signif, 3)),
         notes = shiny::reactive(message_text(r$notes, lang)),
         plot_args = shiny::reactive(list(xtitle = 'x', ytitle = 'y')),
         model = shiny::reactive('LRM'),
         weights = shiny::reactive('none'),
         exclusions = shiny::reactive(data.frame(key = character(0), reason = character(0))),
         comparison = shiny::reactive(compare_models(r$standard_data)),
         effect_tables = shiny::reactive(NULL))
}

test_that('results module translates flags and downloads tables', {
    shiny::testServer(LinearityApp:::mod_results_server,
                      args = list(lang = 'ru', curve = fake_curve('ru')), {
        expect_true(tr('flag.od_le_nc', 'ru') %in% session$returned()$flag)
    })
})

test_that('summary module groups by sample by default, offers quick choices and exports', {
    shiny::testServer(LinearityApp:::mod_summary_server,
                      args = list(lang = 'en', curve = fake_curve(), rounding = rounding_3()), {
        # before the browser answers, the samples are the groups
        out <- session$returned$table()
        expect_equal(names(out)[1:2], c('sample.name', 'sample.date'))
        expect_true(all(c('n', 'n.quantified', 'mean', 'sd', 'cv', 'range', 'precision', 'linearity') %in% names(out)))
        expect_false('flag' %in% names(out))
        # the mean next to the minimum and the maximum, then the spread
        expect_equal(names(out)[names(out) %in% c('mean', 'min', 'max', 'sd', 'cv')],
                     c('mean', 'min', 'max', 'sd', 'cv'))
        expect_equal(sum(out$n), 12)
        expect_equal(out$sample.name, c('M0', 'M0', 'M1', 'M1', 'M2', 'M2'))
        expect_match(output$preview$html, '12 wells', fixed = TRUE)

        session$setInputs(group_vars = c('sample.name', 'sample.date'))
        expect_equal(nrow(session$returned$assessment()$samples), 6)

        # the infographics show the groups
        expect_match(output$infographics$html, 'stat-tile', fixed = TRUE)
        expect_match(output$infographics$html, 'groups_plot', fixed = TRUE)
        expect_match(output$groups_plot$src, '^data:image/png')
        expect_match(output$infographics$html, '4 of 12 sample wells quantified', fixed = TRUE)
        # every infographic explains itself in a tooltip
        for (key in c('samples.quant_help', 'samples.figure_help', 'samples.qc_help')) {
            expect_match(output$infographics$html, html_escape(tr(key, 'en')), fixed = TRUE)
        }

        session$setInputs(group_vars = c('sample.name', 'sample.date', 'dilution'), stats = c('n', 'mean'))
        out <- session$returned$table()
        expect_equal(names(out), c('sample.name', 'sample.date', 'dilution', 'n', 'mean'))
        expect_equal(names(readxl::read_excel(output$download)), names(out))

        session$setInputs(group_vars = character(0))
        expect_null(session$returned$table())
        expect_match(output$empty$html, 'Choose at least one column', fixed = TRUE)
        expect_match(output$infographics$html, 'Choose at least one column', fixed = TRUE)
    })
    expect_equal(LinearityApp:::grouping_columns(data.frame(well = 1, id = 1, sample.name = 1, group = 1,
                                                            dilution = 1, OD = 1, conc = 1, flag = 1)),
                 c('id', 'sample.name', 'group', 'dilution'))
})

test_that('download modules write files', {
    shiny::testServer(LinearityApp:::mod_template_server, args = list(lang = 'en'), {
        session$setInputs(start = 100, step = 3, levels = 5, replicates = 2, samples = 4, export = 'long')
        path <- output$download
        long <- read_table(path)
        expect_equal(nrow(long), 2 * 6 + 4)
        expect_equal(names(long), c('well', 'sample.name', 'sample.date', 'type', 'dilution', 'conc', 'OD'))
    })

    summary <- list(table = shiny::reactive(NULL), assessment = shiny::reactive(NULL))
    shiny::testServer(LinearityApp:::mod_report_server,
                      args = list(lang = 'en', curve = fake_curve(), summary = summary), {
        session$setInputs(title = 'Test', type = 'detailed')
        expect_true(file.exists(output$download))
        expect_match(output$preview$html, 'Quality of the run', fixed = TRUE)
        session$setInputs(type = 'short')
        expect_false(grepl('Quality of the run', output$preview$html, fixed = TRUE))
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
    local_mocked_bindings(mod_template_server = function(id, lang) {
        langs <<- c(langs, lang)
        list(by = NULL)
    })
    local_mocked_bindings(
        mod_import_server = function(...) list(data = NULL), mod_rounding_server = function(...) NULL,
        mod_curve_server = function(...) NULL, mod_results_server = function(...) NULL,
        mod_summary_server = function(...) NULL, mod_report_server = function(...) NULL,
        mod_criteria_server = function(...) NULL,
        mod_quant_options_server = function(...) NULL
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
        expect_match(output$table, 'A1 .*B1 .*A2 ')
        expect_match(output$plate$html, 'well-standard', fixed = TRUE)
        expect_match(output$summary$html, 'stat-tile', fixed = TRUE)

        session$setInputs(file = list(datapath = sample_path('invalid', '05_dilution_missing.xlsx')))
        expect_match(output$problems$html, 'Problems found in the file', fixed = TRUE)
        expect_match(output$problems_table, 'Not all samples have dilution factors specified', fixed = TRUE)
        expect_match(output$problems_table, 'rows 26', fixed = TRUE)
    })
})

test_that('imported wells are listed in the filling order', {
    by <- shiny::reactiveVal('row')
    shiny::testServer(LinearityApp:::mod_import_server, args = list(lang = 'en', by = by), {
        session$setInputs(file = list(datapath = sample_path('bca_plate.xlsx')))
        expect_match(output$table, 'A1 .*A2 .*A3 .*B1 ')
        by('column')
        session$flushReact()
        expect_match(output$table, 'A1 .*B1 .*C1 .*A2 ')
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

test_that('quality controls are added to the template by level', {
    shiny::testServer(LinearityApp:::mod_template_server, args = list(lang = 'en'), {
        session$setInputs(size = '96', by = 'column', position = 'before',
                          start = 100, step = 2, levels = 7, replicates = 2, samples = 10, qc_levels = 0)
        expect_null(output$qc_tool)
        session$setInputs(edit = list(action = 'qc', wells = list('A5'), level = '1', nonce = 1))
        expect_true(is.na(session$returned$layout()[['A', 5]]))

        session$setInputs(qc_levels = 2, qc_conc_1 = 5, qc_conc_2 = NA)
        expect_match(output$qc_tool$html, 'data-qc-level', fixed = TRUE)
        expect_match(output$qc_inputs$html, 'Nominal concentrations', fixed = TRUE)
        expect_match(output$qc_inputs$html, '>QC2:</label>', fixed = TRUE)
        expect_match(output$qc_table, '<td> X </td>', fixed = TRUE)
        session$setInputs(edit = list(action = 'qc', wells = list('A5', 'B5'), level = '1', nonce = 2))
        session$setInputs(edit = list(action = 'qc', wells = list('C5'), level = '2', nonce = 3))
        grid <- session$returned$layout()
        expect_equal(unname(grid[1:3, 5]), c('QC1', 'QC1', 'QC2'))
        expect_equal(session$returned$qc_conc(), c(5, NA))

        session$setInputs(export = 'long')
        long <- read_table(output$download)
        expect_equal(long$conc[long$type == 'QC'], c(5, 5, NA))

        session$setInputs(qc_levels = 1)
        expect_true(is.na(session$returned$layout()[['C', 5]]))
        expect_equal(session$returned$layout()[['A', 5]], 'QC1')
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
    # samples are combinations of names, dates and additional columns, not dilutions
    expect_equal(counts[['samples']], length(unique(d$sample.name[d$type == 'sample'])))
    extra <- data.frame(type = c('sample', 'sample', 'sample', 'sample', 'standard'),
                        sample.name = c('a', 'a', 'a', 'b', NA), sample.date = NA,
                        group = c('x', 'x', 'y', 'x', NA), dilution = c(1, 2, 1, 1, NA),
                        conc = c(NA, NA, NA, NA, 1), OD = 1)
    expect_equal(LinearityApp:::data_counts(extra)[['samples']], 3)
    expect_equal(LinearityApp:::well_order(c('B1', 'A2', 'A1', 'A10', 'A9')), c(3, 1, 2, 5, 4))
    expect_equal(LinearityApp:::well_order(c('B1', 'A2', 'A1', 'A10', 'A9'), 'row'), c(3, 2, 5, 4, 1))
})

test_that('results module shows quality controls', {
    curve <- fake_curve()
    qc_result <- shiny::isolate(curve$result())
    qc_result$qc <- data.frame(id = 'QC-L', nominal = 2, conc = 2.2, recovery = 110, OD = 0.3, flag = 'OK')
    curve$result <- shiny::reactive(qc_result)
    shiny::testServer(LinearityApp:::mod_results_server, args = list(lang = 'en', curve = curve), {
        expect_match(output$qc$html, 'Quality controls', fixed = TRUE)
        expect_match(output$qc_table, 'nominal', fixed = TRUE)
        path <- output$download
        expect_equal(readxl::excel_sheets(path), c('Results', 'Quality controls'))
        expect_equal(readxl::read_excel(path, sheet = 'Quality controls')$id, 'QC-L')
    })
})

test_that('results are ordered by well, columns are chosen and exported to Excel', {
    r <- calculate(check_assay(read_assay(sample_path('bca_plate.xlsx'))), 'LRM')
    curve <- fake_curve()
    curve$result <- shiny::reactive(r)
    by <- shiny::reactiveVal('column')
    shiny::testServer(LinearityApp:::mod_results_server, args = list(lang = 'en', curve = curve, by = by), {
        out <- session$returned()
        expect_equal(names(out)[1:3], c('well', 'id', 'sample.name'))
        expect_equal(tail(names(out), 4), c('conc', 'conc.lower', 'conc.upper', 'flag'))
        expect_equal(out$well[1:3], c('A3', 'B3', 'C3'))
        by('row')
        session$flushReact()
        expect_equal(session$returned()$well[1:3], c('A3', 'A4', 'A5'))

        session$setInputs(columns = c('well', 'conc', 'flag'))
        expect_equal(names(session$returned()), c('well', 'conc', 'flag'))
        saved <- readxl::read_excel(output$download, sheet = 'Results')
        expect_equal(names(saved), c('well', 'conc', 'flag'))
        expect_equal(saved$well[1:3], c('A3', 'A4', 'A5'))
        expect_match(output$table, 'orderData', fixed = TRUE)
    })
})

test_that('out-of-range options reach the calculation', {
    opts <- shiny::reactiveVal(list(extrapolate = 'below', blank_zero = TRUE))
    shiny::testServer(LinearityApp:::mod_curve_server,
                      args = list(lang = 'en', data = shiny::reactive(load_sample('griess')),
                                  rounding = rounding_3(), options = opts), {
        session$setInputs(model = 'LRM', weights = 'none', xtitle = 'x', xunits = '', ytitle = 'y')
        res <- session$returned$result()$result
        expect_false(anyNA(res$conc[res$flag == '< LLOQ']))
        expect_true(all(res$conc[res$flag == 'OD <= blank'] == 0))
    })
    shiny::testServer(LinearityApp:::mod_quant_options_server, {
        session$setInputs(below = TRUE, above = FALSE, blank_zero = FALSE)
        expect_equal(session$returned(), list(extrapolate = 'below', blank_zero = FALSE))
        session$setInputs(above = TRUE, blank_zero = TRUE)
        expect_equal(session$returned(), list(extrapolate = c('below', 'above'), blank_zero = TRUE))
    })
})

test_that('standard points are excluded and restored by clicking on the curve', {
    data <- shiny::reactive(check_assay(read_assay(sample_path('bca_plate.xlsx'))))
    clicked_point <- NULL
    local_mocked_bindings(nearPoints = function(df, ...) df[df$key == clicked_point, ], .package = 'shiny')
    tab_calls <- logical(0)
    local_mocked_bindings(toggle_refit_tab = function(session, show, lang) tab_calls <<- c(tab_calls, show))
    shiny::testServer(LinearityApp:::mod_curve_server,
                      args = list(lang = 'en', data = data, rounding = rounding_3()), {
        session$setInputs(model = 'QRM', weights = 'none',
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
        # the original and the refitted calibration are compared on a tab of their own
        expect_equal(tab_calls, TRUE)
        expect_match(output$exclusions$html, 'Original vs refitted', fixed = TRUE)
        expect_false(is.null(session$returned$result()$original_fit))
        expect_match(output$effect$html, 'Effect of the excluded points', fixed = TRUE)
        expect_match(output$effect_coefs, 'Refitted', fixed = TRUE)
        expect_match(output$effect_back_calc, 'Refitted calculated', fixed = TRUE)
        expect_match(output$effect_back_calc, '<td> Yes </td>', fixed = TRUE)
        expect_match(output$exclusions_table, '<td> S2 </td>', fixed = TRUE)
        expect_match(output$effect_summary, 'Mean absolute relative error', fixed = TRUE)
        expect_match(output$refit_note$html, 'refitted without 1 excluded', fixed = TRUE)
        expect_equal(nrow(session$returned$effect_tables()$back_calc), n_used)
        # the exclusion is listed in the table, not repeated as a warning
        expect_false(any(grepl('excluded from the fit', message_text(session$returned$result()$notes))))

        session$setInputs(curve_click = list(x = 2, y = 2))
        session$setInputs(confirm_restore = 1)
        expect_equal(nrow(session$returned$exclusions()), 0)
        expect_null(session$returned$effect_tables())
        expect_null(output$refit_note)
        expect_equal(tab_calls, c(TRUE, FALSE))

        # a standard reason is stored with its label
        clicked_point <<- 'C2'
        session$setInputs(curve_click = list(x = 3, y = 3))
        session$setInputs(reason_choice = 'pipetting', reason = '', confirm_exclude = 3)
        expect_equal(session$returned$exclusions()$reason, 'Pipetting error')
        session$setInputs(restore_all = 1)
        expect_equal(nrow(session$returned$exclusions()), 0)

        # "other" needs a text
        session$setInputs(curve_click = list(x = 3, y = 3))
        session$setInputs(reason_choice = 'other', reason = '', confirm_exclude = 4)
        expect_equal(nrow(session$returned$exclusions()), 0)
    })
})

test_that('R2 is shown as an infographic and quantification moved to the result summary', {
    data <- shiny::reactive(load_sample('griess'))
    shiny::testServer(LinearityApp:::mod_curve_server,
                      args = list(lang = 'en', data = data, rounding = rounding_3()), {
        session$setInputs(model = 'LRM', weights = 'none', xtitle = 'x', xunits = '', ytitle = 'y')
        expect_error(output$quantification)
        # R2, mean error and lack of fit in one row
        expect_match(output$curve_stats$html,
                     'curve-stats.*R² = .*>0.9847</strong>.*Mean \\|error\\| = .*Lack-of-fit: ')
        # both warnings of this run are shown by the statistics: no warnings box
        expect_null(output$notes)
        expect_match(output$coefs, 'ci.lower', fixed = TRUE)
        # the row of the chosen model is shaded in the comparison of models
        row <- match('LRM', calibration_models())
        expect_match(output$compare_mark$html, sprintf('tr:nth-child(%d)', row), fixed = TRUE)
        session$setInputs(model = '4PL')
        expect_match(output$compare_mark$html, sprintf('tr:nth-child(%d)', match('4PL', calibration_models())),
                     fixed = TRUE)
    })
})

test_that('infographics count the sample wells and place R2 on its scale', {
    flags <- c('OD <= blank', 'OD <= blank', rep('< LLOQ', 8), rep('OK', 6), '> ULOQ', 'Missing OD')
    counts <- LinearityApp:::quantification_counts(flags)
    expect_equal(unname(counts), c(2, 8, 6, 1, 1))
    html <- as.character(LinearityApp:::quantification_ui(flags, 'en'))
    # red, orange and green segments in this order, sized by the counts
    expect_match(html, 'quant-od_le_nc.*flex-grow: 2;.*quant-below.*flex-grow: 8;.*quant-ok.*flex-grow: 6;')
    expect_match(html, '6 of 18 sample wells quantified', fixed = TRUE)
    expect_null(LinearityApp:::quantification_ui(character(0)))

    r2 <- function(x) as.character(LinearityApp:::r2_ui(x))
    expect_match(r2(0.99421), 'R² = <strong class="zone-good">0.9942</strong>', fixed = TRUE)
    expect_match(r2(0.985), 'zone-fair', fixed = TRUE)
    expect_match(r2(0.5), 'zone-poor', fixed = TRUE)
    expect_match(r2(0.5), 'title="low', fixed = TRUE)
    expect_null(LinearityApp:::r2_ui(NA))

    lof <- function(p) as.character(LinearityApp:::lof_ui(p, 'en'))
    expect_match(lof(0.47), 'Lack-of-fit: p = <strong class="zone-good">0.47</strong>', fixed = TRUE)
    expect_match(lof(0.08), 'zone-fair', fixed = TRUE)
    expect_match(lof(0.10), 'zone-fair', fixed = TRUE)
    expect_match(lof(0.05), 'zone-poor', fixed = TRUE)
    expect_match(lof(1e-6), '>&lt; 0.001</strong>', fixed = TRUE)
    expect_match(lof(NA), 'needs replicates', fixed = TRUE)
})

test_that('the mean error of the back-calculated standards matches the comparison of models', {
    d <- load_sample('griess')
    r <- calculate(d, model = 'LRM')
    expected <- compare_models(r$standard_data)
    expected <- expected$mean.abs.error[expected$model == 'LRM']
    html <- as.character(LinearityApp:::mean_error_ui(r$fit, 20, 'en'))
    expect_match(html, sprintf('Mean |error| = <strong class="zone-good">%.1f%%</strong>', expected), fixed = TRUE)
    expect_match(as.character(LinearityApp:::mean_error_ui(r$fit, expected / 2, 'en')), 'zone-poor', fixed = TRUE)
    expect_null(LinearityApp:::mean_error_ui(NULL, 20))
})

test_that('the flags of the sample wells are counted above the result table', {
    flags <- c('OD <= blank', rep('< LLOQ', 3), rep('OK', 5), '> ULOQ', 'Missing OD')
    html <- as.character(LinearityApp:::flags_ui(flags, 'en'))
    expect_match(html, 'Flags of the sample wells: 11 wells', fixed = TRUE)
    # one segment per flag, in the order of the concentration
    expect_match(html, 'quant-segment flag-od_le_nc.*flex-grow: 1;.*flag-below.*flex-grow: 3;.*flag-ok.*flex-grow: 5;.*flag-above.*flag-missing')
    expect_match(html, '>\\s*5\\s*<span class="stat-share">45%</span>')
    # the short flags of the table are spelled out on the tiles
    expect_match(html, 'Below the lowest calibrator', fixed = TRUE)
    expect_match(html, 'Above the highest calibrator', fixed = TRUE)
    # the rare flags get a tile only when they occur
    expect_match(html, 'flag-missing', fixed = TRUE)
    expect_false(grepl('flag-no_solution', html, fixed = TRUE))
    expect_null(LinearityApp:::flags_ui(character(0)))

    # the same counts whatever the options for out-of-range samples
    data <- load_sample('griess')
    counts <- function(...) {
        curve <- fake_curve()
        curve$result <- shiny::reactive(calculate(data, 'LRM', ...))
        html <- NULL
        shiny::testServer(LinearityApp:::mod_results_server, args = list(lang = 'en', curve = curve),
                          html <<- output$flags$html)
        html
    }
    plain <- counts()
    expect_match(plain, 'flag-tile', fixed = TRUE)
    expect_identical(counts(extrapolate = c('below', 'above'), blank_zero = TRUE), plain)
})

test_that('the comparison tab is inserted after the model parameters and removed', {
    calls <- list()
    local_mocked_bindings(
        nav_insert = function(id, nav, target, position, select, session) {
            calls[[length(calls) + 1]] <<- list('insert', id, target, position, select, as.character(nav))
        },
        nav_remove = function(id, target, session) calls[[length(calls) + 1]] <<- list('remove', id, target),
        .package = 'bslib')
    session <- list(ns = shiny::NS('curve'))
    LinearityApp:::toggle_refit_tab(session, TRUE, 'ru')
    LinearityApp:::toggle_refit_tab(session, FALSE, 'ru')
    expect_equal(calls[[1]][2:5], list('tabs', 'coefs', 'after', FALSE))
    expect_match(calls[[1]][[6]], tr('effect.tab', 'ru'), fixed = TRUE)
    expect_match(calls[[1]][[6]], 'curve-effect', fixed = TRUE)
    expect_equal(calls[[2]], list('remove', 'tabs', 'refit'))
    expect_match(as.character(LinearityApp:::mod_curve_ui('curve', 'en')), 'id="curve-tabs"', fixed = TRUE)
})

test_that('very small and very large concentrations are shown in e-notation', {
    table <- data.frame(well = c('A1', 'A2'), conc = c(0.0000541, 2.5), conc.lower = c(1e-6, 2), OD = c(0.06, 0.2),
                        flag = 'OK')
    defs <- LinearityApp:::results_datatable(table, 'en')$x$options$columnDefs
    render <- Filter(function(d) !is.null(d$render), defs)
    expect_length(render, 1)
    # conc and conc.lower (0-based), not the OD
    expect_equal(unlist(render[[1]]$targets), c(1, 2))
    expect_match(render[[1]]$render, 'toExponential', fixed = TRUE)
    # no concentrations: nothing to render
    defs <- LinearityApp:::results_datatable(data.frame(well = 'A1', OD = 0.1), 'en')$x$options$columnDefs
    expect_length(Filter(function(d) !is.null(d$render), defs), 0)
})

test_that('nothing is exported until the units of the calibrator are chosen', {
    data <- shiny::reactive(load_sample('griess'))
    shiny::testServer(LinearityApp:::mod_curve_server,
                      args = list(lang = 'en', data = data, rounding = rounding_3()), {
        session$setInputs(model = 'LRM', xtitle = 'x', xunits = '', ytitle = 'y')
        expect_false(session$returned$units_ok())
        # the button looks the same but asks for the units instead of downloading
        expect_match(output$save_plot_ui$html, 'id="proxy1-save_plot_locked"', fixed = TRUE)
        expect_false(grepl('shiny-download-link', output$save_plot_ui$html, fixed = TRUE))
        session$setInputs(xunits = ', ng/ml')
        expect_true(session$returned$units_ok())
    })
    locked <- fake_curve(units_ok = FALSE)
    shiny::testServer(LinearityApp:::mod_results_server, args = list(lang = 'ru', curve = locked), {
        expect_match(output$download_ui$html, 'export-locked', fixed = TRUE)
        # no message next to the button: it comes with a click
        expect_false(grepl(tr('export.units_needed', 'ru'), output$download_ui$html, fixed = TRUE))
    })
    shiny::testServer(LinearityApp:::mod_summary_server,
                      args = list(lang = 'en', curve = locked, rounding = rounding_3()), {
        expect_match(output$download_ui$html, 'export-locked', fixed = TRUE)
    })
    summary <- list(table = shiny::reactive(NULL), assessment = shiny::reactive(NULL),
                    warnings = shiny::reactive(character(0)))
    shiny::testServer(LinearityApp:::mod_report_server,
                      args = list(lang = 'en', curve = locked, summary = summary), {
        expect_match(output$download_ui$html, 'export-locked', fixed = TRUE)
    })
    modal <- as.character(units_modal('ru'))
    expect_match(modal, tr('export.units_title', 'ru'), fixed = TRUE)
    expect_match(modal, tr('export.units_needed', 'ru'), fixed = TRUE)
    expect_false(units_chosen(''))
    expect_false(units_chosen(NULL))
    expect_true(units_chosen(', nM'))
})
