#' Results Step Module
#'
#' The sidebar holds the handling of samples outside the quantification range
#' (see [mod_quant_options]), the rounding, the columns of the table and the
#' download; the main area shows the counts of the flags of the sample wells
#' (see [flags_ui()]), the sample results with flags and the quality controls
#' as interactive tables.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param curve List of reactives returned by [mod_curve_server()].
#' @param by Reactive with the filling direction of the plate (`"column"` or
#'   `"row"`): the initial order of the wells.
#'
#' @return `mod_results_server()` returns a reactive with the translated result table.
#' @name mod_results
#' @keywords internal
NULL

#' Columns of the Result Table in Display Order
#'
#' Identification columns first, then any additional columns, then the
#' measurement, the concentration with its interval and the flag.
#'
#' @param columns Column names.
#'
#' @return The column names reordered.
#' @keywords internal
result_columns <- function(columns) {
    first <- intersect(c('well', 'id', 'sample.name', 'sample.date'), columns)
    last <- intersect(c('dilution', 'OD', 'conc', 'conc.lower', 'conc.upper', 'flag'), columns)
    c(first, setdiff(columns, c(first, last)), last)
}


#' Result Table in Display Order
#'
#' @param result Well-level results (see [calculate()]).
#' @param by Filling direction: order of the wells (see [well_order()]).
#'
#' @return The table with ordered rows and columns.
#' @keywords internal
order_result <- function(result, by = 'column') {
    if (is.null(result)) return(NULL)
    if ('well' %in% names(result)) result <- result[well_order(result$well, by), , drop = FALSE]
    rownames(result) <- NULL
    result[, result_columns(names(result)), drop = FALSE]
}


#' Concentration Columns of the Interactive Tables
#'
#' Columns shown in e-notation when their values are very small or very large
#' (see [results_datatable()]): the concentrations of the Results step and the
#' QC, and the statistics of the result summary.
#'
#' @return Character vector.
#' @keywords internal
concentration_columns <- function() c('conc', 'conc.lower', 'conc.upper', 'nominal', 'mean', 'min', 'max', 'sd')


#' Interactive Result Table
#'
#' Wells are sorted in the order of the plate rather than alphabetically, and
#' flags are coloured like the infographic of the flags above the table.
#' Concentrations below 1e-4 or from 1e5 in absolute value are shown in
#' e-notation (`5.41e-5` instead of `0.0000541`); the table keeps the numbers,
#' so that sorting and filtering are not affected.
#'
#' @param table Result table (see [order_result()]) with translated flags.
#' @param lang Language code.
#' @param page_length Rows per page.
#'
#' @return A DT widget.
#' @keywords internal
results_datatable <- function(table, lang, page_length = 25) {
    col_defs <- list()
    if ('well' %in% names(table)) {
        table$.order <- seq_len(nrow(table))
        well <- match('well', names(table)) - 1
        hidden <- ncol(table) - 1
        col_defs <- list(list(targets = well, orderData = hidden),
                         list(targets = hidden, visible = FALSE, searchable = FALSE))
    }
    conc <- unname(which(names(table) %in% concentration_columns() & vapply(table, is.numeric, logical(1)))) - 1
    if (length(conc) > 0) {
        col_defs <- c(col_defs, list(list(targets = as.list(conc), render = DT::JS(
            'function(data, type) {',
            '  if (type !== "display" || data === null || data === "" || isNaN(data)) return data;',
            '  var x = Number(data), a = Math.abs(x);',
            '  return x !== 0 && (a < 1e-4 || a >= 1e5) ? x.toExponential() : data;',
            '}'))))
    }
    widget <- DT::datatable(
        table, rownames = FALSE, filter = 'top', selection = 'none', style = 'auto',
        class = 'compact stripe hover',
        options = list(pageLength = page_length, lengthMenu = c(10, 25, 50, 100, 384),
                       order = list(), scrollX = TRUE, columnDefs = col_defs,
                       language = dt_language(lang))
    )
    flags <- c(FLAGS$ok, FLAGS$below, FLAGS$above, FLAGS$od_le_nc, FLAGS$no_solution, FLAGS$missing,
               FLAGS$high_cv, FLAGS$nonlinear, FLAGS$no_background)
    colours <- c('#1b5e20', '#ef8f00', '#1976d2', '#c62828', '#757575', '#757575', '#c62828', '#c62828', '#c62828')
    style <- function(widget, column, values, colours) {
        if (!column %in% names(table)) return(widget)
        DT::formatStyle(widget, column, fontWeight = 'bold', whiteSpace = 'nowrap',
                        color = DT::styleEqual(values, colours))
    }
    widget <- style(widget, 'flag', translate_flags(flags, lang), colours)
    # Checks of the groups of the result summary (see summary_checks())
    widget <- style(widget, 'range', translate_flags(flags[1:6], lang), colours[1:6])
    widget <- style(widget, 'precision', c(tr('flag.ok', lang), tr('flag.high_cv', lang)), c('#1b5e20', '#c62828'))
    widget <- style(widget, 'linearity', c(tr('samples.legend_linear', lang), tr('samples.legend_nonlinear', lang)),
                    c('#1b5e20', '#c62828'))
    widget
}


#' Texts of the Interactive Tables
#'
#' @param lang Language code.
#'
#' @return A list of DataTables language options.
#' @keywords internal
dt_language <- function(lang) {
    t <- function(key) tr(paste0('dt.', key), lang)
    list(search = t('search'), lengthMenu = t('length_menu'), info = t('info'), infoEmpty = t('info_empty'),
         infoFiltered = t('info_filtered'), zeroRecords = t('zero_records'), emptyTable = t('zero_records'),
         paginate = list(first = t('first'), last = t('last'), `next` = t('next'), previous = t('previous')))
}


#' Options for Samples Outside the Quantification Range
#'
#' Extrapolation below the lowest and above the highest calibrator (LLOQ and
#' ULOQ) and concentration 0 for
#' samples with OD at or below the blank (see [calculate()]).
#'
#' @param id Module id.
#' @param lang Language code.
#'
#' @return `mod_quant_options_server()` returns a reactive list with
#'   `extrapolate` and `blank_zero`.
#' @name mod_quant_options
#' @keywords internal
NULL

#' @rdname mod_quant_options
mod_quant_options_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    shiny::tagList(
        shiny::checkboxInput(ns('below'), t('results.extrapolate_below'), value = FALSE),
        shiny::checkboxInput(ns('above'), t('results.extrapolate_above'), value = FALSE),
        shiny::checkboxInput(ns('blank_zero'), t('results.blank_zero'), value = FALSE)
    )
}

#' @rdname mod_quant_options
mod_quant_options_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive({
            list(extrapolate = c(if (isTRUE(input$below)) 'below', if (isTRUE(input$above)) 'above'),
                 blank_zero = isTRUE(input$blank_zero))
        })
    })
}


#' @rdname mod_results
mod_results_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)

    sidebar <- bslib::sidebar(
        width = 330,
        sidebar_group(
            with_tooltip(t('results.range_title'), t('results.range_help')), 'arrows-left-right',
            mod_quant_options_ui(ns('options'), lang)
        ),
        sidebar_group(with_tooltip(t('rounding.title'), t('rounding.help')), 'calculator',
                      mod_rounding_ui('rounding', lang)),
        sidebar_group(
            t('results.columns'), 'table-columns',
            shiny::checkboxGroupInput(ns('columns'), NULL, choices = character(0))
        ),
        sidebar_group(
            t('results.export'), 'file-excel',
            shiny::uiOutput(ns('download_ui'))
        )
    )

    main <- bslib::card(
        full_screen = TRUE,
        bslib::card_header(t('tab.results')),
        bslib::card_body(
            shiny::div(
                class = 'step-view',
                shiny::uiOutput(ns('notes')),
                # Counts of the flags: the same whatever the options for out-of-range samples
                shiny::uiOutput(ns('flags')),
                DT::DTOutput(ns('table')),
                shiny::uiOutput(ns('qc'))
            )
        )
    )

    bslib::layout_sidebar(sidebar = sidebar, main)
}

#' @rdname mod_results
mod_results_server <- function(id, lang, curve, by = shiny::reactive('column')) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        full <- shiny::reactive(order_result(localize_result(curve$result()$result, lang), by()))
        qc <- shiny::reactive(order_result(localize_result(curve$result()$qc, lang), by()))

        # Column choices follow the data; the user's choice is kept, new columns are shown
        known <- shiny::reactiveVal(character(0))
        shiny::observeEvent(names(full()), {
            columns <- names(full())
            selected <- if (is.null(input$columns) && length(known()) == 0) columns else
                c(intersect(columns, input$columns), setdiff(columns, known()))
            known(union(known(), columns))
            shiny::updateCheckboxGroupInput(session, 'columns', choices = columns,
                                            selected = intersect(columns, selected))
        })

        # No choice (before the first update, or nothing ticked): all columns
        table <- shiny::reactive({
            out <- full()
            if (is.null(input$columns)) return(out)
            out[, intersect(names(out), input$columns), drop = FALSE]
        })

        output$notes <- shiny::renderUI(notes_ui(curve$notes(), lang))
        output$flags <- shiny::renderUI(flags_ui(curve$result()$result$flag, lang))
        output$table <- DT::renderDT(results_datatable(table(), lang))
        output$qc <- shiny::renderUI({
            if (is.null(qc())) return(NULL)
            shiny::tagList(shiny::h5(tr('results.qc', lang)), DT::DTOutput(ns('qc_table')))
        })
        output$qc_table <- DT::renderDT(results_datatable(qc(), lang, page_length = 10))

        output$download_ui <- shiny::renderUI(
            export_button_ui(ns('download'), tr('results.download', lang), 'download', is.null(curve$export_lock())))
        observe_locked_export(input, 'download', lang, curve$export_lock)
        output$download <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_result_table.xlsx'),
            content = function(file) {
                shiny::req(is.null(curve$export_lock()))
                sheets <- list(table())
                names(sheets) <- tr('tab.results', lang)
                if (!is.null(qc())) sheets[[tr('results.qc', lang)]] <- qc()
                openxlsx::write.xlsx(sheets, file, rowNames = FALSE, overwrite = TRUE)
            }
        )

        table
    })
}
