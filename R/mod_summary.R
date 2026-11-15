#' Result Summary Step Module
#'
#' The sidebar holds the grouping columns (with the number of distinct values
#' of each column, quick choices and a preview of the number of rows), the
#' statistics to show and the download; the main area shows the aggregated
#' table.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param curve List of reactives returned by [mod_curve_server()].
#' @param rounding Reactive from [mod_rounding_server()].
#'
#' @return `mod_summary_server()` returns a reactive with the aggregated table.
#' @name mod_summary
#' @keywords internal
NULL

#' Columns Available for Grouping
#'
#' All columns of the result table except the measurements, the flag and the
#' well, in display order (see [result_columns()]).
#'
#' @param result Result table from [calculate()].
#'
#' @return Character vector.
#' @keywords internal
grouping_columns <- function(result) {
    exclude <- c('type', 'well', 'OD', 'conc', 'conc.lower', 'conc.upper', 'flag')
    setdiff(result_columns(names(result)), exclude)
}


#' Statistics of the Summary Table
#' @keywords internal
summary_statistics <- function() c('n', 'n.quantified', 'mean', 'sd', 'cv', 'min', 'max')


#' @rdname mod_summary
mod_summary_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    stats <- summary_statistics()

    sidebar <- bslib::sidebar(
        width = 330,
        sidebar_group(
            with_tooltip(t('summary.group_title'), t('summary.group_help')), 'layer-group',
            shiny::div(class = 'summary-presets',
                       shiny::actionButton(ns('preset_samples'), t('summary.preset_samples'),
                                           class = 'btn-sm btn-outline-primary'),
                       shiny::actionButton(ns('preset_dilutions'), t('summary.preset_dilutions'),
                                           class = 'btn-sm btn-outline-primary')),
            shiny::checkboxGroupInput(ns('group_vars'), NULL, choices = character(0)),
            shiny::uiOutput(ns('preview'))
        ),
        sidebar_group(
            t('summary.stats_title'), 'calculator',
            shiny::checkboxGroupInput(ns('stats'), NULL,
                                      choices = stats::setNames(stats, vapply(paste0('summary.stat_', stats), t, '')),
                                      selected = stats)
        ),
        sidebar_group(
            t('results.export'), 'file-excel',
            shiny::downloadButton(ns('download'), t('summary.download'), icon = shiny::icon('download'),
                                  class = 'btn-primary w-100')
        )
    )

    main <- bslib::card(
        full_screen = TRUE,
        bslib::card_header(t('tab.summary')),
        bslib::card_body(shiny::div(class = 'step-view', shiny::uiOutput(ns('empty')), DT::DTOutput(ns('table'))))
    )

    bslib::layout_sidebar(sidebar = sidebar, main)
}

#' @rdname mod_summary
mod_summary_server <- function(id, lang, curve, rounding) {
    shiny::moduleServer(id, function(input, output, session) {
        t <- function(key, ...) tr(key, lang, ...)
        result <- shiny::reactive(curve$result()$result)
        columns <- shiny::reactive(grouping_columns(result()))

        # Default grouping: one row per sample (name, date and additional columns)
        samples <- shiny::reactive(intersect(columns(), sample_columns(result())))

        # Choices show the number of distinct values; the user's choice is kept
        known <- shiny::reactiveVal(character(0))
        shiny::observeEvent(result(), {
            cols <- columns()
            res <- result()
            labels <- lapply(cols, function(col) {
                shiny::span(col, shiny::span(class = 'text-muted small',
                                             t('summary.values', n = length(unique(res[[col]])))))
            })
            selected <- if (length(known()) == 0) samples() else
                c(intersect(cols, input$group_vars), setdiff(intersect(cols, samples()), known()))
            known(union(known(), cols))
            shiny::updateCheckboxGroupInput(session, 'group_vars', choiceNames = labels, choiceValues = cols,
                                            selected = intersect(cols, selected))
        })
        shiny::observeEvent(input$preset_samples,
                            shiny::updateCheckboxGroupInput(session, 'group_vars', selected = samples()))
        shiny::observeEvent(input$preset_dilutions, {
            shiny::updateCheckboxGroupInput(session, 'group_vars',
                                            selected = intersect(columns(), c(samples(), 'dilution')))
        })

        group_vars <- shiny::reactive({
            vars <- if (is.null(input$group_vars) && length(known()) == 0) samples() else input$group_vars
            intersect(columns(), vars)
        })

        full <- shiny::reactive({
            r <- rounding()
            aggregate_result(result(), group_vars = group_vars(), round_fun = r$fun, round_par = r$par)
        })

        table <- shiny::reactive({
            out <- full()
            if (is.null(out)) return(NULL)
            # No choice (before the first answer of the browser, or nothing ticked): all statistics
            stats <- if (is.null(input$stats)) summary_statistics() else input$stats
            out[, c(group_vars(), intersect(summary_statistics(), stats)), drop = FALSE]
        })

        output$preview <- shiny::renderUI({
            shiny::req(result())
            n_rows <- if (is.null(full())) 0 else nrow(full())
            shiny::div(class = 'summary-preview', shiny::icon('arrow-right'), ' ',
                       t('summary.preview', wells = nrow(result()), rows = n_rows))
        })

        output$empty <- shiny::renderUI({
            if (!is.null(table())) return(NULL)
            shiny::div(class = 'empty-state', shiny::icon('layer-group', class = 'fa-2x'),
                       shiny::p(t('summary.choose')))
        })

        output$table <- DT::renderDT({
            shiny::req(table())
            results_datatable(table(), lang)
        })

        output$download <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_summary.xlsx'),
            content = function(file) {
                sheets <- list(table())
                names(sheets) <- t('tab.summary')
                openxlsx::write.xlsx(sheets, file, rowNames = FALSE, overwrite = TRUE)
            }
        )

        table
    })
}
