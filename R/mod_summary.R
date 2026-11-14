#' Result Summary Module
#'
#' Aggregation of results by user-selected columns.
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

#' @rdname mod_summary
mod_summary_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::selectizeInput(ns('group_vars'),
                              label = tr('summary.aggregation', lang),
                              choices = c('sample.name', 'sample.date', 'dilution'),
                              selected = 'sample.name',
                              multiple = TRUE),
        shiny::tableOutput(ns('table')),
        mod_table_download_ui(ns('download'), lang, 'summary.download')
    )
}

#' @rdname mod_summary
mod_summary_server <- function(id, lang, curve, rounding) {
    shiny::moduleServer(id, function(input, output, session) {
        columns <- shiny::reactive({
            setdiff(colnames(curve$result()$result), c('type', 'conc', 'OD', 'flag', 'well'))
        })

        # Update the choices but keep the current selection when possible
        shiny::observeEvent(columns(), {
            cols <- columns()
            selected <- intersect(shiny::isolate(input$group_vars), cols)
            if (length(selected) == 0) selected <- intersect('sample.name', cols)
            shiny::updateSelectizeInput(session, 'group_vars', choices = cols, selected = selected)
        })

        table <- shiny::reactive({
            r <- rounding()
            aggregate_result(curve$result()$result,
                             group_vars = intersect(input$group_vars, columns()),
                             round_fun = r$fun, round_par = r$par)
        })

        output$table <- shiny::renderTable(table(), digits = 4, na = '')
        mod_table_download_server('download', table, 'summary')

        table
    })
}
