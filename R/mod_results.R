#' Results Module
#'
#' Sample results with flags, warnings and download.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param curve List of reactives returned by [mod_curve_server()].
#'
#' @return `mod_results_server()` returns a reactive with the translated result table.
#' @name mod_results
#' @keywords internal
NULL

#' @rdname mod_results
mod_results_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns('notes')),
        shiny::tableOutput(ns('table')),
        mod_table_download_ui(ns('download'), lang, 'results.download'),
        shiny::br(), shiny::br()
    )
}

#' @rdname mod_results
mod_results_server <- function(id, lang, curve) {
    shiny::moduleServer(id, function(input, output, session) {
        table <- shiny::reactive(localize_result(curve$result()$result, lang))

        output$notes <- shiny::renderUI(notes_ui(curve$notes(), lang))
        output$table <- shiny::renderTable(table(), digits = 4, na = '')
        mod_table_download_server('download', table, 'result_table')

        table
    })
}
