#' Import Module
#'
#' File input (sidebar) and the table of imported data (main panel).
#'
#' @param id Module id.
#' @param lang Language code.
#'
#' @return `mod_import_server()` returns a reactive with the validated data.
#' @name mod_import
#' @keywords internal
NULL

#' @rdname mod_import
mod_import_input_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    shiny::wellPanel(
        shiny::h4(tr('import.title', lang)),
        shiny::fileInput(ns('file'), label = tr('import.choose', lang), accept = c('.csv', '.xlsx'))
    )
}

#' @rdname mod_import
mod_import_table_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tableOutput(ns('table'))
}

#' @rdname mod_import
mod_import_server <- function(id, lang) {
    shiny::moduleServer(id, function(input, output, session) {
        data <- shiny::reactive({
            file <- input$file
            shiny::req(file)
            with_validation(
                format_input_table(check_input_file(read_table(file$datapath))),
                'common.import_error', lang)
        })

        output$table <- shiny::renderTable(data(), digits = 4, na = '')

        data
    })
}
