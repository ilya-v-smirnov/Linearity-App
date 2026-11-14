#' Import Module
#'
#' File input (sidebar), the table of problems found in the file and the table
#' of imported data (main panel). Plate workbooks and tables are recognised
#' automatically (see [read_assay()]).
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
mod_import_table_ui <- function(id, lang = 'en') {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns('problems')),
        shiny::tableOutput(ns('table'))
    )
}

#' @rdname mod_import
mod_import_server <- function(id, lang) {
    shiny::moduleServer(id, function(input, output, session) {
        imported <- shiny::reactive({
            file <- input$file
            shiny::req(file)
            tryCatch({
                data <- check_assay(read_assay(file$datapath))
                list(data = data, problems = attr(data, 'problems'), error = NULL)
            }, error = function(e) {
                list(data = NULL, problems = e$problems, error = e)
            })
        })

        data <- shiny::reactive({
            x <- imported()
            if (!is.null(x$error)) {
                shiny::validate(shiny::need(FALSE, tr('common.import_error', lang,
                                                      message = condition_text(x$error, lang))))
            }
            x$data
        })

        problems <- shiny::reactive(problems_table(imported()$problems, lang))

        output$problems <- shiny::renderUI({
            if (is.null(problems())) return(NULL)
            shiny::div(class = 'alert alert-warning',
                       shiny::strong(tr('import.problems', lang)),
                       shiny::tableOutput(session$ns('problems_table')))
        })
        output$problems_table <- shiny::renderTable(problems())

        output$table <- shiny::renderTable({
            out <- data()
            attr(out, 'problems') <- NULL
            out
        }, digits = 4, na = '')

        data
    })
}
