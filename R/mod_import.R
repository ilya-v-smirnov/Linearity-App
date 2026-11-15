#' Data Import Step Module
#'
#' Step 2 of the analysis: file input (sidebar) and the view of the imported
#' data: summary, problems found in the file, plate map and table. Plate
#' workbooks and tables are recognised automatically (see [read_assay()]).
#'
#' @param id Module id.
#' @param lang Language code.
#'
#' @return `mod_import_server()` returns a list of reactives: `data` (the
#'   validated data) and `file` (the uploaded file).
#' @name mod_import
#' @keywords internal
NULL

#' @rdname mod_import
mod_import_ui <- function(id, lang) {
    bslib::layout_sidebar(
        sidebar = bslib::sidebar(width = 320, mod_import_input_ui(id, lang)),
        bslib::card(
            full_screen = TRUE,
            bslib::card_header(tr('data.imported', lang)),
            bslib::card_body(mod_import_table_ui(id, lang))
        )
    )
}

#' @rdname mod_import
mod_import_input_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fileInput(ns('file'), label = tr('import.choose', lang), accept = c('.csv', '.xlsx')),
        shiny::helpText(tr('import.help', lang))
    )
}

#' @rdname mod_import
mod_import_table_ui <- function(id, lang = 'en') {
    ns <- shiny::NS(id)
    shiny::div(
        class = 'step-view',
        shiny::uiOutput(ns('empty')),
        shiny::uiOutput(ns('summary')),
        shiny::uiOutput(ns('problems')),
        shiny::uiOutput(ns('plate')),
        shiny::uiOutput(ns('table_title')),
        shiny::tableOutput(ns('table'))
    )
}

#' @rdname mod_import
mod_import_server <- function(id, lang) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        t <- function(key) tr(key, lang)

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

        # Without the error message: it is shown once, in the summary
        valid <- shiny::reactive({
            shiny::req(is.null(imported()$error))
            imported()$data
        })

        problems <- shiny::reactive(problems_table(imported()$problems, lang))

        output$empty <- shiny::renderUI({
            if (!is.null(input$file)) return(NULL)
            shiny::div(class = 'empty-state',
                       shiny::icon('file-arrow-up', class = 'fa-2x'),
                       shiny::p(t('data.no_file')))
        })

        output$summary <- shiny::renderUI({
            d <- data()
            shiny::tagList(
                shiny::p(class = 'text-muted', shiny::icon('file'), ' ', input$file$name),
                data_summary_ui(data_counts(d), 'well' %in% names(d), lang)
            )
        })

        output$problems <- shiny::renderUI({
            if (is.null(problems())) return(NULL)
            shiny::div(class = 'alert alert-warning',
                       shiny::strong(t('import.problems')),
                       shiny::tableOutput(ns('problems_table')))
        })
        output$problems_table <- shiny::renderTable(problems())

        output$plate <- shiny::renderUI({
            grids <- data_plate_grids(valid())
            if (is.null(grids)) return(NULL)
            shiny::tagList(shiny::h5(t('data.plate_map')),
                           plate_map(grids$codes, grids$values, grids$types, lang))
        })

        output$table_title <- shiny::renderUI({
            shiny::req(valid())
            shiny::h5(t('data.table'))
        })

        output$table <- shiny::renderTable({
            out <- valid()
            attr(out, 'problems') <- NULL
            out
        }, digits = 4, na = '')

        list(data = data, file = shiny::reactive(input$file))
    })
}
