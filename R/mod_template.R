#' Template Module
#'
#' Parameters of the data template and its download.
#'
#' @param id Module id.
#' @param lang Language code.
#'
#' @name mod_template
#' @keywords internal
NULL

#' @rdname mod_template
mod_template_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    shiny::wellPanel(
        shiny::h4(t('template.title')),
        shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns('start'), t('template.start'),
                                                 value = 100, min = 0, step = 10)),
            shiny::column(6, shiny::numericInput(ns('step'), t('template.step'),
                                                 value = 2, min = 1.1, step = 1))
        ),
        shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns('levels'), t('template.levels'),
                                                 value = 7, min = 3, step = 1)),
            shiny::column(6, shiny::numericInput(ns('replicates'), t('template.replicates'),
                                                 value = 2, min = 1, step = 1))
        ),
        shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns('samples'), t('template.samples'),
                                                 value = 10, min = 1, step = 1))
        ),
        mod_table_download_ui(ns('download'), lang, 'template.download', widths = c(8, 4))
    )
}

#' @rdname mod_template
mod_template_server <- function(id, lang) {
    shiny::moduleServer(id, function(input, output, session) {
        template <- shiny::reactive({
            tryCatch(
                make_template(start = input$start,
                              step = input$step,
                              n.standard = input$replicates,
                              n.sample = input$samples,
                              n.levels = input$levels),
                error = function(e) {
                    shiny::showNotification(condition_text(e, lang), type = 'error')
                    shiny::req(FALSE)
                })
        })
        mod_table_download_server('download', template, 'raw.data')
        template
    })
}
