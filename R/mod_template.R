#' Template Module
#'
#' Parameters of the data template and its download: a plate workbook (see
#' [plate_format]) or a table with one row per well.
#'
#' @param id Module id.
#' @param lang Language code.
#'
#' @return `mod_template_server()` returns a reactive list of template parameters.
#' @name mod_template
#' @keywords internal
NULL

#' @rdname mod_template
mod_template_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    shiny::wellPanel(
        shiny::h4(t('template.title')),
        shiny::radioButtons(ns('format'), t('template.format'),
                            choices = stats::setNames(c('plate', 'table'),
                                                      c(t('template.format_plate'), t('template.format_table'))),
                            selected = 'plate', inline = TRUE),
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
                                                 value = 10, min = 1, step = 1)),
            shiny::column(6, shiny::conditionalPanel(
                condition = "input.format == 'plate'", ns = ns,
                shiny::numericInput(ns('sample_reps'), t('template.sample_reps'),
                                    value = 2, min = 1, step = 1)))
        ),
        shiny::conditionalPanel(
            condition = "input.format == 'plate'", ns = ns,
            shiny::selectInput(ns('size'), t('template.plate_size'), choices = c(96, 384),
                               selected = 96, width = '50%'),
            shiny::downloadButton(ns('download_plate'), t('template.download'),
                                  icon = shiny::icon('file-excel'))
        ),
        shiny::conditionalPanel(
            condition = "input.format == 'table'", ns = ns,
            mod_table_download_ui(ns('download'), lang, 'template.download', widths = c(8, 4))
        )
    )
}

#' @rdname mod_template
mod_template_server <- function(id, lang) {
    shiny::moduleServer(id, function(input, output, session) {
        notify_error <- function(e) {
            shiny::showNotification(condition_text(e, lang), type = 'error')
            shiny::req(FALSE)
        }

        template <- shiny::reactive({
            tryCatch(
                make_template(start = input$start,
                              step = input$step,
                              n.standard = input$replicates,
                              n.sample = input$samples,
                              n.levels = input$levels),
                error = notify_error)
        })
        mod_table_download_server('download', template, 'raw.data')

        output$download_plate <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_plate.xlsx'),
            content = function(file) {
                tryCatch(
                    make_plate_template(file, size = as.numeric(input$size),
                                        start = input$start, step = input$step,
                                        n.levels = input$levels, n.standard = input$replicates,
                                        n.sample = input$samples, sample_reps = input$sample_reps,
                                        lang = lang),
                    error = notify_error)
            }
        )

        template
    })
}
