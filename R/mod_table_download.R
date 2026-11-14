#' Table Download Module
#'
#' File type selector (Excel/CSV), CSV dialect selector and a download button.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param button_key Dictionary key of the button label.
#' @param widths Column widths of the file type and dialect selectors.
#' @param table Reactive returning the data.frame to save.
#' @param prefix File name prefix (after the date).
#'
#' @name mod_table_download
#' @keywords internal
NULL

#' @rdname mod_table_download
mod_table_download_ui <- function(id, lang, button_key, widths = c(3, 2)) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(width = widths[1],
                          shiny::selectInput(ns('filetype'),
                                             label = tr('common.file_type', lang),
                                             choices = stats::setNames(c('xlsx', 'csv'),
                                                                       c(tr('common.excel', lang),
                                                                         tr('common.csv', lang))),
                                             selected = 'xlsx',
                                             width = '100%')),
            shiny::column(width = widths[2],
                          shiny::conditionalPanel(
                              condition = "input.filetype == 'csv'", ns = ns,
                              shiny::selectInput(ns('dialect'),
                                                 label = tr('common.sep', lang),
                                                 choices = c(',' = 'comma', ';' = 'semicolon'),
                                                 selected = 'comma', width = '100%')))
        ),
        shiny::downloadButton(ns('download'), tr(button_key, lang), icon = shiny::icon('file-excel'))
    )
}

#' @rdname mod_table_download
mod_table_download_server <- function(id, table, prefix) {
    shiny::moduleServer(id, function(input, output, session) {
        output$download <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_', prefix, '.', input$filetype),
            content = function(file) save_table(table(), file, input$filetype, input$dialect)
        )
    })
}
