#' Rounding Module
#'
#' @param id Module id.
#' @param lang Language code.
#'
#' @return `mod_rounding_server()` returns a reactive list with `fun`
#'   (rounding function) and `par` (its parameter).
#' @name mod_rounding
#' @keywords internal
NULL

#' @rdname mod_rounding
mod_rounding_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    shiny::wellPanel(
        shiny::h4(t('rounding.title')),
        shiny::fluidRow(
            shiny::column(7, shiny::selectInput(ns('fun'), t('rounding.function'),
                                                choices = stats::setNames(c('signif', 'round'),
                                                                          c(t('rounding.signif'),
                                                                            t('rounding.round'))),
                                                selected = 'signif')),
            shiny::column(5, shiny::numericInput(ns('par'), t('rounding.number'),
                                                 value = 3, step = 1, min = 1, max = 7))
        )
    )
}

#' @rdname mod_rounding
mod_rounding_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        shiny::observeEvent(input$fun, ignoreInit = TRUE, {
            if (input$fun == 'round') {
                shiny::updateNumericInput(session, 'par', value = 2, min = -7, max = 7)
            } else {
                shiny::updateNumericInput(session, 'par', value = 3, min = 1, max = 7)
            }
        })

        shiny::reactive({
            shiny::req(input$fun, input$par)
            list(fun = switch(input$fun, 'round' = round, signif), par = input$par)
        })
    })
}
