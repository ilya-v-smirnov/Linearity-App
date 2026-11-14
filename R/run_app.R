#' Run the Linearity App
#'
#' @param lang Default interface language (see [supported_languages()]). It can
#'   be changed in the app or with the URL parameter `?lang=ru`.
#' @param ... Options passed to [shiny::shinyApp()], e.g. `port` or `launch.browser`.
#'
#' @return A shiny app object.
#' @export
#' @examples
#' if (interactive()) {
#'     run_app(lang = 'ru')
#' }
run_app <- function(lang = 'en', ...) {
    lang <- match_lang(lang)
    shiny::shinyApp(
        ui = function(request) app_ui(request, default_lang = lang),
        server = function(input, output, session) app_server(input, output, session, default_lang = lang),
        options = list(...)
    )
}


#' Language Requested by the Browser
#'
#' @param request A shiny request (`session$request` or the request passed to the UI).
#' @param default Default language.
#'
#' @return A supported language code.
#' @keywords internal
request_lang <- function(request, default = 'en') {
    query <- shiny::parseQueryString(if (is.null(request$QUERY_STRING)) '' else request$QUERY_STRING)
    match_lang(if (is.null(query$lang)) default else query$lang)
}


#' Run an Expression and Show Errors as Validation Messages
#'
#' Errors are shown as grey text in the outputs instead of red error messages.
#'
#' @param expr Expression to evaluate.
#' @param key Dictionary key of the message template (with a `{message}` placeholder).
#' @param lang Language code.
#'
#' @return Value of `expr`.
#' @keywords internal
with_validation <- function(expr, key, lang) {
    tryCatch(expr, error = function(e) {
        if (inherits(e, c('shiny.silent.error', 'validation'))) stop(e)
        shiny::validate(shiny::need(FALSE, tr(key, lang, message = condition_text(e, lang))))
    })
}


#' Warnings Box
#'
#' @param notes Character vector of warnings.
#' @param lang Language code.
#'
#' @return A shiny tag or `NULL`.
#' @keywords internal
notes_ui <- function(notes, lang) {
    if (length(notes) == 0) return(NULL)
    shiny::div(class = 'alert alert-warning',
               shiny::strong(tr('common.warnings', lang)),
               shiny::tags$ul(lapply(notes, shiny::tags$li)))
}
