#' Application Server
#'
#' @param input,output,session Shiny server arguments.
#' @param default_lang Language used when the URL has no `lang` parameter.
#'
#' @return Called for its side effects.
#' @keywords internal
app_server <- function(input, output, session, default_lang = 'en') {
    # The websocket request has no query string: read it from the client data
    search <- shiny::isolate(session$clientData$url_search)
    lang <- request_lang(list(QUERY_STRING = search), default_lang)

    mod_template_server('template', lang)
    data <- mod_import_server('import', lang)
    rounding <- mod_rounding_server('rounding')
    curve <- mod_curve_server('curve', lang, data, rounding)
    mod_results_server('results', lang, curve)
    means <- mod_summary_server('summary', lang, curve, rounding)
    mod_report_server('report', lang, curve, means)
}
