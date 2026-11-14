#' Application User Interface
#'
#' @param request A shiny request.
#' @param default_lang Language used when the URL has no `lang` parameter.
#'
#' @return A shiny UI.
#' @keywords internal
app_ui <- function(request, default_lang = 'en') {
    lang <- request_lang(request, default_lang)
    t <- function(key) tr(key, lang)

    shiny::addResourcePath('linapp', system.file('app', 'www', package = 'LinearityApp'))

    shiny::fluidPage(
        title = t('app.title'),
        shiny::div(style = 'float: right; margin-top: 10px;', language_selector(lang)),
        shiny::titlePanel(title = shiny::h1(t('app.title'), align = 'center')),

        shiny::sidebarLayout(
            shiny::sidebarPanel(
                mod_template_ui('template', lang),
                mod_import_input_ui('import', lang),
                mod_rounding_ui('rounding', lang),
                width = 3
            ),
            shiny::mainPanel(
                shiny::navbarPage(
                    t('tab.steps'), id = 'tabs',
                    shiny::tabPanel(t('tab.imported'), value = 'imported',
                                    mod_import_table_ui('import')),
                    shiny::tabPanel(t('tab.curve'), value = 'curve',
                                    mod_curve_ui('curve', lang)),
                    shiny::tabPanel(t('tab.results'), value = 'results',
                                    mod_results_ui('results', lang)),
                    shiny::tabPanel(t('tab.summary'), value = 'summary',
                                    mod_summary_ui('summary', lang)),
                    shiny::tabPanel(t('tab.report'), value = 'report',
                                    mod_report_ui('report', lang)),
                    shiny::tabPanel(t('tab.manual'), value = 'manual',
                                    icon = shiny::icon('file-pdf'),
                                    manual_ui(lang)),
                    shiny::tabPanel(t('tab.about'), value = 'about',
                                    about_ui(lang))
                ),
                width = 9
            )
        )
    )
}


#' Language Selector
#'
#' A drop-down that reloads the page with the `lang` URL parameter.
#'
#' @param lang Current language.
#'
#' @return A shiny tag.
#' @keywords internal
language_selector <- function(lang) {
    options <- lapply(supported_languages(), function(l) {
        shiny::tags$option(value = l, selected = if (l == lang) NA else NULL,
                           tr('language.name', l))
    })
    shiny::tags$label(
        tr('common.language', lang), ' ',
        shiny::tags$select(
            id = 'language_selector',
            onchange = paste0("var p = new URLSearchParams(window.location.search);",
                              "p.set('lang', this.value);",
                              "window.location.search = p.toString();"),
            options
        )
    )
}


#' User Manual Tab
#' @keywords internal
manual_ui <- function(lang) {
    shiny::tagList(
        if (lang != 'en') shiny::p(shiny::em(tr('about.manual_language', lang))),
        shiny::tags$iframe(style = 'height: 800px; width: 100%;',
                           src = 'linapp/user-manual.pdf', type = 'application/pdf')
    )
}


#' About Tab
#' @keywords internal
about_ui <- function(lang) {
    repo <- 'https://github.com/ilya-v-smirnov/Linearity-App'
    email <- 'smirnov.iv.mail@gmail.com'
    shiny::tagList(
        shiny::h4(tr('about.designed', lang)), shiny::br(),
        shiny::h4(tr('about.source', lang)),
        shiny::h4(shiny::a(repo, href = repo)), shiny::br(),
        shiny::h4(tr('about.contact', lang)),
        shiny::h4(shiny::a(email, href = paste0('mailto:', email))),
        shiny::br(),
        shiny::p(paste('LinearityApp', utils::packageVersion('LinearityApp')))
    )
}
