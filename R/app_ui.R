#' Application User Interface
#'
#' Every step of the analysis is a tab with its own sidebar of parameters
#' (left) and the results of these parameters (right).
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

    # A hash of the content in the URL makes browsers reload changed files
    asset <- function(file) {
        hash <- unname(tools::md5sum(system.file('app', 'www', file, package = 'LinearityApp')))
        paste0('linapp/', file, '?v=', substr(hash, 1, 8))
    }

    # Steps that are not redesigned yet keep their content without a sidebar
    plain <- function(...) shiny::div(class = 'step-plain', ...)

    bslib::page_navbar(
        title = t('app.title'),
        window_title = t('app.title'),
        id = 'tabs',
        theme = app_theme(),
        fillable = c('template', 'data', 'curve', 'results'),
        header = shiny::tags$head(shiny::tags$link(rel = 'stylesheet', href = asset('linapp.css')),
                                  shiny::tags$script(src = asset('plate-editor.js'))),
        bslib::nav_panel(t('tab.template'), value = 'template', icon = shiny::icon('table-cells'),
                         mod_template_ui('template', lang)),
        bslib::nav_panel(t('tab.data'), value = 'data', icon = shiny::icon('upload'),
                         mod_import_ui('import', lang)),
        bslib::nav_panel(t('tab.curve'), value = 'curve', icon = shiny::icon('chart-line'),
                         mod_curve_ui('curve', lang)),
        bslib::nav_panel(t('tab.results'), value = 'results', icon = shiny::icon('list-ol'),
                         mod_results_ui('results', lang)),
        bslib::nav_panel(t('tab.quality'), value = 'quality', icon = shiny::icon('circle-check'),
                         plain(mod_quality_ui('quality', lang))),
        bslib::nav_panel(t('tab.summary'), value = 'summary', icon = shiny::icon('layer-group'),
                         plain(mod_summary_ui('summary', lang))),
        bslib::nav_panel(t('tab.report'), value = 'report', icon = shiny::icon('file-word'),
                         plain(mod_report_ui('report', lang))),
        bslib::nav_spacer(),
        bslib::nav_menu(
            t('tab.help'), icon = shiny::icon('circle-question'), align = 'right',
            bslib::nav_panel(t('tab.manual'), value = 'manual', icon = shiny::icon('file-pdf'),
                             plain(manual_ui(lang))),
            bslib::nav_panel(t('tab.about'), value = 'about', icon = shiny::icon('circle-info'),
                             plain(about_ui(lang)))
        ),
        bslib::nav_item(language_selector(lang))
    )
}


#' Application Theme
#' @keywords internal
app_theme <- function() {
    bslib::bs_theme(version = 5, preset = 'shiny', primary = '#2c6e9b')
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
    shiny::tags$select(
        id = 'language_selector',
        class = 'form-select form-select-sm',
        `aria-label` = tr('common.language', lang),
        title = tr('common.language', lang),
        onchange = paste0("var p = new URLSearchParams(window.location.search);",
                          "p.set('lang', this.value);",
                          "window.location.search = p.toString();"),
        options
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
