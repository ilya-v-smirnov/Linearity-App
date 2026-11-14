#' Report Module
#'
#' @param id Module id.
#' @param lang Language code.
#' @param curve List of reactives returned by [mod_curve_server()].
#' @param means Reactive with the aggregated table.
#'
#' @name mod_report
#' @keywords internal
NULL

#' @rdname mod_report
mod_report_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::textInput(ns('title'), tr('report.title_label', lang),
                         placeholder = tr('report.title_placeholder', lang), width = '100%'),
        shiny::downloadButton(ns('download'), label = tr('report.download', lang),
                              icon = shiny::icon('file-word'))
    )
}

#' @rdname mod_report
mod_report_server <- function(id, lang, curve, means) {
    shiny::moduleServer(id, function(input, output, session) {
        output$download <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_report.docx'),
            content = function(file) {
                args <- curve$plot_args()
                save_report(file,
                            date = as.character(Sys.Date()),
                            title = input$title,
                            plot = get_plots(curve$result(),
                                             xtitle = args$xtitle, ytitle = args$ytitle,
                                             plot.theme = report_theme(), lang = lang),
                            method = tr(paste0('model.', curve$model()), lang),
                            scale = tr(if (curve$linear()) 'scale.linear' else 'scale.log', lang),
                            model_table = curve$coef_table(),
                            result_table = localize_result(curve$result()$result, lang),
                            means_table = means(),
                            qc_table = localize_result(curve$result()$qc, lang),
                            notes = curve$notes(),
                            lang = lang)
            }
        )
    })
}
