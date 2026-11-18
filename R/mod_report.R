#' Report Step Module
#'
#' The sidebar holds the experiment title, the type of the report (short or
#' detailed) and the download; the main area shows a preview of the report
#' built from the same blocks as the Word file (see [report_blocks()]).
#'
#' @param id Module id.
#' @param lang Language code.
#' @param curve List of reactives returned by [mod_curve_server()].
#' @param summary List of reactives returned by [mod_summary_server()]: the
#'   aggregated table and the assessment of the samples.
#'
#' @name mod_report
#' @keywords internal
NULL

#' @rdname mod_report
mod_report_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)

    sidebar <- bslib::sidebar(
        width = 330,
        sidebar_group(
            t('report.group_title'), 'heading',
            shiny::textAreaInput(ns('title'), NULL, placeholder = t('report.title_placeholder'),
                                 rows = 4, width = '100%', resize = 'vertical')
        ),
        sidebar_group(
            t('report.type'), 'file-lines',
            shiny::radioButtons(ns('type'), NULL,
                                choiceNames = list(with_tooltip(t('report.type_short'), t('report.type_short_help')),
                                                   with_tooltip(t('report.type_detailed'),
                                                                t('report.type_detailed_help'))),
                                choiceValues = c('short', 'detailed'), selected = 'short')
        ),
        sidebar_group(
            t('results.export'), 'file-word',
            shiny::uiOutput(ns('download_ui'))
        )
    )

    main <- bslib::card(
        full_screen = TRUE,
        bslib::card_header(t('report.preview')),
        bslib::card_body(class = 'report-preview', shiny::uiOutput(ns('preview')))
    )

    bslib::layout_sidebar(sidebar = sidebar, main)
}

#' @rdname mod_report
mod_report_server <- function(id, lang, curve, summary) {
    shiny::moduleServer(id, function(input, output, session) {
        type <- shiny::reactive(if (is.null(input$type)) 'short' else input$type)

        plot <- shiny::reactive({
            args <- curve$plot_args()
            # The report shows the calibration accepted by the user (refitted without excluded points)
            get_plots(curve$result(), xtitle = args$xtitle, ytitle = args$ytitle,
                      plot.theme = report_theme(), lang = lang, show_original = FALSE)
        })

        blocks <- shiny::reactive({
            r <- curve$result()
            # The samples are assessed with the grouping of the result summary
            assessment <- summary$assessment()
            sample_warnings <- if (is.null(assessment)) list() else assessment$warnings
            tables <- quality_tables(r$quality, lang)
            tables$samples <- NULL
            tables$dilution <- if (!is.null(assessment)) quality_tables(assessment, lang)$dilution
            report_blocks(type(),
                          date = as.character(Sys.Date()),
                          title = if (is.null(input$title)) '' else input$title,
                          method = tr(paste0('model.', curve$model()), lang),
                          scale = tr(if (curve$model() %in% c('4PL', '5PL')) 'scale.logistic' else 'scale.log', lang),
                          weights = weight_labels(curve$weights(), lang),
                          model_table = curve$coef_table(),
                          result_table = localize_result(r$result, lang),
                          means_table = summary$table(),
                          qc_table = localize_result(r$qc, lang),
                          exclusions = exclusions_table(r$exclusions, lang),
                          comparison = comparison_table(curve$comparison(), lang),
                          quality = list(summary = quality_summary(r, lang, sample_warnings),
                                         tables = tables),
                          notes = c(curve$notes(), message_text(sample_warnings, lang)),
                          lang = lang,
                          exclusion_effect = curve$effect_tables())
        })

        output$preview <- shiny::renderUI({
            report_preview(blocks(), shiny::plotOutput(session$ns('plot'), height = '260px'), lang)
        })
        output$plot <- shiny::renderPlot(plot())

        output$download_ui <- shiny::renderUI(
            export_button_ui(session$ns('download'), tr('report.download', lang), 'download', curve$units_ok()))
        observe_locked_export(input, 'download', lang)
        output$download <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_report_', type(), '.docx'),
            content = function(file) {
                shiny::req(curve$units_ok())
                write_report(blocks(), file, plot())
            }
        )

        blocks
    })
}
