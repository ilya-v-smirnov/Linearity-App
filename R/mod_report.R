#' Report Step Module
#'
#' The sidebar holds the experiment (date chosen with a calendar, title,
#' operator and plate), the type of the report (short or detailed) and the
#' download; the main area shows a preview of the report
#' built from the same blocks as the Word file (see [report_blocks()]).
#'
#' @param id Module id.
#' @param lang Language code.
#' @param curve List of reactives returned by [mod_curve_server()].
#' @param summary List of reactives returned by [mod_summary_server()]: the
#'   aggregated table and the assessment of the samples.
#' @param rounding Reactive from [mod_rounding_server()]: the rounding of the
#'   concentrations in the tables of the report (see [round_result()]).
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
            shiny::dateInput(ns('date'), t('report.date'), value = Sys.Date(), format = 'dd.mm.yyyy',
                             language = lang, weekstart = 1, width = '100%'),
            shiny::textAreaInput(ns('title'), t('report.title'), placeholder = t('report.title_placeholder'),
                                 rows = 2, width = '100%', resize = 'vertical'),
            shiny::textInput(ns('operator'), t('report.operator'), placeholder = t('report.operator_placeholder'),
                             width = '100%'),
            shiny::textInput(ns('plate'), t('report.plate'), placeholder = t('report.plate_placeholder'),
                             width = '100%')
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
mod_report_server <- function(id, lang, curve, summary, rounding = shiny::reactive(list(fun = signif, par = 3))) {
    shiny::moduleServer(id, function(input, output, session) {
        type <- shiny::reactive(if (is.null(input$type)) 'short' else input$type)
        # The date of the experiment; today until the calendar answers or when it is cleared
        date <- shiny::reactive({
            d <- input$date
            if (length(d) != 1 || is.na(d)) Sys.Date() else as.Date(d)
        })
        text_input <- function(x) if (is.null(x)) '' else x

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
            # The wells used in the result follow the grouping of the summary
            wells <- r$result
            if (length(assessment$included) == nrow(wells)) wells$included <- assessment$included
            wells <- localize_result(round_result(wells, rounding()$fun, rounding()$par), lang)
            report_blocks(type(),
                          date = as.character(date()),
                          title = text_input(input$title),
                          operator = text_input(input$operator),
                          plate = text_input(input$plate),
                          method = tr(paste0('model.', curve$model()), lang),
                          scale = tr(scale_key(curve$model()), lang),
                          weights = weight_labels(curve$weights(), lang),
                          background = background_text(r$background, lang),
                          at_blank = blank_rule_text(r, lang),
                          equation = model_equation_text(r$fit),
                          model_table = curve$coef_table(),
                          result_table = abbreviate_columns(wells[, result_columns(names(wells)), drop = FALSE]),
                          means_table = summary$table(),
                          qc_table = abbreviate_columns(localize_result(
                              round_result(r$qc, rounding()$fun, rounding()$par, r$quality$qc$wells$bias.limit), lang)),
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
            export_button_ui(session$ns('download'), tr('report.download', lang), 'download',
                             is.null(curve$export_lock())))
        observe_locked_export(input, 'download', lang, curve$export_lock)
        output$download <- shiny::downloadHandler(
            filename = function() paste0(date(), '_report_', type(), '.docx'),
            content = function(file) {
                shiny::req(is.null(curve$export_lock()))
                write_report(blocks(), file, plot())
            }
        )

        blocks
    })
}
