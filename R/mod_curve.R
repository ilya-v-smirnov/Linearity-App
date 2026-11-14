#' Standard Curve Module
#'
#' Model selection, standard curve and residual plots, coefficients and warnings.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param data Reactive with validated input data.
#' @param rounding Reactive from [mod_rounding_server()].
#'
#' @return `mod_curve_server()` returns a list of reactives: `result`
#'   (output of [calculate()]), `coef_table`, `notes` (translated warnings),
#'   `plot_args` (axis titles) and `model` / `linear` (selected settings).
#' @name mod_curve
#' @keywords internal
NULL

#' X-axis Unit Choices
#' @keywords internal
unit_choices <- function(lang) {
    stats::setNames(
        c('', ', pg/ml', ', ng/ml', ', \u03bcg/ml', ', mg/ml', ', ng/\u03bcl', ', nM', ', \u03bcM', ', mM'),
        c(tr('curve.unit_none', lang), 'pg/ml', 'ng/ml', 'ug/ml', 'mg/ml', 'ng/ul', 'nM', 'uM', 'mM')
    )
}

#' @rdname mod_curve
mod_curve_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    models <- calibration_models()
    shiny::tagList(
        shiny::plotOutput(ns('plots'), width = '800px'),
        shiny::br(),
        shiny::uiOutput(ns('notes')),
        shiny::tableOutput(ns('coefs')),
        shiny::fluidRow(
            shiny::column(4, shiny::radioButtons(ns('model'), t('curve.model'),
                                                 choices = stats::setNames(models,
                                                                           vapply(paste0('model.', models), t, '')),
                                                 selected = 'LRM')),
            shiny::column(2, shiny::checkboxInput(ns('linear_scale'), t('curve.linear'), value = FALSE))
        ),
        shiny::br(),
        shiny::fluidRow(
            shiny::column(4, shiny::textInput(ns('xtitle'), t('curve.xtitle'),
                                              value = t('curve.xdefault'), width = '100%')),
            shiny::column(2, shiny::selectInput(ns('xunits'), t('curve.xunits'),
                                                choices = unit_choices(lang), selected = '',
                                                width = '100%')),
            shiny::column(4, shiny::textInput(ns('ytitle'), t('curve.ytitle'),
                                              value = t('curve.ydefault'), width = '100%'))
        ),
        shiny::downloadButton(ns('save_plot'), t('curve.save_plot'))
    )
}

#' @rdname mod_curve
mod_curve_server <- function(id, lang, data, rounding) {
    shiny::moduleServer(id, function(input, output, session) {
        result <- shiny::reactive({
            d <- data()
            r <- rounding()
            shiny::req(input$model)
            with_validation(
                calculate(d, model = input$model, linear = isTRUE(input$linear_scale),
                          round_fun = r$fun, round_par = r$par),
                'common.calc_error', lang)
        })

        plot_args <- shiny::reactive({
            list(xtitle = paste0(input$xtitle, input$xunits), ytitle = input$ytitle)
        })

        plots <- shiny::reactive({
            args <- plot_args()
            get_plots(result(), xtitle = args$xtitle, ytitle = args$ytitle, lang = lang)
        })

        notes <- shiny::reactive(message_text(result()$notes, lang))

        coef_table <- shiny::reactive({
            r <- rounding()
            get_coef_ci(result()$model_direct, round_fun = r$fun, round_par = r$par)
        })

        output$plots <- shiny::renderPlot(plots())
        output$notes <- shiny::renderUI(notes_ui(notes(), lang))
        output$coefs <- shiny::renderTable(coef_table())

        output$save_plot <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_plot.jpeg'),
            content = function(file) {
                ggplot2::ggsave(file, plots(), width = 20, height = 10, units = 'cm', dpi = 600)
            }
        )

        list(result = result,
             coef_table = coef_table,
             notes = notes,
             plot_args = plot_args,
             model = shiny::reactive(input$model),
             linear = shiny::reactive(isTRUE(input$linear_scale)))
    })
}
