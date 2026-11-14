#' Standard Curve Module
#'
#' Model and weighting selection, standard curve and residual plots,
#' coefficients, warnings, exclusion of standard points (click on a point,
#' a reason is required) and comparison of models.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param data Reactive with validated input data.
#' @param rounding Reactive from [mod_rounding_server()].
#'
#' @return `mod_curve_server()` returns a list of reactives: `result`
#'   (output of [calculate()]), `coef_table`, `notes` (translated warnings),
#'   `plot_args` (axis titles), `model`, `linear`, `weights` (settings),
#'   `exclusions` (user exclusions: `key`, `reason`) and `comparison`
#'   (see [compare_models()]).
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


#' Translated Labels of Weighting Schemes
#'
#' @param weights Weighting codes.
#' @param lang Language code.
#'
#' @return Character vector of labels.
#' @keywords internal
weight_labels <- function(weights, lang) {
    keys <- c('none' = 'none', '1/y' = 'inv_y', '1/y2' = 'inv_y2', '1/x' = 'inv_x',
              '1/x2' = 'inv_x2', 'power' = 'power')
    vapply(weights, function(w) tr(paste0('weights.', keys[[w]]), lang), '')
}


#' Comparison of Models for Display
#'
#' @param cmp Output of [compare_models()].
#' @param lang Language code.
#'
#' @return A data.frame with translated column names and rounded values.
#' @keywords internal
comparison_table <- function(cmp, lang) {
    fmt <- function(x, pattern) ifelse(is.na(x), '', sprintf(pattern, x))
    out <- data.frame(
        model = cmp$model,
        n.par = as.integer(cmp$n.par),
        AIC = fmt(cmp$AIC, '%.1f'),
        BIC = fmt(cmp$BIC, '%.1f'),
        R2 = fmt(cmp$R2, '%.4f'),
        mean = fmt(cmp$mean.abs.error, '%.1f'),
        max = fmt(cmp$max.abs.error, '%.1f'),
        lof = ifelse(is.na(cmp$lof.p), '', format(signif(cmp$lof.p, 2))),
        rec = ifelse(cmp$recommended, tr('compare.mark', lang), '')
    )
    names(out) <- vapply(c('compare.model', 'compare.n_par', 'compare.aic', 'compare.bic', 'compare.r2',
                           'compare.mean_error', 'compare.max_error', 'compare.lof', 'compare.recommended'),
                         tr, '', lang = lang)
    out
}


#' Exclusions for Display
#'
#' @param ex A data.frame with `key`, `reason` and optionally `conc`, `OD`.
#' @param lang Language code.
#'
#' @return A data.frame with translated column names.
#' @keywords internal
exclusions_table <- function(ex, lang) {
    cols <- intersect(c('key', 'conc', 'OD', 'reason'), names(ex))
    out <- ex[, cols, drop = FALSE]
    names(out) <- vapply(paste0('exclusion.', tolower(cols)), tr, '', lang = lang)
    out
}


#' @rdname mod_curve
mod_curve_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    models <- calibration_models()
    weights <- weighting_schemes()
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(6, shiny::plotOutput(ns('curve'), height = '350px', click = ns('curve_click'))),
            shiny::column(6, shiny::plotOutput(ns('resid'), height = '350px'))
        ),
        shiny::helpText(t('curve.click_hint')),
        shiny::uiOutput(ns('notes')),
        shiny::fluidRow(
            shiny::column(4, shiny::radioButtons(ns('model'), t('curve.model'),
                                                 choices = stats::setNames(models,
                                                                           vapply(paste0('model.', models), t, '')),
                                                 selected = 'LRM')),
            shiny::column(4,
                          shiny::selectInput(ns('weights'), t('curve.weights'),
                                             choices = stats::setNames(weights, weight_labels(weights, lang)),
                                             selected = 'none'),
                          shiny::helpText(t('curve.weights_help'))),
            shiny::column(4, shiny::conditionalPanel(
                condition = "input.model == 'LRM' || input.model == 'QRM'", ns = ns,
                shiny::checkboxInput(ns('linear_scale'), t('curve.linear'), value = FALSE)))
        ),
        shiny::tableOutput(ns('coefs')),
        shiny::uiOutput(ns('exclusions')),
        shiny::h4(t('curve.compare')),
        shiny::tableOutput(ns('compare')),
        shiny::helpText(t('compare.note')),
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
        ns <- session$ns
        t <- function(key, ...) tr(key, lang, ...)
        no_exclusions <- data.frame(key = character(0), reason = character(0))
        exclusions <- shiny::reactiveVal(no_exclusions)
        clicked <- shiny::reactiveVal(NULL)

        # New data: forget the exclusions
        shiny::observeEvent(data(), exclusions(no_exclusions))

        settings <- shiny::reactive({
            shiny::req(input$model)
            list(model = input$model,
                 linear = isTRUE(input$linear_scale) && input$model %in% c('LRM', 'QRM'),
                 weights = if (is.null(input$weights)) 'none' else input$weights)
        })

        result <- shiny::reactive({
            d <- data()
            r <- rounding()
            s <- settings()
            with_validation(
                calculate(d, model = s$model, linear = s$linear, weights = s$weights,
                          exclude = exclusions(), round_fun = r$fun, round_par = r$par),
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
            get_coef_ci(result()$fit, round_fun = r$fun, round_par = r$par)
        })

        comparison <- shiny::reactive({
            s <- settings()
            compare_models(result()$standard_data, linear = s$linear, weights = s$weights)
        })

        output$curve <- shiny::renderPlot({
            args <- plot_args()
            plot_standard_curve(result(), xtitle = args$xtitle, ytitle = args$ytitle)
        })
        output$resid <- shiny::renderPlot(plot_fitted_resid(result()$fit, lang = lang))
        output$notes <- shiny::renderUI(notes_ui(notes(), lang))
        output$coefs <- shiny::renderTable({
            tab <- coef_table()
            tab[-1] <- lapply(tab[-1], as.character)
            tab
        })
        output$compare <- shiny::renderTable(comparison_table(comparison(), lang), na = '')

        # Exclusion of standard points --------------------------------------
        point_text <- function(p) {
            t('curve.point', well = p$key, conc = signif(p$conc, 3), od = signif(p$OD, 3))
        }

        shiny::observeEvent(input$curve_click, {
            r <- result()
            points <- rbind(
                data.frame(key = r$standard_data$.key, conc = r$standard_data$conc,
                           OD = r$standard_data$OD, excluded = FALSE),
                data.frame(key = r$exclusions$key, conc = r$exclusions$conc,
                           OD = r$exclusions$OD, excluded = rep(TRUE, nrow(r$exclusions)))
            )
            near <- shiny::nearPoints(points, input$curve_click, xvar = 'conc', yvar = 'OD',
                                      threshold = 10, maxpoints = 1)
            if (nrow(near) == 0) return()
            clicked(near)
            if (near$excluded) {
                shiny::showModal(shiny::modalDialog(
                    title = t('curve.restore_title'),
                    shiny::p(point_text(near)),
                    shiny::p(t('exclusion.reason'), ': ',
                             exclusions()$reason[exclusions()$key == near$key]),
                    footer = shiny::tagList(shiny::modalButton(t('curve.cancel')),
                                            shiny::actionButton(ns('confirm_restore'), t('curve.restore')))
                ))
            } else {
                shiny::showModal(shiny::modalDialog(
                    title = t('curve.exclude_title'),
                    shiny::p(point_text(near)),
                    shiny::textInput(ns('reason'), t('curve.reason'), width = '100%'),
                    footer = shiny::tagList(shiny::modalButton(t('curve.cancel')),
                                            shiny::actionButton(ns('confirm_exclude'), t('curve.exclude')))
                ))
            }
        })

        shiny::observeEvent(input$confirm_exclude, {
            reason <- trimws(if (is.null(input$reason)) '' else input$reason)
            if (!nzchar(reason)) {
                shiny::showNotification(t('curve.reason_required'), type = 'warning')
                return()
            }
            ex <- exclusions()
            exclusions(rbind(ex[ex$key != clicked()$key, ],
                             data.frame(key = clicked()$key, reason = reason)))
            shiny::removeModal()
        })

        shiny::observeEvent(input$confirm_restore, {
            ex <- exclusions()
            exclusions(ex[ex$key != clicked()$key, , drop = FALSE])
            shiny::removeModal()
        })

        shiny::observeEvent(input$restore_all, exclusions(no_exclusions))

        output$exclusions <- shiny::renderUI({
            if (nrow(exclusions()) == 0) return(NULL)
            shiny::tagList(
                shiny::h4(t('curve.exclusions')),
                shiny::tableOutput(ns('exclusions_table')),
                shiny::actionButton(ns('restore_all'), t('curve.restore_all'), icon = shiny::icon('undo')),
                shiny::br(), shiny::br()
            )
        })
        output$exclusions_table <- shiny::renderTable(exclusions_table(exclusions(), lang))

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
             model = shiny::reactive(settings()$model),
             linear = shiny::reactive(settings()$linear),
             weights = shiny::reactive(settings()$weights),
             exclusions = exclusions,
             comparison = comparison)
    })
}
