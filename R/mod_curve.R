#' Standard Curve Module
#'
#' Model and weighting selection, acceptance criteria of the standards,
#' standard curve and residual plots, warnings, exclusion of standard points
#' (click on a point, a reason is required), comparison of models and the
#' model parameters with the verdict of the run and the quality of the
#' standards.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param data Reactive with validated input data.
#' @param rounding Reactive from [mod_rounding_server()].
#' @param criteria Reactive with acceptance criteria (see [qc_criteria()]).
#' @param options Reactive from [mod_quant_options_server()]: handling of
#'   samples outside the quantification range.
#'
#' @return `mod_curve_server()` returns a list of reactives: `result`
#'   (output of [calculate()]), `coef_table`, `notes` (translated warnings
#'   without those of the samples, see [sample_warning_keys()]),
#'   `plot_args` (axis titles), `model`, `weights` (settings),
#'   `exclusions` (user exclusions: `key`, `reason`) and `comparison`
#'   (see [compare_models()]) and `effect_tables` (original and refitted
#'   calibration after exclusions, see [exclusion_effect_tables()]; `NULL`
#'   without exclusions).
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


#' Are the Units of the Calibrator Chosen?
#'
#' @param units Value of the units input (see [unit_choices()]); `""` is
#'   "None".
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
units_chosen <- function(units) length(units) == 1 && !is.na(units) && nzchar(units)


#' Export Button That Requires the Units of the Calibrator
#'
#' Figures, tables and reports are exported only after the units of the
#' calibrator concentrations are chosen on the standard curve step. Until
#' then the button looks the same but is an action button with the id
#' `<id>_locked`: a click shows a message asking for the units (see
#' [observe_locked_export()]) instead of downloading.
#'
#' @param id Namespaced id of the download output.
#' @param label Label of the button.
#' @param icon Name of the icon.
#' @param ready Whether the units are chosen (see [units_chosen()]).
#'
#' @return A download button or an action button.
#' @keywords internal
export_button_ui <- function(id, label, icon, ready) {
    if (isTRUE(ready)) {
        return(shiny::downloadButton(id, label, icon = shiny::icon(icon), class = 'btn-primary w-100'))
    }
    shiny::actionButton(paste0(id, '_locked'), label, icon = shiny::icon(icon),
                        class = 'btn-primary w-100 export-locked')
}


#' Message Asking for the Units of the Calibrator
#'
#' @param lang Language code.
#' @return A modal dialog.
#' @keywords internal
units_modal <- function(lang = 'en') {
    shiny::modalDialog(
        title = shiny::span(shiny::icon('triangle-exclamation', class = 'text-warning'), ' ',
                            tr('export.units_title', lang)),
        tr('export.units_needed', lang),
        footer = shiny::modalButton(tr('export.ok', lang)),
        easyClose = TRUE
    )
}


#' Message on a Click of a Locked Export Button
#'
#' Shows [units_modal()] when the action button of [export_button_ui()] is
#' clicked, i.e. when the units of the calibrator are not chosen.
#'
#' @param input Input of the module.
#' @param id Id of the download output within the module.
#' @param lang Language code.
#'
#' @return Called for its side effect.
#' @keywords internal
observe_locked_export <- function(input, id, lang = 'en') {
    shiny::observeEvent(input[[paste0(id, '_locked')]], shiny::showModal(units_modal(lang)))
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
#' While all fitted models use the same wells the table is compact and marks
#' the model with the lowest AIC. Otherwise it gets a column with the wells
#' used (`15/16`: wells in the fit / wells of the common set) instead of the
#' mark, and the attribute `note` explains the difference (see
#' [comparison_note()]).
#'
#' @param cmp Output of [compare_models()].
#' @param lang Language code.
#'
#' @return A data.frame with translated column names and rounded values.
#' @keywords internal
comparison_table <- function(cmp, lang) {
    fmt <- function(x, pattern) ifelse(is.na(x), '', sprintf(pattern, x))
    same <- same_wells(cmp)
    out <- data.frame(
        model = vapply(paste0('model_short.', cmp$model), tr, '', lang = lang),
        n.par = as.integer(cmp$n.par),
        wells = ifelse(is.na(cmp$n.used), '', paste0(cmp$n.used, '/', cmp$n.available)),
        AIC = fmt(cmp$AIC, '%.1f'),
        BIC = fmt(cmp$BIC, '%.1f'),
        R2 = fmt(cmp$R2, '%.4f'),
        mean = fmt(cmp$mean.abs.error, '%.1f'),
        max = fmt(cmp$max.abs.error, '%.1f'),
        lof = ifelse(is.na(cmp$lof.p), '', format(signif(cmp$lof.p, 2))),
        rec = ifelse(cmp$lowest.aic, tr('compare.mark', lang), '')
    )
    keys <- c('compare.model', 'compare.n_par', 'compare.wells_used', 'compare.aic', 'compare.bic', 'compare.r2',
              'compare.mean_error', 'compare.max_error', 'compare.lof', 'compare.lowest_aic')
    names(out) <- vapply(keys, tr, '', lang = lang)
    out <- out[, if (same) -3 else -ncol(out)]
    rownames(out) <- NULL
    attr(out, 'note') <- comparison_note(cmp, lang)
    out
}


#' Why the Compared Models Use Different Wells
#'
#' @param cmp Output of [compare_models()].
#' @param lang Language code.
#'
#' @return Text, or `NULL` if all fitted models use the same wells.
#' @keywords internal
comparison_note <- function(cmp, lang) {
    if (same_wells(cmp)) return(NULL)
    fitted <- !vapply(cmp$wells, is.null, logical(1))
    dropped <- vapply(cmp$dropped, function(k) paste(k, collapse = ', '), '')
    groups <- unique(dropped[fitted & nzchar(dropped)])
    lines <- vapply(groups, function(g) {
        models <- cmp$model[fitted & dropped == g]
        tr('compare.dropped_log', lang,
           models = paste(vapply(paste0('model_short.', models), tr, '', lang = lang), collapse = ', '),
           wells = g)
    }, '')
    paste(c(tr('compare.wells_differ', lang), lines), collapse = ' ')
}


#' Standard Reasons for Excluding a Standard Point
#'
#' @param lang Language code; `NULL` returns the codes.
#'
#' @return Codes, or translated labels named by the codes.
#' @keywords internal
exclusion_reasons <- function(lang = NULL) {
    codes <- c('outlier', 'pipetting', 'bubble', 'saturation', 'background')
    if (is.null(lang)) return(codes)
    stats::setNames(vapply(paste0('reason.', codes), tr, '', lang = lang), codes)
}


#' Exclusions for Display
#'
#' @param ex A data.frame with `key`, `reason` and optionally `standard`,
#'   `conc`, `OD` (see the `exclusions` element of [calculate()]).
#' @param lang Language code.
#'
#' @return A data.frame with translated column names.
#' @keywords internal
exclusions_table <- function(ex, lang) {
    cols <- intersect(c('standard', 'key', 'conc', 'OD', 'reason'), names(ex))
    out <- ex[, cols, drop = FALSE]
    # Numbers as text: the tables would otherwise show two decimals
    if ('conc' %in% cols) out$conc <- ifelse(is.na(out$conc), '', as.character(signif(out$conc, 4)))
    if ('OD' %in% cols) out$OD <- ifelse(is.na(out$OD), '', sprintf('%.4f', out$OD))
    names(out) <- vapply(paste0('exclusion.', tolower(cols)), tr, '', lang = lang)
    out
}


#' Equation of a Calibration Model
#'
#' The equation of the fitted model with the names of the coefficients of
#' [get_coef_ci()] (`C` is the concentration, `OD` the background-subtracted
#' optical density). LRM and QRM are on log10 scales unless fitted on
#' untransformed values; LIN and QUAD on the original scale.
#'
#' @param fit A `lin_fit` object.
#'
#' @return A shiny tag.
#' @keywords internal
model_equation_ui <- function(fit) {
    # One HTML string: line breaks between tags would show as spaces in the equation
    v <- function(x) paste0('<i>', x, '</i>')
    log10 <- function(x) paste0('log<sub>10</sub>(', x, ')')
    frac <- function(num, den) {
        paste0('<span class="eq-frac"><span class="eq-num">', num, '</span><span class="eq-den">', den,
               '</span></span>')
    }
    dot <- ' \u00b7 '
    minus <- ' \u2212 '
    conc <- v('C')
    ratio <- paste0('(', conc, ' / ', v('c'), ')<sup>', v('b'), '</sup>')
    degree <- poly_degree(fit$model)
    u <- if (fit$log_scale) log10(conc) else conc
    u2 <- if (fit$log_scale) paste0('[', u, ']<sup>2</sup>') else paste0(u, '<sup>2</sup>')
    rhs <- switch(
        if (degree > 0) 'poly' else fit$model,
        'poly' = paste0(v('a'), ' + ', v('b'), dot, u, if (degree == 2) paste0(' + ', v('c'), dot, u2)),
        '4PL' = paste0(v('d'), ' + ', frac(paste0(v('a'), minus, v('d')), paste0('1 + ', ratio))),
        '5PL' = paste0(v('d'), ' + ', frac(paste0(v('a'), minus, v('d')),
                                           paste0('[1 + ', ratio, ']<sup>', v('g'), '</sup>')))
    )
    lhs <- if (fit$log_scale) log10(v('OD')) else v('OD')
    shiny::div(class = 'model-equation', shiny::HTML(paste0(lhs, ' = ', rhs)))
}


#' Tab Comparing the Original and the Refitted Model
#'
#' Inserted after the model parameters when standard points are excluded and
#' removed when all of them are restored.
#'
#' @param session Session of the curve module.
#' @param show Insert (`TRUE`) or remove (`FALSE`) the tab.
#' @param lang Language code.
#'
#' @return Called for its side effect.
#' @keywords internal
toggle_refit_tab <- function(session, show, lang) {
    if (!show) return(bslib::nav_remove('tabs', 'refit', session = session))
    panel <- bslib::nav_panel(
        tr('effect.tab', lang), value = 'refit', icon = shiny::icon('code-compare'),
        # Panels inserted later are not wrapped in the body of the card like the others
        bslib::card_body(shiny::div(class = 'step-view', shiny::uiOutput(session$ns('effect'))))
    )
    bslib::nav_insert('tabs', panel, target = 'coefs', position = 'after', select = FALSE, session = session)
}


#' @rdname mod_curve
mod_curve_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    models <- calibration_models()
    weights <- weighting_schemes()

    sidebar <- bslib::sidebar(
        width = 330,
        sidebar_group(
            t('curve.group_axes'), 'pen',
            shiny::textInput(ns('xtitle'), t('curve.xtitle'), value = t('curve.xdefault'), width = '100%'),
            shiny::selectInput(ns('xunits'), t('curve.xunits'), choices = unit_choices(lang), selected = '',
                               width = '100%'),
            shiny::textInput(ns('ytitle'), t('curve.ytitle'), value = t('curve.ydefault'), width = '100%'),
            shiny::uiOutput(ns('save_plot_ui'))
        ),
        sidebar_group(
            t('curve.group_model'), 'chart-line',
            shiny::radioButtons(ns('model'), t('curve.model'),
                                choices = stats::setNames(models, vapply(paste0('model.', models), t, '')),
                                selected = 'LRM'),
            shiny::selectInput(ns('weights'), with_tooltip(t('curve.weights'), t('curve.weights_help')),
                               choices = stats::setNames(weights, weight_labels(weights, lang)),
                               selected = 'none')
        ),
        mod_criteria_ui('criteria', lang, 'standards')
    )

    main <- bslib::navset_card_underline(
        id = ns('tabs'),
        full_screen = TRUE,
        bslib::nav_panel(
            with_tooltip(t('curve.title'), t('curve.click_hint'), placement = 'bottom'), value = 'curve',
            shiny::div(
                class = 'step-view',
                # Each plot in a panel of limited width; the panels stay next to each other
                # on the left, the statistics of the curve in one row below them, then the
                # comparison of models
                shiny::div(
                    class = 'curve-panels',
                    shiny::div(class = 'curve-panel',
                               shiny::plotOutput(ns('curve'), height = 'auto', click = ns('curve_click'))),
                    shiny::div(class = 'curve-panel',
                               shiny::plotOutput(ns('resid'), height = 'auto'))
                ),
                shiny::uiOutput(ns('curve_stats')),
                shiny::h5(with_tooltip(t('curve.compare'), t('compare.note'))),
                shiny::uiOutput(ns('compare_mark')),
                shiny::tableOutput(ns('compare')),
                shiny::uiOutput(ns('compare_note')),
                shiny::uiOutput(ns('notes')),
                shiny::uiOutput(ns('exclusions'))
            )
        ),
        bslib::nav_panel(
            t('curve.coefs'), value = 'coefs',
            shiny::div(class = 'step-view',
                       shiny::uiOutput(ns('verdict')),
                       shiny::uiOutput(ns('refit_note')),
                       shiny::h5(t('curve.section_equation')),
                       shiny::uiOutput(ns('equation')),
                       shiny::h5(t('curve.section_coefs')),
                       shiny::tableOutput(ns('coefs')),
                       shiny::h5(t('quality.section_standards')),
                       shiny::tableOutput(ns('standards')),
                       # Collapsed by default
                       shiny::tags$details(class = 'std-points',
                                           shiny::tags$summary(t('quality.section_points')),
                                           shiny::tableOutput(ns('points'))),
                       shiny::h5(t('quality.section_curve')),
                       shiny::tableOutput(ns('curve_quality')))
        )
    )

    bslib::layout_sidebar(sidebar = sidebar, main)
}

#' @rdname mod_curve
mod_curve_server <- function(id, lang, data, rounding, criteria = shiny::reactive(qc_criteria()),
                             options = shiny::reactive(list())) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        t <- function(key, ...) tr(key, lang, ...)
        no_exclusions <- data.frame(key = character(0), reason = character(0))
        exclusions <- shiny::reactiveVal(no_exclusions)
        clicked <- shiny::reactiveVal(NULL)

        # New data: forget the exclusions; the units must be chosen again
        shiny::observeEvent(data(), {
            exclusions(no_exclusions)
            shiny::updateSelectInput(session, 'xunits', selected = '')
        })
        units_ok <- shiny::reactive(units_chosen(input$xunits))
        output$save_plot_ui <- shiny::renderUI(export_button_ui(ns('save_plot'), t('curve.save_plot'), 'image',
                                                                units_ok()))
        observe_locked_export(input, 'save_plot', lang)

        settings <- shiny::reactive({
            shiny::req(input$model)
            list(model = input$model,
                 weights = if (is.null(input$weights)) 'none' else input$weights)
        })

        result <- shiny::reactive({
            d <- data()
            r <- rounding()
            s <- settings()
            o <- options()
            with_validation(
                calculate(d, model = s$model, weights = s$weights,
                          exclude = exclusions(), round_fun = r$fun, round_par = r$par,
                          criteria = criteria(),
                          extrapolate = if (is.null(o$extrapolate)) character(0) else o$extrapolate,
                          blank_zero = isTRUE(o$blank_zero)),
                'common.calc_error', lang)
        })

        plot_args <- shiny::reactive({
            list(xtitle = paste0(input$xtitle, input$xunits), ytitle = input$ytitle)
        })

        plots <- shiny::reactive({
            args <- plot_args()
            get_plots(result(), xtitle = args$xtitle, ytitle = args$ytitle, lang = lang)
        })

        # Warnings of the samples depend on the grouping chosen in the result summary
        notes <- shiny::reactive({
            message_text(Filter(function(m) !m$key %in% sample_warning_keys(), result()$notes), lang)
        })
        quality <- shiny::reactive(quality_tables(result()$quality, lang))
        # Values failing the acceptance criteria are highlighted in the app
        quality_marked <- shiny::reactive(quality_tables(result()$quality, lang, highlight = TRUE))

        coef_table <- shiny::reactive({
            r <- rounding()
            get_coef_ci(result()$fit, round_fun = r$fun, round_par = r$par)
        })

        # All models on the common set of wells: the choice of the model does not change it
        comparison <- shiny::reactive({
            s <- settings()
            compare_models(result()$calibration_pool, weights = s$weights)
        })

        # Original (all standards) and refitted (without the excluded points) calibrations
        effect_tables <- shiny::reactive({
            r <- rounding()
            exclusion_effect_tables(result()$exclusion_effect, lang, r$fun, r$par)
        })

        # The plots are nearly square; the width of their panels follows the height of the
        # window (see the CSS), so that the statistics and the comparison of models fit
        plot_height <- function(name) {
            function() {
                width <- session$clientData[[paste0('output_', ns(name), '_width')]]
                if (is.null(width) || width <= 0) 290 else min(370, max(200, round(0.83 * width)))
            }
        }
        output$curve <- shiny::renderPlot({
            args <- plot_args()
            plot_standard_curve(result(), xtitle = args$xtitle, ytitle = args$ytitle, lang = lang)
        }, height = plot_height('curve'))
        output$resid <- shiny::renderPlot({
            plot_fitted_resid(result(), xtitle = plot_args()$xtitle, lang = lang)
        }, height = plot_height('resid'))
        # Warnings shown by the statistics below the plots are left out of the warnings box
        output$notes <- shiny::renderUI({
            hidden <- c('note.not_quantified', 'quality.warn_r2', 'note.lack_of_fit', sample_warning_keys())
            shown <- Filter(function(m) !m$key %in% hidden, result()$notes)
            notes_ui(message_text(shown, lang), lang)
        })
        output$curve_stats <- shiny::renderUI({
            r <- result()
            shiny::div(class = 'curve-stats',
                       r2_ui(r$quality$curve$r2, lang),
                       mean_error_ui(r$fit, criteria()$std_bias, lang),
                       lof_ui(r$lack_of_fit$p, lang))
        })
        output$coefs <- shiny::renderTable({
            tab <- coef_table()
            tab[-1] <- lapply(tab[-1], as.character)
            tab
        })
        output$verdict <- shiny::renderUI(verdict_ui(result(), lang, unit = sub('^, ', '', input$xunits)))
        output$equation <- shiny::renderUI(model_equation_ui(result()$fit))
        output$standards <- shiny::renderTable(quality_marked()$standards, na = '',
                                               sanitize.text.function = function(x) x)
        output$points <- shiny::renderTable(standard_points_table(result(), lang, highlight = TRUE), na = '',
                                            sanitize.text.function = function(x) x)
        # Rendered while the details are collapsed, so that they open at once
        shiny::outputOptions(output, 'points', suspendWhenHidden = FALSE)
        output$curve_quality <- shiny::renderTable(quality()$curve, na = '')
        output$compare <- shiny::renderTable(comparison_table(comparison(), lang), na = '')
        output$compare_note <- shiny::renderUI({
            note <- comparison_note(comparison(), lang)
            if (is.null(note)) return(NULL)
            shiny::div(class = 'alert alert-warning compare-note', shiny::icon('triangle-exclamation'), ' ', note)
        })
        # The row of the chosen model is shaded (renderTable gives no classes to the rows)
        output$compare_mark <- shiny::renderUI({
            row <- match(settings()$model, comparison()$model)
            if (is.na(row)) return(NULL)
            shiny::tags$style(sprintf('#%s tbody tr:nth-child(%d) td { background-color: #e9ecef; }',
                                      ns('compare'), row))
        })

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
                    shiny::radioButtons(ns('reason_choice'), t('curve.reason'),
                                        choices = stats::setNames(c(exclusion_reasons(), 'other'),
                                                                  c(exclusion_reasons(lang), t('reason.other'))),
                                        selected = character(0), width = '100%'),
                    shiny::conditionalPanel(
                        condition = "input.reason_choice == 'other'", ns = ns,
                        shiny::textInput(ns('reason'), NULL, width = '100%',
                                         placeholder = t('reason.other_placeholder'))
                    ),
                    footer = shiny::tagList(shiny::modalButton(t('curve.cancel')),
                                            shiny::actionButton(ns('confirm_exclude'), t('curve.exclude')))
                ))
            }
        })

        shiny::observeEvent(input$confirm_exclude, {
            choice <- if (is.null(input$reason_choice)) '' else input$reason_choice
            reason <- if (choice %in% exclusion_reasons()) {
                exclusion_reasons(lang)[[choice]]
            } else {
                trimws(if (is.null(input$reason)) '' else input$reason)
            }
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
                shiny::p(class = 'text-muted small mt-2', shiny::icon('code-compare'), ' ', t('effect.hint')),
                shiny::actionButton(ns('restore_all'), t('curve.restore_all'), icon = shiny::icon('undo')),
                shiny::br(), shiny::br()
            )
        })

        # The comparison of the original and the refitted model has its own tab while points are excluded
        has_effect <- shiny::reactive({
            !is.null(tryCatch(result()$exclusion_effect, error = function(e) NULL))
        })
        refit_tab <- shiny::reactiveVal(FALSE)
        shiny::observeEvent(has_effect(), {
            if (has_effect() == refit_tab()) return()
            refit_tab(has_effect())
            toggle_refit_tab(session, has_effect(), lang)
        })
        # The exclusions of the calculation have the level names and the concentrations
        output$exclusions_table <- shiny::renderTable(exclusions_table(result()$exclusions, lang))
        output$effect <- shiny::renderUI({
            exclusion_effect_ui(effect_tables(), ns, result()$quality$criteria, lang)
        })
        output$effect_coefs <- shiny::renderTable(effect_tables()$coefs, na = '')
        output$effect_summary <- shiny::renderTable(effect_tables()$summary, na = '')
        output$effect_back_calc <- shiny::renderTable(effect_tables()$back_calc, na = '')
        output$refit_note <- shiny::renderUI({
            n <- nrow(result()$exclusions)
            if (n == 0) return(NULL)
            shiny::div(class = 'alert alert-info refit-note', shiny::icon('circle-info'), ' ',
                       t('curve.refit_note', n = n))
        })

        output$save_plot <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_plot.jpeg'),
            content = function(file) {
                shiny::req(units_ok())
                ggplot2::ggsave(file, plots(), width = 20, height = 10, units = 'cm', dpi = 600)
            }
        )

        list(result = result,
             coef_table = coef_table,
             notes = notes,
             plot_args = plot_args,
             units_ok = units_ok,
             model = shiny::reactive(settings()$model),
             weights = shiny::reactive(settings()$weights),
             exclusions = exclusions,
             comparison = comparison,
             effect_tables = effect_tables)
    })
}
