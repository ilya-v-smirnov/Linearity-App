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
#'   `plot_args` (axis titles), `units_ok`, `export_lock` (why the exports
#'   are locked, see [export_lock()]), `model`, `weights`, `background` (settings),
#'   `exclusions` (user exclusions: `key`, `reason`), `background_exclusions`
#'   (background wells excluded by the user) and `comparison`
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


#' Message Saying That the Background Is Unavailable
#'
#' @param lang Language code.
#' @return A modal dialog.
#' @keywords internal
background_modal <- function(lang = 'en') {
    shiny::modalDialog(
        title = shiny::span(shiny::icon('triangle-exclamation', class = 'text-danger'), ' ',
                            tr('quality.background_unavailable', lang)),
        tr('export.background_needed', lang),
        footer = shiny::modalButton(tr('export.ok', lang)),
        easyClose = TRUE
    )
}


#' Why Exports Are Locked
#'
#' @param units_ok Whether the units of the calibrator are chosen (see
#'   [units_chosen()]).
#' @param background Status of the background (see [background_summary()]),
#'   or `NULL` when it is not known.
#'
#' @return `NULL` when the exports are ready, otherwise `"units"` or
#'   `"background"`.
#' @keywords internal
export_lock <- function(units_ok, background = NULL) {
    if (!isTRUE(units_ok)) return('units')
    if (identical(background, 'unavailable')) return('background')
    NULL
}


#' Message on a Click of a Locked Export Button
#'
#' Shows [units_modal()] or [background_modal()] when the action button of
#' [export_button_ui()] is clicked, i.e. when the units of the calibrator are
#' not chosen or the background is unavailable.
#'
#' @param input Input of the module.
#' @param id Id of the download output within the module.
#' @param lang Language code.
#' @param lock Function returning the reason of the lock (see
#'   [export_lock()]).
#'
#' @return Called for its side effect.
#' @keywords internal
observe_locked_export <- function(input, id, lang = 'en', lock = function() 'units') {
    shiny::observeEvent(input[[paste0(id, '_locked')]], {
        shiny::showModal(if (identical(lock(), 'background')) background_modal(lang) else units_modal(lang))
    })
}


#' Translated Labels of the Rules for a Signal at the Background
#'
#' @param rules Rule codes (see [blank_rules()]).
#' @param lang Language code.
#'
#' @return Character vector of labels named by the codes.
#' @keywords internal
blank_rule_labels <- function(rules = blank_rules(), lang = 'en') {
    stats::setNames(vapply(paste0('blank.rule_', rules), tr, '', lang = lang), rules)
}


#' Rule for a Signal at the Background as Text
#'
#' For the parameters of the report.
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#'
#' @return A character string, or `NULL` on log10 scales, where the rule does
#'   not apply.
#' @keywords internal
blank_rule_text <- function(result, lang = 'en') {
    if (is.null(result$at_blank) || isTRUE(result$fit$log_scale)) return(NULL)
    unname(blank_rule_labels(result$at_blank, lang))
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
#' the model with the lowest AIC. A column with the number of standards
#' without a back-calculated concentration appears when a model has some
#' (the errors are those of the back-calculated standards). Otherwise it gets a column with the wells
#' used (`15/16`: wells in the fit / wells of the common set) instead of the
#' mark, and the attribute `note` explains the difference (see
#' [comparison_note()]). When a model cannot be fitted, a last column says
#' why (see [comparison_status()]).
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
        failed = ifelse(is.na(cmp$n.failed) | cmp$n.failed == 0, '', as.character(cmp$n.failed)),
        AIC = fmt(cmp$AIC, '%.1f'),
        BIC = fmt(cmp$BIC, '%.1f'),
        R2 = fmt(cmp$R2, '%.4f'),
        mean = fmt(cmp$mean.abs.error, '%.1f'),
        max = fmt(cmp$max.abs.error, '%.1f'),
        lof = ifelse(is.na(cmp$lof.p), '', format(signif(cmp$lof.p, 2))),
        rec = ifelse(cmp$lowest.aic, tr('compare.mark', lang), ''),
        note = comparison_status(cmp, lang)
    )
    # Optional columns: the wells used when they differ (instead of the mark), failed inversions and
    # the reasons why a model is not fitted when there are any
    drop <- c(if (same) 'wells' else 'rec', if (all(out$failed == '')) 'failed', if (all(out$note == '')) 'note')
    keys <- c(model = 'compare.model', n.par = 'compare.n_par', wells = 'compare.wells_used',
              failed = 'compare.n_failed', AIC = 'compare.aic', BIC = 'compare.bic', R2 = 'compare.r2',
              mean = 'compare.mean_error', max = 'compare.max_error', lof = 'compare.lof',
              rec = 'compare.lowest_aic', note = 'compare.note_column')
    out <- out[, setdiff(names(out), drop), drop = FALSE]
    names(out) <- vapply(keys[names(out)], tr, '', lang = lang)
    rownames(out) <- NULL
    attr(out, 'note') <- comparison_note(cmp, lang)
    out
}


#' Why a Compared Model Is Not Fitted
#'
#' @param cmp Output of [compare_models()].
#' @param lang Language code.
#'
#' @return Character vector, one text per model: empty for the fitted
#'   models, otherwise the reason (too few standard levels, with the numbers
#'   available and required, a fit that did not converge or a fit that
#'   failed).
#' @keywords internal
comparison_status <- function(cmp, lang) {
    if (is.null(cmp$status)) return(rep('', nrow(cmp)))
    vapply(seq_len(nrow(cmp)), function(i) {
        switch(cmp$status[i],
               too_few_levels = tr('compare.too_few_levels', lang, n = cmp$n.levels[i],
                                   needed = cmp$n.levels.required[i]),
               not_converged = tr('compare.not_converged', lang),
               fit_failed = tr('compare.fit_failed', lang),
               '')
    }, '')
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


#' Reasons for Excluding a Background Well
#'
#' The reasons of [exclusion_reasons()] without signal saturation and a
#' signal at the background level, which do not apply to background wells.
#'
#' @return Codes.
#' @keywords internal
background_exclusion_reasons <- function() setdiff(exclusion_reasons(), c('saturation', 'background'))


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


#' Equation of the Calibration Model as Text
#'
#' The equation of [model_equation_ui()] in plain text with Unicode
#' subscripts and superscripts, for the report.
#'
#' @param fit A `lin_fit` object.
#'
#' @return A character string.
#' @keywords internal
model_equation_text <- function(fit) {
    log10 <- function(x) paste0('log\u2081\u2080(', x, ')')
    u <- if (fit$log_scale) log10('C') else 'C'
    u2 <- if (fit$log_scale) paste0('[', u, ']\u00b2') else paste0(u, '\u00b2')
    degree <- poly_degree(fit$model)
    ratio <- '(C / c)\u1d47'
    rhs <- switch(
        if (degree > 0) 'poly' else fit$model,
        'poly' = paste0('a + b \u00b7 ', u, if (degree == 2) paste0(' + c \u00b7 ', u2)),
        '4PL' = paste0('d + (a \u2212 d) / (1 + ', ratio, ')'),
        '5PL' = paste0('d + (a \u2212 d) / [1 + ', ratio, ']\u1d4d')
    )
    paste0(if (fit$log_scale) log10('OD') else 'OD', ' = ', rhs)
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
                               selected = 'none'),
            # Models on the original OD scale only: on log10 scales a signal at the background has no inverse
            shiny::conditionalPanel(
                condition = "input.model != 'LRM' && input.model != 'QRM'", ns = ns,
                shiny::radioButtons(ns('at_blank'), with_tooltip(t('blank.rule'), t('blank.rule_help')),
                                    choices = stats::setNames(blank_rules(), blank_rule_labels(lang = lang)),
                                    selected = 'range', width = '100%')
            ),
            # Quadratic models only: the branch of the parabola used for quantification
            shiny::conditionalPanel(
                condition = "input.model == 'QUAD' || input.model == 'QRM'", ns = ns,
                shiny::checkboxInput(ns('use_interval'),
                                     with_tooltip(t('curve.interval_use'), t('curve.interval_help')),
                                     value = FALSE, width = '100%'),
                shiny::conditionalPanel(
                    condition = 'input.use_interval', ns = ns,
                    shiny::div(class = 'crit-row',
                               shiny::tags$label(`for` = ns('interval_from'), t('curve.interval_from')),
                               shiny::numericInput(ns('interval_from'), NULL, value = NA, min = 0)),
                    shiny::div(class = 'crit-row',
                               shiny::tags$label(`for` = ns('interval_to'), t('curve.interval_to')),
                               shiny::numericInput(ns('interval_to'), NULL, value = NA, min = 0))
                )
            )
        ),
        mod_criteria_ui('criteria', lang, 'standards')
    )

    main <- bslib::navset_card_underline(
        id = ns('tabs'),
        full_screen = TRUE,
        # The background comes first (it is subtracted before the curve is fitted); the curve opens
        selected = 'curve',
        bslib::nav_panel(
            t('bg.group'), value = 'background', icon = shiny::icon('fill-drip'),
            shiny::div(class = 'step-view',
                       shiny::h5(with_tooltip(t('bg.section'), t('bg.help'))),
                       # No subtraction is an explicit choice; with subtraction, the roles are ticked and
                       # single wells are excluded on the plot below
                       shiny::radioButtons(ns('bg_mode'), NULL, inline = TRUE,
                                           choices = stats::setNames(c('subtract', 'none'),
                                                                     c(t('bg.mode_subtract'), t('bg.mode_none')))),
                       shiny::conditionalPanel(
                           condition = "input.bg_mode != 'none'", ns = ns,
                           shiny::checkboxGroupInput(ns('background'), t('bg.roles'), choices = character(0),
                                                     inline = TRUE)
                       ),
                       shiny::tableOutput(ns('background_table')),
                       shiny::uiOutput(ns('background_warning')),
                       shiny::uiOutput(ns('background_notes')),
                       shiny::h5(with_tooltip(t('bg.plot_section'), t('bg.click_hint'))),
                       shiny::div(class = 'background-panel',
                                  shiny::plotOutput(ns('background_plot'), height = '300px',
                                                    click = ns('background_click'))),
                       # Calibrators shown on the plot: hiding the strong ones rescales the OD axis to the blanks
                       shiny::div(class = 'level-toggles',
                                  shiny::checkboxGroupInput(ns('bg_levels'),
                                                            with_tooltip(t('bg.levels_shown'), t('bg.levels_help')),
                                                            choices = character(0), inline = TRUE)),
                       shiny::h5(with_tooltip(t('bg.wells_section'), t('bg.table_help'))),
                       # A double click on a row excludes or restores the well
                       shiny::div(class = 'dblclick-table', `data-input` = ns('background_dblclick'),
                                  shiny::tableOutput(ns('background_wells'))),
                       # Under all the tables, while wells are excluded
                       shiny::uiOutput(ns('background_restore')))
        ),
        bslib::nav_panel(
            with_tooltip(t('curve.title'), t('curve.click_hint'), placement = 'bottom'), value = 'curve',
            shiny::div(
                class = 'step-view',
                # The background chosen is unavailable: the curve is shown on the OD as measured
                shiny::uiOutput(ns('background_alert')),
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
                       # The table and the plot of the bias share the width of the table
                       shiny::div(class = 'std-block',
                                  shiny::h5(with_tooltip(t('quality.section_standards'), t('quality.backcalc_legend'))),
                                  shiny::tableOutput(ns('standards')),
                                  # Where the curve over- or underestimates, along the concentration
                                  shiny::h5(with_tooltip(t('quality.section_bias'), t('quality.bias_help'))),
                                  shiny::div(class = 'bias-panel', shiny::plotOutput(ns('bias'), height = 'auto'))),
                       # Collapsed by default
                       shiny::tags$details(class = 'std-points',
                                           shiny::tags$summary(with_tooltip(t('quality.section_points'),
                                                                            t('quality.points_criteria'))),
                                           shiny::p(class = 'text-muted small', t('quality.points_hint')),
                                           # A double click on a row excludes or restores the point
                                           shiny::div(class = 'dblclick-table', `data-input` = ns('points_dblclick'),
                                                      shiny::tableOutput(ns('points')))),
                       # The wells against the plan: only when a level deviates from it, collapsed by default
                       shiny::uiOutput(ns('wells_block')),
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
        # Roles of the background wells: the default of the plate until the user changes them
        bg_roles <- shiny::reactiveVal(NULL)
        # Subtraction (`"subtract"`) or the explicit choice of none (`"none"`, the default without background wells)
        bg_mode <- shiny::reactiveVal('subtract')
        exclusions <- shiny::reactiveVal(no_exclusions)
        bg_exclusions <- shiny::reactiveVal(no_exclusions)
        # The point clicked last, on the curve (`standard`) or among the background wells (`background`)
        clicked <- shiny::reactiveVal(NULL)
        # Standard levels shown on the plot of the background wells (the view only, not the calculation)
        shown_levels <- shiny::reactiveVal(NULL)

        # New data: forget the exclusions; the units must be chosen again
        shiny::observeEvent(data(), {
            exclusions(no_exclusions)
            bg_exclusions(no_exclusions)
            roles <- default_background(data()$type)
            bg_roles(roles)
            bg_mode(if (length(roles) == 0) 'none' else 'subtract')
            shiny::updateRadioButtons(session, 'bg_mode', selected = bg_mode())
            shiny::updateCheckboxGroupInput(session, 'background', choices = background_choices(data()$type, lang),
                                            selected = roles)
            shiny::updateSelectInput(session, 'xunits', selected = '')
            # The monotonic interval starts from the range of the standards of the new data
            conc <- data()$conc[data()$type == 'standard']
            conc <- conc[!is.na(conc)]
            # All standard levels shown again on the plot of the background wells
            levels <- sort(unique(conc))
            shown_levels(as.character(levels))
            shiny::updateCheckboxGroupInput(session, 'bg_levels',
                                            choices = stats::setNames(as.character(levels),
                                                                      standard_names(levels, conc)),
                                            selected = as.character(levels), inline = TRUE)
            if (length(conc) > 0) {
                shiny::updateCheckboxInput(session, 'use_interval', value = FALSE)
                shiny::updateNumericInput(session, 'interval_from', value = min(conc))
                shiny::updateNumericInput(session, 'interval_to', value = max(conc))
            }
        })
        # Unticking all roles subtracts nothing only with "No background subtraction": with
        # subtraction chosen the background is then unavailable
        shiny::observeEvent(input$background, {
            bg_roles(if (is.null(input$background)) character(0) else input$background)
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input$bg_mode, bg_mode(input$bg_mode), ignoreInit = TRUE)
        # No level ticked: the background wells alone
        shiny::observeEvent(input$bg_levels, {
            shown_levels(if (is.null(input$bg_levels)) character(0) else input$bg_levels)
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
        units_ok <- shiny::reactive(units_chosen(input$xunits))
        # The status of the background does not depend on the fit (see background_info below)
        lock <- shiny::reactive({
            bg <- tryCatch(background_info(), error = function(e) NULL)
            export_lock(units_ok(), bg$status)
        })
        output$save_plot_ui <- shiny::renderUI(export_button_ui(ns('save_plot'), t('curve.save_plot'), 'image',
                                                                is.null(lock())))
        observe_locked_export(input, 'save_plot', lang, lock)

        settings <- shiny::reactive({
            shiny::req(input$model)
            quadratic <- poly_degree(input$model) == 2
            subtract <- !identical(bg_mode(), 'none')
            list(model = input$model,
                 background = if (subtract) bg_roles() else character(0),
                 subtract = subtract,
                 at_blank = if (is.null(input$at_blank)) 'range' else input$at_blank,
                 weights = if (is.null(input$weights)) 'none' else input$weights,
                 interval = if (quadratic && isTRUE(input$use_interval)) {
                     as.numeric(c(input$interval_from, input$interval_to))
                 })
        })

        result <- shiny::reactive({
            d <- data()
            r <- rounding()
            s <- settings()
            o <- options()
            with_validation(
                calculate(d, model = s$model, weights = s$weights, interval = s$interval, background = s$background,
                          subtract_background = s$subtract, at_blank = s$at_blank, exclude = exclusions(),
                          exclude_background = bg_exclusions(),
                          round_fun = r$fun, round_par = r$par,
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

        # Common set of calibration wells: the data, the background, the exclusions by the user and the
        # rule for a signal at the background, not the model, so that it is there even when the chosen
        # model cannot be fitted (the same as calibration_pool of calculate())
        pool <- shiny::reactive({
            bg <- background_info()
            at_blank <- if (is.null(input$at_blank)) 'range' else input$at_blank
            calibration_pool(data(), bg$value, exclusions(), at_blank)
        })
        # All models on the common set of wells: the choice of the model does not change it. Without the
        # background chosen the comparison of the OD as measured would point to a model fitted to wrong data
        comparison <- shiny::reactive({
            shiny::validate(shiny::need(!isTRUE(background_info()$empty), t('compare.no_background')))
            compare_models(pool(), weights = if (is.null(input$weights)) 'none' else input$weights)
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
            # The unavailable background has its own box above the plots
            hidden <- c('note.not_quantified', 'note.lack_of_fit', 'quality.reason_background',
                        sample_warning_keys())
            shown <- Filter(function(m) !m$key %in% hidden, result()$notes)
            notes_ui(message_text(shown, lang), lang)
        })
        output$curve_stats <- shiny::renderUI({
            r <- result()
            shiny::div(class = 'curve-stats',
                       r2_ui(r$quality$curve$r2, lang),
                       mean_error_ui(r$fit, lang),
                       lof_ui(r$lack_of_fit$p, lang))
        })
        output$coefs <- shiny::renderTable({
            tab <- coef_table()
            tab[-1] <- lapply(tab[-1], as.character)
            tab
        })
        # The background does not depend on the fit: it is shown even if the curve cannot be fitted
        background_info <- shiny::reactive({
            d <- data()
            shiny::req(d, !is.null(bg_roles()))
            d$.key <- well_keys(d)
            subtract <- !identical(bg_mode(), 'none')
            roles <- if (subtract) bg_roles() else character(0)
            with_validation(background_summary(d, roles, bg_exclusions(), subtract), 'common.calc_error', lang)
        })
        # The background chosen has no usable well: said in the Background and the curve sub-tabs
        unavailable_ui <- function() {
            shiny::div(class = 'alert alert-danger background-unavailable', shiny::icon('circle-xmark'), ' ',
                       shiny::strong(t('quality.background_unavailable')), shiny::br(),
                       t('quality.background_unavailable_hint'))
        }
        output$background_alert <- shiny::renderUI({
            bg <- tryCatch(background_info(), error = function(e) NULL)
            if (isTRUE(bg$empty)) unavailable_ui()
        })
        # The standards as measured, for the scale of the scatter of the blanks
        raw_standards <- shiny::reactive({
            d <- data()
            d[!is.na(d$type) & d$type == 'standard', c('conc', 'OD'), drop = FALSE]
        })
        output$background_table <- shiny::renderTable(background_table(background_info(), lang))
        output$background_wells <- shiny::renderTable(background_wells_table(background_info(), lang))
        output$background_plot <- shiny::renderPlot({
            p <- plot_background(background_info(), raw_standards(), lang, shown = shown_levels())
            shiny::req(p)
            p
        })
        output$background_warning <- shiny::renderUI({
            bg <- background_info()
            if (isTRUE(bg$empty)) return(unavailable_ui())
            if (!isTRUE(bg$scatter$warn)) return(NULL)
            shiny::div(class = 'alert alert-warning', shiny::icon('triangle-exclamation'), ' ',
                       message_text(list(background_scatter_message(bg)), lang))
        })
        output$background_restore <- shiny::renderUI({
            if (nrow(bg_exclusions()) == 0) return(NULL)
            shiny::actionButton(ns('restore_background'), t('bg.restore_all'), icon = shiny::icon('undo'),
                                class = 'btn-sm mb-2')
        })
        shiny::observeEvent(input$restore_background, bg_exclusions(no_exclusions))
        output$background_notes <- shiny::renderUI({
            lines <- background_notes(background_info(), lang)
            if (length(lines) == 0) return(NULL)
            shiny::tagList(lapply(lines, function(x) shiny::p(class = 'text-muted small', x)))
        })
        output$verdict <- shiny::renderUI(verdict_ui(result(), lang, unit = sub('^, ', '', input$xunits)))
        output$equation <- shiny::renderUI(model_equation_ui(result()$fit))
        output$standards <- shiny::renderTable(quality_marked()$standards, na = '',
                                               sanitize.text.function = function(x) x)
        output$points <- shiny::renderTable(standard_points_table(result(), lang, highlight = TRUE), na = '',
                                            sanitize.text.function = function(x) x)
        output$bias <- shiny::renderPlot({
            p <- plot_bias(result(), xtitle = plot_args()$xtitle, lang = lang)
            shiny::req(p)
            p
        }, height = plot_height('bias'))
        output$wells_block <- shiny::renderUI({
            if (is.null(quality_marked()$wells)) return(NULL)
            shiny::tags$details(class = 'std-points std-wells',
                                shiny::tags$summary(t('quality.section_wells')),
                                shiny::tableOutput(ns('wells')))
        })
        output$wells <- shiny::renderTable(quality_marked()$wells, na = '', sanitize.text.function = function(x) x)
        # Rendered while the details are collapsed, so that they open at once
        shiny::outputOptions(output, 'points', suspendWhenHidden = FALSE)
        shiny::outputOptions(output, 'wells', suspendWhenHidden = FALSE)
        output$curve_quality <- shiny::renderTable(quality()$curve, na = '')
        output$compare <- shiny::renderTable(comparison_table(comparison(), lang), na = '')
        # Without the background the table alone says why there is no comparison
        output$compare_note <- shiny::renderUI({
            shiny::req(!isTRUE(background_info()$empty))
            note <- comparison_note(comparison(), lang)
            if (is.null(note)) return(NULL)
            shiny::div(class = 'alert alert-warning compare-note', shiny::icon('triangle-exclamation'), ' ', note)
        })
        # The row of the chosen model is shaded (renderTable gives no classes to the rows)
        output$compare_mark <- shiny::renderUI({
            shiny::req(!isTRUE(background_info()$empty))
            row <- match(settings()$model, comparison()$model)
            if (is.na(row)) return(NULL)
            shiny::tags$style(sprintf('#%s tbody tr:nth-child(%d) td { background-color: #e9ecef; }',
                                      ns('compare'), row))
        })

        # Exclusion of standard points --------------------------------------
        point_text <- function(p) {
            t('curve.point', well = p$key, conc = signif(p$conc, 3), od = signif(p$OD, 3))
        }

        # The same dialog excludes or restores a standard point or a background well
        store <- function(target) if (identical(target, 'background')) bg_exclusions else exclusions
        point_dialog <- function(text, reason = NULL, codes = exclusion_reasons(), section = 'curve') {
            if (!is.null(reason)) {
                return(shiny::showModal(shiny::modalDialog(
                    title = t(paste0(section, '.restore_title')),
                    shiny::p(text),
                    shiny::p(t('exclusion.reason'), ': ', reason),
                    footer = shiny::tagList(shiny::modalButton(t('curve.cancel')),
                                            shiny::actionButton(ns('confirm_restore'), t('curve.restore')))
                )))
            }
            shiny::showModal(shiny::modalDialog(
                title = t(paste0(section, '.exclude_title')),
                shiny::p(text),
                shiny::radioButtons(ns('reason_choice'), t('curve.reason'),
                                    choices = stats::setNames(c(codes, 'other'),
                                                              c(exclusion_reasons(lang)[codes], t('reason.other'))),
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

        # Standard points of the fit and those excluded by the user: a click on the curve or a double
        # click on a row of the individual points opens the same dialog
        standard_points <- function(r) {
            rbind(
                data.frame(key = r$standard_data$.key, conc = r$standard_data$conc,
                           OD = r$standard_data$OD, excluded = rep(FALSE, nrow(r$standard_data))),
                data.frame(key = r$exclusions$key, conc = r$exclusions$conc,
                           OD = r$exclusions$OD, excluded = rep(TRUE, nrow(r$exclusions)))
            )
        }
        open_standard <- function(p) {
            if (is.null(p) || nrow(p) == 0) return()
            p <- p[1, ]
            clicked(list(key = p$key, target = 'standard'))
            point_dialog(point_text(p), if (p$excluded) exclusions()$reason[exclusions()$key == p$key])
        }
        # Background wells: those subtracted and those excluded by the user, not those of a role left out
        open_background <- function(w) {
            if (is.null(w) || nrow(w) == 0) return()
            w <- w[1, ]
            clicked(list(key = w$key, target = 'background'))
            text <- t('bg.point', well = w$key, role = role_labels(w$role, lang), od = signif(w$OD, 3))
            point_dialog(text, if (w$kind == 'excluded') bg_exclusions()$reason[bg_exclusions()$key == w$key],
                         codes = background_exclusion_reasons(), section = 'bg')
        }
        clickable_background <- function() {
            pts <- background_points(background_info(), NULL, lang)
            pts[pts$kind %in% c('used', 'excluded'), , drop = FALSE]
        }

        shiny::observeEvent(input$curve_click, {
            open_standard(shiny::nearPoints(standard_points(result()), input$curve_click, xvar = 'conc',
                                            yvar = 'OD', threshold = 10, maxpoints = 1))
        })
        shiny::observeEvent(input$points_dblclick, {
            pts <- standard_points(result())
            open_standard(pts[pts$key %in% input$points_dblclick$key, , drop = FALSE])
        })

        shiny::observeEvent(input$background_click, {
            open_background(shiny::nearPoints(clickable_background(), input$background_click, xvar = 'x',
                                              yvar = 'OD', threshold = 10, maxpoints = 1))
        })
        shiny::observeEvent(input$background_dblclick, {
            pts <- clickable_background()
            open_background(pts[pts$key %in% input$background_dblclick$key, , drop = FALSE])
        })

        # The dialog acts on the well it was opened for; that well is forgotten once it is done
        shiny::observeEvent(input$confirm_exclude, {
            shiny::req(clicked())
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
            target <- store(clicked()$target)
            ex <- target()
            target(rbind(ex[ex$key != clicked()$key, ],
                         data.frame(key = clicked()$key, reason = reason)))
            clicked(NULL)
            shiny::removeModal()
        })

        shiny::observeEvent(input$confirm_restore, {
            shiny::req(clicked())
            target <- store(clicked()$target)
            ex <- target()
            target(ex[ex$key != clicked()$key, , drop = FALSE])
            clicked(NULL)
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
                shiny::req(is.null(lock()))
                ggplot2::ggsave(file, plots(), width = 20, height = 10, units = 'cm', dpi = 600)
            }
        )

        list(result = result,
             coef_table = coef_table,
             notes = notes,
             plot_args = plot_args,
             units_ok = units_ok,
             export_lock = lock,
             model = shiny::reactive(settings()$model),
             weights = shiny::reactive(settings()$weights),
             background = shiny::reactive(settings()$background),
             background_exclusions = bg_exclusions,
             exclusions = exclusions,
             comparison = comparison,
             effect_tables = effect_tables)
    })
}
