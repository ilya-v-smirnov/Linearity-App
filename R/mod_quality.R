#' Escape Text for HTML
#'
#' @param x Character vector.
#'
#' @return `x` with `&`, `<`, `>` and quotes replaced by HTML entities.
#' @keywords internal
html_escape <- function(x) {
    x <- gsub('&', '&amp;', x, fixed = TRUE)
    x <- gsub('<', '&lt;', x, fixed = TRUE)
    x <- gsub('>', '&gt;', x, fixed = TRUE)
    x <- gsub('"', '&quot;', x, fixed = TRUE)
    gsub("'", '&#39;', x, fixed = TRUE)
}


#' Quality Tables for Display
#'
#' Formats the parts of a quality assessment (see [assess_run()]) as tables
#' with translated column names, for the app and the report.
#'
#' @param quality The `quality` element of [calculate()], or a list with the
#'   same elements (e.g. `samples` and `dilution` from [assess_samples()]).
#' @param lang Language code.
#' @param highlight If `TRUE`, the values failing the acceptance criteria are
#'   wrapped in `<span class="crit-fail">` and the texts from the data are
#'   escaped, for tables rendered as HTML: the bias, CV and verdict of the
#'   standards, the deviation from the mean, the CV between dilutions and the
#'   verdict of the dilutions (limits from `quality$criteria`, by default
#'   those of [qc_criteria()]).
#'
#' @return A named list of data.frames (`standards`, `qc`, `samples`,
#'   `dilution`, `curve`); absent parts are `NULL`. The standards are sorted
#'   by decreasing concentration.
#' @export
quality_tables <- function(quality, lang = 'en', highlight = FALSE) {
    t <- function(key) tr(key, lang)
    num <- function(x, digits = 3) ifelse(is.na(x), '', as.character(signif(x, digits)))
    pct <- function(x) ifelse(is.na(x), '', sprintf('%.1f', x))
    mark <- function(x) ifelse(x, t('quality.pass'), t('quality.fail'))
    name <- function(df, keys) stats::setNames(df, vapply(keys, t, ''))
    # Identification columns keep their order; known ones are translated
    id_keys <- function(cols) {
        known <- c(id = 'col.id', sample.name = 'col.sample', dilution = 'col.dilution', sample.date = 'col.date')
        ifelse(cols %in% names(known), known[cols], cols)
    }
    # Tables rendered as HTML: the texts from the data are escaped
    as_text <- function(df) {
        df[] <- lapply(df, function(x) {
            x <- ifelse(is.na(x), '', as.character(x))
            if (highlight) html_escape(x) else x
        })
        df
    }
    # Values failing the criteria are wrapped in a span for display as HTML
    fail <- function(x, failed) {
        if (highlight) ifelse(!is.na(failed) & failed, paste0('<span class="crit-fail">', x, '</span>'), x) else x
    }
    criteria <- if (is.null(quality$criteria)) qc_criteria() else quality$criteria

    standards <- NULL
    st <- quality$standards
    if (!is.null(st)) {
        st <- st[order(st$conc, decreasing = TRUE), , drop = FALSE]
        # Values that cannot be computed (no points, one point for the CV) as dashes
        dash <- function(x) ifelse(x == '', '\u2014', x)
        bias_failed <- is.na(st$bias) | abs(st$bias) > st$bias.limit
        cv_failed <- !is.na(st$calc.cv) & st$calc.cv > st$cv.limit
        standards <- name(data.frame(num(st$conc), st$n, dash(num(st$od.mean, 4)), dash(num(st$calc.mean)),
                                     fail(dash(pct(st$calc.cv)), cv_failed), fail(dash(pct(st$bias)), bias_failed),
                                     sprintf('%g', st$bias.limit), fail(mark(st$pass), !st$pass)),
                          c('col.conc', 'col.count', 'col.od_mean', 'col.calc', 'col.calc_cv', 'col.bias',
                            'col.limit', 'col.pass'))
    }

    qc <- NULL
    if (!is.null(quality$qc)) {
        q <- quality$qc$levels
        qc <- name(data.frame(q$id, num(q$nominal), q$n, q$n.pass, num(q$mean), pct(q$cv), pct(q$bias),
                              sprintf('%g', q$bias.limit), mark(q$pass)),
                   c('col.id', 'col.nominal', 'col.count', 'col.n_pass', 'col.mean', 'col.cv', 'col.bias',
                     'col.limit', 'col.pass'))
    }

    samples <- NULL
    if (!is.null(quality$samples)) {
        s <- quality$samples
        id_cols <- setdiff(names(s), c('n', 'n.quantified', 'mean', 'sd', 'cv', 'min', 'max', 'cv.limit', 'flag'))
        samples <- data.frame(as_text(s[id_cols]),
                              n = s$n, n.quantified = s$n.quantified, mean = num(s$mean), sd = num(s$sd),
                              cv = pct(s$cv), flag = translate_flags(s$flag, lang))
        samples <- name(samples, c(id_keys(id_cols), 'col.count', 'col.n_quant', 'col.mean', 'col.sd', 'col.cv',
                                   'col.flag'))
    }

    dilution <- NULL
    if (!is.null(quality$dilution)) {
        d <- quality$dilution
        id_cols <- setdiff(names(d), dilution_value_columns())
        counts <- function(col) if (col %in% names(d)) d[[col]] else rep(NA_integer_, nrow(d))
        dilution <- name(data.frame(as_text(d[id_cols]), as.character(d$dilution),
                                    counts('n'), counts('n.quantified'), num(d$mean),
                                    fail(pct(d$deviation), abs(d$deviation) > criteria$dilution_bias),
                                    fail(pct(d$cv), d$cv > criteria$dilution_cv), fail(mark(d$pass), !d$pass)),
                         c(id_keys(id_cols), 'col.dilution', 'col.count', 'col.n_quant', 'col.mean', 'col.deviation',
                           'col.cv_dil', 'col.pass'))
    }

    curve <- NULL
    cv <- quality$curve
    if (!is.null(cv)) {
        curve <- name(data.frame(c(t('quality.r2'), t('quality.sigma'), t('quality.lof')),
                                 c(sprintf('%.4f', cv$r2), num(cv$sigma),
                                   if (is.na(cv$lof_p)) '' else num(cv$lof_p, 2))),
                      c('quality.parameter', 'quality.value'))
    }

    list(standards = standards, qc = qc, samples = samples, dilution = dilution, curve = curve)
}


#' Verdict Lines of a Quality Assessment
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#' @param sample_warnings Warnings of the samples (list of messages, see
#'   [assess_samples()]) that replace those of the default grouping; `NULL`
#'   keeps the warnings of [calculate()].
#'
#' @return A list with `title`, `range`, `levels`, `reasons` and `warnings`
#'   (character).
#' @keywords internal
quality_summary <- function(result, lang = 'en', sample_warnings = NULL) {
    q <- result$quality
    cal <- q$calibration
    warnings <- q$warnings
    if (!is.null(sample_warnings)) {
        warnings <- c(Filter(function(m) !m$key %in% sample_warning_keys(), warnings), sample_warnings)
    }
    list(title = tr(if (q$accepted) 'quality.accepted' else 'quality.rejected', lang),
         range = tr('quality.range', lang, lloq = signif(result$calibration_range[1], 3),
                    uloq = signif(result$calibration_range[2], 3)),
         levels = tr('quality.levels', lang, n = cal$n_pass, total = cal$n_levels, needed = cal$n_needed),
         reasons = message_text(q$reasons, lang),
         warnings = message_text(warnings, lang))
}


#' Individual Standard Points for Display
#'
#' The standards used in the fit with their back-calculated concentrations,
#' sorted by decreasing concentration. A point deviates from the acceptance
#' criteria if its bias exceeds `std_bias` (`std_bias_edge` at the lowest and
#' highest level of the fit, see [qc_criteria()]).
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#' @param highlight If `TRUE`, the biases of deviating points are wrapped in
#'   `<span class="crit-fail">`, for tables rendered as HTML.
#'
#' @return A data.frame with translated column names: level name, position,
#'   nominal concentration, OD, back-calculated concentration, bias (%) and
#'   absolute error (back-calculated minus nominal).
#' @keywords internal
standard_points_table <- function(result, lang = 'en', highlight = FALSE) {
    std <- result$standard_data
    # Three significant digits without scientific notation (small errors near 0)
    num <- function(x) ifelse(is.na(x), '', trimws(formatC(x, digits = 3, format = 'fg')))
    calc <- inverse_fun(result$fit, std$OD)
    levels <- c(std$conc, result$exclusions$conc)
    bias <- 100 * (calc - std$conc) / std$conc
    criteria <- if (is.null(result$quality$criteria)) qc_criteria() else result$quality$criteria
    limit <- ifelse(std$conc %in% range(std$conc), criteria$std_bias_edge, criteria$std_bias)
    bias_text <- ifelse(is.na(bias), '', sprintf('%.1f', bias))
    if (highlight) {
        failed <- is.na(bias) | abs(bias) > limit
        bias_text <- ifelse(failed, paste0('<span class="crit-fail">', bias_text, '</span>'), bias_text)
    }
    out <- data.frame(standard = standard_names(std$conc, levels), key = std$.key, conc = num(std$conc),
                      OD = sprintf('%.4f', std$OD), calc = num(calc), bias = bias_text,
                      error = num(calc - std$conc))
    out <- out[order(std$conc, decreasing = TRUE), , drop = FALSE]
    rownames(out) <- NULL
    names(out) <- vapply(c('exclusion.standard', 'exclusion.key', 'col.nominal', 'exclusion.od', 'col.calc',
                           'col.bias', 'col.error'), tr, '', lang = lang)
    out
}


#' Verdict of the Calibration
#'
#' A green, orange or red box with the acceptance of the calibration (the
#' standards only; the quality controls are assessed in the result summary),
#' the accepted calibration range, the number of accepted standard levels and
#' the reason of a rejection. Orange: accepted, but the CV is assessed for
#' fewer accepted levels than required (levels without replicates).
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#' @param unit Concentration unit appended to the range (e.g. `"nM"`); empty
#'   for none.
#'
#' @return A shiny tag.
#' @keywords internal
verdict_ui <- function(result, lang, unit = '') {
    cal <- result$quality$calibration
    accepted <- isTRUE(cal$accepted)
    no_cv <- accepted && identical(cal$status, 'accepted_no_cv')
    range <- tr('quality.calibration_range', lang, lloq = signif(result$calibration_range[1], 3),
                uloq = signif(result$calibration_range[2], 3),
                unit = if (nzchar(unit)) paste0(' ', unit) else '')
    levels <- tr('quality.levels', lang, n = cal$n_pass, total = cal$n_levels, needed = cal$n_needed)
    reason <- if (!accepted) {
        tr('quality.reason_calibration', lang, n = cal$n_pass, total = cal$n_levels, needed = cal$n_needed)
    }
    if (no_cv) {
        reason <- tr('quality.reason_no_cv', lang, n_cv = cal$n_cv, needed = cal$n_cv_needed, n = cal$n_no_cv)
    }
    key <- if (no_cv) 'calibration_accepted_no_cv' else if (accepted) 'calibration_accepted' else
        'calibration_rejected'
    style <- if (no_cv) 'alert-warning' else if (accepted) 'alert-success' else 'alert-danger'
    icon <- if (no_cv) 'triangle-exclamation' else if (accepted) 'circle-check' else 'circle-xmark'
    shiny::div(class = 'quality-verdict',
               shiny::div(class = paste('alert', style),
                          shiny::h4(shiny::icon(icon), ' ', tr(paste0('quality.', key), lang)),
                          shiny::p(range), shiny::p(levels),
                          if (!is.null(reason)) shiny::tags$ul(shiny::tags$li(reason))))
}


#' Criteria Inputs: Codes, Defaults and Scaling
#'
#' The criteria of the standards are set in the standard curve step, those of
#' the samples, quality controls and dilutions in the result summary step.
#' @keywords internal
criteria_fields <- function() {
    d <- qc_criteria()
    fields <- data.frame(
        name = c('std_bias', 'std_bias_edge', 'cv', 'cv_edge', 'min_fraction', 'min_levels',
                 'sample_cv', 'sample_cv_edge',
                 'qc_bias', 'qc_bias_edge', 'qc_fraction', 'qc_level_fraction', 'dilution_bias', 'dilution_cv'),
        scale = c(1, 1, 1, 1, 100, 1, 1, 1, 1, 1, 100, 100, 1, 1),
        group = c(rep('standards', 6), 'replicates', 'replicates', rep('qc', 4), 'dilution', 'dilution')
    )
    fields$part <- ifelse(fields$group == 'standards', 'standards', 'samples')
    fields$default <- vapply(fields$name, function(n) d[[n]], 0) * fields$scale
    fields
}


#' Acceptance Criteria Module
#'
#' Inputs for the acceptance criteria. The inputs are split in two parts shown
#' in different steps (`part = "standards"` in the standard curve step,
#' `part = "samples"` in the result summary step) and read by one server.
#'
#' The criteria of each part come from a preset (see [criteria_presets()]):
#' the fixed presets show their values, the custom one shows inputs starting
#' from the values of ICH M10. The preset of the samples follows the one
#' chosen for the standards and can then be changed on its own.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param part `"standards"` or `"samples"`.
#'
#' @return `mod_criteria_server()` returns a reactive with a `lin_criteria` object.
#' @name mod_criteria
#' @keywords internal
NULL

#' @rdname mod_criteria
mod_criteria_ui <- function(id, lang, part = c('standards', 'samples')) {
    part <- match.arg(part)
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    fields <- criteria_fields()
    # One criterion per row: a short label on the left, the value on the right
    inputs <- function(group) {
        f <- fields[fields$group == group, ]
        lapply(seq_len(nrow(f)), function(i) {
            id <- ns(f$name[i])
            shiny::div(class = 'crit-row',
                       shiny::tags$label(`for` = id, t(paste0('crit.', f$name[i]))),
                       shiny::numericInput(id, NULL, value = round(f$default[i], 1), min = 0, step = 1))
        })
    }
    reset <- shiny::actionButton(ns(paste0('reset_', part)), t('crit.reset'), icon = shiny::icon('rotate-left'),
                                 class = 'btn-sm btn-outline-secondary w-100')
    title <- function(key, note) with_tooltip(t(key), t(note))

    presets <- criteria_presets(part)
    # Fixed presets: the values as text, in the layout of the inputs
    values <- function(code, group) {
        f <- fields[fields$group == group, ]
        lapply(seq_len(nrow(f)), function(i) {
            shiny::div(class = 'crit-row',
                       shiny::tags$span(class = 'crit-label', t(paste0('crit.', f$name[i]))),
                       shiny::tags$span(class = 'crit-value',
                                        format(round(presets[[code]][[f$name[i]]] * f$scale[i], 1))))
        })
    }
    # The selector of the preset and a panel for each preset; the custom one holds the inputs
    preset_panels <- function(input_id, content) {
        shown <- function(code, ...) shiny::conditionalPanel(sprintf("input.%s == '%s'", input_id, code), ns = ns, ...)
        shiny::tagList(
            shiny::selectInput(ns(input_id), t('crit.preset'), width = '100%',
                               choices = stats::setNames(names(presets),
                                                         vapply(paste0('crit.preset_', names(presets)), t, '')),
                               selected = 'ich_m10'),
            shown('ich_m10', content(function(group) values('ich_m10', group))),
            shown('colorimetric', content(function(group) values('colorimetric', group))),
            shown('custom', content(inputs), reset)
        )
    }

    if (part == 'standards') {
        return(sidebar_group(
            title('crit.title_standards', 'crit.note_standards'), 'vial',
            preset_panels('preset', function(rows) rows('standards')),
            shiny::radioButtons(ns('range'), t('crit.range'),
                                choices = stats::setNames(c('accepted', 'all'),
                                                          c(t('crit.range_accepted'), t('crit.range_all'))),
                                selected = 'accepted')
        ))
    }
    sidebar_group(
        title('crit.title_samples', 'crit.note_samples'), 'clipboard-check',
        preset_panels('preset_samples', function(rows) {
            shiny::tagList(
                shiny::div(class = 'crit-subtitle', t('crit.group_replicates')), rows('replicates'),
                shiny::div(class = 'crit-subtitle', t('crit.group_qc')), rows('qc'),
                shiny::div(class = 'crit-subtitle', t('crit.group_dilution')), rows('dilution')
            )
        })
    )
}

#' @rdname mod_criteria
mod_criteria_server <- function(id, lang) {
    shiny::moduleServer(id, function(input, output, session) {
        fields <- criteria_fields()

        reset <- function(part) {
            f <- fields[fields$part == part, ]
            for (i in seq_len(nrow(f))) {
                shiny::updateNumericInput(session, f$name[i], value = round(f$default[i], 1))
            }
        }
        shiny::observeEvent(input$reset_standards, {
            reset('standards')
            shiny::updateRadioButtons(session, 'range', selected = 'accepted')
        })
        shiny::observeEvent(input$reset_samples, reset('samples'))
        # The criteria of the samples follow the choice of the standards; they can be changed after it
        shiny::observeEvent(input$preset, {
            shiny::updateSelectInput(session, 'preset_samples', selected = input$preset)
        }, ignoreInit = TRUE)

        shiny::reactive({
            values <- lapply(seq_len(nrow(fields)), function(i) {
                v <- input[[fields$name[i]]]
                if (is.null(v)) fields$default[i] / fields$scale[i] else v / fields$scale[i]
            })
            names(values) <- fields$name
            # The criteria of a fixed preset replace the inputs
            for (part in c('standards', 'samples')) {
                preset <- input[[if (part == 'standards') 'preset' else 'preset_samples']]
                if (is.null(preset)) preset <- 'ich_m10'
                fixed <- criteria_presets(part)[[preset]]
                if (preset != 'custom') values[names(fixed)] <- fixed
            }
            values$range <- if (is.null(input$range)) 'accepted' else input$range
            with_validation(do.call(qc_criteria, values), 'common.calc_error', lang)
        })
    })
}
