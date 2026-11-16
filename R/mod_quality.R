#' Quality Tables for Display
#'
#' Formats the parts of a quality assessment (see [assess_run()]) as tables
#' with translated column names, for the app and the report.
#'
#' @param quality The `quality` element of [calculate()], or a list with the
#'   same elements (e.g. `samples` and `dilution` from [assess_samples()]).
#' @param lang Language code.
#'
#' @return A named list of data.frames (`standards`, `qc`, `samples`,
#'   `dilution`, `curve`); absent parts are `NULL`.
#' @export
quality_tables <- function(quality, lang = 'en') {
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
    as_text <- function(df) {
        df[] <- lapply(df, function(x) ifelse(is.na(x), '', as.character(x)))
        df
    }

    standards <- NULL
    st <- quality$standards
    if (!is.null(st)) {
        standards <- name(data.frame(num(st$conc), st$n, num(st$od.mean, 4), pct(st$od.cv), num(st$calc.mean),
                                     pct(st$bias), sprintf('%g', st$bias.limit), mark(st$pass)),
                          c('col.conc', 'col.count', 'col.od_mean', 'col.od_cv', 'col.calc', 'col.bias',
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
        id_cols <- setdiff(names(d), c('dilution', 'mean', 'deviation', 'cv', 'pass'))
        dilution <- name(data.frame(as_text(d[id_cols]), as.character(d$dilution), num(d$mean), pct(d$deviation),
                                    pct(d$cv), mark(d$pass)),
                         c(id_keys(id_cols), 'col.dilution', 'col.mean', 'col.deviation', 'col.cv_dil', 'col.pass'))
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


#' Verdict of the Run
#'
#' A green or red box with the verdict, the quantification range, the number
#' of accepted standard levels and the reasons of a rejection.
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#'
#' @return A shiny tag.
#' @keywords internal
verdict_ui <- function(result, lang) {
    s <- quality_summary(result, lang)
    accepted <- result$quality$accepted
    shiny::div(class = 'quality-verdict',
               shiny::div(class = if (accepted) 'alert alert-success' else 'alert alert-danger',
                          shiny::h4(shiny::icon(if (accepted) 'circle-check' else 'circle-xmark'), ' ', s$title),
                          shiny::p(s$range), shiny::p(s$levels),
                          if (length(s$reasons) > 0) shiny::tags$ul(lapply(s$reasons, shiny::tags$li))))
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
    title <- function(key) with_tooltip(t(key), t('crit.note'))

    if (part == 'standards') {
        return(sidebar_group(
            title('crit.title_standards'), 'vial',
            inputs('standards'),
            shiny::radioButtons(ns('range'), t('crit.range'),
                                choices = stats::setNames(c('accepted', 'all'),
                                                          c(t('crit.range_accepted'), t('crit.range_all'))),
                                selected = 'accepted'),
            reset
        ))
    }
    sidebar_group(
        title('crit.title_samples'), 'clipboard-check',
        shiny::div(class = 'crit-subtitle', t('crit.group_replicates')), inputs('replicates'),
        shiny::div(class = 'crit-subtitle', t('crit.group_qc')), inputs('qc'),
        shiny::div(class = 'crit-subtitle', t('crit.group_dilution')), inputs('dilution'),
        reset
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

        shiny::reactive({
            values <- lapply(seq_len(nrow(fields)), function(i) {
                v <- input[[fields$name[i]]]
                if (is.null(v)) fields$default[i] / fields$scale[i] else v / fields$scale[i]
            })
            names(values) <- fields$name
            values$range <- if (is.null(input$range)) 'accepted' else input$range
            with_validation(do.call(qc_criteria, values), 'common.calc_error', lang)
        })
    })
}
