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


#' Decimals That Tell a Value From Its Limit
#'
#' A value shown next to a verdict must lie on the side of the limit that the
#' verdict says: a bias of 20.04 % fails a limit of 20 %, but shown with one
#' decimal it reads 20.0. One decimal is used by default; where the value so
#' rounded equals a threshold (`center - limit` or `center + limit`) while the
#' value itself does not, more decimals are used, up to `max_digits`, until
#' the shown value differs from the threshold.
#'
#' @param x Values.
#' @param limit Allowed deviation from `center` (recycled).
#' @param center Centre of the allowed interval (recycled): 0 for a bias or a
#'   CV, 100 for a recovery.
#' @param digits,max_digits Default and largest number of decimals.
#'
#' @return Integer vector of decimals, one per value.
#' @keywords internal
limit_digits <- function(x, limit, center = 0, digits = 1L, max_digits = 4L) {
    n <- length(x)
    out <- rep(as.integer(digits), n)
    if (n == 0 || length(limit) == 0) return(out)
    limit <- rep_len(limit, n)
    center <- rep_len(center, n)
    on_threshold <- function(i, d) {
        th <- center[i] + c(-1, 1) * limit[i]
        # Equal to the threshold within the tolerance of the criteria (see within_limit()): shown as equal
        any(abs(round(x[i], d) - th) < 1e-9 * max(1, abs(th))) &&
            !any(abs(x[i] - th) <= limit_tolerance(limit[i]))
    }
    for (i in which(!is.na(x) & !is.na(limit) & !is.na(center))) {
        while (out[i] < max_digits && on_threshold(i, out[i])) out[i] <- out[i] + 1L
    }
    out
}


#' Format Percentages Against Their Limit
#'
#' @inheritParams limit_digits
#'
#' @return Character vector: the values with the decimals of [limit_digits()]
#'   (one by default), `""` for `NA`.
#' @keywords internal
pct_limit <- function(x, limit, center = 0) {
    ifelse(is.na(x), '', sprintf('%.*f', limit_digits(x, limit, center), x))
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
#' @return A named list of data.frames (`standards`, `wells`,
#'   `wells_deviating`, `qc`, `samples`, `dilution`, `curve`); absent parts
#'   are `NULL`. The standards are sorted by decreasing concentration.
#'   `wells` has the wells of every standard level against the plan (planned,
#'   fitted, back-calculated and the lost wells with their reasons) and
#'   `wells_deviating` only its levels with fewer back-calculated wells than
#'   planned; both are `NULL` when no level deviates from the plan.
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
    wells <- NULL
    wells_deviating <- NULL
    st <- quality$standards
    if (!is.null(st)) {
        st <- st[order(st$conc, decreasing = TRUE), , drop = FALSE]
        # Values that cannot be computed (no points, one point for the CV) as dashes
        dash <- function(x) ifelse(x == '', '\u2014', x)
        bias_failed <- !within_limit(st$bias, st$bias.limit)
        cv_failed <- exceeds_limit(st$calc.cv, st$cv.limit)
        # A level planned in replicate without two back-calculated results: the CV is unavailable
        cv_text <- pct_limit(st$calc.cv, st$cv.limit)
        cv_text[cv_text == '' & st$n.planned >= 2 & st$n > 0] <- t('quality.cv_unavailable')
        standards <- name(data.frame(num(st$conc), dash(num(st$od.mean, 4)), dash(num(st$calc.mean)),
                                     fail(dash(cv_text), cv_failed),
                                     fail(dash(pct_limit(st$bias, st$bias.limit)), bias_failed),
                                     sprintf('%g', st$bias.limit),
                                     fail(standard_status(st$status, lang, bias_failed, cv_failed), !st$pass)),
                          c('col.conc', 'col.od_mean', 'col.calc', 'col.calc_cv', 'col.bias', 'col.limit',
                            'col.pass'))
        # The wells of the levels against the plan, apart: only when a level has fewer back-calculated
        # wells than planned
        deviates <- st$n.calc < st$n.planned
        if (any(deviates)) {
            lost <- if (is.null(st$lost)) rep('', nrow(st)) else vapply(st$lost, lost_wells_text, '', lang = lang)
            wells <- name(data.frame(num(st$conc), st$n.planned, st$n, st$n.calc,
                                     if (highlight) html_escape(lost) else lost),
                          c('col.conc', 'col.n_planned', 'col.n_fitted', 'col.n_calc', 'col.lost'))
            rownames(wells) <- NULL
            wells_deviating <- wells[deviates, , drop = FALSE]
            rownames(wells_deviating) <- NULL
        }
    }

    qc <- NULL
    if (!is.null(quality$qc)) {
        q <- quality$qc$levels
        qc <- name(data.frame(q$id, num(q$nominal), q$n, q$n.pass, num(q$mean), pct(q$cv),
                              pct_limit(q$bias, q$bias.limit),
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
                              cv = pct_limit(s$cv, s$cv.limit), flag = translate_flags(s$flag, lang))
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
                                    fail(pct_limit(d$deviation, criteria$dilution_bias),
                                         exceeds_limit(d$deviation, criteria$dilution_bias)),
                                    fail(pct_limit(d$cv, criteria$dilution_cv),
                                         exceeds_limit(d$cv, criteria$dilution_cv)),
                                    fail(mark(d$pass), !d$pass)),
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

    list(standards = standards, wells = wells, wells_deviating = wells_deviating, qc = qc, samples = samples,
         dilution = dilution, curve = curve)
}


#' Lost Wells of a Standard Level as Text
#'
#' @param lost The wells of a level without a back-calculated concentration
#'   (an element of `lost` of [assess_standards()]).
#' @param lang Language code.
#'
#' @return One string: `well: reason` for each well, joined by semicolons
#'   (the reason given by the user for an exclusion, the codes of
#'   [inverse_reason()] for a failed inverse); empty without lost wells.
#' @keywords internal
lost_wells_text <- function(lost, lang = 'en') {
    if (is.null(lost) || nrow(lost) == 0) return('')
    reason <- vapply(seq_len(nrow(lost)), function(i) {
        r <- lost$reason[i]
        if (identical(r, 'excluded')) {
            comment <- lost$comment[i]
            return(if (is.na(comment) || !nzchar(comment)) tr('lost.excluded_plain', lang) else
                tr('lost.excluded', lang, reason = comment))
        }
        if (r %in% c('missing', 'od_le_blank')) return(tr(paste0('lost.', r), lang))
        tr(paste0('backcalc.', r), lang)
    }, '')
    key <- as.character(lost$key)
    paste(ifelse(is.na(key), reason, paste0(key, ': ', reason)), collapse = '; ')
}


#' Status of a Standard Level for Display
#'
#' @param status Codes from [assess_standards()].
#' @param lang Language code.
#' @param bias_failed,cv_failed Logical vectors along `status`: the mean bias
#'   or the CV of the level exceeds its limit. A failed level (`"fail"`) is
#'   marked with the criterion it fails; without them, with the mark alone.
#'
#' @return Character vector: the mark of accepted levels, the mark and the
#'   failed criterion of failed levels, a short text for the other statuses.
#' @keywords internal
standard_status <- function(status, lang = 'en', bias_failed = NULL, cv_failed = NULL) {
    keys <- c(pass = 'quality.pass', fail = 'quality.fail', incomplete = 'quality.status_incomplete',
              pass_no_cv = 'quality.status_pass_no_cv', pass_singlet = 'quality.status_pass_singlet',
              singlet = 'quality.status_singlet', replicate_lost = 'quality.status_replicate_lost',
              no_points = 'quality.status_no_points',
              outside_interval = 'quality.status_outside_interval')
    out <- vapply(status, function(x) tr(keys[[x]], lang), '', USE.NAMES = FALSE)
    if (!is.null(bias_failed) && !is.null(cv_failed)) {
        by_bias <- status == 'fail' & bias_failed & !cv_failed
        by_cv <- status == 'fail' & cv_failed & !bias_failed
        by_both <- status == 'fail' & bias_failed & cv_failed
        out[by_bias] <- tr('quality.status_fail_bias', lang)
        out[by_cv] <- tr('quality.status_fail_cv', lang)
        out[by_both] <- tr('quality.status_fail_both', lang)
    }
    out
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
    title <- if (identical(q$background, 'unavailable')) 'quality.background_unavailable' else
        if (q$accepted) 'quality.accepted' else 'quality.rejected'
    lloq <- signif(result$calibration_range[1], 3)
    uloq <- signif(result$calibration_range[2], 3)
    # A rejected calibration has no quantification range: the range applied to the samples is not reportable
    range <- if (isTRUE(cal$accepted)) tr('quality.range', lang, lloq = lloq, uloq = uloq) else
        tr('quality.range_rejected', lang, lloq = lloq, uloq = uloq)
    # The rejection of the calibration is told by the line of the levels
    reasons <- Filter(function(m) !identical(m$key, 'quality.reason_calibration'), q$reasons)
    list(title = tr(title, lang),
         range = range,
         levels = tr('quality.levels', lang, n = cal$n_pass, total = cal$n_levels, needed = cal$n_needed),
         reasons = message_text(reasons, lang),
         warnings = message_text(warnings, lang))
}


#' Individual Standard Points for Display
#'
#' The standards used in the fit with their back-calculated concentrations,
#' and in their places the points excluded by the user (without a
#' back-calculated concentration, in grey for tables rendered as HTML; the
#' reason is in the wells deviating from the plan), sorted by decreasing
#' concentration, then in the order of the plate: an exclusion does not move a
#' row. A point deviates from the acceptance criteria if its bias exceeds the
#' bias limit of its level in the standards table (`std_bias`, `std_bias_edge`
#' at the boundary calibrators, see [assess_standards()]).
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#' @param highlight If `TRUE`, the biases of deviating points are wrapped in
#'   `<span class="crit-fail">`, for tables rendered as HTML.
#'
#' @return A data.frame with translated column names: level name, position,
#'   nominal concentration, OD as measured and after the background
#'   correction, back-calculated concentration, bias (%) and
#'   absolute error (back-calculated minus nominal); when some points have a
#'   signal at or below the background, a column marking them.
#' @keywords internal
standard_points_table <- function(result, lang = 'en', highlight = FALSE) {
    std <- result$standard_data
    ex <- result$exclusions
    if (is.null(ex)) ex <- data.frame(key = character(0), conc = numeric(0), OD = numeric(0), reason = character(0))
    # Three significant digits without scientific notation (small errors near 0)
    num <- function(x) ifelse(is.na(x), '', trimws(formatC(x, digits = 3, format = 'fg')))
    raw <- if ('OD.raw' %in% names(std)) std$OD.raw else std$OD + result$nc_value
    # The points of the fit, then those excluded by the user; sorted below
    pts <- data.frame(key = c(as.character(std$.key), as.character(ex$key)), conc = c(std$conc, ex$conc),
                      OD = c(std$OD, ex$OD), raw = c(raw, ex$OD + result$nc_value),
                      excluded = c(rep(FALSE, nrow(std)), rep(TRUE, nrow(ex))),
                      comment = c(rep(NA_character_, nrow(std)), as.character(ex$reason)))
    calc <- ifelse(pts$excluded, NA_real_, inverse_fun(result$fit, pts$OD))
    bias <- 100 * (calc - pts$conc) / pts$conc
    # The limit of the level, as in the standards table (see assess_standards())
    criteria <- if (is.null(result$quality$criteria)) qc_criteria() else result$quality$criteria
    st <- result$quality$standards
    limit <- if (is.null(st)) {
        ifelse(pts$conc %in% range(std$conc), criteria$std_bias_edge, criteria$std_bias)
    } else {
        st$bias.limit[match(pts$conc, st$conc)]
    }
    bias_text <- pct_limit(bias, limit)
    if (highlight) {
        failed <- !pts$excluded & !within_limit(bias, limit)
        bias_text <- ifelse(failed, paste0('<span class="crit-fail">', bias_text, '</span>'), bias_text)
    }
    # The IDs of the levels of the whole plan, as in every other table
    level_map <- if (is.null(result$standard_levels)) pts$conc else result$standard_levels
    out <- data.frame(standard = standard_names(pts$conc, level_map), key = pts$key, conc = num(pts$conc),
                      raw = sprintf('%.4f', pts$raw), OD = sprintf('%.4f', pts$OD), calc = num(calc),
                      bias = bias_text, error = num(calc - pts$conc))
    cols <- c('exclusion.standard', 'exclusion.key', 'col.nominal', 'col.od_raw', 'col.od_corrected', 'col.calc',
              'col.bias', 'col.error')
    # The signal at or below the background, as for the samples and the quality controls
    at_blank <- !is.na(pts$OD) & pts$OD <= 0
    if (any(at_blank)) {
        out$at_blank <- ifelse(at_blank, '\u2713', '')
        cols <- c(cols, 'col.od_le_blank')
    }
    # The points excluded by the user in grey; why a point has no back-calculated concentration is told
    # in the wells deviating from the plan
    if (highlight && any(pts$excluded)) {
        grey <- function(x) ifelse(pts$excluded & nzchar(x), paste0('<span class="pt-excluded">', x, '</span>'), x)
        out[] <- lapply(out, grey)
    }
    # By decreasing concentration, then in the order of the plate
    plate <- match(pts$key, result$standard_keys)
    out <- out[order(-pts$conc, plate, seq_len(nrow(pts))), , drop = FALSE]
    rownames(out) <- NULL
    names(out) <- vapply(cols, tr, '', lang = lang)
    out
}


#' Box of an Unavailable Background
#'
#' Background subtraction was chosen, but no well of the chosen roles is
#' usable: the verdict and the concentrations are suspended until a well is
#' restored or "no background subtraction" is chosen.
#'
#' @param lang Language code.
#'
#' @return A shiny tag.
#' @keywords internal
background_unavailable_ui <- function(lang) {
    shiny::div(class = 'quality-verdict background-unavailable',
               shiny::div(class = 'alert alert-danger',
                          shiny::h4(shiny::icon('circle-xmark'), ' ', tr('quality.background_unavailable', lang)),
                          shiny::p(tr('quality.background_unavailable_hint', lang))))
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
            # The presets keep the boundary calibrators of the plate plan; a method may set those of the fit
            shiny::conditionalPanel(
                "input.preset == 'custom'", ns = ns,
                shiny::radioButtons(ns('edge_levels'), with_tooltip(t('crit.edge_levels'), t('crit.edge_levels_help')),
                                    choices = stats::setNames(edge_level_rules(),
                                                              vapply(paste0('crit.edge_levels_', edge_level_rules()),
                                                                     t, '')),
                                    selected = 'plan', width = '100%')
            ),
            shiny::checkboxInput(ns('singlets'), with_tooltip(t('crit.singlets'), t('crit.singlets_help')),
                                 value = TRUE, width = '100%'),
            # A level planned in replicate that keeps one result: a rule of its own, not a singlet
            shiny::radioButtons(ns('replicate_loss'), with_tooltip(t('crit.replicate_loss'),
                                                                    t('crit.replicate_loss_help')),
                                choices = stats::setNames(replicate_loss_rules(),
                                                          vapply(paste0('crit.replicate_loss_', replicate_loss_rules()),
                                                                 t, '')),
                                selected = 'documented', width = '100%'),
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
            shiny::updateCheckboxInput(session, 'singlets', value = TRUE)
            shiny::updateRadioButtons(session, 'replicate_loss', selected = 'documented')
            shiny::updateRadioButtons(session, 'edge_levels', selected = 'plan')
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
            values$singlets <- if (is.null(input$singlets)) TRUE else isTRUE(input$singlets)
            values$replicate_loss <- if (is.null(input$replicate_loss)) 'documented' else input$replicate_loss
            # The rule of the fit only in the custom criteria, when chosen
            custom <- identical(input$preset, 'custom')
            values$edge_levels <- if (custom && !is.null(input$edge_levels)) input$edge_levels else 'plan'
            with_validation(do.call(qc_criteria, values), 'common.calc_error', lang)
        })
    })
}
