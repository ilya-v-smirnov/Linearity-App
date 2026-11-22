#' Counts of Sample Wells by Quantification Status
#'
#' @param flags Flags of the sample wells (see [FLAGS]); combined flags
#'   (`"High CV; OK"`) are not expected here.
#'
#' @return A named integer vector in the display order: `od_le_nc`, `below`,
#'   `ok`, `above`, `other` (no solution, missing OD).
#' @keywords internal
quantification_counts <- function(flags) {
    flags <- flags[!is.na(flags)]
    c(od_le_nc = sum(flags == FLAGS$od_le_nc),
      below = sum(flags == FLAGS$below),
      ok = sum(flags == FLAGS$ok),
      above = sum(flags == FLAGS$above),
      other = sum(!flags %in% c(FLAGS$od_le_nc, FLAGS$below, FLAGS$ok, FLAGS$above)))
}


#' Infographic of Quantified Samples
#'
#' Shown on the infographics tab of the result summary step.
#'
#' A horizontal bar of the sample wells: below the background (red) and below
#' the LLOQ (orange) on the left, quantified (green) in the middle, above the
#' ULOQ (blue) and other problems (grey) on the right: the colours of the
#' infographic of the Results step (see [flags_ui()]).
#'
#' @param flags Flags of the sample wells.
#' @param lang Language code.
#' @param help Text of the tooltip of the title, or `NULL` for none.
#'
#' @return A shiny tag, or `NULL` when there are no sample wells.
#' @keywords internal
quantification_ui <- function(flags, lang = 'en', help = NULL) {
    counts <- quantification_counts(flags)
    total <- sum(counts)
    if (total == 0) return(NULL)
    labels <- c(od_le_nc = tr('flag.od_le_nc', lang), below = tr('flag.below', lang),
                ok = tr('infographic.quantified', lang), above = tr('flag.above', lang),
                other = tr('infographic.other', lang))
    shown <- names(counts)[counts > 0]
    segments <- lapply(shown, function(k) {
        shiny::div(class = paste0('quant-segment quant-', k), style = sprintf('flex-grow: %d;', counts[[k]]),
                   title = paste0(labels[[k]], ': ', counts[[k]]), counts[[k]])
    })
    legend <- lapply(shown, function(k) {
        shiny::span(class = 'quant-legend-item',
                    shiny::span(class = paste0('quant-swatch quant-', k)),
                    paste0(labels[[k]], ' (', counts[[k]], ')'))
    })
    title <- tr('infographic.samples', lang, n = counts[['ok']], total = total)
    shiny::div(
        class = 'infographic',
        shiny::div(class = 'infographic-title', if (is.null(help)) title else with_tooltip(title, help)),
        shiny::div(class = 'quant-bar', segments),
        shiny::div(class = 'quant-legend', legend)
    )
}


#' Infographic of the Flags of the Sample Wells
#'
#' Shown above the result table: a bar of the sample wells split by flag, in
#' the order of the concentration (OD at or below the blank, below the lowest
#' calibrator, quantified, above the highest calibrator, then the wells
#' without a solution or OD), and a tile with the number and share of the
#' wells of each flag. Above the highest calibrator is blue, below the
#' lowest one orange, as in the result table. The tiles of the four main flags are always shown, the
#' others only when they occur.
#'
#' The flags of the wells depend on the OD and the calibration range only, not
#' on the extrapolation or the zero concentration of the out-of-range samples
#' (see [calculate()]), so the infographic does not change with these options.
#'
#' @param flags Flag codes of the sample wells (see [FLAGS]).
#' @param lang Language code.
#'
#' @return A shiny tag, or `NULL` when there are no sample wells.
#' @keywords internal
flags_ui <- function(flags, lang = 'en') {
    flags <- flags[!is.na(flags)]
    total <- length(flags)
    if (total == 0) return(NULL)
    known <- c('od_le_nc', 'below', 'ok', 'above', 'no_solution', 'missing')
    codes <- c(unlist(FLAGS[known]), setdiff(unique(flags), unlist(FLAGS[known])))
    keys <- c(known, setdiff(codes, unlist(FLAGS[known])))
    counts <- stats::setNames(vapply(codes, function(f) sum(flags == f), 0L), keys)
    # The short flags of the table are spelled out on the tiles
    label <- function(k) {
        if (k %in% c('below', 'above')) tr(paste0('results.flag_', k), lang)
        else if (k %in% known) tr(paste0('flag.', k), lang) else translate_flags(k, lang)
    }
    colour <- function(k) if (k %in% known) k else 'other'

    shown <- keys[counts > 0]
    segments <- lapply(shown, function(k) {
        shiny::div(class = paste0('quant-segment flag-', colour(k)), style = sprintf('flex-grow: %d;', counts[[k]]),
                   title = paste0(label(k), ': ', counts[[k]]), counts[[k]])
    })
    tiled <- keys[counts > 0 | keys %in% c('od_le_nc', 'below', 'ok', 'above')]
    tiles <- lapply(tiled, function(k) {
        shiny::div(class = paste0('stat-tile flag-tile flag-', colour(k)),
                   shiny::div(class = 'stat-value', counts[[k]],
                              shiny::span(class = 'stat-share', sprintf('%.0f%%', 100 * counts[[k]] / total))),
                   shiny::div(class = 'stat-label', label(k)))
    })
    shiny::div(
        class = 'infographic flag-infographic',
        shiny::div(class = 'infographic-title',
                   with_tooltip(tr('results.flags_title', lang, total = total), tr('results.flags_help', lang))),
        shiny::div(class = 'quant-bar flag-bar', segments),
        shiny::div(class = 'stat-tiles flag-tiles', tiles)
    )
}


#' Statistic of the Standard Curve as Large Text
#'
#' A line such as `R2 = 0.9942`, the value coloured by its zone: `poor`
#' (red), `fair` (orange) or `good` (green); `NA` leaves it uncoloured. The
#' verdict is shown as a tooltip.
#'
#' @param label Label before the value (tags or text).
#' @param value Formatted value.
#' @param zone Zone of the value: `"poor"`, `"fair"`, `"good"` or `NA`.
#' @param verdict Text of the tooltip, or `NULL`.
#'
#' @return A shiny tag.
#' @keywords internal
curve_stat_ui <- function(label, value, zone = NA, verdict = NULL) {
    shiny::div(class = 'curve-stat', title = verdict,
               label, shiny::strong(class = if (!is.na(zone)) paste0('zone-', zone), value, .noWS = 'outside'))
}


#' Coefficient of Determination of the Standard Curve
#'
#' A descriptive statistic, not coloured: no acceptance criterion of the
#' methods is set on R2.
#'
#' @param r2 Coefficient of determination.
#' @param lang Language code.
#'
#' @return A shiny tag, or `NULL` when `r2` is missing.
#' @keywords internal
r2_ui <- function(r2, lang = 'en') {
    if (length(r2) != 1 || is.na(r2)) return(NULL)
    curve_stat_ui('R\u00b2 = ', sprintf('%.4f', r2), NA, tr('infographic.r2_desc', lang))
}


#' Mean Absolute Error of the Back-Calculated Standards
#'
#' The same statistic as in the comparison of models (see
#' [compare_models()]), over the wells of the fit. Descriptive, not
#' coloured: the allowed bias applies to the mean of each level, not to this
#' mean over all wells.
#'
#' @param fit A `lin_fit` object.
#' @param lang Language code.
#'
#' @return A shiny tag, or `NULL` when the error cannot be computed.
#' @keywords internal
mean_error_ui <- function(fit, lang = 'en') {
    if (is.null(fit)) return(NULL)
    err <- abs(back_calculate(fit)$recovery - 100)
    if (all(is.na(err))) return(NULL)
    e <- mean(err, na.rm = TRUE)
    curve_stat_ui(paste0(tr('infographic.mean_error', lang), ' = '), sprintf('%.1f%%', e), NA,
                  tr('infographic.error_desc', lang))
}


#' Lack-of-Fit Test of the Standard Curve
#'
#' The p-value of the lack-of-fit F test (see [lack_of_fit()]) with zones: up
#' to 0.05 (red, the model does not describe the standards), up to 0.10
#' (orange, borderline) and above 0.10 (green). When the test cannot be done,
#' the line says why (no replicates, too few levels for the model, or zero
#' variance within the levels).
#'
#' @param lof Output of [lack_of_fit()].
#' @param lang Language code.
#'
#' @return A shiny tag.
#' @keywords internal
lof_ui <- function(lof, lang = 'en') {
    p <- lof$p
    if (length(p) != 1 || is.na(p)) {
        reason <- if (is.null(lof$reason) || is.na(lof$reason)) 'no_replicates' else lof$reason
        return(shiny::div(class = 'curve-stat text-muted',
                          tr('infographic.lof_unavailable', lang,
                             reason = tr(paste0('infographic.lof_na_', reason), lang))))
    }
    label <- paste0(tr('infographic.lof_title', lang), ': ')
    zone <- if (p <= 0.05) 'poor' else if (p <= 0.10) 'fair' else 'good'
    value <- if (p < 1e-3) '< 0.001' else format(signif(p, 2))
    curve_stat_ui(paste0(label, 'p = '), value, zone, tr(paste0('infographic.lof_', zone), lang))
}
