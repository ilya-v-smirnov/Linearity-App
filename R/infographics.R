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
#' ULOQ (orange) and other problems (grey) on the right.
#'
#' @param flags Flags of the sample wells.
#' @param lang Language code.
#'
#' @return A shiny tag, or `NULL` when there are no sample wells.
#' @keywords internal
quantification_ui <- function(flags, lang = 'en') {
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
    shiny::div(
        class = 'infographic',
        shiny::div(class = 'infographic-title',
                   tr('infographic.samples', lang, n = counts[['ok']], total = total)),
        shiny::div(class = 'quant-bar', segments),
        shiny::div(class = 'quant-legend', legend)
    )
}


#' Infographic of a Value on a Scale with Coloured Zones
#'
#' The zones are `poor` (red), `fair` (orange) and `good` (green), from left
#' to right; a marker shows the value on the scale.
#'
#' @param title Title (tags or text) before the badge.
#' @param zone Zone of the value: `"poor"`, `"fair"` or `"good"`.
#' @param badge Text of the badge.
#' @param marker Position of the marker, in percent of the width.
#' @param bounds Positions of the boundaries between the zones (two values,
#'   in percent).
#' @param ticks Labels of the ticks.
#' @param positions Positions of the ticks, in percent.
#'
#' @return A shiny tag.
#' @keywords internal
zone_scale_ui <- function(title, zone, badge, marker, bounds, ticks, positions) {
    shiny::div(
        class = 'infographic',
        shiny::div(class = 'infographic-title', title, ' ',
                   shiny::span(class = paste0('zone-badge zone-', zone), badge)),
        shiny::div(
            class = 'zone-scale',
            shiny::div(class = 'zone zone-poor', style = sprintf('width: %.1f%%;', bounds[1])),
            shiny::div(class = 'zone zone-fair', style = sprintf('width: %.1f%%;', bounds[2] - bounds[1])),
            shiny::div(class = 'zone zone-good', style = sprintf('width: %.1f%%;', 100 - bounds[2])),
            shiny::div(class = 'zone-marker', style = sprintf('left: %.1f%%;', marker))
        ),
        shiny::div(class = 'zone-ticks',
                   lapply(seq_along(ticks), function(i) {
                       shiny::span(style = sprintf('left: %.1f%%;', positions[i]), ticks[i])
                   }))
    )
}


#' Infographic of the Coefficient of Determination
#'
#' The \eqn{R^2} of the standard curve on a scale from 0.95 to 1 with zones: below
#' 0.98 (red), 0.98-0.99 (orange) and from 0.99 (green, the criterion of the
#' quality assessment). Values below 0.95 are shown at the left end.
#'
#' @param r2 Coefficient of determination.
#' @param lang Language code.
#'
#' @return A shiny tag, or `NULL` when `r2` is missing.
#' @keywords internal
r2_ui <- function(r2, lang = 'en') {
    if (length(r2) != 1 || is.na(r2)) return(NULL)
    from <- 0.95
    position <- function(x) 100 * (min(max(x, from), 1) - from) / (1 - from)
    zone <- if (r2 >= 0.99) 'good' else if (r2 >= 0.98) 'fair' else 'poor'
    ticks <- c(0.95, 0.98, 0.99, 1)
    zone_scale_ui(shiny::tagList('R\u00b2 = ', shiny::strong(sprintf('%.4f', r2))),
                  zone, tr(paste0('infographic.r2_', zone), lang),
                  marker = position(r2), bounds = c(position(0.98), position(0.99)),
                  ticks = format(ticks), positions = vapply(ticks, position, 0))
}


#' Infographic of the Lack-of-Fit Test
#'
#' The p-value of the lack-of-fit F test (see [lack_of_fit()]) on a log scale
#' from 0.001 to 1 with zones: up to 0.05 (red, the model does not describe
#' the standards), up to 0.10 (orange, borderline) and above 0.10 (green).
#' Values below 0.001 are shown at the left end. Without replicates of the
#' standards the test cannot be done, which the infographic says.
#'
#' @param p P-value of the test.
#' @param lang Language code.
#'
#' @return A shiny tag.
#' @keywords internal
lof_ui <- function(p, lang = 'en') {
    title <- tr('infographic.lof_title', lang)
    if (length(p) != 1 || is.na(p)) {
        return(shiny::div(class = 'infographic',
                          shiny::div(class = 'infographic-title', title),
                          shiny::div(class = 'text-muted small', tr('infographic.lof_na', lang))))
    }
    from <- 1e-3
    position <- function(x) 100 * (log10(min(max(x, from), 1)) - log10(from)) / -log10(from)
    zone <- if (p <= 0.05) 'poor' else if (p <= 0.10) 'fair' else 'good'
    value <- if (p < from) '< 0.001' else format(signif(p, 2))
    ticks <- c(0.001, 0.01, 0.05, 0.1, 1)
    zone_scale_ui(shiny::tagList(paste0(title, ': p = '), shiny::strong(value)),
                  zone, tr(paste0('infographic.lof_', zone), lang),
                  marker = position(p), bounds = c(position(0.05), position(0.10)),
                  ticks = format(ticks, scientific = FALSE, drop0trailing = TRUE),
                  positions = vapply(ticks, position, 0))
}
