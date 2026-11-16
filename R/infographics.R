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
    shiny::div(
        class = 'infographic',
        shiny::div(class = 'infographic-title',
                   'R\u00b2 = ', shiny::strong(sprintf('%.4f', r2)), ' ',
                   shiny::span(class = paste0('r2-badge r2-', zone), tr(paste0('infographic.r2_', zone), lang))),
        shiny::div(
            class = 'r2-scale',
            shiny::div(class = 'r2-zone r2-poor', style = sprintf('width: %.1f%%;', position(0.98))),
            shiny::div(class = 'r2-zone r2-fair', style = sprintf('width: %.1f%%;', position(0.99) - position(0.98))),
            shiny::div(class = 'r2-zone r2-good', style = sprintf('width: %.1f%%;', 100 - position(0.99))),
            shiny::div(class = 'r2-marker', style = sprintf('left: %.1f%%;', position(r2)))
        ),
        shiny::div(class = 'r2-ticks',
                   lapply(ticks, function(x) {
                       shiny::span(style = sprintf('left: %.1f%%;', position(x)), format(x))
                   }))
    )
}
