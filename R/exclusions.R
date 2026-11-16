#' Effect of Excluded Standard Points
#'
#' Compares the calibration fitted on all standards (original) with the
#' calibration refitted without the points excluded by the user: the
#' coefficients, the back-calculated concentrations of every standard and the
#' errors of back-calculation.
#'
#' A standard meets the acceptance criteria if the relative error of its
#' back-calculated concentration is within `std_bias` (`std_bias_edge` at the
#' lowest and highest level used in the fit). The summary of each model uses
#' the standards of its own fit: all standards for the original model, the
#' kept ones for the refitted model.
#'
#' @param original Fit on all standards (`lin_fit`).
#' @param refit Fit without the excluded points (`lin_fit`).
#' @param standards Background-subtracted standards of the original fit with
#'   `.key`, `conc` and `OD`.
#' @param excluded Keys of the points excluded by the user.
#' @param criteria See [qc_criteria()].
#' @param levels Concentrations of all standards of the run, for the level
#'   names (see [standard_names()]).
#'
#' @return A list with `coefs` (`coef`, `original`, `refitted`),
#'   `back_calc` (`standard`: level name, `key`: position, `conc`, `OD`,
#'   `excluded`, `calc.original`,
#'   `calc.refitted`, `error.original`, `error.refitted`; errors in %) and
#'   `summary` (`statistic`: `mean_error`, `max_error`, `n_pass`; `original`,
#'   `refitted`; `n_total` for `n_pass`).
#' @export
exclusion_effect <- function(original, refit, standards, excluded, criteria = qc_criteria(),
                             levels = standards$conc) {
    co <- coef_table_fit(original)
    cr <- coef_table_fit(refit)
    coefs <- data.frame(coef = co$coef, original = co$est, refitted = cr$est[match(co$coef, cr$coef)])

    error <- function(calc) 100 * (calc - standards$conc) / standards$conc
    bc <- data.frame(standard = standard_names(standards$conc, levels), key = standards$.key,
                     conc = standards$conc, OD = standards$OD, excluded = standards$.key %in% excluded)
    bc$calc.original <- inverse_fun(original, standards$OD)
    bc$calc.refitted <- inverse_fun(refit, standards$OD)
    bc$error.original <- error(bc$calc.original)
    bc$error.refitted <- error(bc$calc.refitted)
    rownames(bc) <- NULL

    stats <- function(err, used) {
        e <- abs(err[used])
        levels <- standards$conc[used]
        limit <- ifelse(levels %in% range(levels), criteria$std_bias_edge, criteria$std_bias)
        c(mean_error = if (all(is.na(e))) NA_real_ else mean(e, na.rm = TRUE),
          max_error = if (all(is.na(e))) NA_real_ else max(e, na.rm = TRUE),
          n_pass = sum(!is.na(e) & e <= limit),
          n_total = sum(used))
    }
    s_orig <- stats(bc$error.original, rep(TRUE, nrow(bc)))
    s_refit <- stats(bc$error.refitted, !bc$excluded)
    summary <- data.frame(statistic = c('mean_error', 'max_error', 'n_pass'),
                          original = unname(s_orig[1:3]), refitted = unname(s_refit[1:3]),
                          n_total.original = c(NA, NA, s_orig[['n_total']]),
                          n_total.refitted = c(NA, NA, s_refit[['n_total']]))
    list(coefs = coefs, back_calc = bc, summary = summary)
}


#' Tables of the Effect of Excluded Points for Display
#'
#' @param effect Output of [exclusion_effect()], or `NULL`.
#' @param lang Language code.
#' @param round_fun,round_par Rounding of the coefficients and concentrations.
#'
#' @return A list with the data.frames `coefs`, `back_calc` and `summary`
#'   with translated column names, or `NULL` without exclusions.
#' @keywords internal
exclusion_effect_tables <- function(effect, lang = 'en', round_fun = signif, round_par = 3) {
    if (is.null(effect)) return(NULL)
    t <- function(key, ...) tr(key, lang, ...)
    num <- function(x) ifelse(is.na(x), '', as.character(round_fun(x, round_par)))
    pct <- function(x) ifelse(is.na(x), '', sprintf('%.1f', x))
    name <- function(df, keys) stats::setNames(df, vapply(keys, t, ''))

    coefs <- name(data.frame(effect$coefs$coef, num(effect$coefs$original), num(effect$coefs$refitted)),
                  c('effect.coef', 'effect.original', 'effect.refitted'))

    bc <- effect$back_calc
    back_calc <- name(data.frame(bc$standard, bc$key, num(bc$conc), num(bc$calc.original), num(bc$calc.refitted),
                                 pct(bc$error.original), pct(bc$error.refitted),
                                 ifelse(bc$excluded, t('effect.excluded_yes'), '')),
                      c('effect.standard', 'effect.position', 'effect.nominal', 'effect.calc_original',
                        'effect.calc_refitted', 'effect.error_original', 'effect.error_refitted', 'effect.excluded'))

    s <- effect$summary
    count <- function(n, total) paste0(n, ' / ', total)
    summary <- name(data.frame(
        vapply(paste0('effect.', s$statistic), t, ''),
        c(pct(s$original[1:2]), count(s$original[3], s$n_total.original[3])),
        c(pct(s$refitted[1:2]), count(s$refitted[3], s$n_total.refitted[3]))
    ), c('effect.parameter', 'effect.original', 'effect.refitted'))

    list(coefs = coefs, back_calc = back_calc, summary = summary)
}


#' Effect of Excluded Points in the App
#'
#' @param tables Output of [exclusion_effect_tables()].
#' @param ns Namespace function of the module.
#' @param criteria See [qc_criteria()].
#' @param lang Language code.
#'
#' @return A shiny tag, or `NULL` without exclusions.
#' @keywords internal
exclusion_effect_ui <- function(tables, ns, criteria, lang) {
    if (is.null(tables)) return(NULL)
    t <- function(key, ...) tr(key, lang, ...)
    shiny::div(
        class = 'exclusion-effect',
        shiny::h5(with_tooltip(t('effect.title'),
                               t('effect.note', bias = criteria$std_bias, edge = criteria$std_bias_edge))),
        shiny::fluidRow(
            shiny::column(5, shiny::h5(t('effect.section_coefs')), shiny::tableOutput(ns('effect_coefs'))),
            shiny::column(7, shiny::h5(t('effect.section_summary')), shiny::tableOutput(ns('effect_summary')))
        ),
        shiny::h5(t('effect.section_back_calc')),
        shiny::tableOutput(ns('effect_back_calc'))
    )
}
