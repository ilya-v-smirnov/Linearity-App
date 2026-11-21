#' Effect of Excluded Standard Points
#'
#' Compares the calibration fitted on all standards (original) with the
#' calibration refitted without the points excluded by the user: the
#' coefficients, the back-calculated concentrations of every standard and the
#' errors of back-calculation.
#'
#' The summary tells the wells from the levels. A well is within its
#' individual bias limit if the relative error of its back-calculated
#' concentration is within the bias limit of its level in the standards table
#' (`std_bias`, `std_bias_edge` at the boundary calibrators, see
#' [assess_standards()]: by default the limits of the plate plan, the same for
#' both models); the errors and this count use the wells of each fit: all
#' standards for the original model, the kept ones for the refitted model. A
#' calibrator level is accepted by the rules of the
#' standards table (the bias of its mean and the CV of its replicates, see
#' [assess_standards()]), and the levels are counted against the number
#' required for the calibration (see [calibration_acceptance()]): an
#' exclusion may lower the errors of the wells left without changing the
#' levels accepted.
#'
#' @param original Fit on all standards (`lin_fit`).
#' @param refit Fit without the excluded points (`lin_fit`).
#' @param standards Background-subtracted standards of the original fit with
#'   `.key`, `conc` and `OD`.
#' @param excluded Keys of the points excluded by the user.
#' @param criteria See [qc_criteria()].
#' @param levels Concentrations of all standards of the run, for the level
#'   names (see [standard_names()]).
#' @param wells All standard wells of the run, as `wells` of
#'   [assess_standards()] (with `lost = "excluded"` for the points excluded by
#'   the user); by default the wells of `standards`. The original model is
#'   assessed on them without the exclusions by the user.
#' @param assessed The standards table of the refitted calibration (output of
#'   [assess_standards()]), so that its limits and its levels accepted are
#'   those shown; by default assessed on `wells`.
#'
#' @return A list with `coefs` (`coef`, `original`, `refitted`),
#'   `back_calc` (`standard`: level name, `key`: position, `conc`, `OD`,
#'   `excluded`, `calc.original`,
#'   `calc.refitted`, `error.original`, `error.refitted`; errors in %) and
#'   `summary` (`statistic`: `mean_error`, `max_error`, `n_wells_within`,
#'   `n_levels_accepted`; `original`, `refitted`; `n_total` for the counts:
#'   the wells of the fit, the levels of the run; `n_needed`: the levels
#'   required).
#' @export
exclusion_effect <- function(original, refit, standards, excluded, criteria = qc_criteria(),
                             levels = standards$conc, wells = NULL, assessed = NULL) {
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

    # The levels by the rules of the standards table: their limits and their acceptance
    if (is.null(wells)) {
        wells <- data.frame(conc = standards$conc, OD = standards$OD, key = as.character(standards$.key),
                            lost = ifelse(standards$.key %in% excluded, 'excluded', NA_character_))
    }
    # The original model has no exclusions by the user; the other losses (no OD, OD <= blank) remain
    all_wells <- wells
    if (!is.null(all_wells$lost)) all_wells$lost[all_wells$lost %in% 'excluded'] <- NA_character_
    if (!is.null(all_wells$comment)) all_wells$comment[is.na(all_wells$lost)] <- NA_character_
    st_orig <- assess_standards(original, criteria, wells = all_wells)
    st_refit <- if (is.null(assessed)) assess_standards(refit, criteria, wells = wells) else assessed
    cal_orig <- calibration_acceptance(st_orig, criteria)
    cal_refit <- calibration_acceptance(st_refit, criteria)

    # Each well against the bias limit of its level
    stats <- function(err, used, st) {
        e <- abs(err[used])
        limit <- st$bias.limit[match(standards$conc[used], st$conc)]
        c(mean_error = if (all(is.na(e))) NA_real_ else mean(e, na.rm = TRUE),
          max_error = if (all(is.na(e))) NA_real_ else max(e, na.rm = TRUE),
          n_within = sum(!is.na(e) & e <= limit, na.rm = TRUE),
          n_total = sum(used))
    }
    s_orig <- stats(bc$error.original, rep(TRUE, nrow(bc)), st_orig)
    s_refit <- stats(bc$error.refitted, !bc$excluded, st_refit)

    summary <- data.frame(statistic = c('mean_error', 'max_error', 'n_wells_within', 'n_levels_accepted'),
                          original = c(unname(s_orig[1:3]), cal_orig$n_pass),
                          refitted = c(unname(s_refit[1:3]), cal_refit$n_pass),
                          n_total.original = c(NA, NA, s_orig[['n_total']], cal_orig$n_levels),
                          n_total.refitted = c(NA, NA, s_refit[['n_total']], cal_refit$n_levels),
                          n_needed.original = c(NA, NA, NA, cal_orig$n_needed),
                          n_needed.refitted = c(NA, NA, NA, cal_refit$n_needed))
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
    levels <- function(n, total, needed) t('effect.levels_value', n = n, total = total, needed = needed)
    summary <- name(data.frame(
        vapply(paste0('effect.', s$statistic), t, ''),
        c(pct(s$original[1:2]), count(s$original[3], s$n_total.original[3]),
          levels(s$original[4], s$n_total.original[4], s$n_needed.original[4])),
        c(pct(s$refitted[1:2]), count(s$refitted[3], s$n_total.refitted[3]),
          levels(s$refitted[4], s$n_total.refitted[4], s$n_needed.refitted[4]))
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
