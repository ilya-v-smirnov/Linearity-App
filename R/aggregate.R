#' Aggregate Results by Grouping Parameters
#'
#' @param result Result table from [calculate()].
#' @param group_vars Character vector of grouping columns.
#' @param round_fun,round_par Rounding of the aggregated statistics.
#'
#' @return A data.frame with `n` (all wells), `n.quantified` (wells with a
#'   concentration), `mean`, `sd`, `cv` (%), `min` and `max` of quantified concentrations;
#'   `NULL` if no grouping columns are given.
#' @export
aggregate_result <- function(result, group_vars = 'sample.name',
                             round_fun = signif, round_par = 3) {
    if (length(group_vars) == 0) return(NULL)

    stat_or_na <- function(f, x) if (all(is.na(x))) NA_real_ else f(x, na.rm = TRUE)

    result |>
        group_by(across(all_of(group_vars))) |>
        summarise(n = n(),
                  n.quantified = sum(!is.na(.data$conc)),
                  mean = stat_or_na(base::mean, .data$conc),
                  sd = if (sum(!is.na(.data$conc)) >= 2) stats::sd(.data$conc, na.rm = TRUE) else NA_real_,
                  cv = cv_percent(.data$conc),
                  min = stat_or_na(base::min, .data$conc),
                  max = stat_or_na(base::max, .data$conc),
                  .groups = 'drop') |>
        mutate(across(all_of(c('mean', 'sd', 'min', 'max')), ~ round_fun(.x, round_par)),
               cv = round(.data$cv, 1)) |>
        as.data.frame()
}


#' Round a Result Table for Display and Export
#'
#' [calculate()] keeps the concentrations at full precision, on which the
#' quality controls and the samples are assessed; the rounding chosen by the
#' user applies only to what the tables show and export, so that it cannot
#' change a status.
#'
#' @param result Result or QC table from [calculate()].
#' @param round_fun,round_par Rounding of the concentrations (`conc`,
#'   `conc.lower`, `conc.upper`); the recovery of the quality controls is
#'   rounded to 0.1 %.
#' @param bias_limit Allowed bias (%) of each quality control, in the order
#'   of the rows (`bias.limit` of the wells of [assess_qc()]): a recovery that
#'   would read as 100 % +- the limit while it is not gets more decimals (see
#'   [limit_digits()]).
#'
#' @return The table with rounded values; `NULL` for `NULL`.
#' @export
round_result <- function(result, round_fun = signif, round_par = 3, bias_limit = NULL) {
    if (is.null(result)) return(NULL)
    for (col in intersect(c('conc', 'conc.lower', 'conc.upper'), names(result))) {
        result[[col]] <- round_fun(result[[col]], round_par)
    }
    if ('recovery' %in% names(result)) {
        digits <- if (is.null(bias_limit)) 1 else limit_digits(result$recovery, bias_limit, center = 100)
        result$recovery <- round(result$recovery, digits)
    }
    result
}


#' Translate the Flag Column of a Result Table
#'
#' @param result Result table from [calculate()].
#' @param lang Language code.
#'
#' @return The table with translated `flag` values; the signal at or below
#'   the background (`OD.le.blank`) is marked with a check mark.
#' @export
localize_result <- function(result, lang = 'en') {
    if (is.null(result)) return(NULL)
    if ('flag' %in% names(result)) {
        result$flag <- translate_flags(result$flag, lang)
    }
    if ('OD.le.blank' %in% names(result)) {
        result$OD.le.blank <- ifelse(result$OD.le.blank %in% TRUE, '\u2713', '')
    }
    result
}
