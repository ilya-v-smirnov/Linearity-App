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


#' Translate the Flag Column of a Result Table
#'
#' @param result Result table from [calculate()].
#' @param lang Language code.
#'
#' @return The table with translated `flag` values.
#' @export
localize_result <- function(result, lang = 'en') {
    if (is.null(result)) return(NULL)
    if ('flag' %in% names(result)) {
        result$flag <- translate_flags(result$flag, lang)
    }
    result
}
