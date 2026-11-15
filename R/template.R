#' Concentrations of a Standard Dilution Series
#'
#' @param start Highest concentration.
#' @param step Dilution step (> 1).
#' @param n.levels Number of levels (>= 3).
#'
#' @return Numeric vector rounded to 3 significant digits.
#' @keywords internal
standard_series <- function(start, step, n.levels) {
    if (!is.numeric(start) || length(start) != 1 || is.na(start) || start <= 0) {
        lin_abort('error.template_start')
    }
    if (!is.numeric(step) || length(step) != 1 || is.na(step) || step <= 1) {
        lin_abort('error.template_step')
    }
    if (length(n.levels) != 1 || is.na(n.levels) || n.levels < 3) lin_abort('error.template_levels')
    signif(start / step^(0:(n.levels - 1)), 3)
}


#' Create Experiment Data Template
#'
#' @param start Highest standard concentration.
#' @param step Dilution step of the standard series (> 1).
#' @param n.standard Number of replicate standard series.
#' @param n.sample Number of sample rows.
#' @param n.levels Number of standard concentration levels.
#' @param standards Position of the standard rows: `"before"` or `"after"` the samples.
#'
#' @return A data.frame with columns `sample.name`, `sample.date`, `type`,
#'   `dilution`, `conc` and `OD`. Empty cells are `NA`, so they are written as
#'   blank cells to Excel and CSV files.
#' @export
#' @examples
#' make_template(start = 100, step = 3, n.standard = 3, n.sample = 15)
make_template <- function(start = 100, step = 2,
                          n.standard = 2, n.sample = 10, n.levels = 7,
                          standards = c('before', 'after')) {
    standards <- match.arg(standards)
    titration <- standard_series(start, step, n.levels)

    standard_part <- data.frame(
        type = rep(c(rep('standard', n.levels), 'blank'), times = n.standard),
        dilution = NA_real_,
        conc = rep(c(titration, NA_real_), times = n.standard)
    )
    sample_part <- data.frame(
        type = rep('sample', n.sample),
        dilution = rep(1, n.sample),
        conc = NA_real_
    )
    template <- if (standards == 'before') rbind(standard_part, sample_part) else
        rbind(sample_part, standard_part)

    data.frame(sample.name = NA_character_,
               sample.date = NA_character_,
               type = template$type,
               dilution = template$dilution,
               conc = template$conc,
               OD = NA_real_)
}
