#' Aggregate Results by Grouping Parameters
#'
#' @param result Result table from [calculate()].
#' @param group_vars Character vector of grouping columns.
#' @param round_fun,round_par Rounding of the aggregated statistics.
#'
#' @return A data.frame with `n` (all wells), `n.quantified` (wells with a
#'   value within the accepted range), `mean`, `sd`, `cv` (%), `min` and `max`
#'   of the wells used in the sample result (column `included` when present);
#'   `NULL` if no grouping columns are given.
#' @export
aggregate_result <- function(result, group_vars = 'sample.name',
                             round_fun = signif, round_par = 3) {
    if (length(group_vars) == 0) return(NULL)

    stat_or_na <- function(f, x) if (all(is.na(x))) NA_real_ else f(x, na.rm = TRUE)
    result$.used <- if ('included' %in% names(result)) ifelse(result$included, result$conc, NA_real_) else result$conc

    result |>
        group_by(across(all_of(group_vars))) |>
        summarise(n = n(),
                  n.quantified = sum(!is.na(.data$conc)),
                  mean = stat_or_na(base::mean, .data$.used),
                  sd = if (sum(!is.na(.data$.used)) >= 2) stats::sd(.data$.used, na.rm = TRUE) else NA_real_,
                  cv = cv_percent(.data$.used),
                  min = stat_or_na(base::min, .data$.used),
                  max = stat_or_na(base::max, .data$.used),
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
#'   `conc.est`, `conc.lower`, `conc.upper`); the recovery of the quality
#'   controls is rounded to 0.1 %.
#' @param bias_limit Allowed bias (%) of each quality control, in the order
#'   of the rows (`bias.limit` of the wells of [assess_qc()]): a recovery that
#'   would read as 100 % +- the limit while it is not gets more decimals (see
#'   [limit_digits()]).
#'
#' @return The table with rounded values; `NULL` for `NULL`.
#' @export
round_result <- function(result, round_fun = signif, round_par = 3, bias_limit = NULL) {
    if (is.null(result)) return(NULL)
    for (col in intersect(c('conc', 'conc.est', 'conc.lower', 'conc.upper'), names(result))) {
        result[[col]] <- round_fun(result[[col]], round_par)
    }
    if ('recovery' %in% names(result)) {
        digits <- if (is.null(bias_limit)) 1 else limit_digits(result$recovery, bias_limit, center = 100)
        result$recovery <- round(result$recovery, digits)
    }
    result
}


#' Translate the Status Columns of a Result Table
#'
#' @param result Result table from [calculate()].
#' @param lang Language code.
#'
#' @return The table with translated `flag` and `included`, the short labels
#'   of `value.source` (see [value_source_labels()]), and without
#'   `OD.le.blank`: a signal at or below the background reads from
#'   `OD.corrected`.
#' @export
localize_result <- function(result, lang = 'en') {
    if (is.null(result)) return(NULL)
    if ('flag' %in% names(result)) {
        result$flag <- translate_flags(result$flag, lang)
    }
    if ('value.source' %in% names(result)) {
        result$value.source <- unname(value_source_labels()[result$value.source])
    }
    if ('included' %in% names(result)) {
        result$included <- ifelse(result$included %in% TRUE, tr('value.inc_yes', lang), tr('value.inc_no', lang))
    }
    result$OD.le.blank <- NULL
    result
}


#' Short Column Names of the Result Tables
#'
#' Syntactically valid in R, so that an exported table keeps its headers when
#' it is imported (see [make.names()]), and the same in every language.
#'
#' @return A named character vector: the short name of each column.
#' @keywords internal
column_abbreviations <- function() {
    c(sample.name = 'sample', sample.date = 'date', dilution = 'DF', OD.corrected = 'OD.net', conc = 'C',
      conc.est = 'C.est', conc.lower = 'C.low', conc.upper = 'C.up', flag = 'RS', value.source = 'VS',
      included = 'Inc',
      nominal = 'Nom', recovery = 'Rec', n = 'N', n.quantified = 'N.in', mean = 'Mean', min = 'Min', max = 'Max',
      sd = 'SD', cv = 'CV', range = 'RS', precision = 'Prec', linearity = 'Lin')
}


#' Short Names of Columns
#'
#' The data may have additional columns of the user with any name. A column
#' whose short name is already taken by such a column keeps its full name, so
#' that the headers of a table stay unique.
#'
#' @param columns Column names of one table.
#'
#' @return Character vector of the names to show.
#' @keywords internal
short_columns <- function(columns) {
    short <- column_abbreviations()
    known <- columns %in% names(short)
    out <- columns
    out[known] <- unname(short[columns[known]])
    taken <- known & out %in% columns[!known]
    out[taken] <- columns[taken]
    out
}


#' Shorten the Column Names of a Table
#'
#' @param table A table with the column names of [calculate()] or
#'   [summary_table()].
#'
#' @return The table with the names of [short_columns()]; `NULL` for `NULL`.
#' @keywords internal
abbreviate_columns <- function(table) {
    if (is.null(table)) return(NULL)
    names(table) <- short_columns(names(table))
    table
}


#' Join the Well Counts of the Summary
#'
#' @param table Output of [summary_table()].
#'
#' @return The table with `n.quantified` and `n` joined into `In.N` (`"1/2"`)
#'   at the place of `n`; unchanged unless it has both.
#' @keywords internal
merge_well_counts <- function(table) {
    if (is.null(table) || !all(c('n', 'n.quantified') %in% names(table))) return(table)
    at <- match('n', names(table))
    table[[at]] <- paste0(table$n.quantified, '/', table$n)
    names(table)[at] <- 'In.N'
    table$n.quantified <- NULL
    table
}


#' Legend of the Short Column Names
#'
#' Only the short names present in a table are explained; `sample`, `date`,
#' `Mean`, `Min` and `Max` need no explanation.
#'
#' @param columns Column names of the table(s).
#' @param lang Language code.
#'
#' @return `abbreviations_table()`: a data.frame with the name and its
#'   meaning, `NULL` for none. `abbreviations_text()`: one string, empty for
#'   none. `abbreviations_ui()`: a shiny tag, `NULL` for none.
#' @name abbreviations
#' @keywords internal
NULL

#' @rdname abbreviations
abbreviations_table <- function(columns, lang = 'en') {
    known <- setdiff(c(column_abbreviations(), 'In.N'), c('sample', 'date', 'Mean', 'Min', 'Max'))
    shown <- unique(columns[columns %in% known])
    if (length(shown) == 0) return(NULL)
    out <- data.frame(shown, vapply(paste0('abbr.term_', gsub('.', '_', shown, fixed = TRUE)), tr, '', lang = lang,
                                    USE.NAMES = FALSE))
    names(out) <- c(tr('abbr.name', lang), tr('abbr.meaning', lang))
    out
}

#' @rdname abbreviations
abbreviations_text <- function(columns, lang = 'en') {
    legend <- abbreviations_table(columns, lang)
    if (is.null(legend)) return('')
    paste0(tr('abbr.title', lang), ': ', paste(legend[[1]], legend[[2]], sep = ' – ', collapse = '; '), '.')
}

#' @rdname abbreviations
abbreviations_ui <- function(columns, lang = 'en') {
    legend <- abbreviations_table(columns, lang)
    if (is.null(legend)) return(NULL)
    items <- lapply(seq_len(nrow(legend)), function(i) {
        shiny::span(class = 'abbr-item', shiny::tags$b(legend[[1]][i]), paste0(' – ', legend[[2]][i]))
    })
    shiny::div(class = 'abbr-legend', shiny::span(class = 'abbr-title', paste0(tr('abbr.title', lang), ':')), items)
}
