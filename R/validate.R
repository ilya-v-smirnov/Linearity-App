#' Locations of Rows
#'
#' Uses well names when available, otherwise spreadsheet row numbers
#' (data row + 1 for the header). Long lists are shortened.
#'
#' @param data Input data.
#' @param i Row indices.
#'
#' @return A `lin_message`.
#' @keywords internal
rows_location <- function(data, i) {
    shorten <- function(x) if (length(x) > 10) c(x[1:10], '...') else x
    if ('well' %in% names(data) && !anyNA(data$well[i])) {
        return(loc_wells(shorten(unique(data$well[i]))))
    }
    loc_rows(shorten(i + 1))
}


#' Find All Problems in Input Data
#'
#' Checks mandatory and numeric columns, values of `type`, presence of
#' standards, samples and background wells (NC or blank), dilution factors,
#' concentrations of standards and QC, the number of standard wells and
#' duplicated wells. Unlike [check_assay()], it does not stop at the first
#' problem.
#'
#' @param data Input data (see [read_assay()]).
#'
#' @return A list of problems (see [problems_table()]).
#' @export
validate_assay <- function(data) {
    acc <- new.env(parent = emptyenv())
    acc$problems <- empty_problems()
    err <- function(location, key, ...) {
        acc$problems <- add_problem(acc$problems, 'error', location, key, ...)
    }

    for (mc in c('type', 'OD')) {
        if (!mc %in% names(data)) err(NULL, 'error.mandatory_column', column = mc)
    }
    if (length(acc$problems) > 0) return(acc$problems)

    for (nc in intersect(c('dilution', 'conc', 'OD'), names(data))) {
        if (!is.numeric(data[[nc]])) err(NULL, 'error.non_numeric', column = nc)
    }
    if (length(acc$problems) > 0) return(acc$problems)

    type <- data$type
    allowed <- c('standard', 'sample', 'NC', 'blank', 'QC')
    bad <- which(!type %in% allowed)
    if (length(bad) > 0) {
        err(rows_location(data, bad), 'error.type_not_allowed', values = unique(type[bad]))
    }
    missing_types <- setdiff(c('standard', 'sample'), type)
    if (length(missing_types) > 0) {
        err(NULL, 'error.type_missing', values = missing_types)
    }
    if (!any(type %in% c('NC', 'blank'))) {
        err(NULL, 'error.no_background')
    }

    check_rows <- function(condition, key) {
        i <- which(condition)
        if (length(i) > 0) err(rows_location(data, i), key)
    }
    is_sample <- type %in% 'sample'
    is_std <- type %in% 'standard'
    is_qc <- type %in% 'QC'
    check_rows(is_sample & is.na(data$dilution), 'error.dilution_missing')
    check_rows(is_sample & !is.na(data$dilution) & data$dilution <= 0, 'error.dilution_positive')
    check_rows(is_std & is.na(data$conc), 'error.std_conc_missing')
    check_rows(is_std & !is.na(data$conc) & data$conc <= 0, 'error.std_conc_positive')
    check_rows(is_qc & is.na(data$conc), 'error.qc_conc_missing')
    check_rows(is_qc & !is.na(data$conc) & data$conc <= 0, 'error.qc_conc_positive')

    n_standard <- sum(is_std)
    if (n_standard > 0 && n_standard <= 4) {
        err(NULL, 'error.few_standard_wells', n = n_standard)
    }

    if ('well' %in% names(data)) {
        dup <- unique(data$well[!is.na(data$well) & duplicated(data$well)])
        if (length(dup) > 0) err(loc_wells(dup), 'error.duplicate_well')
    }

    acc$problems
}


#' Validate Input Data and Stop on Errors
#'
#' Combines problems found while reading (attribute `problems`) with
#' [validate_assay()]. If there are errors, stops with a `lin_error` whose
#' `problems` field lists all problems; otherwise returns the data with all
#' (warning) problems in the attribute `problems`.
#'
#' @param data Input data (see [read_assay()]).
#'
#' @return The validated data.
#' @export
#' @examples
#' path <- system.file('extdata', 'bca_plate.xlsx', package = 'LinearityApp')
#' data <- check_assay(read_assay(path))
check_assay <- function(data) {
    problems <- c(attr(data, 'problems'), validate_assay(data))
    errors <- Filter(function(p) p$severity == 'error', problems)
    if (length(errors) > 0) {
        first <- problem_message(errors[[1]])
        if (length(errors) == 1) {
            lin_abort('error.invalid_input_one', first = first, .fields = list(problems = problems))
        }
        lin_abort('error.invalid_input_many', first = first, more = length(errors) - 1,
                  .fields = list(problems = problems))
    }
    attr(data, 'problems') <- problems
    data
}


#' Validate Input Data (Table Format)
#'
#' Normalizes a table read with [read_table()] (see [normalize_table()]) and
#' validates it with [check_assay()].
#'
#' @param data A data.frame with the raw data.
#'
#' @return The validated data.frame.
#' @export
#' @examples
#' path <- system.file('extdata', 'griess.xlsx', package = 'LinearityApp')
#' data <- check_input_file(read_table(path))
check_input_file <- function(data) {
    check_assay(normalize_table(data))
}


#' Message of a Problem Including Its Location
#' @keywords internal
problem_message <- function(p) {
    if (is.null(p$location)) return(p$message)
    lin_message('problem.with_location', message = p$message, location = p$location)
}


#' Problems as a Table for Display
#'
#' @param problems A list of problems (attribute `problems` of the data, or the
#'   `problems` field of the error raised by [check_assay()]).
#' @param lang Language code.
#'
#' @return A data.frame with translated columns severity, location and message;
#'   `NULL` if there are no problems.
#' @export
problems_table <- function(problems, lang = 'en') {
    if (length(problems) == 0) return(NULL)
    out <- data.frame(
        severity = vapply(problems, function(p) tr(paste0('problem.', p$severity), lang), ''),
        location = vapply(problems, function(p) {
            if (is.null(p$location)) '' else message_text(list(p$location), lang)
        }, ''),
        message = vapply(problems, function(p) message_text(list(p$message), lang), '')
    )
    out <- out[order(out$severity != tr('problem.error', lang)), ]
    rownames(out) <- NULL
    names(out) <- c(tr('problem.severity', lang), tr('problem.location', lang),
                    tr('problem.message', lang))
    out
}
