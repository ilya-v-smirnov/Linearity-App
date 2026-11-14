#' Validate Input Data
#'
#' Checks mandatory columns, numeric columns, allowed values of `type`,
#' dilution factors of samples, concentrations of standards and the number of
#' standard wells. Stops with an informative error (class `lin_error`) on the
#' first problem.
#'
#' @param data A data.frame with the raw data.
#'
#' @return The validated data.frame.
#' @export
#' @examples
#' path <- system.file('extdata', 'griess.xlsx', package = 'LinearityApp')
#' data <- check_input_file(read_table(path))
check_input_file <- function(data) {
    mandatory_columns <- c('sample.name', 'sample.date', 'type', 'dilution', 'conc', 'OD')
    for (mc in mandatory_columns) {
        if (!mc %in% colnames(data)) {
            lin_abort('error.mandatory_column', column = mc)
        }
    }

    for (nc in c('dilution', 'conc', 'OD')) {
        if (!is.numeric(data[[nc]])) {
            lin_abort('error.non_numeric', column = nc)
        }
    }

    allowed_types <- c('standard', 'sample', 'NC')
    present_types <- unique(data[['type']])

    not_allowed <- setdiff(present_types, allowed_types)
    if (length(not_allowed) > 0) {
        lin_abort('error.type_not_allowed', values = not_allowed)
    }

    missing_types <- setdiff(allowed_types, present_types)
    if (length(missing_types) > 0) {
        lin_abort('error.type_missing', values = missing_types)
    }

    dilutions <- data$dilution[data$type == 'sample']
    if (any(is.na(dilutions))) {
        lin_abort('error.dilution_missing')
    }
    if (!all(dilutions > 0)) {
        lin_abort('error.dilution_positive')
    }

    std_conc <- data$conc[data$type == 'standard']
    if (any(is.na(std_conc))) {
        lin_abort('error.std_conc_missing')
    }
    if (!all(std_conc > 0)) {
        lin_abort('error.std_conc_positive')
    }

    n_standard <- sum(data$type == 'standard')
    if (n_standard <= 4) {
        lin_abort('error.few_standard_wells', n = n_standard)
    }

    data
}
