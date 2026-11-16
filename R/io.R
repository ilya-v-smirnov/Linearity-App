#' Read a Semicolon- or Comma-Delimited CSV File
#'
#' Semicolon-separated files are expected to use a comma as decimal separator,
#' comma-separated files a dot. Semicolon-separated files with decimal dots
#' are recognised as well: the decimal separator that gives more numeric
#' columns is used.
#'
#' @param path File path.
#'
#' @return A data.frame.
#' @keywords internal
read_csv_auto <- function(path) {
    read <- function(sep, dec) {
        tryCatch(read.csv(path, sep = sep, dec = dec, stringsAsFactors = FALSE),
                 error = function(e) NULL)
    }
    n_numeric <- function(table) sum(vapply(table, is.numeric, logical(1)))

    table <- read(';', ',')
    if (!is.null(table) && ncol(table) > 1) {
        dotted <- read(';', '.')
        if (!is.null(dotted) && n_numeric(dotted) > n_numeric(table)) table <- dotted
        return(table)
    }
    table <- read(',', '.')
    if (!is.null(table) && ncol(table) > 1) return(table)
    lin_abort('error.csv_format')
}


#' Drop Empty Rows at the End of a Table
#'
#' Spreadsheets exported to CSV often end with rows of empty cells. Empty rows
#' inside the table are kept, so that row numbers in messages match the file.
#'
#' @param table A data.frame.
#'
#' @return The data.frame without trailing rows of `NA` or blank cells.
#' @keywords internal
drop_trailing_empty_rows <- function(table) {
    if (nrow(table) == 0 || ncol(table) == 0) return(table)
    blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
    empty <- Reduce(`&`, lapply(table, blank))
    if (!any(empty)) return(table)
    table[seq_len(max(c(0, which(!empty)))), , drop = FALSE]
}


#' Read Data Table from File
#'
#' @param path Path to an `.xlsx` or `.csv` file.
#'
#' @return A data.frame containing the imported data.
#' @export
#' @examples
#' read_table(system.file('extdata', 'bca_semicolon.csv', package = 'LinearityApp'))
read_table <- function(path) {
    file_type <- tolower(tools::file_ext(path))
    switch(file_type,
           'xlsx' = as.data.frame(readxl::read_excel(path, sheet = 1)),
           'csv' = read_csv_auto(path),
           lin_abort('error.file_type')
    )
}


#' Read Assay Data from a File
#'
#' Detects the format: a plate maps workbook (see [plate_maps_format]), a plate
#' workbook (see [plate_format]) or a table (Excel or CSV) with one row per
#' well. Tables are normalized with [normalize_table()].
#'
#' @param path Path to an `.xlsx` or `.csv` file.
#'
#' @return A data.frame with an attribute `problems`; validate it with [check_assay()].
#' @export
#' @examples
#' read_assay(system.file('extdata', 'bca_plate.xlsx', package = 'LinearityApp'))
read_assay <- function(path) {
    if (is_plate_maps_workbook(path)) return(read_plate_maps(path))
    if (is_plate_workbook(path)) return(read_plate(path))
    data <- normalize_table(read_table(path))
    attr(data, 'problems') <- empty_problems()
    data
}


#' Normalize a Table of Wells
#'
#' Only `type` and `OD` are required. Missing `dilution` is set to 1 for
#' samples and QC; missing `conc` to `NA`; missing `sample.name` to `id`.
#' `type` values are case-insensitive (`standard`, `sample`, `blank`, `QC`;
#' `NC` is read as `blank`). Empty numeric columns are converted to numbers;
#' empty rows at the end of the table are dropped. The old six-column
#' format (`sample.name`, `sample.date`, `type`, `dilution`, `conc`, `OD`) is a
#' special case and needs no conversion.
#'
#' @param table A data.frame read with [read_table()].
#'
#' @return The normalized data.frame.
#' @export
normalize_table <- function(table) {
    table <- as.data.frame(table)
    names(table) <- trimws(names(table))
    table <- drop_trailing_empty_rows(table)
    for (col in intersect(c('OD', 'conc', 'dilution'), names(table))) {
        if (is.logical(table[[col]]) && all(is.na(table[[col]]))) {
            table[[col]] <- as.numeric(table[[col]])
        }
    }
    if ('type' %in% names(table)) {
        type <- trimws(as.character(table$type))
        low <- tolower(type)
        simple <- low %in% c('standard', 'sample', 'blank')
        type[simple] <- low[simple]
        type[low %in% 'nc'] <- 'blank'  # NC and blank are equivalent
        type[low %in% 'qc'] <- 'QC'
        table$type <- type
        if (!'dilution' %in% names(table)) {
            table$dilution <- ifelse(type %in% c('sample', 'QC'), 1, NA_real_)
        }
    }
    if (!'conc' %in% names(table)) table$conc <- NA_real_
    if ('id' %in% names(table)) table$id <- as.character(table$id)
    if (!'sample.name' %in% names(table)) {
        table$sample.name <- if ('id' %in% names(table)) table$id else rep(NA_character_, nrow(table))
    }
    format_input_table(table)
}


#' Format Input Data Table
#'
#' Converts date and date-time columns to "dd.mm.yyyy" strings and coerces
#' `sample.name` to character.
#'
#' @param table A data.frame.
#'
#' @return The formatted data.frame.
#' @export
format_input_table <- function(table) {
    for (col in colnames(table)) {
        if (inherits(table[[col]], c('Date', 'POSIXt'))) {
            table[[col]] <- format(table[[col]], '%d.%m.%Y')
        }
    }
    table$sample.name <- as.character(table$sample.name)
    table
}


#' Save Data Frame as CSV File
#'
#' @param X A data.frame.
#' @param file Output path.
#' @param dialect `"comma"` (sep `,`, dec `.`) or `"semicolon"` (sep `;`, dec `,`).
#'
#' @return Called for its side effect.
#' @export
save_csv <- function(X, file, dialect = 'comma') {
    switch(dialect,
           'comma' = write.table(X, file, sep = ',', dec = '.',
                                 row.names = FALSE, na = ''),
           'semicolon' = write.table(X, file, sep = ';', dec = ',',
                                     row.names = FALSE, na = ''),
           lin_abort('error.csv_dialect', dialect = dialect)
    )
}


#' Save Data Frame as Excel File
#'
#' @param X A data.frame.
#' @param file Output path.
#'
#' @return Called for its side effect.
#' @export
save_xlsx <- function(X, file) {
    openxlsx::write.xlsx(X, file, rowNames = FALSE, overwrite = TRUE)
}


#' Save a Table as Excel or CSV File
#'
#' @param X A data.frame.
#' @param file Output path.
#' @param filetype `"xlsx"` or `"csv"`.
#' @param dialect CSV dialect, see [save_csv()].
#'
#' @return Called for its side effect.
#' @keywords internal
save_table <- function(X, file, filetype = 'xlsx', dialect = 'comma') {
    if (filetype == 'csv') {
        save_csv(X, file, dialect)
    } else {
        save_xlsx(X, file)
    }
}
