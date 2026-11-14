#' Read a Semicolon- or Comma-Delimited CSV File
#'
#' Semicolon-separated files are expected to use a comma as decimal separator,
#' comma-separated files a dot.
#'
#' @param path File path.
#'
#' @return A data.frame.
#' @keywords internal
read_csv_auto <- function(path) {
    dialects <- list(c(sep = ';', dec = ','), c(sep = ',', dec = '.'))
    for (d in dialects) {
        table <- tryCatch(read.csv(path, sep = d[['sep']], dec = d[['dec']],
                                   stringsAsFactors = FALSE),
                          error = function(e) NULL)
        if (!is.null(table) && ncol(table) > 1) return(table)
    }
    lin_abort('error.csv_format')
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
