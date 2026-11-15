#' Plate Maps Format
#'
#' A workbook with one plate grid (rows A, B, ...; columns 1, 2, ...) per
#' column of the data set. The sheet name is the column name: `type`, `conc`,
#' `sample.name`, `sample.date`, `dilution`, `OD` or any other name, which
#' becomes an additional column. The `type` sheet is required: every well with
#' a type is a row of the data set (`standard`, `blank`, `NC`, `QC`,
#' `sample`). Cells that still contain the placeholder `X` are treated as empty.
#' Sheets without a grid are ignored.
#'
#' The template is written by [make_plate_maps()] and read by [read_assay()].
#'
#' @name plate_maps_format
NULL


#' Columns of a Data Set Prepared from a Layout
#'
#' @param layout Character matrix of well codes (see [plate_layout_codes()]).
#' @param conc Concentrations of the standard levels (`S1`, `S2`, ...).
#' @param by Filling direction: order of the rows of the result.
#'
#' @return A data.frame with `well`, `code`, `type` and `conc`, one row per
#'   used well.
#' @keywords internal
layout_wells <- function(layout, conc, by = 'column') {
    order <- fill_order(layout, by)
    order <- order[!is.na(layout[order])]
    well_names <- outer(rownames(layout), colnames(layout), paste0)
    code <- layout[order]
    type <- classify_codes(code)
    level <- suppressWarnings(as.integer(sub('^S', '', code)))
    data.frame(well = well_names[order], code = code, type = type,
               conc = ifelse(type %in% 'standard', conc[level], NA_real_))
}


#' Long Table Template from a Plate Layout
#'
#' @inheritParams layout_wells
#'
#' @return A data.frame with one row per used well: `well`, `sample.name`,
#'   `sample.date`, `type`, `dilution` (1 for samples), `conc` (standards) and
#'   `OD` (empty).
#' @export
#' @examples
#' layout <- LinearityApp:::plate_layout_codes(96, n.levels = 7, n.standard = 2, n.sample = 10)
#' head(make_long_table(layout, conc = 100 / 2^(0:6)))
make_long_table <- function(layout, conc, by = 'column') {
    w <- layout_wells(layout, conc, by)
    data.frame(well = w$well,
               sample.name = NA_character_,
               sample.date = NA_character_,
               type = w$type,
               dilution = ifelse(w$type %in% 'sample', 1, NA_real_),
               conc = w$conc,
               OD = NA_real_)
}


#' Write a Plate Maps Template
#'
#' Writes a workbook in the [plate_maps_format]: the `type` and `conc` sheets
#' are filled from the layout, the cells to fill in on the `sample.name`,
#' `sample.date`, `dilution` and `OD` sheets are marked with `X`.
#'
#' @param file Output `.xlsx` path.
#' @param layout Character matrix of well codes (see [plate_layout_codes()]).
#' @param conc Concentrations of the standard levels (`S1`, `S2`, ...).
#'
#' @return The path, invisibly.
#' @export
#' @examples
#' layout <- LinearityApp:::plate_layout_codes(96, n.levels = 7, n.standard = 2, n.sample = 10)
#' path <- tempfile(fileext = '.xlsx')
#' make_plate_maps(path, layout, conc = 100 / 2^(0:6))
make_plate_maps <- function(file, layout, conc) {
    used <- !is.na(layout)
    type <- matrix(classify_codes(layout), nrow = nrow(layout))
    level <- suppressWarnings(as.integer(sub('^S', '', layout)))
    conc_grid <- matrix(ifelse(type %in% 'standard', conc[level], NA_real_), nrow = nrow(layout))
    mark <- function(where) matrix(ifelse(where, 'X', NA_character_), nrow = nrow(layout))
    is_sample <- !is.na(type) & type == 'sample'

    wb <- openxlsx::createWorkbook()
    header <- openxlsx::createStyle(textDecoration = 'bold', halign = 'center',
                                    fgFill = '#DDEBF7', border = 'TopBottomLeftRight')
    cell <- openxlsx::createStyle(halign = 'center', border = 'TopBottomLeftRight')
    fills <- c(standard = '#CFE2F3', blank = '#E3E3E3', NC = '#D9D2E9', QC = '#FCE5CD',
               sample = '#D9EAD3', todo = '#FFF2CC')

    add_grid <- function(sheet, values, colour) {
        openxlsx::addWorksheet(wb, sheet)
        openxlsx::writeData(wb, sheet, t(colnames(layout)), startCol = 2, startRow = 1, colNames = FALSE)
        openxlsx::writeData(wb, sheet, rownames(layout), startCol = 1, startRow = 2, colNames = FALSE)
        openxlsx::writeData(wb, sheet, as.data.frame(values), startCol = 2, startRow = 2, colNames = FALSE)
        openxlsx::addStyle(wb, sheet, header, rows = 1, cols = seq_len(ncol(layout) + 1))
        openxlsx::addStyle(wb, sheet, header, rows = seq_len(nrow(layout)) + 1, cols = 1)
        openxlsx::addStyle(wb, sheet, cell, rows = seq_len(nrow(layout)) + 1,
                           cols = seq_len(ncol(layout)) + 1, gridExpand = TRUE)
        for (k in unique(colour[!is.na(colour)])) {
            idx <- which(!is.na(colour) & colour == k, arr.ind = TRUE)
            openxlsx::addStyle(wb, sheet, openxlsx::createStyle(fgFill = fills[[k]], halign = 'center',
                                                                border = 'TopBottomLeftRight'),
                               rows = idx[, 1] + 1, cols = idx[, 2] + 1, gridExpand = FALSE)
        }
        openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(layout) + 1), widths = 10)
    }
    todo <- function(where) matrix(ifelse(where, 'todo', NA_character_), nrow = nrow(layout))

    add_grid('sample.name', mark(is_sample), todo(is_sample))
    add_grid('sample.date', mark(is_sample), todo(is_sample))
    add_grid('type', type, type)
    add_grid('dilution', mark(is_sample), todo(is_sample))
    add_grid('conc', conc_grid, type)
    add_grid('OD', mark(used), todo(used))
    openxlsx::activeSheet(wb) <- 1

    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    invisible(file)
}


#' Is the File a Plate Maps Workbook?
#'
#' @param path File path.
#'
#' @return `TRUE` for an `.xlsx` file with a sheet named `type`.
#' @keywords internal
is_plate_maps_workbook <- function(path) {
    tolower(tools::file_ext(path)) == 'xlsx' && 'type' %in% tolower(trimws(readxl::excel_sheets(path)))
}


#' Read a Plate Maps Workbook
#'
#' See [plate_maps_format].
#'
#' @param path File path.
#'
#' @return A data.frame with one row per well with a type, columns `well`,
#'   `type` and one column per sheet with a grid, and an attribute `problems`.
#' @export
read_plate_maps <- function(path) {
    sheets <- readxl::excel_sheets(path)
    columns <- trimws(sheets)
    known <- c('sample.name', 'sample.date', 'type', 'dilution', 'conc', 'OD')
    canonical <- known[match(tolower(columns), tolower(known))]
    columns[!is.na(canonical)] <- canonical[!is.na(canonical)]

    type_sheet <- sheets[columns == 'type'][1]
    type <- read_grid(path, type_sheet)
    well_names <- outer(rownames(type), colnames(type), paste0)
    problems <- empty_problems()

    grids <- list()
    for (i in seq_along(sheets)) {
        if (columns[i] == 'type' || columns[i] %in% names(grids)) next
        grid <- tryCatch(read_grid(path, sheets[i]), error = function(e) NULL)
        if (is.null(grid)) next
        if (!identical(dim(grid), dim(type))) {
            problems <- add_problem(problems, 'error', loc_sheet(sheets[i]), 'error.maps_size_mismatch',
                                    size = paste(dim(grid), collapse = 'x'),
                                    type = paste(dim(type), collapse = 'x'))
            next
        }
        placeholder <- !is.na(grid) & toupper(grid) == 'X'
        if (any(placeholder & !is.na(type))) {
            problems <- add_problem(problems, 'warning', loc_sheet(sheets[i]), 'warning.placeholder_left',
                                    n = sum(placeholder & !is.na(type)))
        }
        grid[placeholder] <- NA
        orphan <- !is.na(grid) & is.na(type)
        if (any(orphan)) {
            problems <- add_problem(problems, 'warning', loc_sheet(sheets[i]), 'warning.values_without_type',
                                    wells = well_names[orphan])
        }
        grids[[columns[i]]] <- grid
    }

    used <- which(!is.na(type))
    rows <- (used - 1) %% nrow(type) + 1
    cols <- (used - 1) %/% nrow(type) + 1
    used <- used[order(rows, cols)]
    out <- data.frame(well = well_names[used], type = type[used])

    for (col in names(grids)) {
        values <- grids[[col]][used]
        if (col %in% c('OD', 'conc', 'dilution')) {
            number <- suppressWarnings(as.numeric(sub(',', '.', values, fixed = TRUE)))
            for (j in which(!is.na(values) & is.na(number))) {
                problems <- add_problem(problems, 'warning', loc_well(out$well[j]),
                                        'warning.value_not_numeric', sheet = col, value = values[j])
            }
            values <- number
        } else if (col == 'sample.date') {
            serial <- suppressWarnings(as.numeric(values))
            is_serial <- !is.na(serial)
            values[is_serial] <- format(as.Date(serial[is_serial], origin = '1899-12-30'), '%d.%m.%Y')
        } else {
            values <- utils::type.convert(values, as.is = TRUE)
        }
        out[[col]] <- values
    }

    out <- normalize_table(out)
    attr(out, 'problems') <- problems
    out
}
