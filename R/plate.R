#' Plate Format
#'
#' The plate workbook has the sheets:
#'
#' * `OD` - optical densities as a plate grid (rows A, B, ...; columns 1, 2, ...),
#'   pasted from the plate reader export;
#' * `Layout` - the same grid with well codes: `S1`...`Sn` standards, `B` blank,
#'   `NC` negative control, `QC...` quality controls (e.g. `QC-L`), `X1`...`Xn`
#'   samples (or any id listed on the `Samples` sheet); empty cells are not used;
#' * `Standards` - columns `level` (`S1` or `1`) and `conc`;
#' * `Samples` - columns `id`, `name`, `dilution` and any metadata (e.g. `date`);
#' * `QC` - columns `id` and `conc` (nominal concentration).
#'
#' Sheet names may be in any supported language (see [supported_languages()]);
#' column names and well codes are the same in all languages. Replicates are
#' wells with the same code.
#'
#' @name plate_format
NULL


#' Plate Dimensions
#'
#' @param size 96 or 384.
#'
#' @return A list with `rows` (letters) and `cols` (integers).
#' @keywords internal
plate_dims <- function(size = 96) {
    switch(as.character(size),
           '96' = list(rows = LETTERS[1:8], cols = 1:12),
           '384' = list(rows = LETTERS[1:16], cols = 1:24),
           stop('Plate size must be 96 or 384', call. = FALSE))
}


#' Names of a Plate Sheet in All Languages
#' @keywords internal
sheet_names <- function(kind) {
    unique(vapply(supported_languages(), function(l) tr(paste0('plate.sheet_', kind), l), ''))
}


#' Find a Sheet of a Given Kind
#' @keywords internal
find_sheet <- function(sheets, kind) {
    hit <- sheets[tolower(sheets) %in% tolower(sheet_names(kind))]
    if (length(hit) == 0) NA_character_ else hit[1]
}


#' Is the File a Plate Workbook?
#'
#' @param path File path.
#'
#' @return `TRUE` for an `.xlsx` file with a layout sheet.
#' @keywords internal
is_plate_workbook <- function(path) {
    tolower(tools::file_ext(path)) == 'xlsx' &&
        !is.na(find_sheet(readxl::excel_sheets(path), 'layout'))
}


#' Read a Plate Grid from a Sheet
#'
#' Finds a cell `A` (row label) with `1` (column label) diagonally above it and
#' reads the grid of consecutive row letters and column numbers. The grid may
#' start anywhere on the sheet.
#'
#' @param path File path.
#' @param sheet Sheet name.
#'
#' @return A character matrix with row names (letters) and column names (numbers).
#' @keywords internal
read_grid <- function(path, sheet) {
    raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                                               col_types = 'text', .name_repair = 'minimal'))
    m <- unname(as.matrix(raw))
    if (length(m) == 0) lin_abort('error.plate_no_grid', sheet = sheet)
    m <- trimws(m)
    m[!is.na(m) & m == ''] <- NA

    starts <- which(!is.na(m) & toupper(m) == 'A', arr.ind = TRUE)
    for (k in seq_len(nrow(starts))) {
        r <- starts[k, 1]
        c <- starts[k, 2]
        if (r < 2 || c >= ncol(m) || !identical(m[r - 1, c + 1], '1')) next

        n_rows <- 0
        while (r + n_rows <= nrow(m) && n_rows < 26 &&
               identical(toupper(m[r + n_rows, c]), LETTERS[n_rows + 1])) {
            n_rows <- n_rows + 1
        }
        n_cols <- 0
        while (c + n_cols + 1 <= ncol(m) && identical(m[r - 1, c + n_cols + 1], as.character(n_cols + 1))) {
            n_cols <- n_cols + 1
        }
        grid <- m[r:(r + n_rows - 1), (c + 1):(c + n_cols), drop = FALSE]
        dimnames(grid) <- list(LETTERS[seq_len(n_rows)], seq_len(n_cols))
        return(grid)
    }
    lin_abort('error.plate_no_grid', sheet = sheet)
}


#' Read an Optional Table Sheet
#' @keywords internal
read_sheet_table <- function(path, sheet) {
    if (is.na(sheet)) return(NULL)
    tab <- as.data.frame(readxl::read_excel(path, sheet = sheet))
    names(tab) <- tolower(trimws(names(tab)))
    tab
}


#' Problems Found in Input Data
#'
#' Problems are kept as a list of records with `severity` (`"error"` or
#' `"warning"`), `location` (a `lin_message` or `NULL`) and `message`
#' (a `lin_message`). See [problems_table()] for display.
#'
#' @param problems A list of problems.
#' @param severity `"error"` or `"warning"`.
#' @param location A `lin_message` describing the location, or `NULL`.
#' @param key,... Message key and parameters.
#'
#' @return `empty_problems()` returns an empty list, `add_problem()` the list
#'   with a new problem.
#' @name problems
#' @keywords internal
NULL

#' @rdname problems
empty_problems <- function() list()

#' @rdname problems
add_problem <- function(problems, severity, location, key, ...) {
    c(problems, list(list(severity = severity, location = location,
                          message = lin_message(key, ...))))
}


#' Location Messages
#' @keywords internal
loc_well <- function(well) lin_message('location.well', well = well)

#' @rdname loc_well
loc_wells <- function(wells) {
    if (length(wells) == 1) loc_well(wells) else lin_message('location.wells', wells = wells)
}

#' @rdname loc_well
loc_rows <- function(rows) {
    if (length(rows) == 1) loc_row(rows) else lin_message('location.rows', rows = rows)
}

#' @rdname loc_well
loc_row <- function(row) lin_message('location.row', row = row)

#' @rdname loc_well
loc_sheet <- function(sheet) lin_message('location.sheet', sheet = sheet)


#' Classify Well Codes
#'
#' @param code Upper-case well codes.
#' @param sample_ids Upper-case sample ids from the Samples sheet.
#'
#' @return Character vector of types (`NA` for unknown codes).
#' @keywords internal
classify_codes <- function(code, sample_ids = character(0)) {
    type <- rep(NA_character_, length(code))
    type[grepl('^X[0-9]+$', code) | code %in% sample_ids] <- 'sample'
    type[grepl('^QC', code)] <- 'QC'
    type[code %in% c('B', 'BLANK')] <- 'blank'
    type[code == 'NC'] <- 'NC'
    type[grepl('^S[0-9]+$', code)] <- 'standard'
    type
}


#' Read a Plate Workbook
#'
#' Converts a plate workbook (see [plate_format]) into the table used by the
#' calculations: one row per well with columns `well`, `type`, `id`,
#' `sample.name`, `dilution`, `conc`, `OD` and sample metadata.
#'
#' @param path Path to the `.xlsx` file.
#'
#' @return A data.frame with an attribute `problems` (warnings and errors found
#'   while reading; see [check_assay()]).
#' @export
#' @examples
#' path <- system.file('extdata', 'bca_plate.xlsx', package = 'LinearityApp')
#' head(read_plate(path))
read_plate <- function(path) {
    sheets <- readxl::excel_sheets(path)
    layout_sheet <- find_sheet(sheets, 'layout')
    od_sheet <- find_sheet(sheets, 'od')
    if (is.na(layout_sheet)) lin_abort('error.plate_sheet_missing', sheet = tr('plate.sheet_layout', 'en'))
    if (is.na(od_sheet)) lin_abort('error.plate_sheet_missing', sheet = tr('plate.sheet_od', 'en'))

    layout <- read_grid(path, layout_sheet)
    od <- read_grid(path, od_sheet)
    if (!identical(dim(layout), dim(od))) {
        lin_abort('error.plate_size_mismatch',
                  layout = paste(dim(layout), collapse = 'x'), od = paste(dim(od), collapse = 'x'))
    }

    problems <- empty_problems()
    wells <- data.frame(
        well = as.vector(outer(rownames(layout), colnames(layout), paste0)),
        row = rep(seq_len(nrow(layout)), times = ncol(layout)),
        col = rep(seq_len(ncol(layout)), each = nrow(layout)),
        code = toupper(as.vector(layout)),
        od_raw = as.vector(od)
    )
    wells <- wells[order(wells$row, wells$col), ]

    # Optical densities: decimal comma allowed, text values become missing
    wells$OD <- suppressWarnings(as.numeric(sub(',', '.', wells$od_raw, fixed = TRUE)))
    bad_od <- !is.na(wells$od_raw) & is.na(wells$OD) & !is.na(wells$code)
    for (i in which(bad_od)) {
        problems <- add_problem(problems, 'warning', loc_well(wells$well[i]),
                                'warning.od_not_numeric', value = wells$od_raw[i])
    }
    orphan <- !is.na(wells$od_raw) & is.na(wells$code)
    if (any(orphan)) {
        problems <- add_problem(problems, 'warning', NULL, 'warning.od_without_code',
                                wells = wells$well[orphan])
    }
    wells <- wells[!is.na(wells$code), ]

    standards <- read_sheet_table(path, find_sheet(sheets, 'standards'))
    samples <- read_sheet_table(path, find_sheet(sheets, 'samples'))
    qc <- read_sheet_table(path, find_sheet(sheets, 'qc'))

    sample_ids <- if (!is.null(samples) && 'id' %in% names(samples)) {
        toupper(trimws(as.character(samples$id)))
    } else {
        character(0)
    }

    wells$type <- classify_codes(wells$code, sample_ids)
    for (i in which(is.na(wells$type))) {
        problems <- add_problem(problems, 'error', loc_well(wells$well[i]),
                                'error.unknown_code', code = wells$code[i])
    }
    wells <- wells[!is.na(wells$type), ]

    out <- data.frame(well = wells$well, type = wells$type, id = wells$code,
                      sample.name = NA_character_, dilution = NA_real_, conc = NA_real_,
                      OD = wells$OD)

    # Standards
    is_std <- out$type %in% 'standard'
    if (any(is_std)) {
        std_conc <- stats::setNames(numeric(0), character(0))
        if (!is.null(standards) && all(c('level', 'conc') %in% names(standards))) {
            level <- toupper(trimws(as.character(standards$level)))
            level <- ifelse(grepl('^[0-9]+$', level), paste0('S', level), level)
            std_conc <- stats::setNames(suppressWarnings(as.numeric(standards$conc)), level)
        } else {
            problems <- add_problem(problems, 'error', loc_sheet(tr('plate.sheet_standards', 'en')),
                                    'error.sheet_columns', columns = c('level', 'conc'))
        }
        out$conc[is_std] <- unname(std_conc[out$id[is_std]])
    }

    # Quality controls
    is_qc <- out$type %in% 'QC'
    if (any(is_qc)) {
        qc_conc <- stats::setNames(numeric(0), character(0))
        if (!is.null(qc) && all(c('id', 'conc') %in% names(qc))) {
            qc_conc <- stats::setNames(suppressWarnings(as.numeric(qc$conc)),
                                       toupper(trimws(as.character(qc$id))))
        }
        out$conc[is_qc] <- unname(qc_conc[out$id[is_qc]])
        out$dilution[is_qc] <- 1
        out$sample.name[is_qc] <- out$id[is_qc]
    }

    # Samples and their metadata
    is_sample <- out$type %in% 'sample'
    out$dilution[is_sample] <- 1
    out$sample.name[is_sample] <- out$id[is_sample]
    if (!is.null(samples) && 'id' %in% names(samples)) {
        samples$id <- sample_ids
        names(samples)[names(samples) == 'name'] <- 'sample.name'
        names(samples)[names(samples) == 'date'] <- 'sample.date'
        idx <- match(out$id, samples$id)
        if ('sample.name' %in% names(samples)) {
            nm <- as.character(samples$sample.name[idx])
            use <- is_sample & !is.na(nm) & nzchar(nm)
            out$sample.name[use] <- nm[use]
        }
        if ('dilution' %in% names(samples)) {
            described <- is_sample & !is.na(idx)
            dil <- suppressWarnings(as.numeric(samples$dilution[idx]))
            out$dilution[described] <- dil[described]
        }
        meta <- setdiff(names(samples), c('id', 'sample.name', 'dilution'))
        for (col in meta) {
            out[[col]] <- samples[[col]][idx]
            out[[col]][!is_sample] <- NA
        }
        for (id in setdiff(unique(out$id[is_sample]), samples$id)) {
            problems <- add_problem(problems, 'warning', NULL, 'warning.sample_not_described', id = id)
        }
        for (id in setdiff(samples$id[!is.na(samples$id)], out$id)) {
            problems <- add_problem(problems, 'warning', NULL, 'warning.sample_not_on_plate', id = id)
        }
    } else if (any(is_sample)) {
        problems <- add_problem(problems, 'warning', NULL, 'warning.no_samples_sheet')
    }

    rownames(out) <- NULL
    out <- format_input_table(out)
    attr(out, 'problems') <- problems
    out
}


#' Arrange Codes on a Plate
#'
#' @keywords internal
plate_layout_codes <- function(size, n.levels, n.standard, n.sample, sample_reps) {
    dims <- plate_dims(size)
    # each standard series starts in a new column
    series <- c(paste0('S', seq_len(n.levels)), 'B')
    series <- c(series, rep(NA_character_, (-length(series)) %% length(dims$rows)))
    codes <- c(rep(series, times = n.standard),
               rep(paste0('X', seq_len(n.sample)), each = sample_reps))
    n_wells <- length(dims$rows) * length(dims$cols)
    if (length(codes) > n_wells) lin_abort('error.plate_too_small', size = size, n = length(codes))
    grid <- matrix(NA_character_, nrow = length(dims$rows), ncol = length(dims$cols),
                   dimnames = list(dims$rows, dims$cols))
    grid[seq_along(codes)] <- codes  # column by column
    grid
}


#' Create a Plate Template Workbook
#'
#' Standards (in `n.standard` series) and a blank are placed column by column,
#' followed by the samples in `sample_reps` replicates. The layout can be edited
#' in the workbook before the measurement.
#'
#' @param file Output `.xlsx` path.
#' @param size Plate size: 96 or 384.
#' @param start Highest standard concentration.
#' @param step Dilution step of the standard series.
#' @param n.levels Number of standard levels.
#' @param n.standard Number of standard series.
#' @param n.sample Number of samples.
#' @param sample_reps Replicates per sample.
#' @param lang Language of sheet names and instructions.
#'
#' @return The path, invisibly.
#' @export
#' @examples
#' path <- tempfile(fileext = '.xlsx')
#' make_plate_template(path, n.sample = 20, lang = 'ru')
make_plate_template <- function(file, size = 96, start = 100, step = 2, n.levels = 7,
                                 n.standard = 2, n.sample = 10, sample_reps = 2, lang = 'en') {
    lang <- match_lang(lang)
    standards <- data.frame(level = paste0('S', seq_len(n.levels)),
                            conc = standard_series(start, step, n.levels))
    layout <- plate_layout_codes(size, n.levels, n.standard, n.sample, sample_reps)
    dims <- plate_dims(size)

    wb <- openxlsx::createWorkbook()
    header <- openxlsx::createStyle(textDecoration = 'bold', halign = 'center',
                                    fgFill = '#DDEBF7', border = 'TopBottomLeftRight')
    cell <- openxlsx::createStyle(halign = 'center', border = 'TopBottomLeftRight')

    add_grid <- function(sheet, values) {
        openxlsx::addWorksheet(wb, sheet)
        openxlsx::writeData(wb, sheet, t(as.character(dims$cols)), startCol = 2, startRow = 1,
                            colNames = FALSE)
        openxlsx::writeData(wb, sheet, dims$rows, startCol = 1, startRow = 2, colNames = FALSE)
        if (!is.null(values)) {
            openxlsx::writeData(wb, sheet, as.data.frame(values), startCol = 2, startRow = 2,
                                colNames = FALSE)
        }
        openxlsx::addStyle(wb, sheet, header, rows = 1, cols = seq_len(length(dims$cols) + 1))
        openxlsx::addStyle(wb, sheet, header, rows = seq_along(dims$rows) + 1, cols = 1)
        openxlsx::addStyle(wb, sheet, cell, rows = seq_along(dims$rows) + 1,
                           cols = seq_along(dims$cols) + 1, gridExpand = TRUE)
        openxlsx::setColWidths(wb, sheet, cols = seq_len(length(dims$cols) + 1), widths = 8)
    }
    add_table <- function(sheet, data) {
        openxlsx::addWorksheet(wb, sheet)
        openxlsx::writeData(wb, sheet, data, headerStyle = header)
        openxlsx::setColWidths(wb, sheet, cols = seq_along(data), widths = 14)
    }

    openxlsx::addWorksheet(wb, tr('plate.sheet_instructions', lang))
    help_keys <- paste0('plate.help_', 1:8)
    openxlsx::writeData(wb, tr('plate.sheet_instructions', lang),
                        vapply(help_keys, tr, '', lang = lang), colNames = FALSE)
    openxlsx::setColWidths(wb, tr('plate.sheet_instructions', lang), cols = 1, widths = 120)

    add_grid(tr('plate.sheet_layout', lang), layout)
    add_grid(tr('plate.sheet_od', lang), NULL)
    add_table(tr('plate.sheet_standards', lang), standards)
    add_table(tr('plate.sheet_samples', lang),
              data.frame(id = paste0('X', seq_len(n.sample)), name = NA_character_,
                         dilution = rep(1, n.sample), date = NA_character_))
    add_table(tr('plate.sheet_qc', lang), data.frame(id = character(0), conc = numeric(0)))
    openxlsx::activeSheet(wb) <- 3

    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    invisible(file)
}
