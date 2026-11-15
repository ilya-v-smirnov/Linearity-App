#' Plate Map
#'
#' An HTML grid of a plate with wells coloured by type. An editable map lets
#' the user select wells (see `inst/app/www/plate-editor.js`).
#'
#' @param codes Character matrix of well codes with row and column names
#'   (see [plate_layout_codes()]).
#' @param values Optional numeric matrix of the same shape shown under the codes.
#' @param types Optional character matrix of well types; by default derived
#'   from the codes (see [classify_codes()]).
#' @param lang Language code.
#' @param editable Whether wells can be selected.
#'
#' @return A shiny tag.
#' @keywords internal
plate_map <- function(codes, values = NULL, types = NULL, lang = 'en', editable = FALSE) {
    if (is.null(types)) {
        types <- matrix(classify_codes(codes), nrow = nrow(codes), dimnames = dimnames(codes))
    }
    header <- shiny::tags$tr(
        shiny::tags$th(class = 'plate-corner'),
        lapply(colnames(codes), function(col) shiny::tags$th(class = 'plate-col', `data-col` = col, col))
    )
    rows <- lapply(seq_len(nrow(codes)), function(i) {
        row <- rownames(codes)[i]
        cells <- lapply(seq_len(ncol(codes)), function(j) {
            well <- paste0(row, colnames(codes)[j])
            code <- codes[i, j]
            type <- types[i, j]
            if (is.na(code)) {
                return(shiny::tags$td(class = 'well-empty', title = well, `data-well` = well))
            }
            value <- if (!is.null(values) && !is.na(values[i, j])) {
                shiny::div(class = 'well-value', format(signif(values[i, j], 3)))
            }
            shiny::tags$td(class = paste0('well-', if (is.na(type)) 'unknown' else tolower(type)),
                           title = well, `data-well` = well,
                           shiny::div(class = 'well-code', code), value)
        })
        shiny::tags$tr(shiny::tags$th(class = 'plate-row', `data-row` = row, row), cells)
    })

    legend_types <- c('standard', 'blank', 'QC', 'sample')
    counts <- vapply(legend_types, function(x) sum(types == x, na.rm = TRUE), 0)
    legend <- lapply(legend_types[counts > 0], function(type) {
        shiny::span(class = paste0('legend-item well-', tolower(type)),
                    paste0(tr(paste0('data.type_', tolower(type)), lang), ' (', counts[[type]], ')'))
    })
    n_empty <- sum(is.na(codes))
    if (n_empty > 0) {
        legend <- c(legend, list(shiny::span(class = 'legend-item well-empty',
                                             paste0(tr('data.type_empty', lang), ' (', n_empty, ')'))))
    }

    shiny::div(
        class = 'plate-map-wrap',
        shiny::tags$table(class = paste('plate-map', if (ncol(codes) > 12) 'plate-384',
                                        if (editable) 'editable'),
                          shiny::tags$thead(header), shiny::tags$tbody(rows)),
        shiny::div(class = 'plate-legend', legend)
    )
}


#' Plate Grids of Imported Data
#'
#' @param data Validated data with a `well` column.
#'
#' @return A list of matrices `codes`, `values` and `types`, or `NULL` when the
#'   data has no wells.
#' @keywords internal
data_plate_grids <- function(data) {
    if (!'well' %in% names(data) || all(is.na(data$well))) return(NULL)
    row <- toupper(sub('^([A-Za-z]+).*$', '\\1', data$well))
    col <- as.integer(sub('^[A-Za-z]+', '', data$well))
    size <- if (any(row > 'H' | nchar(row) > 1, na.rm = TRUE) || any(col > 12, na.rm = TRUE)) 384 else 96
    dims <- plate_dims(size)
    empty <- matrix(NA_character_, nrow = length(dims$rows), ncol = length(dims$cols),
                    dimnames = list(dims$rows, as.character(dims$cols)))
    codes <- types <- empty
    values <- matrix(NA_real_, nrow = nrow(empty), ncol = ncol(empty), dimnames = dimnames(empty))
    idx <- cbind(match(row, dims$rows), match(col, dims$cols))
    ok <- stats::complete.cases(idx)
    code <- if ('id' %in% names(data)) data$id else rep(NA_character_, nrow(data))
    code[is.na(code)] <- data$type[is.na(code)]
    codes[idx[ok, , drop = FALSE]] <- code[ok]
    types[idx[ok, , drop = FALSE]] <- data$type[ok]
    values[idx[ok, , drop = FALSE]] <- data$OD[ok]
    list(codes = codes, values = values, types = types)
}


#' Order of Wells
#'
#' @param well Well names, e.g. `c("B1", "A2")`.
#' @param by `"column"` (A1, B1, ..., A2, B2, ...) or `"row"` (A1, A2, ...,
#'   B1, B2, ...).
#'
#' @return Integer permutation (as [order()]); missing wells come last.
#' @keywords internal
well_order <- function(well, by = 'column') {
    row <- match(toupper(sub('[0-9]+$', '', well)), LETTERS)
    col <- suppressWarnings(as.integer(sub('^[A-Za-z]+', '', well)))
    if (by == 'row') order(row, col) else order(col, row)
}


#' Columns that Identify a Sample
#'
#' `sample.name`, `sample.date` and any additional columns, i.e. all columns
#' except the technical ones (`well`, `type`, `dilution`, `conc`, `OD`, the
#' well code `id` and the legacy replicate number `rep`).
#'
#' @param data Input data.
#'
#' @return Character vector of column names.
#' @keywords internal
sample_columns <- function(data) {
    setdiff(names(data), c('well', 'type', 'dilution', 'conc', 'OD', 'id', 'rep'))
}


#' Summary Counts of Imported Data
#'
#' @param data Validated data.
#'
#' @return A named integer vector: `wells`, `levels`, `series`, `samples`
#'   (distinct combinations of [sample_columns()] among the samples), `qc`
#'   (distinct QC codes, or nominal concentrations without codes) and `blanks`.
#' @keywords internal
data_counts <- function(data) {
    is_type <- function(x) !is.na(data$type) & data$type == x
    std <- data[is_type('standard'), , drop = FALSE]
    smp <- data[is_type('sample'), sample_columns(data), drop = FALSE]
    qc_key <- if ('id' %in% names(data)) data$id[is_type('QC')] else data$conc[is_type('QC')]
    n_levels <- length(unique(std$conc))
    c(wells = nrow(data),
      levels = n_levels,
      series = if (n_levels > 0) as.integer(round(nrow(std) / n_levels)) else 0L,
      samples = if (ncol(smp) > 0) nrow(unique(smp)) else nrow(smp),
      qc = length(unique(qc_key)),
      blanks = sum(is_background(data$type)))
}


#' Summary Tiles
#'
#' @param counts Output of [data_counts()].
#' @param has_wells Whether the data come from a plate (label "wells" or "rows").
#' @param lang Language code.
#'
#' @return A shiny tag.
#' @keywords internal
data_summary_ui <- function(counts, has_wells, lang) {
    tile <- function(value, key) {
        shiny::div(class = 'stat-tile', shiny::div(class = 'stat-value', value),
                   shiny::div(class = 'stat-label', tr(key, lang)))
    }
    shiny::div(class = 'stat-tiles',
               tile(counts[['wells']], if (has_wells) 'data.n_wells' else 'data.n_rows'),
               tile(paste0(counts[['levels']], ' \u00d7 ', counts[['series']]), 'data.n_standards'),
               tile(counts[['samples']], 'data.n_samples'),
               tile(counts[['qc']], 'data.n_qc'),
               tile(counts[['blanks']], 'data.n_blanks'))
}
