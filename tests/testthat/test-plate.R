# Writes a plate workbook; grids are matrices with row letters / column numbers
write_plate <- function(layout, od, standards = NULL, samples = NULL, qc = NULL,
                        start_row = 1, start_col = 1, names = c(layout = 'Layout', od = 'OD',
                                                                 standards = 'Standards',
                                                                 samples = 'Samples', qc = 'QC')) {
    wb <- openxlsx::createWorkbook()
    add_grid <- function(sheet, values) {
        openxlsx::addWorksheet(wb, sheet)
        openxlsx::writeData(wb, sheet, t(as.character(seq_len(ncol(values)))),
                            startRow = start_row, startCol = start_col + 1, colNames = FALSE)
        openxlsx::writeData(wb, sheet, LETTERS[seq_len(nrow(values))],
                            startRow = start_row + 1, startCol = start_col, colNames = FALSE)
        openxlsx::writeData(wb, sheet, as.data.frame(values),
                            startRow = start_row + 1, startCol = start_col + 1, colNames = FALSE)
    }
    add_grid(names[['layout']], layout)
    add_grid(names[['od']], od)
    for (kind in c('standards', 'samples', 'qc')) {
        tab <- get(kind)
        if (!is.null(tab)) {
            openxlsx::addWorksheet(wb, names[[kind]])
            openxlsx::writeData(wb, names[[kind]], tab)
        }
    }
    path <- tempfile(fileext = '.xlsx')
    openxlsx::saveWorkbook(wb, path)
    path
}

# A small plate: 6 standard levels in duplicate (columns 1-2), NC in row G,
# samples X1-X3 in duplicate in columns 3-4; OD = 0.1 * conc + 0.05
small_plate <- function(extra_layout = NULL, extra_od = NULL) {
    conc <- c(32, 16, 8, 4, 2, 1)
    layout <- matrix(NA_character_, 7, 4)
    od <- matrix(NA_real_, 7, 4)
    layout[, 1:2] <- c(paste0('S', 1:6), 'NC')
    od[, 1:2] <- c(0.1 * conc + 0.05, 0.05)
    layout[1:3, 3:4] <- paste0('X', 1:3)
    od[1:3, 3:4] <- 0.1 * c(5, 10, 20) + 0.05
    list(layout = layout, od = od,
         standards = data.frame(level = paste0('S', 1:6), conc = conc),
         samples = data.frame(id = paste0('X', 1:3), name = c('a', 'b', 'c'), dilution = c(1, 2, 1),
                              date = as.Date('2026-11-14')))
}

test_that('example plate gives the same results as the table format', {
    plate <- check_assay(read_assay(sample_path('bca_plate.xlsx')))
    expect_true(all(c('well', 'id') %in% names(plate)))
    # the NC wells of the example are negative controls, kept apart from reagent blanks
    expect_equal(as.vector(table(plate$type)[c('NC', 'sample', 'standard')]), c(2, 24, 14))

    r_plate <- calculate(plate, 'QRM')
    r_table <- calculate(load_sample('bca'), 'QRM')
    a_plate <- aggregate_result(r_plate$result, c('sample.name', 'dilution'))
    a_table <- aggregate_result(r_table$result, c('sample.name', 'dilution'))
    expect_equal(a_plate, a_table)
    expect_true(all(c('well', 'id') %in% names(r_plate$result)))
    expect_false(any(c('well', 'id') %in% names(r_table$result)))
})

test_that('plate template can be filled in and read back in any language', {
    for (lang in supported_languages()) {
        path <- tempfile(fileext = '.xlsx')
        make_plate_template(path, start = 32, step = 2, n.levels = 6, n.standard = 2,
                            n.sample = 3, sample_reps = 2, lang = lang)
        sheets <- readxl::excel_sheets(path)
        expect_true(tr('plate.sheet_layout', lang) %in% sheets)

        layout <- LinearityApp:::read_grid(path, tr('plate.sheet_layout', lang))
        expect_equal(dim(layout), c(8, 12))
        expect_equal(unname(layout[1:7, 1]), paste0('S', 1:7)[1:7] |> replace(7, 'B'))

        conc <- c(32, 16, 8, 4, 2, 1)
        od <- matrix(NA_real_, 8, 12)
        od[1:7, 1:2] <- c(0.1 * conc + 0.05, 0.05)
        od[1:6, 3] <- 0.1 * c(5, 5, 10, 10, 20, 20) + 0.05
        wb <- openxlsx::loadWorkbook(path)
        openxlsx::writeData(wb, tr('plate.sheet_od', lang), as.data.frame(od),
                            startRow = 2, startCol = 2, colNames = FALSE)
        openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

        data <- check_assay(read_assay(path))
        r <- calculate(data, 'LRM', linear = TRUE, round_fun = function(x, ...) x)
        expect_equal(r$result$conc, c(5, 5, 10, 10, 20, 20), tolerance = 1e-8, info = lang)
    }
})

test_that('grid may be offset; decimal commas and text values are handled', {
    p <- small_plate()
    od <- matrix(as.character(p$od), nrow(p$od))
    od[1, 3] <- '1,05'           # decimal comma
    od[2, 4] <- 'OVRFLW'         # reader overflow
    od[7, 4] <- '0.9'            # OD without layout code
    path <- write_plate(p$layout, od, p$standards, p$samples, start_row = 3, start_col = 2)

    data <- read_assay(path)
    expect_equal(data$OD[data$well == 'A3'], 1.05)
    expect_true(is.na(data$OD[data$well == 'B4']))
    tab <- problems_table(attr(data, 'problems'))
    expect_equal(nrow(tab), 2)
    expect_match(tab$Problem, "Non-numeric OD value 'OVRFLW'", fixed = TRUE, all = FALSE)
    expect_match(tab$Problem, 'ignored: G4', fixed = TRUE, all = FALSE)

    data <- check_assay(data)
    r <- calculate(data, 'LRM', linear = TRUE, round_fun = function(x, ...) x)
    expect_equal(r$result$flag[r$result$well == 'B4'], 'Missing OD')
    expect_equal(r$result$conc[r$result$well == 'A3'], 10)
    expect_equal(r$result$conc[r$result$well == 'B3'], 20)   # dilution 2
    expect_equal(unique(r$result$sample.date), '14.11.2026')
})

test_that('all problems of a plate are reported at once', {
    p <- small_plate()
    p$layout[7, 3] <- 'Y1'                  # unknown code
    p$layout[5, 3:4] <- 'X9'                # sample not on the Samples sheet (warning)
    p$od[5, 3:4] <- 0.5
    p$standards <- p$standards[-6, ]        # S6 has no concentration
    path <- write_plate(p$layout, p$od, p$standards, p$samples)

    cnd <- tryCatch(check_assay(read_assay(path)), error = identity)
    expect_s3_class(cnd, 'lin_error')
    expect_equal(cnd$key, 'error.invalid_input_many')
    tab <- problems_table(cnd$problems, 'en')
    expect_equal(sum(tab$Severity == 'Error'), 2)
    expect_equal(tab$Severity[1:2], c('Error', 'Error'))
    expect_match(tab$Problem, 'Unknown well code: Y1', fixed = TRUE, all = FALSE)
    expect_match(tab$Location, 'well G3', fixed = TRUE, all = FALSE)
    expect_match(tab$Location, 'wells F1, F2', fixed = TRUE, all = FALSE)
    expect_match(tab$Problem, 'Sample X9 is not described', fixed = TRUE, all = FALSE)
    expect_match(conditionMessage(cnd), 'and 1 more error', fixed = TRUE)

    ru <- problems_table(cnd$problems, 'ru')
    expect_equal(names(ru), c(tr('problem.severity', 'ru'), tr('problem.location', 'ru'),
                              tr('problem.message', 'ru')))
})

test_that('structural errors of a plate workbook are reported', {
    p <- small_plate()
    path <- write_plate(p$layout, p$od[1:6, ], p$standards, p$samples)
    expect_error(read_assay(path), 'different sizes')

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, 'Layout')
    openxlsx::writeData(wb, 'Layout', data.frame(x = 1))
    path <- tempfile(fileext = '.xlsx')
    openxlsx::saveWorkbook(wb, path)
    expect_error(read_assay(path), 'Sheet OD is missing')

    openxlsx::addWorksheet(wb, 'OD')
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    expect_error(read_assay(path), 'No plate grid')

    path <- write_plate(p$layout, p$od, standards = data.frame(x = 1), samples = p$samples)
    cnd <- tryCatch(check_assay(read_assay(path)), error = identity)
    expect_match(problems_table(cnd$problems)$Problem, 'Required columns are missing: level, conc',
                 fixed = TRUE, all = FALSE)
})

test_that('sheets are optional where possible', {
    p <- small_plate()
    path <- write_plate(p$layout, p$od, p$standards)
    data <- check_assay(read_assay(path))
    expect_equal(unique(data$dilution[data$type == 'sample']), 1)
    expect_equal(unique(data$sample.name[data$type == 'sample']), paste0('X', 1:3))
    expect_match(problems_table(attr(data, 'problems'))$Problem, 'There is no Samples sheet', fixed = TRUE)

    extra <- rbind(p$samples, data.frame(id = 'X7', name = 'g', dilution = 1, date = NA))
    path <- write_plate(p$layout, p$od, p$standards, extra)
    data <- check_assay(read_assay(path))
    expect_match(problems_table(attr(data, 'problems'))$Problem, 'X7 from the Samples sheet', fixed = TRUE)
})

test_that('sheet names are recognised in Russian and sample ids may be any text', {
    p <- small_plate()
    p$layout[1:3, 3:4] <- c('ctrl', 'mouse 2', 'X3')
    p$samples$id <- c('CTRL', 'mouse 2', 'X3')
    ru_names <- c(layout = tr('plate.sheet_layout', 'ru'), od = 'OD',
                  standards = tr('plate.sheet_standards', 'ru'),
                  samples = tr('plate.sheet_samples', 'ru'), qc = 'QC')
    path <- write_plate(p$layout, p$od, p$standards, p$samples, names = ru_names)
    data <- check_assay(read_assay(path))
    expect_equal(sort(unique(data$sample.name[data$type == 'sample'])), c('a', 'b', 'c'))
})

test_that('quality controls get recovery', {
    p <- small_plate()
    p$layout[4:5, 3:4] <- c('QC-L', 'QC-H')
    p$od[4:5, 3:4] <- 0.1 * c(3, 24) + 0.05
    qc <- data.frame(id = c('QC-L', 'QC-H'), conc = c(2.5, 24))
    path <- write_plate(p$layout, p$od, p$standards, p$samples, qc)
    r <- calculate(check_assay(read_assay(path)), 'LRM', linear = TRUE)
    expect_equal(nrow(r$qc), 4)
    expect_equal(names(r$qc), c('well', 'id', 'nominal', 'conc', 'recovery', 'OD', 'flag'))
    expect_equal(r$qc$recovery[r$qc$id == 'QC-L'], c(120, 120))
    expect_equal(r$qc$recovery[r$qc$id == 'QC-H'], c(100, 100))
    expect_false(any(r$result$id %in% c('QC-L', 'QC-H')))

    path <- write_plate(p$layout, p$od, p$standards, p$samples, qc[1, ])
    expect_error(check_assay(read_assay(path)), 'nominal concentrations')
})

test_that('384-well layouts are generated and too large layouts rejected', {
    grid <- LinearityApp:::plate_layout_codes(384, n.levels = 8, n.standard = 3, n.sample = 100,
                                              sample_reps = 3)
    expect_equal(dim(grid), c(16, 24))
    expect_equal(sum(grid == 'X100', na.rm = TRUE), 3)
    expect_error(LinearityApp:::plate_layout_codes(96, 7, 2, 50, 2), 'does not fit')
    expect_error(LinearityApp:::plate_dims(48), '96 or 384')
})

test_that('layouts are filled by column or row with the standards before or after the samples', {
    by_col <- LinearityApp:::plate_layout_codes(96, 7, 2, 10)
    expect_equal(unname(by_col[, 1]), c(paste0('S', 1:7), 'B'))
    expect_equal(unname(by_col[, 3]), paste0('X', 1:8))
    expect_equal(unname(by_col[1:2, 4]), c('X9', 'X10'))

    by_row <- LinearityApp:::plate_layout_codes(96, 7, 2, 10, by = 'row')
    expect_equal(unname(by_row['A', 1:8]), c(paste0('S', 1:7), 'B'))
    expect_true(all(is.na(by_row['A', 9:12])))
    expect_equal(unname(by_row['C', 1:10]), paste0('X', 1:10))

    after <- LinearityApp:::plate_layout_codes(96, 7, 2, 10, standards = 'after')
    expect_equal(unname(after[, 1]), paste0('X', 1:8))
    expect_equal(unname(after[, 3]), c(paste0('S', 1:7), 'B'))
    expect_equal(sum(!is.na(after)), 26)
    expect_error(LinearityApp:::plate_layout_codes(96, 7, 2, 10, by = 'diagonal'))
})

test_that('layout edits assign levels in the filling order and renumber samples', {
    grid <- LinearityApp:::plate_layout_codes(96, 5, 1, 4)
    std <- LinearityApp:::edit_layout(grid, c('A4', 'B4', 'A5', 'B5'), 'standard', n.levels = 3)
    expect_equal(unname(std[1:2, 4]), c('S1', 'S2'))
    expect_equal(unname(std[1:2, 5]), c('S3', 'S1'))
    std_row <- LinearityApp:::edit_layout(grid, c('A4', 'B4', 'A5', 'B5'), 'standard', n.levels = 3, by = 'row')
    expect_equal(unname(std_row['A', 4:5]), c('S1', 'S2'))

    smp <- LinearityApp:::edit_layout(grid, c('A1', 'H12'), 'sample')
    expect_equal(smp[['A', 1]], 'X1')
    expect_equal(unname(smp[1:4, 2]), paste0('X', 2:5))
    expect_equal(smp[['H', 12]], 'X6')
    expect_true(is.na(LinearityApp:::edit_layout(grid, 'A1', 'empty')[['A', 1]]))
    expect_equal(LinearityApp:::edit_layout(grid, 'G1', 'blank')[['G', 1]], 'B')
})

test_that('the number of samples of a layout is changed from its end', {
    grid <- LinearityApp:::plate_layout_codes(96, 7, 2, 10, standards = 'after')
    more <- LinearityApp:::resize_samples(grid, 12)
    expect_equal(unname(more[3:4, 2]), c('X11', 'X12'))
    expect_equal(unname(more[, 3]), c(paste0('S', 1:7), 'B'))
    less <- LinearityApp:::resize_samples(more, 3)
    expect_equal(LinearityApp:::count_samples(less), 3)
    expect_equal(unname(less[1:3, 1]), paste0('X', 1:3))
    full <- LinearityApp:::plate_layout_codes(96, 7, 2, 80)
    expect_error(LinearityApp:::resize_samples(full, 81), 'does not fit')
    # new samples wrap to the empty wells before the last sample
    gap <- LinearityApp:::edit_layout(grid, c('A1', 'B1'), 'empty')
    gap <- LinearityApp:::edit_layout(gap, 'H12', 'sample')
    wrapped <- LinearityApp:::resize_samples(gap, 11)
    expect_equal(wrapped[['A', 1]], 'X1')
    expect_equal(wrapped[['H', 12]], 'X11')
})

test_that('layouts are shifted while no used well leaves the plate', {
    grid <- LinearityApp:::plate_layout_codes(96, 7, 1, 4)
    expect_null(LinearityApp:::shift_layout(grid, 'left'))
    expect_null(LinearityApp:::shift_layout(grid, 'up'))
    right <- LinearityApp:::shift_layout(grid, 'right')
    expect_equal(unname(right[, 2]), unname(grid[, 1]))
    down <- LinearityApp:::shift_layout(LinearityApp:::edit_layout(grid, 'H1', 'empty'), 'down')
    expect_equal(down[['B', 1]], 'S1')
    expect_identical(LinearityApp:::shift_layout(right, 'left'), grid)
})

test_that('quality controls are placed by level and trimmed with the number of levels', {
    grid <- LinearityApp:::plate_layout_codes(96, 7, 1, 4)
    qc <- LinearityApp:::edit_layout(grid, c('A5', 'B5'), 'qc', qc_level = 2)
    expect_equal(unname(qc[1:2, 5]), c('QC2', 'QC2'))
    expect_equal(LinearityApp:::classify_codes(qc[1, 5]), 'QC')
    qc <- LinearityApp:::edit_layout(qc, 'C5', 'qc', qc_level = 1)
    expect_equal(LinearityApp:::qc_nominal(qc[1:3, 5], c(10, 50)), c(50, 50, 10))
    expect_equal(LinearityApp:::qc_nominal(c('QC3', 'S1', NA), c(10, 50)), c(NA_real_, NA, NA))
    trimmed <- LinearityApp:::trim_qc(qc, 1)
    expect_true(all(is.na(trimmed[1:2, 5])))
    expect_equal(trimmed[['C', 5]], 'QC1')
})

test_that('plate template is written from an edited layout', {
    grid <- LinearityApp:::plate_layout_codes(96, 7, 1, 3)
    grid <- LinearityApp:::edit_layout(grid, c('A6', 'B6'), 'sample')
    path <- tempfile(fileext = '.xlsx')
    make_plate_template(path, n.levels = 7, layout = grid)
    expect_equal(unname(LinearityApp:::read_grid(path, 'Layout')), unname(grid))
    expect_equal(readxl::read_excel(path, 'Samples')$id, paste0('X', 1:5))
})

test_that('minimal table format needs only type and OD', {
    tab <- data.frame(type = c(rep('Standard', 5), 'blank', 'Sample', 'qc'),
                      conc = c(1, 2, 4, 8, 16, NA, NA, 4),
                      OD = c(0.1, 0.2, 0.4, 0.8, 1.6, 0, 0.5, 0.4))
    data <- check_assay(normalize_table(tab))
    expect_equal(data$type, c(rep('standard', 5), 'blank', 'sample', 'QC'))
    expect_equal(data$dilution[7:8], c(1, 1))
    r <- calculate(data, 'LRM', linear = TRUE)
    expect_equal(r$result$conc, 5)
    expect_equal(r$qc$recovery, 100)
})

test_that('validation collects every problem of a table', {
    tab <- data.frame(well = c('A1', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6'),
                      type = c('standard', 'standard', 'standard', 'standard', 'sample', 'sample', 'NC'),
                      conc = c(1, NA, 4, -8, NA, NA, NA),
                      dilution = c(NA, NA, NA, NA, NA, 0, NA),
                      OD = c(0.1, 0.2, 0.4, 0.8, 0.5, 0.5, 0))
    problems <- validate_assay(tab)
    keys <- vapply(problems, function(p) p$message$key, '')
    expect_setequal(keys, c('error.dilution_missing', 'error.dilution_positive', 'error.std_conc_missing',
                            'error.std_conc_positive', 'error.few_standard_wells', 'error.duplicate_well'))
    expect_null(problems_table(list()))
})

test_that('absurd layout parameters are rejected before anything is allocated', {
    codes <- LinearityApp:::plate_layout_codes
    expect_error(codes(96, 7, 2, 1e9), 'does not fit', class = 'lin_error')
    expect_error(codes(96, 1e9, 2, 10), 'does not fit', class = 'lin_error')
    expect_error(codes(384, 7, 1e9, 10), 'does not fit', class = 'lin_error')
    expect_error(codes(96, 7, 2, 10, sample_reps = 1e9), 'does not fit', class = 'lin_error')
    expect_error(codes(96, 7, 2, -1), 'non-negative', class = 'lin_error')
    expect_error(codes(96, 7, NA, 10), 'non-negative', class = 'lin_error')
    # a plate without samples or without standards is still a layout
    no_samples <- codes(96, 7, 2, 0)
    expect_equal(sum(!is.na(no_samples)), 16)
    expect_equal(LinearityApp:::count_samples(no_samples), 0)
    expect_equal(sort(unique(as.vector(codes(96, 7, 0, 5)))), paste0('X', 1:5))
    expect_setequal(stats::na.omit(as.vector(codes(96, 0, 1, 2))), c('B', 'X1', 'X2'))
})

test_that('wells with unusual names do not break the plate map', {
    data <- data.frame(well = c('A1', 'B2x', 'foo', NA), type = 'sample', OD = c(1, 2, 3, 4))
    expect_no_warning(grids <- LinearityApp:::data_plate_grids(data))
    expect_equal(sum(!is.na(grids$values)), 1)
    expect_equal(grids$values[['A', '1']], 1)
})
