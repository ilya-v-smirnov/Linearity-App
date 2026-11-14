# Creates example files in inst/extdata. Run from the package root:
#   Rscript data-raw/make_examples.R

pkgload::load_all(quiet = TRUE)

# --- BCA assay in plate format (same measurements as bca_semicolon.csv) ------
legacy <- read_table('inst/extdata/bca_semicolon.csv')
path <- 'inst/extdata/bca_plate.xlsx'
make_plate_template(path, size = 96, start = 2, step = 2, n.levels = 7, n.standard = 2,
                    n.sample = 8, sample_reps = 3, lang = 'en')

layout <- matrix(NA_character_, 8, 12)
od <- matrix(NA_real_, 8, 12)

# Standards: two series in columns 1-2, NC in row H
standards <- legacy[legacy$type != 'sample', ]
layout[, 1:2] <- c(paste0('S', 1:7), 'NC')
od[, 1] <- standards$OD[1:8]
od[, 2] <- standards$OD[9:16]

# Samples: T1-T4 at dilutions 5 and 10 (X1-X8) in triplicates, columns 3-5
samples <- legacy[legacy$type == 'sample', ]
for (r in 1:3) {
    layout[, 2 + r] <- paste0('X', 1:8)
    od[, 2 + r] <- samples$OD[(r - 1) * 8 + 1:8]
}

wb <- openxlsx::loadWorkbook(path)
openxlsx::writeData(wb, 'Layout', as.data.frame(layout), startCol = 2, startRow = 2, colNames = FALSE)
openxlsx::writeData(wb, 'OD', as.data.frame(od), startCol = 2, startRow = 2, colNames = FALSE)
openxlsx::writeData(wb, 'Standards',
                    data.frame(level = paste0('S', 1:7), conc = standards$conc[1:7]))
openxlsx::writeData(wb, 'Samples',
                    data.frame(id = paste0('X', 1:8), name = samples$sample.name[1:8],
                               dilution = samples$dilution[1:8], date = NA_character_))
openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

# --- Invalid table: mandatory column "type" is missing -----------------------
griess <- read_table('inst/extdata/griess.xlsx')
griess$type <- NULL
save_xlsx(griess, 'inst/extdata/invalid/01_mandatory_column_missing.xlsx')
