# testthat runs tests with tests/testthat as the working directory
project_root <- normalizePath(file.path('..', '..'))

suppressMessages(source(file.path(project_root, 'linearity.R')))

sample_path <- function(...) file.path(project_root, 'Sample Data', ...)

sample_files <- c(
    bca = '#01_BCA assay_semicolon-delimited.csv',
    elisa = '#02_Sandwich ELISA (sMICB)_comma-delimited.csv',
    griess = '#03_Griess assay_Excel.xlsx'
)

load_sample <- function(name) {
    read_table(sample_path(sample_files[[name]])) |>
        check_input_file() |>
        format_input_table()
}

# Synthetic data set: standards follow OD = f(conc) exactly, NC = 0.05
synthetic_data <- function(f, conc = c(1, 2, 4, 8, 16, 32), sample_od = 0.5,
                           nc = 0.05) {
    rbind(
        data.frame(sample.name = NA, sample.date = NA, type = 'standard',
                   dilution = NA, conc = rep(conc, 2), OD = rep(f(conc), 2) + nc),
        data.frame(sample.name = NA, sample.date = NA, type = 'NC',
                   dilution = NA, conc = NA, OD = c(nc, nc)),
        data.frame(sample.name = paste0('S', seq_along(sample_od)), sample.date = NA,
                   type = rep('sample', length(sample_od)), dilution = 1, conc = NA,
                   OD = sample_od + nc)
    )
}
