#' Result Flags
#'
#' Status codes assigned to wells. Only sample wells flagged `"OK"` receive a
#' numeric concentration; all other wells get `NA` in `conc`. The codes are
#' stored in English; use [translate_flags()] for display.
#'
#' - `ok` (`"OK"`): the OD lies within the calibration range.
#' - `below` (`"< LLOQ"`): below the lowest standard used in the fit.
#' - `above` (`"> ULOQ"`): above the highest standard used in the fit.
#' - `od_le_nc` (`"OD <= blank"`): background-subtracted OD is zero or negative.
#' - `no_solution` (`"No solution"`): the OD cannot be reached by the curve.
#' - `missing` (`"Missing OD"`): the OD value is empty.
#' - `excluded` (`"Excluded"`): a standard point excluded by the user.
#'
#' Flags of replicate groups (see [assess_run()]):
#'
#' - `high_cv` (`"High CV"`): the CV of replicates exceeds the limit.
#' - `nonlinear` (`"Dilution non-linear"`): the dilutions of the sample do not agree.
#'
#' LLOQ and ULOQ are the lowest and highest accepted standard levels (see
#' [qc_criteria()], argument `range`).
#'
#' @export
FLAGS <- list(ok = 'OK',
              below = '< LLOQ',
              above = '> ULOQ',
              od_le_nc = 'OD <= blank',
              no_solution = 'No solution',
              missing = 'Missing OD',
              excluded = 'Excluded',
              high_cv = 'High CV',
              nonlinear = 'Dilution non-linear')


##################### BACKGROUND (NC) ##########################################

#' Geometric Mean of Positive Values
#'
#' @param x Numeric vector; missing values are ignored.
#'
#' @return A numeric value, `NA` if `x` is empty or has non-positive values.
#' @keywords internal
geom_mean <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0 || any(x <= 0)) return(NA_real_)
    10^mean(log10(x))
}


#' Estimate the Background Value
#'
#' The background is the geometric mean of the background ODs. If some values
#' are zero or negative, the arithmetic mean is used instead.
#'
#' @param od Numeric vector of background ODs.
#'
#' @return A list with `value` and `notes` (list of `lin_message`).
#' @keywords internal
background_value <- function(od) {
    od <- od[!is.na(od)]
    if (length(od) == 0) {
        lin_abort('error.no_nc')
    }
    if (any(od <= 0)) {
        return(list(value = mean(od), notes = list(lin_message('note.nc_arithmetic'))))
    }
    list(value = geom_mean(od), notes = list())
}


#' Wells Used as Background
#'
#' Blank and NC wells are equivalent: both are background wells.
#'
#' @param type Vector of well types.
#'
#' @return Logical vector.
#' @keywords internal
is_background <- function(type) {
    type %in% c('blank', 'NC')
}


#' Subtract Background Values
#'
#' @param data A data.frame with columns `type` and `OD`.
#' @param nc_value Background value; estimated with [background_value()] from
#'   the blank (or NC) wells if `NULL`.
#'
#' @return A data.frame without NC and blank rows, with background-subtracted `OD`.
#' @keywords internal
subtract_nc <- function(data, nc_value = NULL) {
    if (is.null(nc_value)) {
        nc_value <- background_value(data$OD[is_background(data$type)])$value
    }
    out <- data[!is_background(data$type), , drop = FALSE]
    out$OD <- out$OD - nc_value
    out
}


##################### STANDARDS ################################################

#' Keys of Wells
#'
#' Well names when available, otherwise spreadsheet rows (`"row 5"`).
#' Used to identify standard points excluded by the user.
#'
#' @param data Input data.
#'
#' @return Character vector.
#' @export
well_keys <- function(data) {
    keys <- paste('row', seq_len(nrow(data)) + 1)
    if ('well' %in% names(data)) {
        has_well <- !is.na(data$well)
        keys[has_well] <- as.character(data$well[has_well])
    }
    keys
}


#' Names of Standard Levels
#'
#' Standard levels are named `S1`, `S2`, ... from the highest to the lowest
#' concentration.
#'
#' @param conc Concentrations of the standards to name.
#' @param levels Concentrations of all standards of the run.
#'
#' @return Character vector.
#' @keywords internal
standard_names <- function(conc, levels = conc) {
    if (length(conc) == 0) return(character(0))
    paste0('S', match(conc, sort(unique(levels), decreasing = TRUE)))
}


#' Select Standard Wells Usable for Fitting
#'
#' Standards with a missing OD, (on log scales) a non-positive OD after
#' background subtraction, or excluded by the user are not used for fitting.
#'
#' @param standard_data Background-subtracted standard rows with `.key`.
#' @param log_scale Logical; `TRUE` if the model is fitted on log10 scales.
#' @param exclude Data.frame with columns `key` and `reason`, or `NULL`.
#'
#' @return A list with `used` and `excluded` data.frames; `excluded` has a
#'   `reason` column with flag codes and a `comment` column with the user's reason.
#' @keywords internal
split_standards <- function(standard_data, log_scale = TRUE, exclude = NULL) {
    reason <- rep(NA_character_, nrow(standard_data))
    comment <- rep(NA_character_, nrow(standard_data))
    if (!is.null(exclude) && nrow(exclude) > 0) {
        idx <- match(standard_data$.key, exclude$key)
        manual <- !is.na(idx)
        reason[manual] <- FLAGS$excluded
        comment[manual] <- as.character(exclude$reason[idx[manual]])
    }
    if (log_scale) reason[is.na(reason) & !is.na(standard_data$OD) & standard_data$OD <= 0] <- FLAGS$od_le_nc
    reason[is.na(standard_data$OD)] <- FLAGS$missing

    excluded <- standard_data[!is.na(reason), , drop = FALSE]
    excluded$reason <- reason[!is.na(reason)]
    excluded$comment <- comment[!is.na(reason)]
    list(used = standard_data[is.na(reason), , drop = FALSE],
         excluded = excluded)
}


#' Assign Result Flags
#'
#' @param od Background-subtracted OD.
#' @param conc Concentration estimate before dilution correction.
#' @param conc_range Range of standard concentrations used in the fit.
#'
#' @return Character vector of flags (see [FLAGS]).
#' @keywords internal
assign_flags <- function(od, conc, conc_range) {
    tol <- 1e-9 * diff(range(conc_range))
    flag <- rep(FLAGS$ok, length(od))
    flag[!is.na(conc) & conc > conc_range[2] + tol] <- FLAGS$above
    flag[!is.na(conc) & conc < conc_range[1] - tol] <- FLAGS$below
    flag[is.na(conc)] <- FLAGS$no_solution
    flag[!is.na(od) & od <= 0] <- FLAGS$od_le_nc
    flag[is.na(od)] <- FLAGS$missing
    flag
}


#' Check Monotonicity of the Calibration Curve
#'
#' Only the quadratic model can turn within the standard range.
#'
#' @param fit A `lin_fit` object.
#'
#' @return `TRUE` if the curve is monotonic within the standard range.
#' @keywords internal
is_monotonic <- function(fit) {
    if (fit$model != 'QRM' || fit$par[['a']] == 0) return(TRUE)
    x_lim <- if (fit$log_scale) log10(fit$conc_range) else fit$conc_range
    vertex <- -fit$par[['b']] / (2 * fit$par[['a']])
    !(vertex > x_lim[1] && vertex < x_lim[2])
}


#' Extract Coefficients and Confidence Intervals
#'
#' @param fit A fitted calibration (`lin_fit`, see [fit_curve()]).
#' @param round_fun Optional rounding function (e.g. `signif`).
#' @param round_par Optional rounding parameter.
#'
#' @return A data.frame with columns `coef`, `est`, `ci.lower`, `ci.upper`.
#' @export
get_coef_ci <- function(fit, round_fun = NULL, round_par = NULL) {
    if (!inherits(fit, 'lin_fit')) {
        stop('Unsupported model object', call. = FALSE)
    }
    tab <- coef_table_fit(fit)
    rownames(tab) <- NULL
    if (!is.null(round_fun) && !is.null(round_par)) {
        num <- c('est', 'ci.lower', 'ci.upper')
        tab[num] <- lapply(tab[num], round_fun, round_par)
    }
    tab
}


##################### CALCULATION ##############################################

#' Calculate Sample Concentrations
#'
#' Subtracts the background, fits the calibration curve OD = f(conc) on the
#' standards, inverts the curve for samples and quality controls and
#' multiplies the result by the dilution factor. Samples outside the
#' calibration range are flagged and receive `NA` instead of a concentration.
#'
#' @param data Validated input data (see [check_assay()]).
#' @param model Model code (see [calibration_models()]).
#' @param linear For LRM and QRM: fit on untransformed values instead of log10 scales.
#' @param weights Weighting code (see [weighting_schemes()]).
#' @param exclude Standard points excluded by the user: a data.frame with
#'   columns `key` (see [well_keys()]) and `reason`.
#' @param round_fun,round_par Rounding applied to the final concentrations only.
#' @param level Confidence level of the concentration intervals.
#' @param criteria Acceptance criteria (see [qc_criteria()]).
#' @param extrapolate Samples outside the quantification range that still get
#'   a concentration: any of `"below"` (under the LLOQ) and `"above"` (over
#'   the ULOQ). Their flags are kept.
#' @param blank_zero If `TRUE`, samples with OD at or below the blank get
#'   concentration 0 (flag kept).
#'
#' @return A list with components:
#'   * `model_direct`, `fit`: the fitted calibration (`lin_fit`, see [fit_curve()]);
#'   * `model`, `linear_scale`, `weights`: settings;
#'   * `nc_value`: background value subtracted from all ODs;
#'   * `standard_data`: standards used in the fit (with `.key`);
#'   * `excluded_standards`: standards not used in the fit with a `reason`;
#'   * `exclusions`: standards excluded by the user (`standard`: level name,
#'     see [standard_names()]; `key`: position; `conc`, `OD`, `reason`);
#'   * `original_fit`: with exclusions, the calibration fitted on all standards
#'     (`NULL` without exclusions or if it cannot be fitted);
#'   * `exclusion_effect`: comparison of the original and the refitted
#'     calibration (see [exclusion_effect()]; `NULL` without `original_fit`);
#'   * `calibration_range`: quantification range (LLOQ, ULOQ) used for the flags;
#'   * `standard_range`: range of all standard concentrations used in the fit;
#'   * `quality`: quality assessment of the run (see [assess_run()]);
#'   * `back_calc`: back-calculated standards (see [back_calculate()]);
#'   * `lack_of_fit`: lack-of-fit test (see [lack_of_fit()]);
#'   * `notes`: warnings as a list of messages (see [message_text()]);
#'   * `result`: sample results with `OD` as measured (before the blank is
#'     subtracted), `conc` (dilution-corrected), its confidence interval
#'     `conc.lower`, `conc.upper` and `flag`;
#'   * `qc`: quality controls with nominal and measured concentration, OD as measured,
#'     recovery (%) and flag (`NULL` if there are no QC wells).
#'
#' The background is the geometric mean of the blank wells (type `blank` or
#' `NC`, which are equivalent). Columns `well` and `id` are kept in the results
#' when they contain values.
#' @export
#' @examples
#' path <- system.file('extdata', 'bca_plate.xlsx', package = 'LinearityApp')
#' data <- check_assay(read_assay(path))
#' res <- calculate(data, '4PL', weights = '1/y2')
#' head(res$result)
#' message_text(res$notes, 'en')
calculate <- function(data, model = 'LRM', linear = FALSE, weights = 'none', exclude = NULL,
                      round_fun = signif, round_par = 3, level = 0.95, criteria = qc_criteria(),
                      extrapolate = character(0), blank_zero = FALSE) {
    if (length(model) != 1 || !model %in% calibration_models()) {
        lin_abort('error.unknown_model', model = model, choices = calibration_models())
    }
    data$.key <- well_keys(data)

    # Background subtraction; the results show the OD as measured
    bg <- background_value(data$OD[is_background(data$type)])
    notes <- bg$notes
    data$.od_raw <- data$OD
    data <- subtract_nc(data, bg$value)

    # Standards used for fitting
    log_scale <- model %in% c('LRM', 'QRM') && !linear
    std <- split_standards(data[data$type == 'standard', , drop = FALSE], log_scale, exclude)
    standard_data <- std$used
    # Points excluded by the user are listed in `exclusions`; warn about the others
    automatic <- std$excluded$reason[std$excluded$reason != FLAGS$excluded]
    if (length(automatic) > 0) {
        notes <- c(notes, list(lin_message('note.standards_excluded',
                                           n = length(automatic),
                                           details = flag_counts(automatic))))
    }
    min_levels <- n_params(model) + 1
    n_levels <- length(unique(standard_data$conc))
    if (n_levels < min_levels) {
        lin_abort('error.too_few_levels', min = min_levels, found = n_levels)
    }

    # Fit OD = f(conc)
    fit <- fit_curve(standard_data, model, linear, weights)
    notes <- c(notes, fit$notes)

    # Quantification range: accepted standard levels
    standards <- assess_standards(fit, criteria)
    calibration <- calibration_acceptance(standards, criteria)
    conc_range <- fit$conc_range
    if (criteria$range == 'accepted') {
        if (is.na(calibration$lloq)) {
            notes <- c(notes, list(lin_message('note.no_accepted_standards')))
        } else if (calibration$lloq > conc_range[1] || calibration$uloq < conc_range[2]) {
            conc_range <- c(calibration$lloq, calibration$uloq)
            notes <- c(notes, list(lin_message('note.range_narrowed', lloq = signif(conc_range[1], 3),
                                               uloq = signif(conc_range[2], 3))))
        }
    }
    if (!is_monotonic(fit)) {
        notes <- c(notes, list(lin_message('note.not_monotonic')))
    }
    lof <- lack_of_fit(fit)
    if (!is.na(lof$p) && lof$p < 0.05) {
        notes <- c(notes, list(lin_message('note.lack_of_fit', p = signif(lof$p, 2))))
    }

    # Samples and QC: inverse prediction, flags, dilution correction
    quantify <- function(rows, extrapolate = character(0), blank_zero = FALSE) {
        rows$dilution[is.na(rows$dilution)] <- 1
        pred <- inverse_predict_ci(fit, rows$OD, level)
        flag <- assign_flags(rows$OD, pred$conc, conc_range)
        ok <- flag == FLAGS$ok |
            (flag == FLAGS$below & 'below' %in% extrapolate) |
            (flag == FLAGS$above & 'above' %in% extrapolate)
        rows$conc <- ifelse(ok, round_fun(pred$conc * rows$dilution, round_par), NA_real_)
        rows$conc.lower <- ifelse(ok, round_fun(pred$lower * rows$dilution, round_par), NA_real_)
        rows$conc.upper <- ifelse(ok, round_fun(pred$upper * rows$dilution, round_par), NA_real_)
        if (blank_zero) rows$conc[flag == FLAGS$od_le_nc] <- 0
        rows$OD <- round(rows$.od_raw, 4)
        rows$.od_raw <- NULL
        rows$flag <- flag
        rownames(rows) <- NULL
        rows
    }

    sample_data <- quantify(data[data$type == 'sample', , drop = FALSE], extrapolate, blank_zero)
    # Wells without a concentration (extrapolated values and zeros are reported)
    bad <- sample_data$flag[is.na(sample_data$conc)]
    if (length(bad) > 0) {
        notes <- c(notes, list(lin_message('note.not_quantified',
                                           n = length(bad), total = nrow(sample_data),
                                           details = flag_counts(bad))))
    }

    qc <- NULL
    qc_rows <- data[data$type == 'QC', , drop = FALSE]
    if (nrow(qc_rows) > 0) {
        measured <- quantify(qc_rows)
        qc <- measured[, intersect(c('well', 'id'), names(measured)), drop = FALSE]
        qc$nominal <- qc_rows$conc
        qc$conc <- measured$conc
        qc$recovery <- round(100 * measured$conc / qc_rows$conc, 1)
        qc$OD <- measured$OD
        qc$flag <- measured$flag
        qc <- drop_empty_columns(qc, c('well', 'id'))
    }

    manual <- std$excluded[std$excluded$reason == FLAGS$excluded, , drop = FALSE]
    all_levels <- data$conc[data$type == 'standard']
    exclusions <- data.frame(standard = standard_names(manual$conc, all_levels), key = manual$.key,
                             conc = manual$conc, OD = round(manual$OD, 4), reason = manual$comment)

    # With points excluded by the user, the fit on all standards is kept for comparison
    original_fit <- NULL
    effect <- NULL
    if (nrow(manual) > 0) {
        all_std <- split_standards(data[data$type == 'standard', , drop = FALSE], log_scale)$used
        original_fit <- tryCatch(fit_curve(all_std, model, linear, weights), error = function(e) NULL)
        if (!is.null(original_fit)) {
            effect <- exclusion_effect(original_fit, fit, all_std, manual$.key, criteria, levels = all_levels)
        }
    }

    rownames(standard_data) <- NULL
    standard_data$.od_raw <- NULL
    sample_data$type <- NULL
    sample_data$.key <- NULL
    sample_data <- drop_empty_columns(sample_data, c('well', 'id'))

    quality <- assess_run(fit, standards, calibration, sample_data, qc, conc_range, lof, criteria)
    notes <- c(notes, quality$reasons, quality$warnings)

    list(model_direct = fit,
         fit = fit,
         model = model,
         linear_scale = linear,
         weights = weights,
         nc_value = bg$value,
         standard_data = standard_data,
         excluded_standards = std$excluded,
         exclusions = exclusions,
         original_fit = original_fit,
         exclusion_effect = effect,
         calibration_range = conc_range,
         standard_range = fit$conc_range,
         quality = quality,
         back_calc = back_calculate(fit),
         lack_of_fit = lof,
         notes = notes,
         result = sample_data,
         qc = qc)
}


#' Drop Columns Without Values
#'
#' @param data A data.frame.
#' @param columns Columns to drop if they are absent or contain only `NA`.
#'
#' @return The data.frame.
#' @keywords internal
drop_empty_columns <- function(data, columns) {
    for (col in columns) {
        if (is.null(data[[col]]) || all(is.na(data[[col]]))) data[[col]] <- NULL
    }
    data
}
