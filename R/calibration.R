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


##################### BACKGROUND ###############################################

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


#' Roles of Background Wells
#'
#' * `blank`: reagent blank (plate codes `B`, `BLANK`);
#' * `zero`: zero calibrator (plate codes `Z`, `ZERO`, `S0`);
#' * `NC`: negative control (plate code `NC`).
#'
#' The roles are kept apart: the wells subtracted as background are chosen
#' explicitly (see [calculate()], argument `background`), and roles are
#' combined only when the method says so.
#'
#' @return Character vector of well types.
#' @export
background_roles <- function() c('blank', 'zero', 'NC')


#' Wells of a Background Role
#'
#' @param type Vector of well types.
#'
#' @return Logical vector: `TRUE` for reagent blanks, zero calibrators and
#'   negative controls (see [background_roles()]).
#' @keywords internal
is_background <- function(type) {
    type %in% background_roles()
}


#' Default Background Roles
#'
#' The reagent blanks; without them the zero calibrators. Negative controls
#' are used by default only when the plate has neither (files of earlier
#' versions, where `NC` meant the blank).
#'
#' @param type Vector of well types.
#'
#' @return A role (see [background_roles()]), or `character(0)` without
#'   background wells.
#' @export
default_background <- function(type) {
    for (role in background_roles()) {
        if (any(type %in% role)) return(role)
    }
    character(0)
}


#' Estimate the Background Value
#'
#' The background is the arithmetic mean of the background ODs, whatever
#' their sign (the same rule for every plate).
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
    list(value = mean(od), notes = list())
}


#' Background of Earlier Versions
#'
#' Earlier versions subtracted the geometric mean of all blank and NC wells
#' (the arithmetic mean if some were zero or negative). Kept to show when a
#' plate is analysed again with a different background.
#'
#' @param data A data.frame with columns `type` and `OD`.
#'
#' @return A numeric value, `NA` without blank or NC wells.
#' @keywords internal
legacy_background <- function(data) {
    od <- data$OD[data$type %in% c('blank', 'NC') & !is.na(data$OD)]
    if (length(od) == 0) return(NA_real_)
    if (any(od <= 0)) mean(od) else geom_mean(od)
}


#' Background Correction of a Plate
#'
#' @param data A data.frame with columns `type`, `OD` and optionally `.key`
#'   (well IDs, see [well_keys()]).
#' @param roles Roles of the wells subtracted as background (see
#'   [background_roles()]); `character(0)` subtracts nothing.
#' @param exclude Background wells excluded by the user: a data.frame with
#'   `key` and `reason`, or `NULL`.
#'
#' @return A list with `roles`, `keys` and `od` (the wells used), `n`,
#'   `n_roles` (wells of the chosen roles, used or not), `mean`, `sd` (`NA`
#'   with fewer than two wells), `value` (the value subtracted: the mean; 0
#'   without roles or without a usable well, then `empty` is `TRUE`), `unused` (a named vector with
#'   the number of wells of the roles left out), `legacy` (see
#'   [legacy_background()]) and `wells` (all wells of the background roles:
#'   `role`, `key`, `OD`, `used`, `excluded`, `reason`).
#' @keywords internal
background_summary <- function(data, roles, exclude = NULL) {
    bad <- setdiff(roles, background_roles())
    if (length(bad) > 0) lin_abort('error.background_role', roles = bad, choices = background_roles())
    keys <- if ('.key' %in% names(data)) data$.key else well_keys(data)
    all <- is_background(data$type)
    reason <- rep(NA_character_, nrow(data))
    if (!is.null(exclude) && nrow(exclude) > 0) {
        idx <- match(keys, exclude$key)
        reason[all & !is.na(idx)] <- as.character(exclude$reason[idx[all & !is.na(idx)]])
    }
    excluded <- !is.na(reason)
    in_roles <- data$type %in% roles
    used <- in_roles & !is.na(data$OD) & !excluded
    od <- data$OD[used]
    empty <- length(roles) > 0 && length(od) == 0
    value <- if (length(roles) == 0 || empty) 0 else background_value(od)$value
    others <- setdiff(background_roles(), roles)
    unused <- vapply(others, function(r) sum(data$type %in% r), 0)
    wells <- data.frame(role = data$type[all], key = keys[all], OD = data$OD[all], used = used[all],
                        excluded = excluded[all], reason = reason[all])
    list(roles = roles, keys = keys[used], od = od, n = length(od), n_roles = sum(in_roles), empty = empty,
         mean = if (length(od) > 0) mean(od) else NA_real_,
         sd = if (length(od) >= 2) stats::sd(od) else NA_real_,
         value = value, unused = unused[unused > 0], legacy = legacy_background(data), wells = wells)
}


#' Subtract Background Values
#'
#' @param data A data.frame with columns `type` and `OD`.
#' @param nc_value Background value; if `NULL`, the mean of the wells of the
#'   default role (see [default_background()]).
#'
#' @return A data.frame without background rows (all roles), with
#'   background-subtracted `OD`.
#' @keywords internal
subtract_nc <- function(data, nc_value = NULL) {
    if (is.null(nc_value)) {
        nc_value <- background_summary(data, default_background(data$type))$value
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
#' Only the quadratic models can turn within the working interval (the
#' monotonic interval set by the user or the standard range, see
#' [working_interval()]); for the log-log quadratic the vertex is found on
#' the log10 scale of the concentration.
#'
#' @param fit A `lin_fit` object.
#'
#' @return `TRUE` unless the vertex lies strictly inside the working interval.
#' @keywords internal
is_monotonic <- function(fit) {
    vertex <- quad_vertex(fit)
    is.null(vertex) || vertex$side != 'inside'
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
#' @param linear For LRM and QRM: fit on untransformed values instead of
#'   log10 scales (as LIN and QUAD).
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
#' @param exclude_background Background wells excluded by the user: a
#'   data.frame with columns `key` and `reason`.
#' @param background Roles of the wells subtracted as background (see
#'   [background_roles()]); `NULL` for the default role of the plate (see
#'   [default_background()]); `character(0)` subtracts nothing.
#' @param interval For the quadratic models: the monotonic interval of
#'   concentrations (lower and upper bound) set by the user, or `NULL` for the
#'   range of the standards. The curve is inverted on the branch of the
#'   parabola that holds the interval, and the quantification range is limited
#'   to it. If the vertex of the parabola lies inside the interval, the
#'   concentrations are ambiguous and are not calculated (see [quad_vertex()]).
#'   Ignored for the other models.
#'
#' @return A list with components:
#'   * `model_direct`, `fit`: the fitted calibration (`lin_fit`, see [fit_curve()]);
#'   * `model`, `linear_scale`, `weights`, `interval`: settings;
#'   * `monotonic`: `FALSE` if a quadratic curve turns inside its working
#'     interval (the concentrations are then not calculated);
#'   * `nc_value`: background value subtracted from all ODs;
#'   * `background`: the background correction (see [background_summary()]);
#'   * `standard_data`: standards used in the fit (with `.key`; `OD` corrected,
#'     `OD.raw` as measured);
#'   * `calibration_pool`: common set of calibration wells (with `.key`):
#'     background-subtracted standards with an OD, without those excluded by
#'     the user, before the restrictions of the model (see [compare_models()]);
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
#'     subtracted), `OD.corrected` (after it), `conc` (dilution-corrected), its confidence interval
#'     `conc.lower`, `conc.upper` and `flag`;
#'   * `qc`: quality controls with nominal and measured concentration, OD as measured,
#'     recovery (%) and flag (`NULL` if there are no QC wells).
#'
#' The background is the arithmetic mean of the wells of the chosen roles
#' (argument `background`): reagent blank, zero calibrator and negative
#' control are kept apart. Columns `well` and `id` are kept in the results
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
                      extrapolate = character(0), blank_zero = FALSE, interval = NULL, background = NULL,
                      exclude_background = NULL) {
    if (length(model) != 1 || !model %in% calibration_models()) {
        lin_abort('error.unknown_model', model = model, choices = calibration_models())
    }
    data$.key <- well_keys(data)

    # Background subtraction; the results show the OD as measured and corrected
    defaulted <- is.null(background)
    if (defaulted) background <- default_background(data$type)
    bg <- background_summary(data, background, exclude_background)
    notes <- list()
    if (length(background) == 0) {
        notes <- c(notes, list(lin_message('note.no_background')))
    } else if (bg$empty) {
        notes <- c(notes, list(lin_message('note.background_empty')))
    } else if (defaulted && identical(background, 'NC')) {
        notes <- c(notes, list(lin_message('note.background_nc_legacy', n = bg$n)))
    }
    # A plate analysed again: the background of earlier versions differs by more than 1 %
    if (!is.na(bg$legacy) && abs(bg$value - bg$legacy) > 0.01 * abs(bg$legacy)) {
        notes <- c(notes, list(lin_message('note.background_changed', value = signif(bg$value, 4),
                                           legacy = signif(bg$legacy, 4))))
    }
    data$.od_raw <- data$OD
    data <- subtract_nc(data, bg$value)

    # Standards used for fitting
    log_scale <- model_log_scale(model, linear)
    std <- split_standards(data[data$type == 'standard', , drop = FALSE], log_scale, exclude)
    standard_data <- std$used
    # Common set of calibration wells for the comparison of models: after the exclusions
    # by the user, before the restrictions of the chosen model
    pool <- split_standards(data[data$type == 'standard', , drop = FALSE], log_scale = FALSE, exclude)$used
    rownames(pool) <- NULL
    pool$.od_raw <- NULL
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
    if (poly_degree(model) == 2 && !is.null(interval)) {
        ok <- is.numeric(interval) && length(interval) == 2 && all(is.finite(interval)) && all(interval > 0) &&
            interval[1] < interval[2] && interval[1] < fit$conc_range[2] && interval[2] > fit$conc_range[1]
        if (!ok) lin_abort('error.interval', lo = signif(fit$conc_range[1], 3), hi = signif(fit$conc_range[2], 3))
        fit$interval <- as.numeric(interval)
        notes <- c(notes, list(lin_message('note.interval', lo = signif(interval[1], 3), hi = signif(interval[2], 3))))
    }
    monotonic <- is_monotonic(fit)

    # Quantification range: accepted standard levels
    # All levels of the run count, also those without points in the fit
    standards <- assess_standards(fit, criteria, wells = data[data$type == 'standard', c('conc', 'OD')])
    calibration <- calibration_acceptance(standards, criteria)
    # Levels whose fitted wells are not all back-calculated
    all_levels <- data$conc[data$type == 'standard']
    # A curve that turns inside the interval has one warning of its own (below)
    for (i in which(standards$status == 'incomplete' & monotonic)) {
        lost <- standards$failed[[i]]
        wells <- lapply(seq_len(nrow(lost)), function(k) {
            lin_message('backcalc.well', well = lost$key[k], reason = lin_message(paste0('backcalc.', lost$reason[k])))
        })
        notes <- c(notes, list(lin_message('note.incomplete_backcalc',
                                           level = standard_names(standards$conc[i], all_levels),
                                           conc = signif(standards$conc[i], 3), n = standards$n.calc[i],
                                           total = standards$n[i], wells = wells)))
    }
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
    if (!is.null(fit$interval)) {
        conc_range <- c(max(conc_range[1], fit$interval[1]), min(conc_range[2], fit$interval[2]))
    }
    if (!monotonic) {
        x_lim <- working_interval(fit)
        notes <- c(notes, list(lin_message('note.not_monotonic', vertex = signif(quad_vertex(fit)$conc, 3),
                                           lo = signif(x_lim[1], 3), hi = signif(x_lim[2], 3))))
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
        corrected <- rows$OD
        rows$OD <- round(rows$.od_raw, 4)
        rows$OD.corrected <- round(corrected, 4)
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
        if (!is.null(original_fit)) original_fit$interval <- fit$interval
        if (!is.null(original_fit)) {
            effect <- exclusion_effect(original_fit, fit, all_std, manual$.key, criteria, levels = all_levels)
        }
    }

    rownames(standard_data) <- NULL
    # The standards keep the OD as measured next to the corrected one
    standard_data$OD.raw <- standard_data$.od_raw
    standard_data$.od_raw <- NULL
    sample_data$type <- NULL
    sample_data$.key <- NULL
    sample_data <- drop_empty_columns(sample_data, c('well', 'id'))

    quality <- assess_run(fit, standards, calibration, sample_data, qc, conc_range, lof, criteria)
    notes <- c(notes, quality$reasons, quality$warnings)

    list(model_direct = fit,
         fit = fit,
         model = model,
         linear_scale = poly_degree(model) > 0 && !log_scale,
         interval = fit$interval,
         monotonic = monotonic,
         weights = weights,
         nc_value = bg$value,
         background = bg,
         standard_data = standard_data,
         calibration_pool = pool,
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
