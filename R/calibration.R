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
#' - `no_background` (`"Background unavailable"`): background subtraction was
#'   chosen, but no well of the chosen roles is usable; no well gets a
#'   concentration until a background well is restored or "no background
#'   subtraction" is chosen (see [background_summary()]).
#'
#' Flags of replicate groups (see [assess_run()]):
#'
#' - `high_cv` (`"High CV"`): the CV of replicates exceeds the limit.
#' - `nonlinear` (`"Dilution non-linear"`): the dilutions of the sample do not agree.
#'
#' The codes `"< LLOQ"` and `"> ULOQ"` are kept for compatibility: the
#' limits are the lowest and highest accepted calibrators of the plate (see
#' [qc_criteria()], argument `range`), not the limits set by a method; the
#' app shows them as below the lowest and above the highest calibrator.
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
              nonlinear = 'Dilution non-linear',
              no_background = 'Background unavailable')


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


#' Background Wells Against the Lowest Calibrator
#'
#' Observed values: the range of the background wells used (maximum minus
#' minimum OD, with two wells or more) and the net signal of the lowest
#' standard level by concentration (its mean OD as measured minus the value
#' subtracted). The lowest level is not taken for a validated LLOQ.
#'
#' Two findings, only for a calibration whose signal increases with the
#' concentration (judged from the standards as measured: the mean OD of the
#' highest level above that of the lowest one), where the comparison makes
#' sense; on a decreasing calibration (e.g. a competitive ELISA) the lowest
#' concentration gives the highest signal and nothing is concluded:
#'
#' * `"signal_at_background"`: the net signal of the lowest calibrator is
#'   zero or negative (with any number of background wells);
#' * `"range_exceeds_signal"`: the net signal is positive, but the range of
#'   the background wells exceeds it.
#'
#' @param od ODs of the background wells used.
#' @param value Value subtracted.
#' @param standards All standard wells of the plan as measured, with `conc`
#'   and `OD` (`NA` where it is missing: the IDs of the levels come from all
#'   of them).
#'
#' @return A list with `range` (`NA` with a single well), `conc` and `level`
#'   (concentration and name of the lowest standard level, see
#'   [standard_names()]), `signal`, `direction` (`"increasing"`,
#'   `"decreasing"`, or `NA` with a single level or equal means), `reason`
#'   (one of the findings above, `NULL` for none) and `warn` (a finding);
#'   `NULL` without background wells or without standards.
#' @keywords internal
blank_scatter <- function(od, value, standards) {
    # The IDs of the levels come from the whole plan, before the wells without an OD are left out
    level_map <- standard_level_map(standards$conc)
    standards <- standards[!is.na(standards$conc) & !is.na(standards$OD), , drop = FALSE]
    if (length(od) == 0 || nrow(standards) == 0) return(NULL)
    lowest <- min(standards$conc)
    highest <- max(standards$conc)
    low_od <- mean(standards$OD[standards$conc == lowest])
    high_od <- mean(standards$OD[standards$conc == highest])
    direction <- if (highest == lowest || high_od == low_od) NA_character_ else
        if (high_od > low_od) 'increasing' else 'decreasing'
    signal <- low_od - value
    spread <- if (length(od) >= 2) max(od) - min(od) else NA_real_
    reason <- NULL
    if (identical(direction, 'increasing')) {
        if (signal <= 0) {
            reason <- 'signal_at_background'
        } else if (!is.na(spread) && spread > signal) {
            reason <- 'range_exceeds_signal'
        }
    }
    list(range = spread, conc = lowest, level = standard_names(lowest, level_map), signal = signal,
         direction = direction, reason = reason, warn = !is.null(reason))
}


#' Background Correction of a Plate
#'
#' @param data A data.frame with columns `type`, `OD` and optionally `.key`
#'   (well IDs, see [well_keys()]).
#' @param roles Roles of the wells subtracted as background (see
#'   [background_roles()]); `character(0)` subtracts nothing.
#' @param exclude Background wells excluded by the user: a data.frame with
#'   `key` and `reason`, or `NULL`.
#' @param subtract Whether background subtraction was chosen. `FALSE` (the
#'   default without roles) is the explicit choice of no subtraction; `TRUE`
#'   without roles means that subtraction was chosen but no role ticked.
#'
#' @return A list with `roles`, `keys` and `od` (the wells used), `n`,
#'   `n_roles` (wells of the chosen roles, used or not), `mean`, `sd` (`NA`
#'   with fewer than two wells), `status` (`"ok"`: the background is the mean
#'   of the wells used; `"none"`: no subtraction was chosen; `"unavailable"`:
#'   subtraction was chosen, but no well is usable or no role is ticked),
#'   `empty` (`TRUE` when `status` is `"unavailable"`), `value` (the value
#'   subtracted: the mean; 0 otherwise), `unused` (a named vector with
#'   the number of wells of the roles left out), `legacy` (see
#'   [legacy_background()]), `wells` (all wells of the background roles:
#'   `role`, `key`, `OD`, `used`, `excluded`, `reason`) and `scatter` (see
#'   [blank_scatter()]).
#' @keywords internal
background_summary <- function(data, roles, exclude = NULL, subtract = length(roles) > 0) {
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
    status <- if (!isTRUE(subtract)) 'none' else if (length(od) == 0) 'unavailable' else 'ok'
    if (status == 'none') {
        used[] <- FALSE
        od <- numeric(0)
    }
    empty <- status == 'unavailable'
    value <- if (status == 'ok') background_value(od)$value else 0
    others <- setdiff(background_roles(), roles)
    unused <- vapply(others, function(r) sum(data$type %in% r), 0)
    wells <- data.frame(role = data$type[all], key = keys[all], OD = data$OD[all], used = used[all],
                        excluded = excluded[all], reason = reason[all])
    list(roles = roles, keys = keys[used], od = od, n = length(od), n_roles = sum(in_roles), status = status,
         empty = empty,
         mean = if (length(od) > 0) mean(od) else NA_real_,
         sd = if (length(od) >= 2) stats::sd(od) else NA_real_,
         value = value, unused = unused[unused > 0], legacy = legacy_background(data), wells = wells,
         scatter = blank_scatter(od, value, data[data$type %in% 'standard', c('conc', 'OD'), drop = FALSE]))
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

#' Common Set of Calibration Wells
#'
#' The standards after the background subtraction, the exclusions by the user
#' and the rule of the method for a signal at or below the background, before
#' the restrictions of any model. It does not depend on the model chosen, so
#' that the models can be compared (see [compare_models()]) even when the
#' chosen one cannot be fitted.
#'
#' @param data Input data (with or without `.key`, see [well_keys()]).
#' @param nc_value Background value subtracted from all ODs.
#' @param exclude Standard points excluded by the user: a data.frame with
#'   `key` and `reason`, or `NULL`.
#' @param at_blank Rule for a signal at or below the background (see
#'   [blank_rules()]).
#'
#' @return The standard rows with `.key` and the background-subtracted `OD`.
#' @keywords internal
calibration_pool <- function(data, nc_value = 0, exclude = NULL, at_blank = 'range') {
    if (!'.key' %in% names(data)) data$.key <- well_keys(data)
    data <- subtract_nc(data, nc_value)
    pool <- split_standards(data[data$type %in% 'standard', , drop = FALSE], log_scale = FALSE, exclude,
                            at_blank)$used
    pool$.od_raw <- NULL
    rownames(pool) <- NULL
    pool
}


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


#' IDs of the Standard Levels of the Plate Plan
#'
#' The standard levels of the plan are named `S1`, `S2`, ... from the highest
#' to the lowest concentration. The map is made from every standard well of
#' the plan, whether it has an OD, is excluded or is in the fit, so that a
#' level keeps its ID in every table, plot and message when other levels
#' are lost.
#'
#' @param conc Nominal concentrations of all standard wells of the plan (`NA`
#'   are ignored).
#'
#' @return A data.frame with `conc` (each level once, from the highest) and
#'   `id`.
#' @keywords internal
standard_level_map <- function(conc) {
    levels <- sort(unique(conc[!is.na(conc)]), decreasing = TRUE)
    data.frame(conc = levels, id = paste0('S', seq_along(levels)))
}


#' Names of Standard Levels
#'
#' The IDs of [standard_level_map()].
#'
#' @param conc Concentrations of the standards to name.
#' @param levels Concentrations of all standard wells of the plan, or the
#'   output of [standard_level_map()].
#'
#' @return Character vector.
#' @keywords internal
standard_names <- function(conc, levels = conc) {
    if (length(conc) == 0) return(character(0))
    map <- if (is.data.frame(levels)) levels else standard_level_map(levels)
    map$id[match(conc, map$conc)]
}


#' Select Standard Wells Usable for Fitting
#'
#' Standards with a missing OD, a non-positive OD after background
#' subtraction (on log scales, or with the rule `"exclude"`, see
#' [blank_rules()]), or excluded by the user are not used for fitting.
#'
#' @param standard_data Background-subtracted standard rows with `.key`.
#' @param log_scale Logical; `TRUE` if the model is fitted on log10 scales.
#' @param exclude Data.frame with columns `key` and `reason`, or `NULL`.
#' @param at_blank Rule for a signal at or below the background (see
#'   [blank_rules()]).
#'
#' @return A list with `used` and `excluded` data.frames; `excluded` has a
#'   `reason` column with flag codes and a `comment` column with the user's reason.
#' @keywords internal
split_standards <- function(standard_data, log_scale = TRUE, exclude = NULL, at_blank = 'range') {
    reason <- rep(NA_character_, nrow(standard_data))
    comment <- rep(NA_character_, nrow(standard_data))
    if (!is.null(exclude) && nrow(exclude) > 0) {
        idx <- match(standard_data$.key, exclude$key)
        manual <- !is.na(idx)
        reason[manual] <- FLAGS$excluded
        comment[manual] <- as.character(exclude$reason[idx[manual]])
    }
    if (!blank_usable(log_scale, at_blank)) {
        reason[is.na(reason) & !is.na(standard_data$OD) & standard_data$OD <= 0] <- FLAGS$od_le_nc
    }
    reason[is.na(standard_data$OD)] <- FLAGS$missing

    excluded <- standard_data[!is.na(reason), , drop = FALSE]
    excluded$reason <- reason[!is.na(reason)]
    excluded$comment <- comment[!is.na(reason)]
    list(used = standard_data[is.na(reason), , drop = FALSE],
         excluded = excluded)
}


#' Rules for a Signal at or Below the Background
#'
#' How a well whose background-subtracted OD is zero or negative is treated
#' by the models fitted on the original OD scale (on log10 scales such a
#' well has no inverse in any case). The same rule applies to the
#' calibrators and to the quality controls and samples:
#'
#' * `"range"`: like any other OD, through the inverse of the curve and the
#'   calibration range;
#' * `"exclude"`: not reportable; calibrators with such an OD are left out of
#'   the fit (flag `"OD <= blank"`), quality controls and samples get the
#'   flag `"OD <= blank"` and no concentration.
#'
#' @return Character vector of rule codes.
#' @export
blank_rules <- function() c('range', 'exclude')


#' Is a Signal at or Below the Background Usable?
#'
#' @param log_scale Logical; `TRUE` if the model is fitted on log10 scales.
#' @param at_blank Rule for such a signal (see [blank_rules()]).
#'
#' @return `TRUE` if a well with OD <= 0 is inverted like any other.
#' @keywords internal
blank_usable <- function(log_scale, at_blank = 'range') !isTRUE(log_scale) && identical(at_blank, 'range')


#' Does the Curve Increase With the Concentration?
#'
#' Compares the curve at the ends of its working interval (see
#' [working_interval()]).
#'
#' @param fit A `lin_fit` object.
#'
#' @return `TRUE` or `FALSE`; `NA` if the curve cannot be evaluated.
#' @keywords internal
curve_increasing <- function(fit) {
    y <- curve_fun(fit, working_interval(fit))
    if (any(!is.finite(y))) return(NA)
    y[2] > y[1]
}


#' Classify Wells by Signal, Inverse and Calibration Range
#'
#' Three separate properties of each well, from which the flag follows:
#'
#' * `at.blank`: the background-subtracted OD is zero or negative (a fact
#'   about the signal; by itself it does not remove the concentration);
#' * `inverse`: why the curve gives no concentration (see
#'   [inverse_reason()]), `NA` when it gives one; with the rule `"exclude"`
#'   (or on log10 scales) a signal at or below the background has none
#'   (`"od_le_blank"`);
#' * `range`: `"below"`, `"within"` or `"above"` the calibration range, `NA`
#'   without a concentration.
#'
#' The flag: `"Missing OD"` without an OD; `"OD <= blank"` when the signal at
#' or below the background has no inverse; `"No solution"` for the other
#' failures of the inverse; otherwise `"< LLOQ"`, `"OK"` or `"> ULOQ"`. The
#' range is never overwritten by the signal.
#'
#' @param fit A `lin_fit` object.
#' @param od Background-subtracted ODs.
#' @param conc Concentrations from the inverse of the curve (before the
#'   dilution correction), see [inverse_predict_ci()].
#' @param conc_range Quantification range (LLOQ, ULOQ).
#' @param at_blank Rule for a signal at or below the background (see
#'   [blank_rules()]).
#'
#' @return A data.frame with `at.blank`, `inverse`, `range` and `flag`.
#' @keywords internal
classify_wells <- function(fit, od, conc, conc_range, at_blank = 'range') {
    at <- !is.na(od) & od <= 0
    inverse <- inverse_reason(fit, od)
    if (!blank_usable(fit$log_scale, at_blank)) inverse[at] <- 'od_le_blank'
    inverse[is.na(inverse) & is.na(conc)] <- 'no_solution'
    has <- is.na(inverse)
    tol <- 1e-9 * diff(range(conc_range))
    range <- rep(NA_character_, length(od))
    range[has] <- 'within'
    range[has & conc > conc_range[2] + tol] <- 'above'
    range[has & conc < conc_range[1] - tol] <- 'below'
    flag <- c(below = FLAGS$below, within = FLAGS$ok, above = FLAGS$above)[range]
    flag[!has] <- FLAGS$no_solution
    flag[!has & inverse == 'od_le_blank'] <- FLAGS$od_le_nc
    flag[is.na(od)] <- FLAGS$missing
    data.frame(at.blank = at, inverse = inverse, range = range, flag = unname(flag))
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
#' @return A data.frame with columns `coef`, `est`, `ci.lower`, `ci.upper`;
#'   for a 5PL the last row `C50` is the concentration at the midpoint of the
#'   response, which `c` is not (see [coef_table_fit()]).
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
#' @param level Confidence level of the concentration intervals.
#' @param criteria Acceptance criteria (see [qc_criteria()]).
#' @param extrapolate Samples outside the quantification range that still get
#'   a concentration: any of `"below"` (under the LLOQ) and `"above"` (over
#'   the ULOQ). Their flags are kept.
#' @param blank_zero If `TRUE`, samples with OD at or below the blank and
#'   outside the calibration range (or without a concentration) get
#'   concentration 0 (flag kept). Only for a curve that increases with the
#'   concentration: on a decreasing curve a weak signal means a high
#'   concentration, the option is then ignored with a note.
#' @param at_blank Rule for a signal at or below the background on the
#'   original OD scale, applied alike to the calibrators, quality controls and
#'   samples (see [blank_rules()]): `"range"` (the default) inverts it like
#'   any other OD, `"exclude"` makes it not reportable. Ignored on log10
#'   scales, where such a signal has no inverse.
#' @param exclude_background Background wells excluded by the user: a
#'   data.frame with columns `key` and `reason`.
#' @param background Roles of the wells subtracted as background (see
#'   [background_roles()]); `NULL` for the default role of the plate (see
#'   [default_background()]); `character(0)` subtracts nothing.
#' @param subtract_background Whether background subtraction was chosen;
#'   `NULL` for `TRUE` when `background` has roles. `FALSE` (or no roles) is
#'   the explicit choice of no subtraction. When subtraction was chosen but no
#'   well of the roles is usable (all excluded or without OD, or no role
#'   ticked), the background is unavailable: the run is not accepted, samples
#'   and quality controls get no concentration (flag `"Background
#'   unavailable"`), while the curve is fitted on the OD as measured for
#'   inspection.
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
#'     `conc.lower`, `conc.upper`, `flag` and, when some wells have a signal at
#'     or below the background, `OD.le.blank` (logical; see [classify_wells()]);
#'   * `qc`: quality controls with nominal and measured concentration, OD as measured,
#'     recovery (%), flag and `OD.le.blank` as for the samples (`NULL` if there
#'     are no QC wells);
#'   * `at_blank`: the rule for a signal at or below the background (see
#'     [blank_rules()]); `increasing`: whether the curve increases with the
#'     concentration (see [curve_increasing()]);
#'   * `standard_keys`: the keys of all standard wells in the order of the plate;
#'   * `standard_levels`: the IDs of the standard levels of the plan (see
#'     [standard_level_map()]), used by every table, plot and message.
#'
#' The background is the arithmetic mean of the wells of the chosen roles
#' (argument `background`): reagent blank, zero calibrator and negative
#' control are kept apart. Columns `well` and `id` are kept in the results
#' when they contain values.
#'
#' Concentrations and recoveries are kept at full precision, so that the
#' rounding chosen for display cannot change the assessment of the quality
#' controls and the samples; tables round them for display and export (see
#' [round_result()]).
#' @export
#' @examples
#' path <- system.file('extdata', 'bca_plate.xlsx', package = 'LinearityApp')
#' data <- check_assay(read_assay(path))
#' res <- calculate(data, '4PL', weights = '1/y2')
#' head(res$result)
#' message_text(res$notes, 'en')
calculate <- function(data, model = 'LRM', linear = FALSE, weights = 'none', exclude = NULL,
                      level = 0.95, criteria = qc_criteria(),
                      extrapolate = character(0), blank_zero = FALSE, interval = NULL, background = NULL,
                      exclude_background = NULL, subtract_background = NULL, at_blank = 'range') {
    if (length(model) != 1 || !model %in% calibration_models()) {
        lin_abort('error.unknown_model', model = model, choices = calibration_models())
    }
    if (length(at_blank) != 1 || !at_blank %in% blank_rules()) {
        lin_abort('error.blank_rule', rule = at_blank, choices = blank_rules())
    }
    data$.key <- well_keys(data)

    # Background subtraction; the results show the OD as measured and corrected
    defaulted <- is.null(background)
    if (defaulted) background <- default_background(data$type)
    subtract <- if (is.null(subtract_background)) length(background) > 0 else isTRUE(subtract_background)
    if (!subtract) background <- character(0)
    bg <- background_summary(data, background, exclude_background, subtract)
    # An unavailable background is a reason of the verdict (see assess_run()), not a note
    notes <- list()
    if (bg$status == 'none') {
        no_wells <- defaulted && !any(is_background(data$type))
        notes <- c(notes, list(lin_message(if (no_wells) 'note.no_background_wells' else 'note.no_background')))
    } else if (bg$status == 'ok' && defaulted && identical(background, 'NC')) {
        notes <- c(notes, list(lin_message('note.background_nc_legacy', n = bg$n)))
    }
    if (isTRUE(bg$scatter$warn)) notes <- c(notes, list(background_scatter_message(bg)))
    # A plate analysed again: the background of earlier versions differs by more than 1 %
    if (bg$status == 'ok' && !is.na(bg$legacy) && abs(bg$value - bg$legacy) > 0.01 * abs(bg$legacy)) {
        notes <- c(notes, list(lin_message('note.background_changed', value = signif(bg$value, 4),
                                           legacy = signif(bg$legacy, 4))))
    }
    # Common set of calibration wells for the comparison of models (the same in the app)
    pool <- calibration_pool(data, bg$value, exclude, at_blank)
    data$.od_raw <- data$OD
    data <- subtract_nc(data, bg$value)

    # Standards used for fitting
    log_scale <- model_log_scale(model, linear)
    std <- split_standards(data[data$type == 'standard', , drop = FALSE], log_scale, exclude, at_blank)
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
    # Every standard well with the reason why it is not in the fit, for the rule of a lost replicate
    std_wells <- data[data$type == 'standard', c('conc', 'OD', '.key'), drop = FALSE]
    names(std_wells)[3] <- 'key'
    idx <- match(std_wells$key, std$excluded$.key)
    codes <- c(stats::setNames('excluded', FLAGS$excluded), stats::setNames('missing', FLAGS$missing),
               stats::setNames('od_le_blank', FLAGS$od_le_nc))
    std_wells$lost <- unname(codes[std$excluded$reason[idx]])
    std_wells$comment <- std$excluded$comment[idx]
    standards <- assess_standards(fit, criteria, wells = std_wells)
    calibration <- calibration_acceptance(standards, criteria)
    # A fit narrower than the plate plan: say which limits its new ends keep
    edge_rule <- if (is.null(criteria$edge_levels)) 'plan' else criteria$edge_levels
    for (i in which(standards$role == 'inner' & standards$position %in% c('lowest', 'highest'))) {
        notes <- c(notes, list(lin_message(paste0('note.edge_', edge_rule, '_', standards$position[i]),
                                           conc = signif(standards$conc[i], 3), bias = standards$bias.limit[i],
                                           cv = standards$cv.limit[i])))
    }
    # The IDs of the standard levels, once, from the whole plan of the plate
    level_map <- standard_level_map(data$conc[data$type == 'standard'])
    # Levels whose fitted wells are not all back-calculated
    # A curve that turns inside the interval has one warning of its own (below)
    for (i in which(standards$status == 'incomplete' & monotonic)) {
        lost <- standards$failed[[i]]
        wells <- lapply(seq_len(nrow(lost)), function(k) {
            lin_message('backcalc.well', well = lost$key[k], reason = lin_message(paste0('backcalc.', lost$reason[k])))
        })
        notes <- c(notes, list(lin_message('note.incomplete_backcalc',
                                           level = standard_names(standards$conc[i], level_map),
                                           conc = signif(standards$conc[i], 3), n = standards$n.calc[i],
                                           total = standards$n[i], wells = wells)))
    }
    conc_range <- fit$conc_range
    if (criteria$range == 'accepted') {
        if (is.na(calibration$lloq)) {
            notes <- c(notes, list(lin_message('note.no_accepted_standards')))
        } else if (calibration$lloq > conc_range[1] || calibration$uloq < conc_range[2]) {
            conc_range <- c(calibration$lloq, calibration$uloq)
            # A rejected calibration has no quantification range (see below)
            if (calibration$accepted) {
                notes <- c(notes, list(lin_message('note.range_narrowed', lloq = signif(conc_range[1], 3),
                                                   uloq = signif(conc_range[2], 3))))
            }
        }
    }
    if (!is.null(fit$interval)) {
        conc_range <- c(max(conc_range[1], fit$interval[1]), min(conc_range[2], fit$interval[2]))
    }
    # The concentrations of a rejected calibration are kept for inspection, not for reporting
    if (!calibration$accepted) {
        notes <- c(notes, list(lin_message('note.not_reportable', lloq = signif(conc_range[1], 3),
                                           uloq = signif(conc_range[2], 3))))
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

    # Zero for a signal at the background only where a weak signal means a low concentration
    increasing <- curve_increasing(fit)
    if (blank_zero && !isTRUE(increasing)) {
        notes <- c(notes, list(lin_message('note.blank_zero_decreasing')))
        blank_zero <- FALSE
    }

    # Samples and QC: inverse prediction, then the signal, the inverse and the range
    # (classify_wells()) give the flag, as for the calibrators; dilution correction
    quantify <- function(rows, extrapolate = character(0), blank_zero = FALSE) {
        rows$dilution[is.na(rows$dilution)] <- 1
        pred <- inverse_predict_ci(fit, rows$OD, level)
        cls <- classify_wells(fit, rows$OD, pred$conc, conc_range, at_blank)
        flag <- cls$flag
        ok <- flag == FLAGS$ok |
            (flag == FLAGS$below & 'below' %in% extrapolate) |
            (flag == FLAGS$above & 'above' %in% extrapolate)
        # Full precision: the statuses of the QC and the samples are assessed on it, the tables
        # round only what they show (see round_result())
        rows$conc <- ifelse(ok, pred$conc * rows$dilution, NA_real_)
        rows$conc.lower <- ifelse(ok, pred$lower * rows$dilution, NA_real_)
        rows$conc.upper <- ifelse(ok, pred$upper * rows$dilution, NA_real_)
        # A concentration within the range is kept even if the signal is at the background
        if (blank_zero) rows$conc[cls$at.blank & flag != FLAGS$ok] <- 0
        at_blank_well <- cls$at.blank
        # The background chosen is unavailable: no concentration from the OD as measured
        if (bg$empty) {
            rows$conc <- rows$conc.lower <- rows$conc.upper <- rep(NA_real_, nrow(rows))
            flag <- rep(FLAGS$no_background, nrow(rows))
            at_blank_well <- rep(NA, nrow(rows))
        }
        corrected <- rows$OD
        rows$OD <- round(rows$.od_raw, 4)
        rows$OD.corrected <- if (bg$empty) NA_real_ else round(corrected, 4)
        rows$.od_raw <- NULL
        rows$flag <- flag
        # The signal at or below the background is shown apart from the flag, when it occurs
        if (any(at_blank_well, na.rm = TRUE)) rows$OD.le.blank <- at_blank_well
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
        qc$recovery <- 100 * measured$conc / qc_rows$conc
        qc$OD <- measured$OD
        qc$flag <- measured$flag
        qc$OD.le.blank <- measured$OD.le.blank
        qc <- drop_empty_columns(qc, c('well', 'id'))
    }

    manual <- std$excluded[std$excluded$reason == FLAGS$excluded, , drop = FALSE]
    exclusions <- data.frame(standard = standard_names(manual$conc, level_map), key = manual$.key,
                             conc = manual$conc, OD = round(manual$OD, 4), reason = manual$comment)

    # With points excluded by the user, the fit on all standards is kept for comparison
    original_fit <- NULL
    effect <- NULL
    if (nrow(manual) > 0) {
        all_std <- split_standards(data[data$type == 'standard', , drop = FALSE], log_scale, at_blank = at_blank)$used
        original_fit <- tryCatch(fit_curve(all_std, model, linear, weights), error = function(e) NULL)
        if (!is.null(original_fit)) original_fit$interval <- fit$interval
        if (!is.null(original_fit)) {
            # The levels accepted after the exclusions are those of the standards table
            effect <- exclusion_effect(original_fit, fit, all_std, manual$.key, criteria, levels = level_map,
                                       wells = std_wells, assessed = standards)
        }
    }

    rownames(standard_data) <- NULL
    # The standards keep the OD as measured next to the corrected one
    standard_data$OD.raw <- standard_data$.od_raw
    standard_data$.od_raw <- NULL
    sample_data$type <- NULL
    sample_data$.key <- NULL
    sample_data <- drop_empty_columns(sample_data, c('well', 'id'))

    quality <- assess_run(fit, standards, calibration, sample_data, qc, conc_range, lof, criteria,
                          background = bg$status)
    notes <- c(notes, quality$reasons, quality$warnings)
    # The notes of a curve fitted on the OD as measured would mislead: only the reason is kept
    if (bg$empty) notes <- quality$reasons

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
         qc = qc,
         at_blank = at_blank,
         increasing = increasing,
         standard_keys = as.character(data$.key[data$type == 'standard']),
         standard_levels = level_map)
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
