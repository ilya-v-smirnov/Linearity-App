#' Result Flags
#'
#' Status codes assigned to every sample well. Only wells flagged `"OK"`
#' receive a numeric concentration; all other wells get `NA` in `conc`.
#' The codes are stored in English; use [translate_flags()] for display.
#'
#' - `ok` (`"OK"`): the OD lies within the calibration range.
#' - `below` (`"< LLOQ"`): below the lowest standard used in the fit.
#' - `above` (`"> ULOQ"`): above the highest standard used in the fit.
#' - `od_le_nc` (`"OD <= NC"`): background-subtracted OD is zero or negative.
#' - `no_solution` (`"No solution"`): the OD cannot be reached by the quadratic curve.
#' - `missing` (`"Missing OD"`): the OD value is empty.
#'
#' @export
FLAGS <- list(ok = 'OK',
              below = '< LLOQ',
              above = '> ULOQ',
              od_le_nc = 'OD <= NC',
              no_solution = 'No solution',
              missing = 'Missing OD')


#' Calibration Model Codes
#'
#' @return Character vector of supported model codes.
#' @export
calibration_models <- function() c('LRM', 'QRM')


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


#' Estimate the Background (Negative Control) Value
#'
#' The background is the geometric mean of NC optical densities. If some NC
#' values are zero or negative, the arithmetic mean is used instead.
#'
#' @param od Numeric vector of NC optical densities.
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
#' NC wells if present, otherwise blank wells.
#'
#' @param type Vector of well types.
#'
#' @return `"NC"` or `"blank"`.
#' @keywords internal
background_type <- function(type) {
    if (any(type %in% 'NC')) 'NC' else 'blank'
}


#' Subtract Background Values
#'
#' @param data A data.frame with columns `type` and `OD`.
#' @param nc_value Background value; estimated with [background_value()] from
#'   NC wells (or blank wells if there are no NC wells) if `NULL`.
#'
#' @return A data.frame without NC and blank rows, with background-subtracted `OD`.
#' @keywords internal
subtract_nc <- function(data, nc_value = NULL) {
    if (is.null(nc_value)) {
        nc_value <- background_value(data$OD[data$type %in% background_type(data$type)])$value
    }
    out <- data[!data$type %in% c('NC', 'blank'), , drop = FALSE]
    out$OD <- out$OD - nc_value
    out
}


##################### CALIBRATION MODELS #######################################

#' Fit a Calibration Model
#'
#' `fit_lrm()` fits OD ~ conc, `fit_qrm()` fits OD ~ conc^2 + conc; both on
#' log10 scales unless `linear` is `TRUE`.
#'
#' @param data Background-subtracted standard data.
#' @param xvar,yvar Names of the concentration and OD columns.
#' @param linear Logical; if `TRUE` the model is fitted on untransformed values.
#'
#' @return An object of class `lm`.
#' @name fit_models
#' @keywords internal
NULL

#' @rdname fit_models
fit_lrm <- function(data, xvar = 'conc', yvar = 'OD', linear = FALSE) {
    if (linear) {
        formula_str <- paste0(yvar, ' ~ ', xvar)
    } else {
        formula_str <- paste0('log10(', yvar, ') ~ log10(', xvar, ')')
    }
    lm(as.formula(formula_str), data)
}

#' @rdname fit_models
fit_qrm <- function(data, xvar = 'conc', yvar = 'OD', linear = FALSE) {
    if (linear) {
        formula_str <- paste0(yvar, ' ~ I(', xvar, '^2) + ', xvar)
    } else {
        formula_str <- paste0('log10(', yvar, ') ~ I(log10(', xvar, ')^2) + log10(', xvar, ')')
    }
    lm(as.formula(formula_str), data)
}


#' Name Model Coefficients
#'
#' Maps `lm` coefficient names to the notation y = a*x^2 + b*x + c.
#'
#' @param term_names Character vector of coefficient names.
#'
#' @return Character vector with values `"a"`, `"b"` or `"c"`.
#' @keywords internal
coef_labels <- function(term_names) {
    ifelse(term_names == '(Intercept)', 'c',
           ifelse(grepl('^2', term_names, fixed = TRUE), 'a', 'b'))
}


#' Curve Coefficients as a Named List
#'
#' @param fit A fitted calibration model.
#'
#' @return A list with `a` (0 for linear models), `b` and `c`.
#' @keywords internal
curve_coefs <- function(fit) {
    cf <- coef(fit)
    lab <- coef_labels(names(cf))
    list(a = if ('a' %in% lab) unname(cf[lab == 'a']) else 0,
         b = unname(cf[lab == 'b']),
         c = unname(cf[lab == 'c']))
}


#' Extract Coefficients and Confidence Intervals
#'
#' @param fit A fitted calibration model (`lm`).
#' @param round_fun Optional rounding function (e.g. `signif`).
#' @param round_par Optional rounding parameter.
#'
#' @return A data.frame with columns `coef`, `est`, `ci.lower`, `ci.upper`.
#' @export
get_coef_ci <- function(fit, round_fun = NULL, round_par = NULL) {
    if (!inherits(fit, 'lm')) {
        stop('Unsupported model object', call. = FALSE)
    }
    tab <- as.data.frame(cbind(coef(fit), confint(fit)))
    tab <- stats::setNames(tab, c('est', 'ci.lower', 'ci.upper'))
    tab$coef <- coef_labels(rownames(tab))
    tab <- tab[order(tab$coef), c('coef', 'est', 'ci.lower', 'ci.upper')]
    rownames(tab) <- NULL

    if (!is.null(round_fun) && !is.null(round_par)) {
        num <- c('est', 'ci.lower', 'ci.upper')
        tab[num] <- lapply(tab[num], round_fun, round_par)
    }
    tab
}


#' Inverse Prediction from the Calibration Curve
#'
#' Calculates the concentration that corresponds to a background-subtracted
#' OD by inverting the fitted curve OD = f(conc). For the quadratic model the
#' root on the same branch of the parabola as the standards is returned.
#'
#' @param fit A fitted calibration model.
#' @param od Numeric vector of background-subtracted ODs.
#' @param conc_range Range of standard concentrations used in the fit.
#' @param linear Logical; `TRUE` if the model was fitted on untransformed values.
#'
#' @return Numeric vector of concentrations (`NA` where no solution exists or OD <= 0).
#' @keywords internal
inverse_predict <- function(fit, od, conc_range, linear = FALSE) {
    k <- curve_coefs(fit)

    y <- rep(NA_real_, length(od))
    positive <- !is.na(od) & od > 0
    y[positive] <- if (linear) od[positive] else log10(od[positive])

    x_lim <- if (linear) conc_range else log10(conc_range)

    if (k$a == 0) {
        x <- (y - k$c) / k$b
    } else {
        disc <- k$b^2 - 4 * k$a * (k$c - y)
        vertex <- -k$b / (2 * k$a)
        side <- if (mean(x_lim) >= vertex) 1 else -1
        x <- rep(NA_real_, length(y))
        ok <- !is.na(disc) & disc >= 0
        x[ok] <- vertex + side * sqrt(disc[ok]) / (2 * abs(k$a))
    }

    if (linear) x else 10^x
}


#' Check Monotonicity of the Calibration Curve
#'
#' @inheritParams inverse_predict
#'
#' @return `TRUE` if the curve is monotonic within the standard range.
#' @keywords internal
is_monotonic <- function(fit, conc_range, linear = FALSE) {
    k <- curve_coefs(fit)
    if (k$a == 0) return(TRUE)
    x_lim <- if (linear) conc_range else log10(conc_range)
    vertex <- -k$b / (2 * k$a)
    !(vertex > x_lim[1] && vertex < x_lim[2])
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


#' Select Standard Wells Usable for Fitting
#'
#' Standards with a missing OD, or (on log scales) a non-positive OD after
#' background subtraction, cannot be used for fitting.
#'
#' @param standard_data Background-subtracted standard rows.
#' @param linear Logical.
#'
#' @return A list with `used` and `excluded` data.frames; `excluded` has a
#'   `reason` column with flag codes.
#' @keywords internal
split_standards <- function(standard_data, linear = FALSE) {
    reason <- rep(NA_character_, nrow(standard_data))
    if (!linear) reason[!is.na(standard_data$OD) & standard_data$OD <= 0] <- FLAGS$od_le_nc
    reason[is.na(standard_data$OD)] <- FLAGS$missing

    excluded <- standard_data[!is.na(reason), , drop = FALSE]
    excluded$reason <- reason[!is.na(reason)]
    list(used = standard_data[is.na(reason), , drop = FALSE],
         excluded = excluded)
}


#' Calculate Sample Concentrations
#'
#' Subtracts the background, fits the calibration curve OD = f(conc) on the
#' standards, inverts the curve for the samples and multiplies the result by
#' the dilution factor. Samples outside the calibration range are flagged and
#' receive `NA` instead of a concentration.
#'
#' @param data Validated input data (see [check_input_file()]).
#' @param model `"LRM"` or `"QRM"` (see [calibration_models()]).
#' @param linear Logical; if `TRUE` the model is fitted on untransformed values.
#' @param round_fun,round_par Rounding applied to the final concentrations only.
#'
#' @return A list with components:
#'   * `model_direct`: fitted calibration model (OD = f(conc));
#'   * `model`: model code;
#'   * `linear_scale`: logical;
#'   * `nc_value`: background value subtracted from all ODs;
#'   * `standard_data`: standards used in the fit;
#'   * `excluded_standards`: standards excluded from the fit with a `reason`;
#'   * `calibration_range`: range of standard concentrations used in the fit;
#'   * `notes`: warnings as a list of messages (see [message_text()]);
#'   * `result`: sample results with `conc` (dilution-corrected) and `flag`;
#'   * `qc`: quality controls with nominal and measured concentration,
#'     recovery (%) and flag (`NULL` if there are no QC wells).
#'
#' The background is the geometric mean of NC wells, or of blank wells if
#' there are no NC wells. Columns `well` and `id` are kept in the results
#' when they contain values.
#' @export
#' @examples
#' path <- system.file('extdata', 'griess.xlsx', package = 'LinearityApp')
#' data <- format_input_table(check_input_file(read_table(path)))
#' res <- calculate(data, 'LRM')
#' head(res$result)
#' message_text(res$notes, 'en')
calculate <- function(data, model = 'LRM', linear = FALSE,
                      round_fun = signif, round_par = 3) {
    if (length(model) != 1 || !model %in% calibration_models()) {
        lin_abort('error.unknown_model', model = model, choices = calibration_models())
    }

    # Background subtraction
    bg_type <- background_type(data$type)
    bg <- background_value(data$OD[data$type %in% bg_type])
    notes <- bg$notes
    if (bg_type == 'blank') notes <- c(notes, list(lin_message('note.blank_background')))
    data <- subtract_nc(data, bg$value)

    # Standards used for fitting
    std <- split_standards(data[data$type == 'standard', , drop = FALSE], linear)
    standard_data <- std$used
    if (nrow(std$excluded) > 0) {
        notes <- c(notes, list(lin_message('note.standards_excluded',
                                           n = nrow(std$excluded),
                                           details = flag_counts(std$excluded$reason))))
    }
    min_levels <- if (model == 'QRM') 4 else 3
    n_levels <- length(unique(standard_data$conc))
    if (n_levels < min_levels) {
        lin_abort('error.too_few_levels', min = min_levels, found = n_levels)
    }

    # Fit OD = f(conc)
    fit_fun <- switch(model, 'LRM' = fit_lrm, 'QRM' = fit_qrm)
    fit <- fit_fun(standard_data, xvar = 'conc', yvar = 'OD', linear = linear)
    conc_range <- range(standard_data$conc)

    if (!is_monotonic(fit, conc_range, linear)) {
        notes <- c(notes, list(lin_message('note.not_monotonic')))
    }

    # Samples and QC: inverse prediction, flags, dilution correction
    quantify <- function(rows) {
        rows$dilution[is.na(rows$dilution)] <- 1
        conc <- inverse_predict(fit, rows$OD, conc_range, linear)
        flag <- assign_flags(rows$OD, conc, conc_range)
        conc[flag != FLAGS$ok] <- NA_real_
        rows$conc <- round_fun(conc * rows$dilution, round_par)
        rows$OD <- round(rows$OD, 4)
        rows$flag <- flag
        rownames(rows) <- NULL
        rows
    }

    sample_data <- quantify(data[data$type == 'sample', , drop = FALSE])
    bad <- sample_data$flag[sample_data$flag != FLAGS$ok]
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

    rownames(standard_data) <- NULL
    sample_data$type <- NULL
    sample_data <- drop_empty_columns(sample_data, c('well', 'id'))

    list(model_direct = fit,
         model = model,
         linear_scale = linear,
         nc_value = bg$value,
         standard_data = standard_data,
         excluded_standards = std$excluded,
         calibration_range = conc_range,
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
