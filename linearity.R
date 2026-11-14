library(dplyr)
library(ggplot2)
library(cowplot)

# Excel files are read with readxl and written with openxlsx (both are called
# with explicit namespaces, so neither package needs to be attached).


######################## CONSTANTS #############################################

#' Result flags
#'
#' Status assigned to every sample well. Only wells flagged "OK" receive a
#' numeric concentration; all other wells get NA in the `conc` column.
#'
#' - OK          : the OD lies within the calibration range.
#' - < LLOQ      : the estimate is below the lowest standard used in the fit.
#' - > ULOQ      : the estimate is above the highest standard used in the fit.
#' - OD <= NC    : the background-subtracted OD is zero or negative.
#' - No solution : the OD cannot be reached by the fitted curve (quadratic model).
#' - Missing OD  : the OD value is empty.

FLAGS <- list(ok = 'OK',
              below = '< LLOQ',
              above = '> ULOQ',
              od_le_nc = 'OD <= NC',
              no_solution = 'No solution',
              missing = 'Missing OD')

MODELS <- c('LRM' = 'Linear regression model',
            'QRM' = 'Quadratic regression model')


##################### BACKGROUND (NC) ##########################################

#' Calculate Geometric Mean
#'
#' Computes the geometric mean of a numeric vector of positive values.
#' Missing values are ignored. No rounding is applied.
#'
#' @param x Numeric vector.
#'
#' @return A numeric value (NA if `x` has no non-missing values or contains
#'   non-positive values).

geom_mean <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0 || any(x <= 0)) return(NA_real_)
    10^mean(log10(x))
}


#' Estimate the Background (Negative Control) Value
#'
#' The background is the geometric mean of the NC optical densities. If some NC
#' values are zero or negative (e.g. the reader already subtracted a blank), the
#' geometric mean is undefined and the arithmetic mean is used instead.
#'
#' @param od Numeric vector of NC optical densities.
#'
#' @return A list with `value` (numeric) and `note` (character, possibly empty).

background_value <- function(od) {
    od <- od[!is.na(od)]
    if (length(od) == 0) {
        stop('No valid OD values in NC wells')
    }
    if (any(od <= 0)) {
        return(list(value = mean(od),
                    note = paste('Some NC values are zero or negative:',
                                 'the arithmetic mean was used as background',
                                 'instead of the geometric mean.')))
    }
    list(value = geom_mean(od), note = character(0))
}


#' Subtract Negative Control Values
#'
#' Removes NC rows and subtracts the background value from the OD of all other rows.
#'
#' @param data A data.frame containing at least the columns `type` and `OD`.
#' @param nc_value Background value; if NULL it is estimated with [background_value()].
#'
#' @return A data.frame without NC rows, with background-subtracted `OD`.

subtract_nc <- function(data, nc_value = NULL) {
    if (is.null(nc_value)) {
        nc_value <- background_value(data$OD[data$type == 'NC'])$value
    }
    out <- data[data$type != 'NC', , drop = FALSE]
    out$OD <- out$OD - nc_value
    out
}


##################### CALIBRATION MODELS #######################################

#' Calculate Ordinary Linear Regression Model (LRM)
#'
#' Fits OD ~ conc (linear = TRUE) or log10(OD) ~ log10(conc) (linear = FALSE).
#'
#' @param data A data.frame with background-subtracted standard data.
#' @param xvar,yvar Names of the concentration and OD columns.
#' @param linear Logical; if TRUE the model is fitted on untransformed values.
#'
#' @return An object of class `lm`.

calculate_LRM <- function(data, xvar = 'conc', yvar = 'OD', linear = FALSE) {
    if (linear) {
        formula_str <- paste0(yvar, ' ~ ', xvar)
    } else {
        formula_str <- paste0('log10(', yvar, ') ~ log10(', xvar, ')')
    }
    lm(as.formula(formula_str), data)
}


#' Calculate Quadratic Regression Model (QRM)
#'
#' Fits OD ~ conc^2 + conc (linear = TRUE) or the same model on log10 scales.
#'
#' @inheritParams calculate_LRM
#'
#' @return An object of class `lm`.

calculate_QRM <- function(data, xvar = 'conc', yvar = 'OD', linear = FALSE) {
    if (linear) {
        formula_str <-
            paste0(yvar, ' ~ I(' , xvar, '^2) + ', xvar)
    } else {
        formula_str <-
            paste0('log10(', yvar, ') ~ I(log10(', xvar, ')^2) + log10(', xvar, ')')
    }
    lm(as.formula(formula_str), data)
}


#' Name Model Coefficients
#'
#' Maps lm coefficient names to the notation y = a*x^2 + b*x + c.
#'
#' @param term_names Character vector of coefficient names from `coef(fit)`.
#'
#' @return Character vector with values "a", "b" or "c".

coef_labels <- function(term_names) {
    ifelse(term_names == '(Intercept)', 'c',
           ifelse(grepl('^2', term_names, fixed = TRUE), 'a', 'b'))
}


#' Curve Coefficients as a Named List
#'
#' @param fit A fitted `lm` calibration model.
#'
#' @return A list with numeric elements `a` (0 for linear models), `b` and `c`.

curve_coefs <- function(fit) {
    cf <- coef(fit)
    lab <- coef_labels(names(cf))
    list(a = if ('a' %in% lab) unname(cf[lab == 'a']) else 0,
         b = unname(cf[lab == 'b']),
         c = unname(cf[lab == 'c']))
}


#' Extract Coefficients and Confidence Intervals
#'
#' @param fit A fitted `lm` calibration model.
#' @param round_fun Optional rounding function (e.g. `signif`).
#' @param round_par Optional rounding parameter.
#'
#' @return A data.frame with columns `coef`, `est`, `ci.lower`, `ci.upper`.

get_coef_ci <- function(fit, round_fun = NULL, round_par = NULL) {
    if (!inherits(fit, 'lm')) {
        stop('Unsupported model object')
    }
    tab <- as.data.frame(cbind(coef(fit), confint(fit)))
    tab <- setNames(tab, c('est', 'ci.lower', 'ci.upper'))
    tab$coef <- coef_labels(rownames(tab))
    tab <- tab[order(tab$coef), c('coef', 'est', 'ci.lower', 'ci.upper')]
    rownames(tab) <- NULL

    if (!is.null(round_fun) && !is.null(round_par)) {
        tab <- mutate(tab, across(where(is.numeric), ~ round_fun(., round_par)))
    }
    tab
}


#' Inverse Prediction from the Calibration Curve
#'
#' Calculates the concentration that corresponds to a given (background-subtracted)
#' OD by inverting the fitted curve OD = f(conc). For the quadratic model the root
#' lying on the same branch of the parabola as the standards is returned.
#'
#' @param fit A fitted `lm` calibration model (OD as a function of concentration).
#' @param od Numeric vector of background-subtracted ODs.
#' @param conc_range Range of standard concentrations used in the fit.
#' @param linear Logical; TRUE if the model was fitted on untransformed values.
#'
#' @return Numeric vector of concentrations (NA where no solution exists or OD <= 0).

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


#' Check Monotonicity of the Quadratic Curve
#'
#' @inheritParams inverse_predict
#'
#' @return TRUE if the curve is monotonic within the standard range.

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


#' Summarise Flags as a Sentence
#'
#' @param flags Character vector of result flags.
#'
#' @return Character string (empty vector if all wells are OK).

summarise_flags <- function(flags) {
    bad <- flags[flags != FLAGS$ok]
    if (length(bad) == 0) return(character(0))
    counts <- table(factor(bad, levels = unique(bad)))
    paste0(length(bad), ' of ', length(flags),
           ' sample wells were not quantified: ',
           paste0(names(counts), ' (', counts, ')', collapse = ', '), '.')
}


#' Select Standard Wells Usable for Fitting
#'
#' Standards with a missing OD, or (on log scales) with an OD that is not positive
#' after background subtraction, cannot be used for fitting.
#'
#' @param standard_data Background-subtracted standard rows.
#' @param linear Logical; TRUE if the model is fitted on untransformed values.
#'
#' @return A list with `used` and `excluded` data.frames; `excluded` has a
#'   `reason` column.

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
#' standards, inverts the curve for the samples and multiplies the result by the
#' dilution factor. Samples outside the calibration range are flagged and receive
#' NA instead of a concentration.
#'
#' @param data Validated input data (see [check_input_file()]).
#' @param model 'LRM' or 'QRM'.
#' @param xvar,yvar Kept for backward compatibility; must be 'conc' and 'OD'.
#' @param linear Logical; if TRUE the model is fitted on untransformed values.
#' @param round_fun,round_par Rounding applied to the final concentrations only.
#'
#' @return A list with components:
#'   $model_direct: fitted calibration model (OD = f(conc));
#'   $model: model code;
#'   $linear_scale: logical;
#'   $nc_value: background value subtracted from all ODs;
#'   $standard_data: standards used in the fit;
#'   $excluded_standards: standards excluded from the fit with a reason;
#'   $calibration_range: range of standard concentrations used in the fit;
#'   $notes: character vector of warnings for the user;
#'   $result: sample results with `conc` (dilution-corrected) and `flag` columns.

calculate <- function(data, model = 'LRM',
                      xvar = 'conc', yvar = 'OD', linear = FALSE,
                      round_fun = signif, round_par = 3) {
    if (!model %in% names(MODELS)) {
        stop('Unknown model: ', model, '. Use one of: ',
             paste(names(MODELS), collapse = ', '))
    }
    notes <- character(0)

    # Background subtraction
    bg <- background_value(data$OD[data$type == 'NC'])
    notes <- c(notes, bg$note)
    data <- subtract_nc(data, bg$value)

    # Standards used for fitting
    std <- split_standards(data[data$type == 'standard', , drop = FALSE], linear)
    standard_data <- std$used
    if (nrow(std$excluded) > 0) {
        counts <- table(std$excluded$reason)
        notes <- c(notes,
                   paste0(nrow(std$excluded), ' standard well(s) excluded from the fit: ',
                          paste0(names(counts), ' (', counts, ')', collapse = ', '), '.'))
    }
    min_levels <- if (model == 'QRM') 4 else 3
    n_levels <- length(unique(standard_data$conc))
    if (n_levels < min_levels) {
        stop('At least ', min_levels, ' standard concentration levels with valid OD ',
             'are required for the ', MODELS[[model]], ' (found ', n_levels, ')')
    }

    # Fit OD = f(conc)
    lin_fun <- switch(model, 'LRM' = calculate_LRM, 'QRM' = calculate_QRM)
    fit <- lin_fun(standard_data, xvar = 'conc', yvar = 'OD', linear = linear)
    conc_range <- range(standard_data$conc)

    if (!is_monotonic(fit, conc_range, linear)) {
        notes <- c(notes,
                   paste('The quadratic curve is not monotonic within the standard range:',
                         'concentrations near the turning point are unreliable.'))
    }

    # Samples: inverse prediction, flags, dilution correction
    sample_data <- data[data$type == 'sample', , drop = FALSE]
    conc <- inverse_predict(fit, sample_data$OD, conc_range, linear)
    flag <- assign_flags(sample_data$OD, conc, conc_range)
    conc[flag != FLAGS$ok] <- NA_real_

    sample_data$conc <- round_fun(conc * sample_data$dilution, round_par)
    sample_data$OD <- round(sample_data$OD, 4)
    sample_data$flag <- flag
    notes <- c(notes, summarise_flags(flag))

    rownames(sample_data) <- NULL
    rownames(standard_data) <- NULL

    list(model_direct = fit,
         model = model,
         linear_scale = linear,
         nc_value = bg$value,
         standard_data = standard_data,
         excluded_standards = std$excluded,
         calibration_range = conc_range,
         notes = notes,
         result = select(sample_data, -type))
}


#' Aggregate Results by Grouping Parameters
#'
#' @param result Result table from [calculate()].
#' @param group_vars Character vector of grouping columns.
#' @param round_fun,round_par Rounding of the aggregated statistics.
#'
#' @return A data.frame with `n` (all wells), `n.quantified` (wells with a
#'   concentration), `mean`, `min` and `max` of quantified concentrations.

aggregate_result <- function(result, group_vars = 'sample.name',
                             round_fun = signif, round_par = 3) {
    if (length(group_vars) == 0) return(NULL)

    stat_or_na <- function(f, x) if (all(is.na(x))) NA_real_ else f(x, na.rm = TRUE)

    result %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(n = n(),
                  n.quantified = sum(!is.na(conc)),
                  mean = stat_or_na(base::mean, conc),
                  min = stat_or_na(base::min, conc),
                  max = stat_or_na(base::max, conc),
                  .groups = 'drop') %>%
        mutate(across(c('mean', 'min', 'max'), ~ round_fun(., round_par))) %>%
        as.data.frame()
}


######################## THEME & PLOTTING ######################################


#' Custom ggplot2 Theme for Plots

my_theme <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11, face = "plain"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))

theme_set(my_theme)


#' Design X-Axis Scale for Logarithmic Data
#'
#' @param x Numeric vector representing the x-values.
#' @param round_par Number of significant digits for the break labels.

design_x_scale <- function(x, round_par = 3) {
    x <- x[!is.na(x)]
    log_x <- log10(x[x > 0])
    unique_x <- unique(log_x)
    if (length(unique_x) < 2) return(signif(10^unique_x, round_par))
    min_dist <- min(dist(unique_x))
    range_x <- range(unique_x)
    min_dist_perc <- min_dist * 100 / diff(range_x)
    if (length(unique_x) > 10 || min_dist_perc < 5) {
        splited_x <- seq(range_x[1], range_x[2], length.out = 8)
        return(signif(10^splited_x, round_par))
    } else {
        return(signif(10^log_x, round_par))
    }
}


#' Quadratic Calibration Function
#'
#' y = a*x^2 + b*x + c, on log10 scales unless `linear` is TRUE.
#'
#' @param x Concentrations.
#' @param a,b,c Coefficients.
#' @param linear Logical.

qrm_fun <- function(x, a = 0, b, c, linear = FALSE) {
    if (linear) {
        return(a * x^2 + b * x + c)
    }
    x <- log10(x)
    10^(a * x^2 + b * x + c)
}


#' Plot Standard Curve with Fitted Regression
#'
#' @param standard_data Standards used in the fit.
#' @param fit Fitted calibration model.
#' @param linear Logical.
#' @param xvar,yvar Column names.
#' @param xtitle,ytitle Axis titles.
#'
#' @return A ggplot2 object.

plot_standard_curve <- function(standard_data,
                                fit,
                                linear = FALSE,
                                xvar = 'conc', yvar = 'OD',
                                xtitle = '', ytitle = '') {
    arg_list <- curve_coefs(fit)
    arg_list$linear <- linear

    plot <-
        ggplot(standard_data, aes(.data[[xvar]], .data[[yvar]])) +
        geom_point(size = 1.5, alpha = 0.5) +
        geom_function(fun = qrm_fun, args = arg_list,
                      color = 'blue',
                      linewidth = 0.75) +
        labs(x = xtitle, y = ytitle)

    if (!linear) {
        x_scale <- design_x_scale(standard_data[[xvar]])

        plot <- plot +
            scale_x_log10(breaks = x_scale,
                          labels = as.character(x_scale)) +
            scale_y_log10()
    }

    plot
}


#' Retrieve Fitted Values and Residuals
#'
#' @param fit Fitted calibration model.
#' @param linear Logical; if FALSE fitted values are back-transformed from log10.
#'
#' @return A data.frame with columns `fitted` (OD scale) and `resid`
#'   (on the scale of the fit).

get_fit_resid <- function(fit, linear = FALSE) {
    df <- data.frame(fitted = unname(fitted(fit)),
                     resid = unname(resid(fit)))
    if (!linear) {
        df$fitted <- 10^df$fitted
    }
    df
}


#' Plot Fitted Values vs. Residuals
#'
#' @inheritParams get_fit_resid
#'
#' @return A ggplot2 object.

plot_fitted_resid <- function(fit, linear = FALSE) {
    data <- get_fit_resid(fit, linear = linear)

    plot <-
        ggplot(data, aes(fitted, resid)) +
        geom_point(size = 1.5, alpha = 0.5) +
        geom_hline(yintercept = 0) +
        geom_smooth(method = 'loess',
                    formula = y ~ x,
                    color = 'blue',
                    se = FALSE,
                    span = 2,
                    linewidth = 0.75) +
        labs(x = 'Fitted OD', y = 'Residuals')

    if (!linear) {
        x_scale <- design_x_scale(data$fitted)

        plot <- plot +
            scale_x_log10(breaks = x_scale,
                          labels = as.character(x_scale))
    }

    plot
}


#' Generate Combined Plots for Standard Curve and Residuals
#'
#' @param result Output of [calculate()].
#' @param xvar,yvar Column names.
#' @param xtitle,ytitle Axis titles of the standard curve.
#' @param plot.theme Optional ggplot2 theme added to both plots.
#'
#' @return A cowplot grid.

get_plots <- function(result,
                      xvar = 'conc', yvar = 'OD',
                      xtitle = '', ytitle = '',
                      plot.theme = NULL) {

    standard_plot <-
        plot_standard_curve(
            standard_data = result$standard_data,
            fit = result$model_direct,
            linear = result$linear_scale,
            xvar = xvar, yvar = yvar,
            xtitle = xtitle, ytitle = ytitle
        )

    fit_resid_plot <-
        plot_fitted_resid(result$model_direct,
                          linear = result$linear_scale)

    if (!is.null(plot.theme)) {
        standard_plot <- standard_plot + plot.theme
        fit_resid_plot <- fit_resid_plot + plot.theme
    }

    plot_grid(standard_plot, fit_resid_plot)
}


###################### SUPPLEMENTARY FUNCTIONS #################################

#' Create Experiment Data Template
#'
#' @param start Highest standard concentration.
#' @param step Dilution step of the standard series (> 1).
#' @param n.standard Number of replicate standard series.
#' @param n.sample Number of sample rows.
#' @param n.levels Number of standard concentration levels.
#'
#' @return A data.frame with columns sample.name, sample.date, type, dilution,
#'   conc and OD.
#'
#' @examples
#' template <- make_template(start = 100, step = 3, n.standard = 3, n.sample = 15)

make_template <- function(start = 100, step = 2,
                          n.standard = 2, n.sample = 10, n.levels = 7) {
    if (!is.numeric(start) || length(start) != 1 || is.na(start) || start <= 0) {
        stop('Start concentration must be a positive number')
    }
    if (!is.numeric(step) || length(step) != 1 || is.na(step) || step <= 1) {
        stop('Dilution step must be greater than 1')
    }
    if (n.levels < 3) stop('At least 3 standard levels are required')

    titration <- signif(start / step^(0:(n.levels - 1)), 3)

    standard_part <- data.frame(
        type = rep(c(rep('standard', n.levels), 'NC'), times = n.standard),
        dilution = NA_real_,
        conc = rep(c(titration, NA_real_), times = n.standard)
    )
    sample_part <- data.frame(
        type = rep('sample', n.sample),
        dilution = rep(1, n.sample),
        conc = NA_real_
    )
    template <- rbind(standard_part, sample_part)

    # Empty cells are written as NA so that Excel/CSV files contain blank cells
    data.frame(sample.name = NA_character_,
               sample.date = NA_character_,
               type = template$type,
               dilution = template$dilution,
               conc = template$conc,
               OD = NA_real_)
}


#' Read a Semicolon- or Comma-Delimited CSV File
#'
#' Semicolon-separated files are expected to use a comma as decimal separator,
#' comma-separated files a dot.
#'
#' @param path File path.
#'
#' @return A data.frame.

read_csv_auto <- function(path) {
    dialects <- list(c(sep = ';', dec = ','), c(sep = ',', dec = '.'))
    for (d in dialects) {
        table <- tryCatch(read.csv(path, sep = d[['sep']], dec = d[['dec']],
                                   stringsAsFactors = FALSE),
                          error = function(e) NULL)
        if (!is.null(table) && ncol(table) > 1) return(table)
    }
    stop('Unknown CSV format: expected comma- or semicolon-separated values')
}


#' Read Data Table from File
#'
#' @param path Path to an .xlsx or .csv file.
#'
#' @return A data.frame containing the imported data.

read_table <- function(path) {
    file_type <- tolower(tools::file_ext(path))
    switch(file_type,
           'xlsx' = as.data.frame(readxl::read_excel(path, sheet = 1)),
           'csv' = read_csv_auto(path),
           stop('This file type is not supported: use .xlsx or .csv')
    )
}


#' Validate Input Data for the Linearity App
#'
#' Checks mandatory columns, numeric columns, allowed values of `type`,
#' dilution factors of samples, concentrations of standards and the number of
#' standard wells. Stops with an informative message on the first problem.
#'
#' @param data A data.frame with the raw data.
#'
#' @return The validated data.frame.

check_input_file <- function(data) {
    # Mandatory columns
    mandatory_columns <- c('sample.name', 'sample.date', 'type', 'dilution', 'conc', 'OD')

    for (mc in mandatory_columns) {
        if (!mc %in% colnames(data)) {
            stop(paste('Mandatory', mc, 'column is missing'))
        }
    }

    # Numeric columns
    numeric_columns <- c('dilution', 'conc', 'OD')

    for (nc in numeric_columns) {
        if (!is.numeric(data[[nc]])) {
            stop(paste('Non-numeric values in', nc, 'column'))
        }
    }

    # Values of the type column
    allowed_types <- c('standard', 'sample', 'NC')
    present_types <- unique(data[['type']])

    not_allowed <- setdiff(present_types, allowed_types)
    if (length(not_allowed) > 0) {
        stop(paste('Not allowed values in the type column:',
                   paste(not_allowed, collapse = ', ')))
    }

    missing_types <- setdiff(allowed_types, present_types)
    if (length(missing_types) > 0) {
        stop(paste('Missing values in the type column:',
                   paste(missing_types, collapse = ', ')))
    }

    # Dilution factors of samples
    dilutions <- data$dilution[data$type == 'sample']
    if (any(is.na(dilutions))) {
        stop('Not all samples have dilution factors specified')
    }
    if (!all(dilutions > 0)) {
        stop('All dilution factors must be positive')
    }

    # Concentrations of standards
    std_conc <- data$conc[data$type == 'standard']
    if (any(is.na(std_conc))) {
        stop('Not all standard wells have concentrations specified')
    }
    if (!all(std_conc > 0)) {
        stop('All standard concentrations must be positive')
    }

    # Enough standard wells
    n_standard <- sum(data$type == 'standard')
    if (n_standard <= 4) {
        stop(paste('Insufficient number of standard wells:', n_standard))
    }

    data
}


#' Format Input Data Table
#'
#' Converts date and date-time columns to "dd.mm.yyyy" strings and coerces
#' `sample.name` to character.
#'
#' @param table A data.frame.
#'
#' @return The formatted data.frame.

format_input_table <- function(table) {
    for (col in colnames(table)) {
        if (inherits(table[[col]], c('Date', 'POSIXt'))) {
            table[[col]] <- format(table[[col]], '%d.%m.%Y')
        }
    }
    table$sample.name <- as.character(table$sample.name)
    table
}


#' Save Data Frame as CSV File
#'
#' @param X A data.frame.
#' @param file Output path.
#' @param dialect "comma" (sep ",", dec ".") or "semicolon" (sep ";", dec ",").

save_csv <- function(X, file, dialect = 'comma') {
    switch(dialect,
           'comma' = write.table(X, file, sep = ',', dec = '.',
                                 row.names = FALSE, na = ''),
           'semicolon' = write.table(X, file, sep = ';', dec = ',',
                                     row.names = FALSE, na = ''),
           stop('Unknown CSV dialect: ', dialect)
    )
}


#' Save Data Frame as Excel File
#'
#' @param X A data.frame.
#' @param file Output path.

save_xlsx <- function(X, file) {
    openxlsx::write.xlsx(X, file, rowNames = FALSE, overwrite = TRUE)
}
