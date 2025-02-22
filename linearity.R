
library(dplyr)
library(deming)
library(ggplot2)
library(cowplot)
library(xlsx)
library(stringr)

##################### LINEAR MODELS FUNCTIONS ##################################

#' Calculate Geometric Mean
#'
#' Computes the geometric mean of a numeric vector. If the vector has only one element,
#' that value is returned. Otherwise, the function takes the mean of the log10‐transformed
#' values, exponentiates back (base 10), and rounds the result using the specified number
#' of significant digits.
#'
#' @param x Numeric vector.
#' @param round_fun A function applied to round calculated geometric means (default is signif).
#' @param round_par Integer specifying the number of significant digits (default is 3).
#'
#' @return A numeric value representing the geometric mean.

geom_mean <- function(x, round_fun = signif, round_par = 3) {
    if (length(x) == 1) return(x)
    log10(x[x > 0]) %>% mean() %>% 10^ . %>% round_fun(round_par)
}


#' Subtract Negative Control Values
#'
#' Subtracts the negative control (NC) value from the observed data. The NC value is
#' computed as the geometric mean (using [geom_mean]) of the 'OD' column for rows where
#' the 'type' is "NC". The function then subtracts this NC value from the 'OD' values
#' in the remaining data.
#'
#' @param data A data.frame containing at least the columns 'type' and 'OD'. Additional
#'   grouping variables may be present.
#' @param round_par Integer specifying the number of significant digits (default is 3).
#'
#' @return A data.frame with the adjusted 'OD' values (NC subtracted).

subtract_nc <- function(data, round_fun = signif, round_par = 3) {
    NC_data <- subset(data, type == 'NC')
    nc_value <- NC_data$OD %>% geom_mean(round_fun, round_par)
    subset(data, type != 'NC') %>% 
        mutate(OD = OD - nc_value)
}


#' Calculate Ordinary Linear Regression Model (LRM)
#'
#' Fits a linear regression model on either log10‐transformed variables or raw values.
#' When linear is FALSE (the default), the model is fitted on log10‐transformed data.
#' When linear is TRUE, the model is fitted on the original (non-transformed) values.
#'
#' @param data A data.frame with negative control values already subtracted.
#' @param xvar Character string specifying the independent variable name.
#' @param yvar Character string specifying the dependent variable name.
#' @param linear Logical; if TRUE, fits the model on raw (non-transformed) values (default is FALSE).
#'
#' @return An object of class lm representing the fitted regression model.

calculate_LRM <- function(data, xvar, yvar, linear = FALSE) {
    if (linear) {
        formula_str <- paste0(yvar, ' ~ ', xvar)
    } else {
        formula_str <- paste0('log10(', yvar, ') ~ log10(', xvar, ')')
    }
    lm(as.formula(formula_str), data)
}


#' Calculate Quadratic Regression Model (QRM)
#'
#' Fits a quadratic regression model on either log10‐transformed data or raw values.
#' When linear is FALSE (default), the model is fitted on log10‐transformed data;
#' when linear is TRUE, it is fitted on the original data.
#'
#' @param data A data.frame with negative control values already subtracted.
#' @param xvar Character string specifying the independent variable name.
#' @param yvar Character string specifying the dependent variable name.
#' @param linear Logical; if TRUE, fits the model on raw values (default is FALSE).
#'
#' @return An object of class lm} representing the fitted quadratic regression model.

calculate_QRM <- function(data, xvar, yvar, linear = FALSE) {
    if (linear) {
        formula_str <-
            paste0(yvar, ' ~ I(' , xvar, '^2) + ', xvar)
    } else {
        formula_str <-
            paste0('log10(', yvar, ') ~ I(log10(', xvar, ')^2) + log10(', xvar, ')')
    }
    lm(as.formula(formula_str), data)
}


#' Calculate Deming Regression
#'
#' Fits a Deming regression model using either log10‐transformed variables or raw values.
#' Deming regression accounts for measurement error in both the dependent and independent variables.
#'
#' @param data A data.frame with negative control values already subtracted.
#' @param xvar Character string specifying the independent variable name.
#' @param yvar Character string specifying the dependent variable name.
#' @param linear Logical; if TRUE, fits the model on raw values (default is FALSE).
#'
#' @return An object of class deming representing the fitted Deming regression model.

calculate_deming <- function(data, xvar, yvar, linear = FALSE) {
    if (linear) {
        formula_str <- paste0(yvar, ' ~ ', xvar)
    } else {
        formula_str <- paste0('log10(', yvar, ') ~ log10(', xvar, ')')
    }
    deming(as.formula(formula_str), data = data)
}


#' Extract Coefficients and Confidence Intervals
#'
#' Extracts and formats the coefficients and their confidence intervals from a fitted model.
#' Supports both standard `lm` objects and Deming regression objects.
#' If round_fun and and round_par specified, coefficients are rounded.
#'
#' @param fit A fitted model object (either of class `lm` or `deming`).
#' @param round_fun A function applied to round regression coefficients (default is NULL).
#' @param round_par Integer specifying the number of significant digits for rounding (default is NULL).
#'
#' @return A data.frame with columns: `coef`, `est`, `ci.lower`, and `ci.upper`.

get_coef_ci <- function(fit, round_fun = NULL, round_par = NULL) {
    tab <- 
        if (class(fit) == 'deming') {
            cbind(
                fit$coefficients,
                fit$ci
            )
        } else {
            cbind(
                coef(fit),
                confint(fit)
            )
        }
    if(nrow(tab) == 3) {
        coef <- c('c', 'a', 'b')
    } else {
        coef <- c('c', 'b')
    }
    tab <-
    tab %>% 
        as.data.frame() %>% 
        setNames(c('est', 'ci.lower', 'ci.upper')) %>% 
        mutate(coef = coef) %>%
        arrange(coef)
    
    if (!is.null(round_fun) && !is.null(round_par)) {
        tab <- tab %>% 
            mutate(across(where(is.numeric), ~ round_fun(., round_par)))
    }
    select(tab, coef, everything())
}


#' Retrieve Fitted Values and Residuals
#'
#' Returns a data.frame containing the fitted values and residuals from a fitted regression model.
#' If linear is FALSE (default), the fitted values are back-transformed from the log10 scale.
#' If linear is TRUE, the fitted values are returned without transformation.
#'
#' @param fit A fitted regression model object.
#' @param linear Logical; if TRUE, no back-transformation is applied to the fitted values (default is FALSE).
#'
#' @return A data.frame with columns fitted and resid.

get_fit_resid <- function(fit, linear = FALSE) {
    n <- ncol(fit$model)
    df <- data.frame(fitted = fit$model[,n],
                     resid = resid(fit))
    if (!linear) {
        df$fitted <- 10^df$fitted
    }
    df
}


#' Predict Method for Deming Regression
#'
#' Custom predict method for Deming regression objects. Extracts the regression coefficients,
#' identifies the independent variable from the model, and computes predictions. If the independent
#' variable is log10-transformed, predictions are computed on the log10 scale.
#'
#' @param object A fitted Deming regression model.
#' @param data A data.frame containing the independent variable.
#'
#' @return A numeric vector of predicted values. If the predictor is log-transformed, the result is on the log10 scale.
#'

predict.deming <- function(object, data) {
    coef_ci <- get_coef_ci(object)
    coef <- coef_ci$est
    xvar <- row.names(coef_ci)[1]
    if (str_detect(xvar, 'log10')) {
        xvar <- str_extract(xvar, pattern = '(?<=log10\\().*(?=\\))')
        return(coef[1] * log10(data[, xvar]) + coef[2])
    }
    coef[1] * data[, xvar] + coef[2]
}


#' Predict Using Log-Transformed Regression Model
#'
#' Generates predictions from a fitted regression model. When linear is FALSE (default),
#' the predictions are assumed to be on the log10 scale and are back-transformed; when TRUE,
#' predictions are computed on the original scale.
#' The results are rounded using the specified rounding function.
#'
#' @param data A data.frame of new observations.
#' @param fit A fitted regression model.
#' @param linear Logical; if TRUE, no back-transformation is applied (default is FALSE).
#' @param round_fun A function used to round the calculated predictions (default is signif).
#' @param round_par Integer specifying the number of significant digits for rounding (default is 3).
#'
#' @return A numeric vector of predicted values.

predict_log_regr <- function(data, fit,
                             linear = FALSE,
                             round_fun = signif, round_par = 3) {
    predicted <- suppressMessages(predict(fit, data))
    if (!linear) predicted <- 10^predicted
    return(round_fun(predicted, round_par))
}


#' Calculate Sample Concentrations Using a Regression Model
#'
#' Prepares experimental data, fits regression models on the standard samples (both direct and reverse),
#' predicts sample concentrations, and returns a list containing the fitted models, standard data,
#' and processed sample data.
#'
#' @param data A data.frame containing experimental data with columns such as type, conc, OD, and dilution.
#' @param model Character string specifying the model type to use ('LRM', 'QRM', or 'deming';
#' default is 'LRM').
#' @param xvar Character string specifying the independent variable name (default is 'conc').
#' @param yvar Character string specifying the dependent variable name (default is 'OD').
#' @param linear Logical; if TRUE, models are fitted using raw (non-log-transformed) data (default is FALSE).
#' @param round_fun A function used to round predicted concentrations (default is signif).
#' @param round_par Integer specifying the number of significant digits for rounding (default is 3).
#'
#' @return A list with components:
#'   $model_direct: The fitted regression model on standard data.
#'   $model_reverse: The fitted reverse regression model used for prediction.
#'   $linear_scale: Logical value indicating if raw (linear) data was used.
#'   $standard_data: The data used for model fitting (standard samples).
#'   $result: A data.frame of sample results with estimated concentrations.

calculate <- function(data, model = 'LRM',
                      xvar = 'conc', yvar = 'OD', linear = FALSE,
                      round_fun = signif, round_par = 3) {
    # Data preparation: subtract negative control values
    data <- subtract_nc(data)
    standard_data <- subset(data, type == 'standard')
    sample_data <- subset(data, type == 'sample')
    
    #  Choose regression model function based on input model type
    lin_fun <- switch (model,
                       'LRM' = calculate_LRM,
                       'QRM' = calculate_QRM,
                       'deming' = calculate_deming
    )
    
    # Model fitting on standard data (direct and reverse)
    fit_direct <- lin_fun(standard_data,  xvar = 'conc', yvar = 'OD'  , linear = linear)
    fit_reverse <- lin_fun(standard_data, xvar = 'OD',   yvar = 'conc', linear = linear)
    
    # Predict sample concentrations using reverse model
    predicted <- predict_log_regr(sample_data, fit_reverse, linear = linear,
                                  round_fun = round_fun, round_par = round_par)
    sample_data[, 'conc'] <- predicted * sample_data[, 'dilution']
    sample_data$conc <- ifelse(is.nan(sample_data$conc),
                               0,
                               sample_data$conc)
    sample_data$OD <- round(sample_data$OD, 4)
    
    # Return list with model and processed results
    list(model_direct = fit_direct,
         model_reverse = fit_reverse,
         linear_scale = linear,
         standard_data = standard_data,
         result = sample_data %>% select(-c(type, OD)))
}


#' Aggregate Results by Grouping Parameters
#'
#' Aggregates concentration data by the specified grouping parameters, calculating the count,
#' mean, minimum, and maximum of the concentrations.
#'
#' @param result A data.frame containing at least the `conc` column and grouping variables.
#' @param group_vars A character vector of column names to group by (default is "sample.name").
#' @param round_fun A function applied to round calculated concentrations (default is signif).
#' @param round_par Integer for rounding aggregated values (default is 3).
#'
#' @return A data.frame with aggregated statistics: count (`n`), mean, min, and max concentration.

aggregate_result <- function(result, group_vars = 'sample.name',
                             round_fun = signif, round_par = 3) {
    if (is.null(group_vars)) return()
    
    fml_str <- paste0('conc ~ ',
                      paste(group_vars, collapse = ' + '))
    fml <- as.formula(fml_str)
    
    df.n <- aggregate(fml, result, FUN = length)
    df.mean <- aggregate(fml, result, FUN = mean)
    df.min <- aggregate(fml, result, FUN = min)
    df.max <- aggregate(fml, result, FUN = max)
    
    merge(df.n, df.mean, by = group_vars, suffixes = c('.n', '.mean')) %>% 
        merge(df.min, by = group_vars, suffixes = c('', '.min')) %>% 
        merge(df.max, by = group_vars, suffixes = c('', '.max')) %>% 
        setNames(c(group_vars, 'n', 'mean', 'min', 'max')) %>% 
        mutate(across(where(is.numeric), ~ round_fun(., round_par)))
}


######################## THEME & PLOTTING ######################################


#' Custom ggplot2 Theme for Plots
#'
#' A custom theme based on `theme_bw()` with modifications for plot title, axis text,
#' and other panel elements to standardize the look of plots.

my_theme <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11, face = "plain"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))

theme_set(my_theme)

#' Transform Data Frame to Named List
#'
#' Converts a data.frame with columns `est` (estimates) and `coef` (names of coefficients)
#' into a named list, where each name is taken from the `coef` column and its value from `est`.
#'
#' @param df A data.frame containing at least the columns `est` and `coef`.
#'
#' @return A named list of coefficient estimates.

transform_to_list <- function(df) {
    setNames(as.list(df$est), df$coef)
}


#' Design X-Axis Scale for Logarithmic Data
#'
#' Determines appropriate break points for the x-axis on a logarithmic scale based on the
#' distribution of the data. If the number of unique log-transformed values is large or
#' too closely spaced, it generates a sequence of 8 breaks.
#'
#' @param x Numeric vector representing the x-values.
#' @param round_par Integer specifying the number of significant digits for rounding (default is 3).

design_x_scale <- function(x, round_par = 3) {
    x <- x[!is.na(x)]
    log_x <- log10(x[x > 0])
    unique_x <- unique(log_x)
    min_dist <- min(dist(unique_x))
    range_x <- range(unique_x)
    min_dist_perc <- min_dist * 100 / diff(range_x)
    if (length(unique_x) > 10 | min_dist_perc < 5) {
        splited_x <- seq(range_x[1], range_x[2], length.out = 8)
        return(signif(10^splited_x, round_par))
    } else {
        return(signif(10^log_x, round_par))
    }
}


#' Quadratic Regression Function for Prediction
#'
#' Computes predicted values using a quadratic function. When linear is FALSE (default),
#' the input x is log10-transformed before applying the quadratic formula and then back-transformed.
#' If linear is TRUE, the function uses the original x values.
#'
#' @param x Numeric vector of independent variable values.
#' @param a Coefficient for the quadratic term (default is 0).
#' @param b Coefficient for the linear term.
#' @param c Intercept term.
#' @param linear Logical; if TRUE, predictions are computed on the original scale (default is FALSE).
#'
#' @return A numeric vector of predicted values on the appropriate scale.

qrm_fun <- function(x, a = 0, b, c, linear = FALSE) {
    if (linear) {
        return(a * x^2 + b * x + c)
    }
    x <- log10(x)
    10^(a * x^2 + b * x + c)
}


#' Plot Standard Curve with Fitted Regression
#'
#' Creates a ggplot2 plot of standard sample data with points and overlays a regression
#' curve generated using a quadratic function. When linear is FALSE (default), the x- and y-axes
#' are scaled logarithmically.
#'
#' @param standard_data A data.frame containing standard sample data.
#' @param fit A fitted regression model object.
#' @param linear Logical; if TRUE, plots are generated on the original scale (default is FALSE).
#' @param xvar Character string specifying the x-axis variable name (default is 'conc').
#' @param yvar Character string specifying the y-axis variable name (default is 'OD').
#' @param xtitle Character string for the x-axis label.
#' @param ytitle Character string for the y-axis label.
#'
#' @return A ggplot2 plot object.
plot_standard_curve <- function(standard_data,
                                fit,
                                linear = FALSE,
                                xvar = 'conc', yvar = 'OD',
                                xtitle = '', ytitle = '') {
    coef_ci <- get_coef_ci(fit)
    arg_list <- select(coef_ci, c('est', 'coef')) %>%
        transform_to_list()
    arg_list['linear'] <- linear
    
    plot <-
        ggplot(standard_data, aes_string(xvar, yvar)) +
        geom_point(size = 1.5, alpha = 0.5) +
        geom_function(fun = qrm_fun, args = arg_list,
                      color = 'blue',
                      linewidth = 0.75) +
        labs(x = xtitle, y = ytitle)
    
    if (!linear) {
        x_scale <- design_x_scale(standard_data[, xvar])
        
        plot <- plot +
        scale_x_log10(breaks = x_scale,
                      labels = as.character(x_scale)) +
        scale_y_log10()
    }
    
    plot
}


#' Plot Fitted Values vs. Residuals
#'
#' Generates a residual plot for a fitted regression model by plotting the fitted values
#' (back-transformed from the log10 scale if applicable) against the residuals.
#' A smooth loess line is added for visualizing trends.
#'
#' @param fit A fitted regression model object.
#' @param linear Logical; if TRUE, fitted values are not back-transformed (default is FALSE).
#'
#' @return A ggplot2 plot object displaying the residuals.

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
        labs(x = 'Fitted values', y = 'Residuals')
    
    if (!linear) {
        x_scale <- design_x_scale(data[, 'fitted'])
        
        plot <- plot +
        scale_x_log10(breaks = x_scale,
                      labels = as.character(x_scale))
    }
    
    plot
}


#' Generate Combined Plots for Standard Curve and Residuals
#'
#' Creates a grid of plots that includes both the standard curve (with the fitted regression)
#' and the residual plot. Optionally, a custom ggplot2 theme can be applied.
#'
#' @param result A list containing at least `standard_data` and `model_direct` (fitted model).
#' @param xvar Character string specifying the x variable name (default is "conc").
#' @param yvar Character string specifying the y variable name (default is "OD").
#' @param xtitle Character string for the x-axis label.
#' @param ytitle Character string for the y-axis label.
#' @param plot.theme Optional ggplot2 theme to further customize the plots.
#'
#' @return A combined ggplot2 plot grid.

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

#' Check if an Object is a Date
#'
#' Determines whether the provided object inherits from the "Date" class.
#'
#' @param x An object to be tested.
#'
#' @return A logical value: TRUE if `x` is of class "Date", otherwise FALSE.

is.Date <- function(x) inherits(x, 'Date')


#' Create Experiment Data Template
#'
#' Generates a template data.frame for an experiment containing both standard and sample entries.
#' The standard part includes titration values and a negative control (NC) row, while the sample
#' part includes placeholder rows.
#'
#' @param start Numeric value indicating the starting concentration (default is 100).
#' @param step Unused parameter (reserved for future use; default is 2).
#' @param n.standard Number of replicates for the standard samples (default is 2).
#' @param n.sample Number of sample entries (default is 10).
#'
#' @return A data.frame template with columns: sample.name, sample.date, type, dilution, conc, and OD.
#'
#' @examples
#' template <- make_template(start = 100, n.standard = 3, n.sample = 15)

make_template <- function(start = 100, step = 2,
                          n.standard = 2, n.sample = 10) {
    
    templater <- function(
        sample.name = '',
        sample.date = '',
        type,
        dilution,
        conc,
        OD = '') {
        data.frame(
            sample.name = sample.name,
            sample.date = sample.date,
            type = type,
            dilution = dilution,
            conc = conc,
            OD = OD
        )
    }
    
    template <- data.frame()
    
    # Standard part
    titration <- start/(2^(0:6))
    titration <- sapply(titration, FUN = signif, 3)
    st_part <-
        templater(
            type = c(rep('standard', times = 7), 'NC'),
            dilution = '',
            conc = c(titration, '')
        )
    
    for (i in 1:n.standard) {
        template <- rbind(template, st_part)
    }
    
    # Sample part
    sampl_part <-
        templater(
            type = 'sample',
            dilution = 1,
            conc = ''
        )
    
    for (i in 1:n.sample) {
        template <- rbind(template, sampl_part)
    }
    
    template$conc <- as.numeric(template$conc)
    template$dilution <- as.numeric(template$dilution)
    template
}


#' Read Data Table from File
#'
#' Reads a table from a file path supporting both Excel (.xlsx) and CSV formats. For CSV files,
#' the function attempts to detect the correct separator and decimal character.
#'
#' @param path Character string specifying the file path.
#'
#' @return A data.frame containing the imported data.

read_table <- function(path) {
    file_type <- tools::file_ext(path) %>% tolower()
    switch (file_type,
        'xlsx' = {
            table <- read.xlsx(path, 1)
        },
        'csv' = {
            # determining csv format
            table <- try(read.csv(path, sep = ';', dec = ','))
            if (inherits(table, "try-error") | ncol(table) == 1) {
                table <- try(read.csv(path, sep = ',', dec = '.'))
                if (class(table) == 'try-error' | ncol(table) == 1) {
                    stop('Unknown csv format of file')
                }
            }
        },
        stop('This filetype is not supported')
    )
    
    return(table)
}


#' Validate Input Data for the Linearity App
#'
#' This function ensures that the imported data meets the minimal requirements needed for
#' processing in the Linearity App.
#' 
#' It performs several validations:
#' 
#' - Mandatory Columns: Confirms that the data contains all required columns: 
#'   "sample.name", "sample.date", "type", "dilution", "conc", and "OD".
#' - Numeric Data: Verifies that the "dilution", "conc", and "OD" columns are numeric.
#' - Type Values: Checks that the "type" column includes only the allowed values:
#'   "standard", "sample", and "NC",
#'   and that none of these values are missing.
#' - Dilution Factors: Ensures that every sample (rows where "type" is "sample") has a valid,
#'   non-missing, positive dilution factor.
#' - Sufficient Standard Wells: Validates that there are more than four rows marked as "standard"
#'   to ensure reliable standard curve fitting.
#'
#' If any of these checks fail, the function stops execution and returns an informative error message.
#'
#' @param data A data.frame containing the raw data to be validated. Expected columns are 
#' "sample.name", "sample.date", "type", "dilution", "conc", and "OD".
#'
#' @return A validated data.frame that is ready for further processing in the app.

check_input_file <- function(data) {
    # check for mandatory columns
    munadatory_columns <- c('sample.name', 'sample.date', 'type', 'dilution', 'conc', 'OD')
    
    for (mc in munadatory_columns) {
        if (!mc %in% colnames(data)) {
            stop(paste('Mundatory', mc, 'column is missing'))
        }
    }
    
    # Verify that specified columns are numeric
    numeric_colmns <- c('dilution', 'conc', 'OD')
    
    for (nc in numeric_colmns) {
        if (!is.numeric(data[, nc])) {
            stop(paste('Non-numeric values in', nc, 'column'))
        }
    }
    
    # Validate the 'type' column values
    allowed_types <- c('standard', 'sample', 'NC')
    present_types <- unique(data[, 'type'])
        
    # Check for any disallowed type values
    not_allowed <- setdiff(present_types, allowed_types)
    if (length(not_allowed) > 0) {
        stop(paste('Not allowed values in the type column:',
                   paste(not_allowed, collapse = ', ')))
    }
    
    # Check for missing required type values
    missing_types <- setdiff(allowed_types, present_types)
    if (length(missing_types) > 0) {
        stop(paste('Missing values in the type column:',
                   paste(missing_types, collapse = ', ')))
    }
    
    # Check dilution values for sample rows
    dilutions <- subset(data, type == 'sample')$dilution
    if (any(is.na(dilutions))) {
        stop('Not all samples have dilution factors specified')
    }
    if (!all(dilutions > 0)) {
        stop('All dilution factors must be positive')
    }
    
    # Ensure there are more than four standard wells for reliable analysis
    n_standard <- sum(data$type == 'standard')
    if (n_standard <= 4) {
        stop(paste('Insufficient number of standard wells:', n_standard))
    }
    
    data
}


#' Format Input Data Table
#'
#' This function formats an input data table by converting all columns that are of the Date class
#' into character strings with the format "dd.mm.yyyy". It also ensures that the sample.name
#' column is stored as a character vector.
#'
#' @param table A data.frame representing the input data table.
#'
#' @return A formatted data.frame with date columns converted to character strings (formatted as "dd.mm.yyyy")
#' and the sample.name column coerced to character type.

format_input_table <- function(table) {
    
    # Format columns with dates
    for(col in colnames(table)) {
        if (is.Date(table[, col])) {
            table[, col] <- as.character(format(table[, col], '%d.%m.%Y'))
        }
    }
    
    # Ensure the sample.name column is of character type
    table$sample.name <- as.character(table$sample.name)
    
    table
}


#' Save Data Frame as CSV File
#'
#' Saves a data.frame to a CSV file using the specified dialect. The dialect determines the separator
#' and decimal character used in the output.
#'
#' @param X A data.frame to be saved.
#' @param file Character string specifying the output file path.
#' @param dialect Character string indicating the CSV dialect; valid options are "comma" (default)
#' or "semicolon".
#'
#' @return None. The function writes the file to disk.

save_csv <- function(X, file, dialect = 'comma') {
    switch (dialect,
            'comma' = write.table(X,
                                  file,
                                  sep = ',', dec = '.',
                                  row.names = FALSE),
            'semicolon' = write.table(X,
                                      file,
                                      sep = ';', dec = ',',
                                      row.names = FALSE)
    )
}