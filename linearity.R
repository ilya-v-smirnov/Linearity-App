

##################### LINEAR MODELS FUNCTIONS ##################################

# Calculates geometric mean
# x          - numeric vector
# round_par  - integer
# output:      numeric vector of length 1

geom_mean <- function(x, round_par = 3) {
    if (length(x) == 1) return(x)
    log10(x) %>% mean() %>% 10^. %>% signif(round_par)
}

# Subtracts negative control values from studied variable (var)
# data      - data.frame containting at least two columns:
#             'type', var, and group_var (if needed).

subtract_NC <- function(data, round_par = 3) {
    NC_data <- subset(data, type == 'NC')
    OD.NC <- NC_data$OD %>% geom_mean(round_par)
    subset(data, type != 'NC') %>% 
        mutate(OD = OD - OD.NC)
}

# Calculates ordinary Linear Regression Model
# data      - data.frame, standard data with NC subtracted
# xvar      - character vector of length 1, name of independent variable
# yvar      - character vector of lenght 1, name of dependent variable
# slope_1   - logical vector of length 1, should be slope(s) equal to 1
# group_var - character vector of length 1, name of grouping variable


calculate_LRM <- function(data, xvar, yvar, slope_1 = FALSE) {
    if (slope_1) {
        formula_str <- paste0('log10(', yvar, ') - log10(', xvar, ') ~ 1')
    } else {
        formula_str <- paste0('log10(', yvar, ') ~ log10(', xvar, ')')
    }
    lm(as.formula(formula_str), data)
}


# Calculates Quadric Regression Model
# data      - data.frame, standard data with NC subtracted
# xvar      - character vector of length 1, name of independent variable
# yvar      - character vector of lenght 1, name of dependent variable


calculate_QRM <- function(data, xvar, yvar) {
    formula_str <-
        paste0('log10(', yvar, ') ~ I(log10(', xvar, ')^2) + log10(', xvar, ')')
    lm(as.formula(formula_str), data)
}


# Calculates Deming Regression
# data      - data.frame, standard data with NC subtracted
# xvar      - character vector of length 1, name of independent variable
# yvar      - character vector of lenght 1, name of dependent variable
# group_var - character vector of length 1, name of grouping variable

calculate_deming <- function(data, xvar, yvar) {
    formula_str <- paste0('log10(', yvar, ') ~ log10(', xvar, ')')
    deming(as.formula(formula_str), data = data)
}


get_coef_ci <- function(fit) {
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
    # print('linearity.R')
    # print(tab)
    tab
}


get_fit_resid <- function(fit) {
    n <- ncol(fit$model)
    data.frame(fitted = 10^fit$model[,n],
               resid = resid(fit))
}


predict.deming <- function(object, data) {
    coef_ci <- get_coef_ci(object)
    coef <- coef_ci$est
    xvar <- row.names(coef_ci)[1] %>%
        str_extract(pattern = '(?<=log10\\().*(?=\\))')
    coef[1]*log10(data[, xvar]) + coef[2]
}


predict_log_regr <- function(data, fit, round_par = 3) {
    predicted <- suppressMessages(10^predict(fit, data))
    return(signif(predicted, round_par))
}


calculate <- function(data, model = 'LRM',
                      xvar = 'conc', yvar = 'OD',
                      round_par = 4) {
    # Data preparation
    data <- subtract_NC(data)
    standard_data <- subset(data, type == 'standard')
    sample_data <- subset(data, type == 'sample')
    
    # Choosing model
    lin_fun <- switch (model,
                       'LRM' = calculate_LRM,
                       'QRM' = calculate_QRM,
                       'deming' = calculate_deming
    )
    
    # Model fitting
    fit_direct <- lin_fun(standard_data,  xvar = 'conc', yvar = 'OD'  )
    fit_reverse <- lin_fun(standard_data, xvar = 'OD',   yvar = 'conc')
    
    # Results
    predicted <- predict_log_regr(sample_data, fit_reverse, round_par = round_par)
    sample_data[, 'conc'] <- predicted * sample_data[, 'dilution']
    sample_data$conc <- ifelse(is.nan(sample_data$conc),
                               0,
                               sample_data$conc)
    sample_data$OD <- round(sample_data$OD, 4)
    
    # Return
    list(model_direct = fit_direct,
         model_reverse = fit_reverse,
         standard_data = standard_data,
         result = sample_data)
}


rounding <- function(est, ci.lower, ci.upper, sig_digits = 1) {
    if (any(!is.finite(c(est, ci.lower, ci.upper)))) {
        return(c(est = est,
                 est.rounded = 0,
                 ci.lower = 0,
                 ci.upper = 0))
    }
    delta <- ci.upper - ci.lower
    if (delta == 0) {
        # If delta is zero, we cannot compute log10; default to no rounding
        round_par <- max(getOption("digits") - 1, 0)
    } else {
        dexp <- floor(log10(abs(delta)))
        round_par <- -(dexp - sig_digits + 1)
    }
    round_par <- as.integer(round_par)
    est_rounded <- round(est, digits = round_par)
    ci.lower_rounded <- round(ci.lower, digits = round_par) 
    ci.upper_rounded <- round(ci.upper, digits = round_par)
    return(c(est = round(est, 2),
             est.rounded = est_rounded,
             ci.lower = ci.lower_rounded,
             ci.upper = ci.upper_rounded))
}


mean_ci <- function(x) {
    t <- t.test(x)
    c(est = as.numeric(t$estimate),
      ci.lower = t$conf.int[1],
      ci.upper = t$conf.int[2])
}


geo_mean_ci <- function(x, conf.level = 0.95) {
    if (all(x <= 0)) {
        return(c(est = 0, ci.lower = 0, ci.upper = 0))
    } else if(any(x <= 0)) {
        x <- x[x > 0]
    }
    
    log_x <- log(x)
    t <- t.test(log_x, conf.level = conf.level)
    est <- exp(mean(log_x))
    ci.lower <- exp(t$conf.int[1])
    ci.upper <- exp(t$conf.int[2])
    
    c(est = est, ci.lower = ci.lower, ci.upper = ci.upper)
}


apply_rounding <- function(df, sig_digits = 1) {
    # Ensure required columns exist
    required_columns <- c("est", "ci.lower", "ci.upper")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
        stop(paste("The data.frame is missing required columns:", 
                   paste(missing_columns, collapse = ", ")))
    }
    
    # Apply rounding to each row
    rounded_results <- apply(df, 1, function(row) {
        result <- rounding(
            est = as.numeric(row["est"]),
            ci.lower = as.numeric(row["ci.lower"]),
            ci.upper = as.numeric(row["ci.upper"]),
            sig_digits = sig_digits
        )
        return(result)
    }) %>% t() %>% as.data.frame()
    
    
    # Merge rounded columns into original data.frame
    df <- cbind(rounded_results,
                df[, setdiff(names(df), c("est", "ci.lower", "ci.upper")), drop = F])
    
    # Return modified data.frame
    return(df)
}

######################## THEME & PLOTTING ######################################

my_theme <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11, face = "plain"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))



transform_to_list <- function(df) {
    setNames(as.list(df$est), df$coef)
}


design_x_scale <- function(x, round_par=3) {
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


qrm_fun <- function(x, a = 0, b, c) {
    x <- log10(x)
    10^(a*x^2 + b*x + c)
}


plot_standard_curve <- function(standard_data,
                                fit,
                                xvar = 'conc', yvar = 'OD',
                                xtitle = '', ytitle = '') {
    coef_ci <- get_coef_ci(fit)
    arg_list <- select(coef_ci, c('est', 'coef')) %>%
        transform_to_list()
    
    x_scale <- design_x_scale(standard_data[, xvar])
    
    plot <-
        ggplot(standard_data, aes_string(xvar, yvar)) +
        geom_point(size = 1.5, alpha = 0.5) +
        geom_function(fun = qrm_fun, args = arg_list,
                      color = 'blue',
                      linewidth = 0.75) +
        scale_x_log10(breaks = x_scale,
                      labels = as.character(x_scale)) +
        scale_y_log10() +
        my_theme +
        labs(x = xtitle, y = ytitle)
    
    plot
}


plot_fitted_resid <- function(fit) {
    data <- get_fit_resid(fit)
    
    x_scale <- design_x_scale(data[, 'fitted'])
    
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
        scale_x_log10(breaks = x_scale,
                      labels = as.character(x_scale)) +
        my_theme +
        labs(x = 'Fitted values', y = 'Residuals')
    
    plot
}


get_plots <- function(result,
                      xvar = 'conc', yvar = 'OD',
                      xtitle = '', ytitle = '',
                      plot.theme = NULL) {
    
    standard_plot <-
        plot_standard_curve(
            result$standard_data,
            result$model_direct,
            xvar = xvar, yvar = yvar,
            xtitle = xtitle, ytitle = ytitle
        )
    
    fit_resid_plot <- 
        plot_fitted_resid(result$model_direct)
    
    if (!is.null(plot.theme)) {
        standard_plot <- standard_plot + plot.theme
        fit_resid_plot <- fit_resid_plot + plot.theme
    }
    
    plot_grid(standard_plot, fit_resid_plot)
}


###################### SUPPLEMENTARY FUNCTIONS #################################

is.Date <- function(x) inherits(x, 'Date')

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


read_table <- function(path, dialect = 'semicolon') {
    file_type <- tools::file_ext(path) %>% tolower()
    switch (file_type,
        'xlsx' = {
            table <- read.xlsx(path, 1)
            for(col in colnames(table)) {
                if (is.Date(table[, col])) {
                    table[, col] <- as.character(format(table[, col], '%d.%m.%Y'))
                }
            }
            return(table)
        },
        'csv' = {
            # gessing csv format
            semicol_df <- read.csv(path, sep = ';', dec = ',')
            comma_df <- read.csv(path, sep = ',', dec = '.')
            if (ncol(semicol_df) > ncol(comma_df)) {
                return(semicol_df)
            } else {
                return(comma_df)
            }
        },
        stop('This filetype is not supported')
    )
}



# Saves data.frames as csv files separeated by ;
# and commas as decimal signs. Prints the path to the saved files
# in console.

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



############################################################
# 
# 
# aggregate_results <- function(result, aggr.pars = 'sample.name', method = 'geo_mean') {
#     if (is.null(aggr.pars)) return()
# 
#     # result <- subset(result, conc > 0)
#     # if (nrow(result) == 0) return()
# 
#     fml_str <- paste0('conc ~ ',
#                       paste(aggr.pars, collapse = ' + '))
#     fml <- as.formula(fml_str)
# 
#     df.n <- aggregate(fml, result, FUN = length)
#     colnames(df.n)[ncol(df.n)] <- 'n'
# 
#     fun <- switch(method,
#                   'mean' = mean_ci,
#                   'geo_mean' = geo_mean_ci,
#                   stop('Check method'))
# 
#     df.mean_ci <-
#         aggregate(fml, result, FUN = fun) %>%
#         as.list() %>%
#         as.data.frame
#     
#     colnames(df.mean_ci) <- str_remove_all(colnames(df.mean_ci), 'conc\\.')
#     
#     df.mean_ci <- apply_rounding(df.mean_ci)
#     merge(df.n,
#           df.mean_ci,
#           by = aggr.pars)
# }


# test <- xlsx::read.xlsx('E:/Work Files/2024-08-27 Griess/2024-08-27_Griess.xlsx',  1)
# test$type <- ifelse(test$type == '', 'sample', test$type)
# 
# 
# result <- calculate(test, 'LRM')
# result$result
# 
# (plots <- get_plots(result,
#                     xtitle = 'Nitrite concentration, uM',
#                     ytitle = 'Optical density, 520 nm'))
# 
# 
# result$model_direct %>% get_coef_ci() %>% apply_rounding()
# 
# 
# 
# ggsave('plots.jpeg', plots, width = 15, height = 7.5, units = 'cm')
# 
# aggregate_results(result$result, method = 'mean')
# 
# write.table(result$result)





