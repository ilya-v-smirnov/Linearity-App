#' Plot Theme of the Application
#'
#' @return A ggplot2 theme.
#' @keywords internal
lin_theme <- function() {
    ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 11, face = 'plain'),
                       axis.title = ggplot2::element_text(size = 14),
                       axis.text = ggplot2::element_text(size = 12, color = 'black'),
                       panel.grid = ggplot2::element_blank(),
                       strip.background = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = 'black'))
}


#' Plot Theme for the Word Report
#'
#' Smaller axis titles and labels.
#'
#' @return A ggplot2 theme.
#' @export
report_theme <- function() {
    ggplot2::theme(axis.title = ggplot2::element_text(size = 9, color = 'black'),
                   axis.text = ggplot2::element_text(size = 8, color = 'black'))
}


#' Design X-Axis Breaks for Logarithmic Data
#'
#' @param x Numeric vector.
#' @param round_par Number of significant digits for the break labels.
#'
#' @return Numeric vector of breaks.
#' @keywords internal
design_x_scale <- function(x, round_par = 3) {
    x <- x[!is.na(x)]
    log_x <- log10(x[x > 0])
    unique_x <- unique(log_x)
    if (length(unique_x) < 2) return(signif(10^unique_x, round_par))
    min_dist <- min(dist(unique_x))
    range_x <- range(unique_x)
    min_dist_perc <- min_dist * 100 / diff(range_x)
    if (length(unique_x) > 10 || min_dist_perc < 5) {
        return(signif(10^seq(range_x[1], range_x[2], length.out = 8), round_par))
    }
    signif(10^log_x, round_par)
}


#' Quadratic Calibration Function
#'
#' y = a*x^2 + b*x + c, on log10 scales unless `linear` is `TRUE`.
#'
#' @param x Concentrations.
#' @param a,b,c Coefficients.
#' @param linear Logical.
#'
#' @return Predicted OD.
#' @keywords internal
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
#' @keywords internal
plot_standard_curve <- function(standard_data, fit, linear = FALSE,
                                xvar = 'conc', yvar = 'OD',
                                xtitle = '', ytitle = '') {
    arg_list <- curve_coefs(fit)
    arg_list$linear <- linear

    plot <-
        ggplot2::ggplot(standard_data, ggplot2::aes(.data[[xvar]], .data[[yvar]])) +
        ggplot2::geom_point(size = 1.5, alpha = 0.5) +
        ggplot2::geom_function(fun = qrm_fun, args = arg_list,
                               color = 'blue', linewidth = 0.75) +
        ggplot2::labs(x = xtitle, y = ytitle) +
        lin_theme()

    if (!linear) {
        x_scale <- design_x_scale(standard_data[[xvar]])
        plot <- plot +
            ggplot2::scale_x_log10(breaks = x_scale, labels = as.character(x_scale)) +
            ggplot2::scale_y_log10()
    }

    plot
}


#' Retrieve Fitted Values and Residuals
#'
#' @param fit Fitted calibration model.
#' @param linear Logical; if `FALSE` fitted values are back-transformed from log10.
#'
#' @return A data.frame with `fitted` (OD scale) and `resid` (scale of the fit).
#' @keywords internal
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
#' @param lang Language of the axis titles.
#'
#' @return A ggplot2 object.
#' @keywords internal
plot_fitted_resid <- function(fit, linear = FALSE, lang = 'en') {
    data <- get_fit_resid(fit, linear = linear)

    plot <-
        ggplot2::ggplot(data, ggplot2::aes(.data$fitted, .data$resid)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.5) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_smooth(method = 'loess', formula = y ~ x,
                             color = 'blue', se = FALSE, span = 2, linewidth = 0.75) +
        ggplot2::labs(x = tr('plot.fitted', lang), y = tr('plot.residuals', lang)) +
        lin_theme()

    if (!linear) {
        x_scale <- design_x_scale(data$fitted)
        plot <- plot +
            ggplot2::scale_x_log10(breaks = x_scale, labels = as.character(x_scale))
    }

    plot
}


#' Standard Curve and Residual Plots
#'
#' @param result Output of [calculate()].
#' @param xtitle,ytitle Axis titles of the standard curve.
#' @param plot.theme Optional ggplot2 theme added to both plots.
#' @param lang Language of the residual plot titles.
#'
#' @return A cowplot grid.
#' @export
get_plots <- function(result, xtitle = '', ytitle = '', plot.theme = NULL, lang = 'en') {
    standard_plot <- plot_standard_curve(standard_data = result$standard_data,
                                         fit = result$model_direct,
                                         linear = result$linear_scale,
                                         xtitle = xtitle, ytitle = ytitle)
    fit_resid_plot <- plot_fitted_resid(result$model_direct,
                                        linear = result$linear_scale, lang = lang)

    if (!is.null(plot.theme)) {
        standard_plot <- standard_plot + plot.theme
        fit_resid_plot <- fit_resid_plot + plot.theme
    }

    cowplot::plot_grid(standard_plot, fit_resid_plot)
}
