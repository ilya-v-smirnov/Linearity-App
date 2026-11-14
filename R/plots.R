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


#' Plot Standard Curve with Fitted Model
#'
#' Standards used in the fit are shown as points, standards excluded by the
#' user as red crosses. The concentration axis is logarithmic except for the
#' LRM and QRM fitted on linear scales; the OD axis is logarithmic for models
#' fitted on log10 scales.
#'
#' @param result Output of [calculate()].
#' @param xtitle,ytitle Axis titles.
#'
#' @return A ggplot2 object.
#' @keywords internal
plot_standard_curve <- function(result, xtitle = '', ytitle = '') {
    fit <- result$fit
    log_x <- !(fit$model %in% c('LRM', 'QRM') && !fit$log_scale)
    std <- result$standard_data
    excl <- result$exclusions

    all_x <- c(std$conc, excl$conc)
    grid_x <- if (log_x) {
        exp(seq(log(min(all_x)), log(max(all_x)), length.out = 200))
    } else {
        seq(min(all_x), max(all_x), length.out = 200)
    }
    curve <- data.frame(conc = grid_x, OD = curve_fun(fit, grid_x))
    if (fit$log_scale) {
        curve <- curve[curve$OD > 0, ]
        excl <- excl[!is.na(excl$OD) & excl$OD > 0, ]
    }

    plot <-
        ggplot2::ggplot(std, ggplot2::aes(.data$conc, .data$OD)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.6) +
        ggplot2::geom_line(data = curve, color = 'blue', linewidth = 0.75) +
        ggplot2::labs(x = xtitle, y = ytitle) +
        lin_theme()

    if (nrow(excl) > 0) {
        plot <- plot + ggplot2::geom_point(data = excl, shape = 4, size = 3, stroke = 1.2,
                                           color = 'red')
    }
    if (log_x) {
        x_scale <- design_x_scale(all_x)
        plot <- plot + ggplot2::scale_x_log10(breaks = x_scale, labels = as.character(x_scale))
    }
    if (fit$log_scale) {
        plot <- plot + ggplot2::scale_y_log10()
    }
    plot
}


#' Plot Fitted Values vs. Residuals
#'
#' Residuals are on the scale of the fit; weighted residuals are shown for
#' weighted fits.
#'
#' @param fit A `lin_fit` object.
#' @param lang Language of the axis titles.
#'
#' @return A ggplot2 object.
#' @keywords internal
plot_fitted_resid <- function(fit, lang = 'en') {
    data <- fit_residuals(fit)
    weighted <- fit$weights != 'none'
    data$y <- if (weighted) data$wresid else data$resid

    plot <-
        ggplot2::ggplot(data, ggplot2::aes(.data$fitted, .data$y)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.5) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::labs(x = tr('plot.fitted', lang),
                      y = tr(if (weighted) 'plot.weighted_residuals' else 'plot.residuals', lang)) +
        lin_theme()

    if (length(unique(data$fitted)) > 3) {
        plot <- plot + ggplot2::geom_smooth(method = 'loess', formula = y ~ x, color = 'blue',
                                            se = FALSE, span = 2, linewidth = 0.75)
    }
    if (fit$log_scale) {
        x_scale <- design_x_scale(data$fitted)
        plot <- plot + ggplot2::scale_x_log10(breaks = x_scale, labels = as.character(x_scale))
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
    standard_plot <- plot_standard_curve(result, xtitle = xtitle, ytitle = ytitle)
    fit_resid_plot <- plot_fitted_resid(result$fit, lang = lang)

    if (!is.null(plot.theme)) {
        standard_plot <- standard_plot + plot.theme
        fit_resid_plot <- fit_resid_plot + plot.theme
    }

    cowplot::plot_grid(standard_plot, fit_resid_plot)
}
