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
    unique(signif(10^unique_x, round_par))
}


#' Concentration Axis of the Plots
#'
#' @param result Output of [calculate()].
#'
#' @return A list with `log` (whether the concentration axis is logarithmic)
#'   and `breaks` (for a logarithmic axis).
#' @keywords internal
conc_axis <- function(result) {
    fit <- result$fit
    log_x <- !(poly_degree(fit$model) > 0 && !fit$log_scale)
    all_x <- c(result$standard_data$conc, result$exclusions$conc)
    list(log = log_x, breaks = if (log_x) design_x_scale(all_x) else NULL, range = range(all_x))
}


#' Plot Standard Curve with Fitted Model
#'
#' Standards used in the fit are shown as points, standards excluded by the
#' user as red crosses. The concentration axis is logarithmic except for the
#' polynomial models on the original scale; the OD axis is logarithmic for models
#' fitted on log10 scales. With excluded points, the curve fitted on all
#' standards can be shown as a dashed line next to the refitted curve.
#'
#' @param result Output of [calculate()].
#' @param xtitle,ytitle Axis titles.
#' @param show_original Show the curve fitted on all standards (if there are
#'   exclusions).
#' @param lang Language of the legend.
#'
#' @return A ggplot2 object.
#' @keywords internal
plot_standard_curve <- function(result, xtitle = '', ytitle = '', show_original = TRUE, lang = 'en') {
    fit <- result$fit
    axis <- conc_axis(result)
    log_x <- axis$log
    std <- result$standard_data
    excl <- result$exclusions

    all_x <- c(std$conc, excl$conc)
    grid_x <- if (log_x) {
        exp(seq(log(min(all_x)), log(max(all_x)), length.out = 200))
    } else {
        seq(min(all_x), max(all_x), length.out = 200)
    }
    original <- if (show_original) result$original_fit
    curve <- data.frame(conc = grid_x, OD = curve_fun(fit, grid_x), line = 'refitted')
    if (!is.null(original)) {
        curve <- rbind(data.frame(conc = grid_x, OD = curve_fun(original, grid_x), line = 'original'), curve)
    }
    if (fit$log_scale) {
        curve <- curve[curve$OD > 0, ]
        excl <- excl[!is.na(excl$OD) & excl$OD > 0, ]
    }

    plot <-
        ggplot2::ggplot(std, ggplot2::aes(.data$conc, .data$OD)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.6) +
        ggplot2::labs(x = xtitle, y = ytitle) +
        lin_theme()

    if (is.null(original)) {
        plot <- plot + ggplot2::geom_line(data = curve, color = 'blue', linewidth = 0.75)
    } else {
        # The original curve stays as a dashed line; the legend tells the curves apart
        labels <- c(original = tr('plot.curve_original', lang), refitted = tr('plot.curve_refitted', lang))
        plot <- plot +
            ggplot2::geom_line(data = curve, ggplot2::aes(colour = .data$line, linetype = .data$line),
                               linewidth = 0.75) +
            ggplot2::scale_colour_manual(values = c(original = 'grey45', refitted = 'blue'), labels = labels,
                                         name = NULL) +
            ggplot2::scale_linetype_manual(values = c(original = 'dashed', refitted = 'solid'), labels = labels,
                                           name = NULL) +
            ggplot2::theme(legend.position = 'inside', legend.position.inside = c(0.02, 0.98),
                           legend.justification = c(0, 1), legend.background = ggplot2::element_blank(),
                           legend.key = ggplot2::element_blank(), legend.key.width = ggplot2::unit(1.6, 'lines'),
                           legend.text = ggplot2::element_text(size = 10))
    }

    if (nrow(excl) > 0) {
        plot <- plot + ggplot2::geom_point(data = excl, shape = 4, size = 3, stroke = 1.2,
                                           color = 'red')
    }
    if (log_x) {
        plot <- plot + ggplot2::scale_x_log10(breaks = axis$breaks, labels = as.character(axis$breaks))
    }
    if (fit$log_scale) {
        plot <- plot + ggplot2::scale_y_log10()
    }
    plot
}


#' Plot Residuals vs. Concentration
#'
#' Residuals are on the scale of the fit; weighted residuals are shown for
#' weighted fits. The concentration axis is the same as on the standard curve
#' (see [plot_standard_curve()]).
#'
#' @param result Output of [calculate()].
#' @param xtitle Title of the concentration axis.
#' @param lang Language of the residual axis title.
#'
#' @return A ggplot2 object.
#' @keywords internal
plot_fitted_resid <- function(result, xtitle = '', lang = 'en') {
    fit <- result$fit
    axis <- conc_axis(result)
    data <- fit_residuals(fit)
    weighted <- fit$weights != 'none'
    data$y <- if (weighted) data$wresid else data$resid

    plot <-
        ggplot2::ggplot(data, ggplot2::aes(.data$conc, .data$y)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.5) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::labs(x = xtitle,
                      y = tr(if (weighted) 'plot.weighted_residuals' else 'plot.residuals', lang)) +
        lin_theme()

    if (length(unique(data$conc)) > 3) {
        plot <- plot + ggplot2::geom_smooth(method = 'loess', formula = y ~ x, color = 'blue',
                                            se = FALSE, span = 2, linewidth = 0.75)
    }
    if (axis$log) {
        plot <- plot + ggplot2::scale_x_log10(breaks = axis$breaks, labels = as.character(axis$breaks),
                                              limits = axis$range)
    } else {
        plot <- plot + ggplot2::scale_x_continuous(limits = axis$range)
    }
    plot
}


#' Standard Curve and Residual Plots
#'
#' @param result Output of [calculate()].
#' @param xtitle,ytitle Axis titles of the standard curve.
#' @param plot.theme Optional ggplot2 theme added to both plots.
#' @param lang Language of the residual plot titles and the legend.
#' @param show_original Show the curve fitted on all standards as a dashed
#'   line (see [plot_standard_curve()]).
#'
#' @return A cowplot grid.
#' @export
get_plots <- function(result, xtitle = '', ytitle = '', plot.theme = NULL, lang = 'en', show_original = TRUE) {
    standard_plot <- plot_standard_curve(result, xtitle = xtitle, ytitle = ytitle, show_original = show_original,
                                         lang = lang)
    fit_resid_plot <- plot_fitted_resid(result, xtitle = xtitle, lang = lang)

    if (!is.null(plot.theme)) {
        standard_plot <- standard_plot + plot.theme
        fit_resid_plot <- fit_resid_plot + plot.theme
    }

    cowplot::plot_grid(standard_plot, fit_resid_plot)
}
