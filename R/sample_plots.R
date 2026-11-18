#' Colours of the Sample Infographics
#'
#' The quantification status uses the colours of the infographic of the
#' Results step (see [flags_ui()]): red at or below the blank, orange below
#' the lowest calibrator, green quantified, blue above the highest
#' calibrator, grey other. The precision of the replicates and the accuracy
#' of the QC use a neutral blue-grey for "within limits" and red for
#' "outside".
#'
#' @return A named character vector.
#' @keywords internal
sample_colours <- function() {
    c(od_le_nc = '#c62828', below = '#ef8f00', ok = '#1b5e20', above = '#1976d2', other = '#9e9e9e',
      pass = '#78909c', fail = '#c62828')
}


#' Quantification Status of Wells
#'
#' @param flags Flags of the wells (see [FLAGS]).
#'
#' @return Character vector of the categories of [quantification_counts()].
#' @keywords internal
quantification_status <- function(flags) {
    known <- c(od_le_nc = FLAGS$od_le_nc, below = FLAGS$below, ok = FLAGS$ok, above = FLAGS$above)
    status <- names(known)[match(flags, known)]
    status[is.na(status)] <- 'other'
    status
}


#' Labels of Groups
#'
#' Values of the grouping columns joined by a dot; columns that are empty in
#' every group are left out, dilutions are shown as `x5`.
#'
#' @param groups A data.frame with the grouping columns.
#' @param group_vars Grouping columns.
#'
#' @return Character vector, one label per row.
#' @keywords internal
group_labels <- function(groups, group_vars) {
    cols <- group_vars[vapply(group_vars, function(v) !all(is.na(groups[[v]])), logical(1))]
    if (length(cols) == 0) return(rep('\u2014', nrow(groups)))
    parts <- lapply(cols, function(v) {
        x <- groups[[v]]
        out <- if (v == 'dilution') paste0('\u00d7', format(x, trim = TRUE, drop0trailing = TRUE)) else
            as.character(x)
        out[is.na(x)] <- '\u2014'
        out
    })
    do.call(paste, c(parts, sep = ' \u00b7 '))
}


#' Data of the Sample Figure
#'
#' @param result Well-level sample results.
#' @param samples `samples` element of [assess_samples()] for `group_vars`.
#' @param group_vars Grouping columns.
#'
#' @return A list with `groups` (one row per group: `label`, `n`,
#'   `n.quantified`, `mean`, `sd`, `cv`, `cv.limit`, `cv.pass`) and `wells`
#'   (`label`, `conc`, `status`); `label` is a factor with the first group on
#'   top of the plot.
#' @keywords internal
sample_figure_data <- function(result, samples, group_vars) {
    labels <- group_labels(samples, group_vars)
    levels <- rev(unique(labels))
    groups <- data.frame(label = factor(labels, levels = levels),
                         samples[c('n', 'n.quantified', 'mean', 'sd', 'cv', 'cv.limit')])
    groups$cv.pass <- is.na(groups$cv) | groups$cv <= groups$cv.limit
    key <- replicate_groups(result, group_vars)
    group_key <- replicate_groups(samples, group_vars)
    wells <- data.frame(label = factor(labels[match(key, group_key)], levels = levels),
                        conc = result$conc,
                        status = quantification_status(result$flag))
    list(groups = groups, wells = wells)
}


#' Height of the Sample Figure in Pixels
#'
#' @param n Number of groups.
#' @return A number.
#' @keywords internal
groups_plot_height <- function(n) max(240, 26 * n + 90)


#' Theme of the Sample Infographics
#' @keywords internal
sample_theme <- function() {
    lin_theme() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = 'bold', hjust = 0),
                       axis.title = ggplot2::element_text(size = 11),
                       axis.text = ggplot2::element_text(size = 10, color = 'grey20'),
                       axis.line = ggplot2::element_line(colour = 'grey40', linewidth = 0.4),
                       axis.ticks = ggplot2::element_line(colour = 'grey40', linewidth = 0.4),
                       panel.grid.major.y = ggplot2::element_line(colour = 'grey92', linewidth = 0.4),
                       plot.margin = ggplot2::margin(6, 10, 6, 10),
                       legend.position = 'none')
}


#' Figure of the Sample Groups
#'
#' Three panels sharing the groups (one row per row of the summary):
#' concentrations of the wells with the mean and SD, CV of the replicates
#' against the limit, and the number of wells by quantification status.
#'
#' @param figure Output of [sample_figure_data()].
#' @param lang Language code.
#'
#' @return A plot from [cowplot::plot_grid()].
#' @keywords internal
plot_sample_groups <- function(figure, lang = 'en') {
    t <- function(key) tr(key, lang)
    colours <- sample_colours()
    groups <- figure$groups
    wells <- figure$wells[!is.na(figure$wells$label), , drop = FALSE]
    quantified <- wells[!is.na(wells$conc), , drop = FALSE]
    y_scale <- ggplot2::scale_y_discrete(drop = FALSE)

    # Concentrations: a logarithmic axis when they span more than a decade
    conc <- quantified$conc
    log_x <- length(conc) > 0 && all(conc > 0) && max(conc) / min(conc) >= 20
    sd_data <- groups[!is.na(groups$sd), , drop = FALSE]
    if (log_x) sd_data <- sd_data[sd_data$mean - sd_data$sd > 0, , drop = FALSE]
    p_conc <- ggplot2::ggplot(groups, ggplot2::aes(y = .data$label)) +
        y_scale +
        ggplot2::geom_segment(data = sd_data,
                              ggplot2::aes(x = .data$mean - .data$sd, xend = .data$mean + .data$sd,
                                           yend = .data$label),
                              colour = 'grey25', linewidth = 0.6) +
        ggplot2::geom_point(data = quantified, ggplot2::aes(x = .data$conc, colour = .data$status),
                            size = 2.2, alpha = 0.85,
                            position = ggplot2::position_jitter(width = 0, height = 0.12, seed = 1)) +
        ggplot2::geom_errorbar(data = groups[!is.na(groups$mean), , drop = FALSE],
                               ggplot2::aes(xmin = .data$mean, xmax = .data$mean),
                               width = 0.55, linewidth = 0.9, colour = 'grey10', orientation = 'y') +
        ggplot2::geom_text(data = groups[groups$n.quantified == 0, , drop = FALSE], ggplot2::aes(x = -Inf),
                           label = t('samples.not_quantified'), hjust = -0.05, colour = 'grey45',
                           fontface = 'italic', size = 3.4) +
        ggplot2::scale_colour_manual(values = colours, drop = FALSE) +
        ggplot2::labs(title = t('samples.panel_conc'), x = t('samples.axis_conc'), y = NULL) +
        sample_theme()
    if (log_x) p_conc <- p_conc + ggplot2::scale_x_log10()

    # CV of the replicates; groups without a CV are marked with a dash
    cv_data <- groups
    cv_data$fill <- ifelse(cv_data$cv.pass, 'pass', 'fail')
    limit <- min(groups$cv.limit)
    p_cv <- ggplot2::ggplot(cv_data, ggplot2::aes(y = .data$label)) +
        y_scale +
        ggplot2::geom_col(data = cv_data[!is.na(cv_data$cv), , drop = FALSE],
                          ggplot2::aes(x = .data$cv, fill = .data$fill), width = 0.6) +
        ggplot2::geom_text(data = cv_data[is.na(cv_data$cv), , drop = FALSE], ggplot2::aes(x = 0),
                           label = '\u2013', hjust = 0, colour = 'grey50', size = 3.5) +
        ggplot2::geom_vline(xintercept = limit, linetype = 'dashed', colour = 'grey30', linewidth = 0.5) +
        ggplot2::scale_fill_manual(values = colours) +
        ggplot2::scale_x_continuous(limits = c(0, max(c(cv_data$cv, limit), na.rm = TRUE) * 1.08),
                                    expand = ggplot2::expansion(mult = c(0, 0.02))) +
        ggplot2::labs(title = t('samples.panel_cv'), x = t('samples.axis_cv'), y = NULL) +
        sample_theme() +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

    # Number of wells by quantification status
    counts <- as.data.frame(table(label = wells$label, status = wells$status), stringsAsFactors = FALSE)
    counts <- counts[counts$Freq > 0, , drop = FALSE]
    counts$label <- factor(counts$label, levels = levels(groups$label))
    counts$status <- factor(counts$status, levels = rev(c('od_le_nc', 'below', 'ok', 'above', 'other')))
    p_status <- ggplot2::ggplot(counts, ggplot2::aes(x = .data$Freq, y = .data$label, fill = .data$status)) +
        y_scale +
        ggplot2::geom_col(width = 0.6, colour = 'white', linewidth = 0.4) +
        ggplot2::scale_fill_manual(values = colours) +
        ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(x))),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(title = t('samples.panel_status'), x = t('samples.axis_wells'), y = NULL) +
        sample_theme() +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

    cowplot::plot_grid(p_conc, p_cv, p_status, nrow = 1, align = 'h', axis = 'tb', rel_widths = c(3, 1.3, 1.3))
}


#' Legend of the Sample Figure
#'
#' @param lang Language code.
#' @return A shiny tag.
#' @keywords internal
samples_legend_ui <- function(lang) {
    t <- function(key) tr(key, lang)
    item <- function(class, label) {
        shiny::span(class = 'quant-legend-item', shiny::span(class = paste('quant-swatch', class)), label)
    }
    shiny::div(class = 'quant-legend',
               item('quant-ok', t('infographic.quantified')),
               item('quant-below', t('flag.below')),
               item('quant-above', t('flag.above')),
               item('quant-od_le_nc', t('flag.od_le_nc')),
               item('quant-other', t('infographic.other')),
               shiny::span(class = 'quant-legend-item', shiny::span(class = 'legend-mean', '|'),
                           t('samples.legend_mean')),
               item('cv-pass', t('samples.legend_cv_pass')),
               item('cv-fail', t('samples.legend_cv_fail')))
}


#' Summary Tiles of the Samples
#'
#' @param assessment Output of [assess_samples()].
#' @param qc QC assessment (the `qc` element of the quality assessment) or `NULL`.
#' @param lang Language code.
#'
#' @return A shiny tag.
#' @keywords internal
samples_tiles_ui <- function(assessment, qc, lang) {
    t <- function(key, ...) tr(key, lang, ...)
    s <- assessment$samples
    tile <- function(value, label, status = NULL) {
        shiny::div(class = paste(c('stat-tile', if (!is.null(status)) paste0('stat-', status)), collapse = ' '),
                   shiny::div(class = 'stat-value', value), shiny::div(class = 'stat-label', label))
    }
    of <- function(n, total) paste0(n, ' / ', total)
    good_if <- function(ok) if (ok) 'good' else 'bad'

    n_full <- sum(s$n.quantified == s$n)
    n_cv <- sum(grepl(FLAGS$high_cv, s$flag, fixed = TRUE))
    with_cv <- sum(!is.na(s$cv))
    tiles <- list(
        tile(nrow(s), t('samples.tile_groups')),
        tile(of(n_full, nrow(s)), t('samples.tile_quantified'), good_if(n_full == nrow(s))),
        tile(if (with_cv > 0) sprintf('%.1f %%', stats::median(s$cv, na.rm = TRUE)) else '\u2013',
             t('samples.tile_median_cv')),
        tile(of(n_cv, with_cv), t('samples.tile_high_cv'), if (with_cv > 0) good_if(n_cv == 0))
    )
    d <- assessment$dilution
    if (!is.null(d)) {
        cols <- setdiff(names(d), dilution_value_columns())
        keys <- unique(d[c(cols, 'pass')])
        tiles <- c(tiles, list(tile(of(sum(keys$pass), nrow(keys)), t('samples.tile_linear'),
                                    good_if(all(keys$pass)))))
    }
    if (!is.null(qc)) {
        tiles <- c(tiles, list(tile(of(sum(qc$levels$pass), nrow(qc$levels)), t('samples.tile_qc'),
                                    good_if(qc$accepted))))
    }
    shiny::div(class = 'stat-tiles', tiles)
}


#' Colours of the Samples in the Dilution Plot
#'
#' The Okabe-Ito palette (distinguishable with colour vision deficiency)
#' without its yellow, which is hard to see on white; more samples get evenly
#' spaced hues.
#'
#' @param n Number of samples.
#' @return Character vector of `n` colours.
#' @keywords internal
dilution_palette <- function(n) {
    okabe_ito <- c('#0072B2', '#D55E00', '#009E73', '#CC79A7', '#56B4E9', '#E69F00', '#000000')
    if (n <= length(okabe_ito)) okabe_ito[seq_len(n)] else grDevices::hcl(seq(15, 375, length.out = n + 1)[-1], 80, 55)
}


#' Samples of the Dilution Plot
#'
#' @param dilution `dilution` element of [assess_samples()].
#' @return Number of samples (lines of the plot).
#' @keywords internal
dilution_samples <- function(dilution) {
    if (is.null(dilution)) return(0L)
    cols <- setdiff(names(dilution), dilution_value_columns())
    nrow(unique(dilution[cols]))
}


#' Height of the Dilution Plot in Pixels
#'
#' Grows with the number of samples in the legend on the right.
#'
#' @param n Number of samples.
#' @return A number.
#' @keywords internal
dilution_plot_height <- function(n) max(400, 24 * n + 160)


#' Plot of Dilution Linearity
#'
#' Deviation of the mean at each dilution from the mean over all dilutions of
#' a sample, with the allowed deviation shaded. Each sample has its own colour
#' (see [dilution_palette()]); the lines of the samples whose dilutions do not
#' agree are dashed, also in the legend of the samples.
#'
#' @param dilution `dilution` element of [assess_samples()].
#' @param criteria See [qc_criteria()].
#' @param lang Language code.
#'
#' @return A ggplot2 object.
#' @keywords internal
plot_dilution_linearity <- function(dilution, criteria = qc_criteria(), lang = 'en') {
    t <- function(key) tr(key, lang)
    cols <- setdiff(names(dilution), dilution_value_columns())
    d <- dilution
    d$sample <- group_labels(d, cols)
    samples <- unique(d$sample)
    d$sample <- factor(d$sample, levels = samples)
    pass <- vapply(samples, function(x) all(d$pass[d$sample == x]), logical(1))
    dil <- sort(unique(d$dilution))
    d$x <- factor(d$dilution, levels = dil, labels = paste0('\u00d7', format(dil, trim = TRUE, drop0trailing = TRUE)))
    d$status <- factor(ifelse(d$pass, 'pass', 'fail'), levels = c('pass', 'fail'))
    limit <- criteria$dilution_bias
    span <- max(limit * 1.3, abs(d$deviation) * 1.1, na.rm = TRUE)
    ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$deviation, group = .data$sample, colour = .data$sample)) +
        ggplot2::annotate('rect', xmin = -Inf, xmax = Inf, ymin = -limit, ymax = limit, fill = 'grey93') +
        ggplot2::geom_hline(yintercept = 0, colour = 'grey50', linewidth = 0.4) +
        # Keys of both statuses are shown even when only one occurs
        ggplot2::geom_line(ggplot2::aes(linetype = .data$status), linewidth = 0.8, alpha = 0.9,
                           position = ggplot2::position_dodge(width = 0.15), show.legend = TRUE) +
        ggplot2::geom_point(ggplot2::aes(shape = .data$status), size = 2.6,
                            position = ggplot2::position_dodge(width = 0.15), show.legend = TRUE) +
        ggplot2::scale_colour_manual(values = stats::setNames(dilution_palette(length(samples)), samples),
                                     name = NULL) +
        ggplot2::scale_linetype_manual(values = c(pass = 'solid', fail = 'dashed'), drop = FALSE,
                                       labels = c(t('samples.legend_linear'), t('samples.legend_nonlinear')),
                                       name = NULL) +
        ggplot2::scale_shape_manual(values = c(pass = 16, fail = 17), drop = FALSE,
                                    labels = c(t('samples.legend_linear'), t('samples.legend_nonlinear')),
                                    name = NULL) +
        ggplot2::scale_y_continuous(limits = c(-span, span)) +
        ggplot2::labs(x = t('col.dilution'), y = t('col.deviation')) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, order = 1, override.aes = list(
                            linetype = ifelse(pass, 'solid', 'dashed'), shape = ifelse(pass, 16, 17))),
                        linetype = ggplot2::guide_legend(order = 2, override.aes = list(colour = 'grey30')),
                        shape = ggplot2::guide_legend(order = 2)) +
        sample_theme() +
        ggplot2::theme(legend.position = 'right', legend.box = 'vertical', legend.justification = 'top',
                       legend.text = ggplot2::element_text(size = 10),
                       legend.key.width = ggplot2::unit(26, 'pt'), panel.grid.major.y = ggplot2::element_blank())
}


#' Plot of QC Accuracy
#'
#' Bias of each QC well from the nominal concentration, with the allowed bias
#' of each level shaded.
#'
#' @param qc QC assessment (see [assess_qc()]).
#' @param lang Language code.
#'
#' @return A ggplot2 object.
#' @keywords internal
plot_qc_accuracy <- function(qc, lang = 'en') {
    t <- function(key) tr(key, lang)
    lv <- qc$levels[order(qc$levels$nominal, decreasing = TRUE), , drop = FALSE]
    lv$y <- factor(lv$id, levels = lv$id)
    w <- qc$wells[!is.na(qc$wells$bias), , drop = FALSE]
    w$y <- factor(w$id, levels = lv$id)
    w$status <- ifelse(w$pass, 'pass', 'fail')
    span <- max(lv$bias.limit * 1.3, abs(w$bias) * 1.1, na.rm = TRUE)
    ggplot2::ggplot(w, ggplot2::aes(.data$bias, .data$y)) +
        ggplot2::geom_tile(data = lv, ggplot2::aes(x = 0, y = .data$y, width = 2 * .data$bias.limit),
                           height = 0.7, fill = 'grey93') +
        ggplot2::geom_vline(xintercept = 0, colour = 'grey50', linewidth = 0.4) +
        ggplot2::geom_point(ggplot2::aes(colour = .data$status), size = 2.6, alpha = 0.9,
                            position = ggplot2::position_jitter(width = 0, height = 0.12, seed = 1)) +
        ggplot2::scale_colour_manual(values = sample_colours(), breaks = c('pass', 'fail'),
                                     labels = c(t('samples.legend_qc_pass'), t('samples.legend_qc_fail'))) +
        ggplot2::scale_x_continuous(limits = c(-span, span)) +
        ggplot2::scale_y_discrete(drop = FALSE) +
        ggplot2::labs(x = t('col.bias'), y = NULL, colour = NULL) +
        sample_theme() +
        ggplot2::theme(legend.position = 'bottom')
}
