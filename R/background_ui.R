#' Labels of Background Roles
#'
#' @param roles Roles (see [background_roles()]).
#' @param lang Language code.
#'
#' @return Character vector of translated labels.
#' @keywords internal
role_labels <- function(roles, lang = 'en') {
    vapply(roles, function(r) tr(paste0('data.type_', tolower(r)), lang), '', USE.NAMES = FALSE)
}


#' Choices of Background Roles
#'
#' The roles present on the plate, labelled with their number of wells.
#'
#' @param type Vector of well types.
#' @param lang Language code.
#'
#' @return A character vector of roles named by their labels.
#' @keywords internal
background_choices <- function(type, lang = 'en') {
    roles <- background_roles()
    n <- vapply(roles, function(r) sum(type %in% r), 0)
    roles <- roles[n > 0]
    stats::setNames(roles, paste0(role_labels(roles, lang), ' (', n[n > 0], ')'))
}


#' Roles with Their Number of Wells as Text
#' @keywords internal
roles_text <- function(counts, lang) {
    paste0(role_labels(names(counts), lang), ' (', tr('bg.n_wells', lang, n = unname(counts)), ')', collapse = ', ')
}


#' Wells Excluded by the User as Text
#' @keywords internal
excluded_text <- function(bg) {
    w <- bg$wells[bg$wells$excluded, , drop = FALSE]
    paste0(w$key, ' (', w$reason, ')', collapse = ', ')
}


#' Background Correction as a Table
#'
#' @param bg The `background` element of [calculate()] (see
#'   [background_summary()]).
#' @param lang Language code.
#'
#' @return A data.frame with translated column names: the roles used, the
#'   wells used out of the wells of these roles (e.g. `3/4`), their mean OD,
#'   SD and the value subtracted.
#' @keywords internal
background_table <- function(bg, lang = 'en') {
    num <- function(x) if (is.na(x)) '—' else sprintf('%.4f', x)
    source <- if (length(bg$roles) == 0) tr('bg.none', lang) else paste(role_labels(bg$roles, lang), collapse = ' + ')
    out <- data.frame(source, paste0(bg$n, '/', bg$n_roles), num(bg$mean), num(bg$sd), sprintf('%.4f', bg$value))
    names(out) <- vapply(c('bg.source', 'bg.used', 'bg.mean', 'bg.sd', 'bg.subtracted'), tr, '', lang = lang)
    out
}


#' Notes Under the Background Table
#'
#' @inheritParams background_table
#'
#' @return Character vector: the wells excluded by the user and the roles of
#'   the plate left out.
#' @keywords internal
background_notes <- function(bg, lang = 'en') {
    c(if (any(bg$wells$excluded)) tr('bg.excluded', lang, wells = excluded_text(bg)),
      if (length(bg$unused) > 0) tr('bg.unused', lang, roles = roles_text(bg$unused, lang)))
}


#' Background Correction in One Line
#'
#' For the parameters of the report.
#'
#' @inheritParams background_table
#'
#' @return A character string.
#' @keywords internal
background_text <- function(bg, lang = 'en') {
    if (length(bg$roles) == 0) return(tr('bg.none_subtracted', lang))
    out <- tr('bg.report', lang, roles = paste(role_labels(bg$roles, lang), collapse = ' + '),
              n = paste0(bg$n, '/', bg$n_roles),
              mean = if (is.na(bg$mean)) '—' else sprintf('%.4f', bg$mean),
              sd = if (is.na(bg$sd)) '—' else sprintf('%.4f', bg$sd),
              value = sprintf('%.4f', bg$value))
    if (any(bg$wells$excluded)) out <- paste0(out, '; ', tr('bg.report_excluded', lang, wells = excluded_text(bg)))
    out
}


#' Background Wells of an Imported Plate
#'
#' Tells on import which wells are subtracted as background by default and
#' which background-like wells are not.
#'
#' @param data Imported data.
#' @param lang Language code.
#'
#' @return A shiny tag.
#' @keywords internal
background_import_ui <- function(data, lang = 'en') {
    role <- default_background(data$type)
    if (length(role) == 0) {
        return(shiny::p(class = 'text-muted', shiny::icon('circle-info'), ' ', tr('bg.import_none', lang)))
    }
    count <- function(roles) stats::setNames(vapply(roles, function(r) sum(data$type %in% r), 0), roles)
    unused <- count(setdiff(background_roles(), role))
    unused <- unused[unused > 0]
    legacy <- identical(role, 'NC')
    shiny::div(class = paste('alert', if (legacy) 'alert-warning' else 'alert-light', 'background-import'),
               shiny::icon('fill-drip'), ' ',
               tr('bg.import_used', lang, roles = roles_text(count(role), lang)),
               if (legacy) paste0(' ', tr('bg.import_legacy', lang)),
               if (length(unused) > 0) paste0(' ', tr('bg.unused', lang, roles = roles_text(unused, lang))),
               paste0(' ', tr('bg.import_hint', lang)))
}


#' Wells of the Background Roles for Display
#'
#' @inheritParams background_table
#'
#' @return A data.frame with translated column names: role, position, OD,
#'   deviation from the value subtracted and the status of the well (used,
#'   excluded by the user with the reason, or of a role left out); the wells
#'   used first, then by role in the order of [background_roles()]. `NULL`
#'   without background wells.
#' @keywords internal
background_wells_table <- function(bg, lang = 'en') {
    w <- bg$wells
    if (is.null(w) || nrow(w) == 0) return(NULL)
    w <- w[order(!w$used, match(w$role, background_roles())), , drop = FALSE]
    num <- function(x, fmt) ifelse(is.na(x), '', sprintf(fmt, x))
    status <- ifelse(w$used, tr('bg.used_yes', lang),
                     ifelse(w$excluded, paste0(tr('bg.status_excluded', lang), ': ', w$reason), tr('bg.used_no', lang)))
    out <- data.frame(role_labels(w$role, lang), w$key, num(w$OD, '%.4f'), num(w$OD - bg$value, '%+.4f'), status)
    names(out) <- vapply(c('bg.role', 'bg.position', 'bg.od', 'bg.deviation', 'bg.status'), tr, '', lang = lang)
    out
}


#' Points of the Background Plot
#'
#' The wells of the background roles, then the standards as measured, each
#' group at its own position on the x axis: the roles in the order of
#' [background_roles()], then the standard levels from the lowest; the
#' points of a group are spread around its position.
#'
#' @inheritParams background_table
#' @param standards Standard wells as measured: a data.frame with `conc` and
#'   `OD`, or `NULL`.
#'
#' @return A data.frame with `x`, `OD`, `group` (index of the group), `kind`
#'   (`"used"`, `"unused"`, `"excluded"` or `"standard"`), `key` and
#'   `role`, with the attributes `labels` (labels of the groups) and
#'   `n_roles` (number of background groups).
#' @keywords internal
background_points <- function(bg, standards = NULL, lang = 'en') {
    w <- bg$wells
    w <- w[!is.na(w$OD), , drop = FALSE]
    roles <- intersect(background_roles(), w$role)
    kind <- ifelse(w$used, 'used', ifelse(w$excluded, 'excluded', 'unused'))
    pts <- data.frame(group = match(w$role, roles), OD = w$OD, kind = kind, key = w$key, role = w$role)
    labels <- sub(' ', '\n', role_labels(roles, lang))
    if (!is.null(standards)) {
        standards <- standards[!is.na(standards$OD) & !is.na(standards$conc), , drop = FALSE]
        levels <- sort(unique(standards$conc))
        if (length(levels) > 0) {
            pts <- rbind(pts, data.frame(group = length(roles) + match(standards$conc, levels), OD = standards$OD,
                                         kind = 'standard', key = NA_character_, role = 'standard'))
            labels <- c(labels, paste0(standard_names(levels, standards$conc), '\n', signif(levels, 3)))
        }
    }
    # The points of a group side by side, centred on its position
    spread <- stats::ave(seq_len(nrow(pts)), pts$group, FUN = function(i) (seq_along(i) - (length(i) + 1) / 2) * 0.12)
    pts$x <- pts$group + spread
    attr(pts, 'labels') <- labels
    attr(pts, 'n_roles') <- length(roles)
    pts
}


#' Plot of the Background Wells
#'
#' The OD of every well of a background role next to the standards as
#' measured (from the lowest level), so that the scatter of the blanks can be
#' judged against the signal of the calibrators. Wells used are filled,
#' wells of a role left out open, wells excluded by the user red crosses; the
#' dashed line is the value subtracted. A click on a background well excludes
#' or restores it.
#'
#' @inheritParams background_points
#'
#' @return A ggplot2 object, or `NULL` without background wells.
#' @keywords internal
plot_background <- function(bg, standards = NULL, lang = 'en') {
    if (is.null(bg$wells) || all(is.na(bg$wells$OD))) return(NULL)
    pts <- background_points(bg, standards, lang)
    labels <- attr(pts, 'labels')
    n_roles <- attr(pts, 'n_roles')
    kinds <- c(used = tr('bg.legend_used', lang), unused = tr('bg.legend_unused', lang),
               excluded = tr('bg.legend_excluded', lang), standard = tr('bg.legend_standard', lang))
    shown <- intersect(names(kinds), pts$kind)
    plot <- ggplot2::ggplot(pts, ggplot2::aes(.data$x, .data$OD, shape = .data$kind, fill = .data$kind,
                                               colour = .data$kind)) +
        ggplot2::geom_point(size = 2.8, stroke = 1) +
        ggplot2::scale_shape_manual(values = c(used = 21, unused = 21, excluded = 4, standard = 21),
                                    labels = kinds[shown], breaks = shown, name = NULL) +
        ggplot2::scale_fill_manual(values = c(used = 'grey20', unused = 'white', excluded = 'red', standard = '#9fc5e8'),
                                   labels = kinds[shown], breaks = shown, name = NULL) +
        ggplot2::scale_colour_manual(values = c(used = 'grey20', unused = 'grey20', excluded = 'red',
                                                standard = '#3d85c6'),
                                     labels = kinds[shown], breaks = shown, name = NULL) +
        ggplot2::scale_x_continuous(breaks = seq_along(labels), labels = labels,
                                    limits = c(0.5, length(labels) + 0.5), expand = c(0, 0)) +
        ggplot2::labs(x = NULL, y = tr('bg.od_measured', lang)) +
        lin_theme() +
        ggplot2::theme(legend.position = 'bottom', axis.text.x = ggplot2::element_text(size = 9),
                       panel.grid.minor.x = ggplot2::element_blank())
    if (n_roles < length(labels)) {
        plot <- plot + ggplot2::geom_vline(xintercept = n_roles + 0.5, colour = 'grey60')
    }
    if (length(bg$roles) > 0) {
        plot <- plot + ggplot2::geom_hline(yintercept = bg$value, linetype = 'dashed', colour = 'blue')
    }
    plot
}
