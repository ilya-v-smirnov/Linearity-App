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


#' Background Correction as a Table
#'
#' @param bg The `background` element of [calculate()] (see
#'   [background_summary()]).
#' @param lang Language code.
#'
#' @return A data.frame with translated column names: the roles used, the
#'   wells, their mean OD, SD and the value subtracted.
#' @keywords internal
background_table <- function(bg, lang = 'en') {
    num <- function(x) if (is.na(x)) '—' else sprintf('%.4f', x)
    source <- if (length(bg$roles) == 0) tr('bg.none', lang) else paste(role_labels(bg$roles, lang), collapse = ' + ')
    wells <- if (bg$n == 0) '0' else paste0(bg$n, ' (', paste(bg$keys, collapse = ', '), ')')
    out <- data.frame(source, wells, num(bg$mean), num(bg$sd), sprintf('%.4f', bg$value))
    names(out) <- vapply(c('bg.source', 'bg.wells', 'bg.mean', 'bg.sd', 'bg.subtracted'), tr, '', lang = lang)
    out
}


#' Notes Under the Background Table
#'
#' @inheritParams background_table
#'
#' @return Character vector: the roles of the plate left out, and the value of
#'   earlier versions when it differs (to 4 significant digits).
#' @keywords internal
background_notes <- function(bg, lang = 'en') {
    c(if (length(bg$unused) > 0) tr('bg.unused', lang, roles = roles_text(bg$unused, lang)),
      if (!is.na(bg$legacy) && signif(bg$legacy, 4) != signif(bg$value, 4)) {
          tr('bg.legacy_value', lang, legacy = signif(bg$legacy, 4))
      })
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
    tr('bg.report', lang, roles = paste(role_labels(bg$roles, lang), collapse = ' + '), n = bg$n,
       mean = sprintf('%.4f', bg$mean), sd = if (is.na(bg$sd)) '—' else sprintf('%.4f', bg$sd),
       value = sprintf('%.4f', bg$value))
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
