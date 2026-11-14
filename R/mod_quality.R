#' Quality Tables for Display
#'
#' Formats the parts of a quality assessment (see [assess_run()]) as tables
#' with translated column names, for the app and the report.
#'
#' @param quality The `quality` element of [calculate()].
#' @param lang Language code.
#'
#' @return A named list of data.frames (`standards`, `qc`, `samples`,
#'   `dilution`, `curve`); absent parts are `NULL`.
#' @export
quality_tables <- function(quality, lang = 'en') {
    t <- function(key) tr(key, lang)
    num <- function(x, digits = 3) ifelse(is.na(x), '', as.character(signif(x, digits)))
    pct <- function(x) ifelse(is.na(x), '', sprintf('%.1f', x))
    mark <- function(x) ifelse(x, t('quality.pass'), t('quality.fail'))
    name <- function(df, keys) stats::setNames(df, vapply(keys, t, ''))

    st <- quality$standards
    standards <- name(data.frame(num(st$conc), st$n, num(st$od.mean, 4), pct(st$od.cv), num(st$calc.mean),
                                 pct(st$bias), sprintf('%g', st$bias.limit), mark(st$pass)),
                      c('col.conc', 'col.count', 'col.od_mean', 'col.od_cv', 'col.calc', 'col.bias',
                        'col.limit', 'col.pass'))

    qc <- NULL
    if (!is.null(quality$qc)) {
        q <- quality$qc$levels
        qc <- name(data.frame(q$id, num(q$nominal), q$n, q$n.pass, num(q$mean), pct(q$cv), pct(q$bias),
                              sprintf('%g', q$bias.limit), mark(q$pass)),
                   c('col.id', 'col.nominal', 'col.count', 'col.n_pass', 'col.mean', 'col.cv', 'col.bias',
                     'col.limit', 'col.pass'))
    }

    samples <- NULL
    if (!is.null(quality$samples)) {
        s <- quality$samples
        id_cols <- intersect(c('id', 'sample.name', 'dilution', 'sample.date'), names(s))
        keys <- c(id = 'col.id', sample.name = 'col.sample', dilution = 'col.dilution', sample.date = 'col.date')
        s[id_cols] <- lapply(s[id_cols], as.character)
        samples <- data.frame(s[id_cols],
                              n = s$n, n.quantified = s$n.quantified, mean = num(s$mean), sd = num(s$sd),
                              cv = pct(s$cv), flag = translate_flags(s$flag, lang))
        samples <- name(samples, c(keys[id_cols], 'col.count', 'col.n_quant', 'col.mean', 'col.sd', 'col.cv',
                                   'col.flag'))
    }

    dilution <- NULL
    if (!is.null(quality$dilution)) {
        d <- quality$dilution
        id_cols <- intersect(c('sample.name', 'sample.date'), names(d))
        keys <- c(sample.name = 'col.sample', sample.date = 'col.date')
        dilution <- name(data.frame(d[id_cols], as.character(d$dilution), num(d$mean), pct(d$deviation), pct(d$cv),
                                    mark(d$pass)),
                         c(keys[id_cols], 'col.dilution', 'col.mean', 'col.deviation', 'col.cv_dil', 'col.pass'))
    }

    cv <- quality$curve
    curve <- name(data.frame(c(t('quality.r2'), t('quality.sigma'), t('quality.lof')),
                             c(sprintf('%.4f', cv$r2), num(cv$sigma), if (is.na(cv$lof_p)) '' else num(cv$lof_p, 2))),
                  c('quality.parameter', 'quality.value'))

    list(standards = standards, qc = qc, samples = samples, dilution = dilution, curve = curve)
}


#' Verdict Lines of a Quality Assessment
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#'
#' @return A list with `title`, `range`, `levels`, `reasons` and `warnings`
#'   (character).
#' @keywords internal
quality_summary <- function(result, lang = 'en') {
    q <- result$quality
    cal <- q$calibration
    list(title = tr(if (q$accepted) 'quality.accepted' else 'quality.rejected', lang),
         range = tr('quality.range', lang, lloq = signif(result$calibration_range[1], 3),
                    uloq = signif(result$calibration_range[2], 3)),
         levels = tr('quality.levels', lang, n = cal$n_pass, total = cal$n_levels, needed = cal$n_needed),
         reasons = message_text(q$reasons, lang),
         warnings = message_text(q$warnings, lang))
}


#' Criteria Inputs: Codes, Defaults and Scaling
#' @keywords internal
criteria_fields <- function() {
    d <- qc_criteria()
    fields <- data.frame(
        name = c('std_bias', 'std_bias_edge', 'cv', 'cv_edge', 'min_fraction', 'min_levels',
                 'qc_bias', 'qc_bias_edge', 'qc_fraction', 'qc_level_fraction', 'dilution_bias', 'dilution_cv'),
        scale = c(1, 1, 1, 1, 100, 1, 1, 1, 100, 100, 1, 1)
    )
    fields$default <- vapply(fields$name, function(n) d[[n]], 0) * fields$scale
    fields
}


#' Acceptance Criteria Module
#'
#' Inputs for the acceptance criteria, shown in a collapsible block.
#'
#' @param id Module id.
#' @param lang Language code.
#'
#' @return `mod_criteria_server()` returns a reactive with a `lin_criteria` object.
#' @name mod_criteria
#' @keywords internal
NULL

#' @rdname mod_criteria
mod_criteria_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    fields <- criteria_fields()
    inputs <- lapply(seq_len(nrow(fields)), function(i) {
        shiny::column(4, shiny::numericInput(ns(fields$name[i]), t(paste0('crit.', fields$name[i])),
                                             value = round(fields$default[i], 1), min = 0, step = 1))
    })
    shiny::tags$details(
        shiny::tags$summary(shiny::strong(t('quality.section_criteria'))),
        shiny::helpText(t('crit.note')),
        do.call(shiny::fluidRow, inputs),
        shiny::radioButtons(ns('range'), t('crit.range'),
                            choices = stats::setNames(c('accepted', 'all'),
                                                      c(t('crit.range_accepted'), t('crit.range_all'))),
                            selected = 'accepted', inline = TRUE),
        shiny::actionButton(ns('reset'), t('crit.reset'), icon = shiny::icon('undo'))
    )
}

#' @rdname mod_criteria
mod_criteria_server <- function(id, lang) {
    shiny::moduleServer(id, function(input, output, session) {
        fields <- criteria_fields()

        shiny::observeEvent(input$reset, {
            for (i in seq_len(nrow(fields))) {
                shiny::updateNumericInput(session, fields$name[i], value = round(fields$default[i], 1))
            }
            shiny::updateRadioButtons(session, 'range', selected = 'accepted')
        })

        shiny::reactive({
            values <- lapply(seq_len(nrow(fields)), function(i) {
                v <- input[[fields$name[i]]]
                if (is.null(v)) fields$default[i] / fields$scale[i] else v / fields$scale[i]
            })
            names(values) <- fields$name
            values$range <- if (is.null(input$range)) 'accepted' else input$range
            with_validation(do.call(qc_criteria, values), 'common.calc_error', lang)
        })
    })
}


#' Quality Module
#'
#' Verdict of the run, standards, quality controls, sample replicates,
#' dilution linearity, curve quality and the acceptance criteria.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param curve List of reactives returned by [mod_curve_server()].
#'
#' @name mod_quality
#' @keywords internal
NULL

#' @rdname mod_quality
mod_quality_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    shiny::tagList(
        shiny::uiOutput(ns('verdict')),
        shiny::h4(t('quality.section_standards')),
        shiny::tableOutput(ns('standards')),
        shiny::uiOutput(ns('qc')),
        shiny::h4(t('quality.section_samples')),
        shiny::tableOutput(ns('samples')),
        shiny::uiOutput(ns('dilution')),
        shiny::h4(t('quality.section_curve')),
        shiny::tableOutput(ns('curve')),
        mod_criteria_ui(ns('criteria'), lang),
        shiny::br()
    )
}

#' @rdname mod_quality
mod_quality_server <- function(id, lang, curve) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        tables <- shiny::reactive(quality_tables(curve$result()$quality, lang))

        output$verdict <- shiny::renderUI({
            s <- quality_summary(curve$result(), lang)
            accepted <- curve$result()$quality$accepted
            shiny::tagList(
                shiny::div(class = if (accepted) 'alert alert-success' else 'alert alert-danger',
                           shiny::h4(s$title),
                           shiny::p(s$range), shiny::p(s$levels),
                           if (length(s$reasons) > 0) shiny::tags$ul(lapply(s$reasons, shiny::tags$li))),
                notes_ui(s$warnings, lang)
            )
        })
        output$standards <- shiny::renderTable(tables()$standards)
        output$samples <- shiny::renderTable(tables()$samples)
        output$curve <- shiny::renderTable(tables()$curve)

        output$qc <- shiny::renderUI({
            if (is.null(tables()$qc)) return(NULL)
            shiny::tagList(shiny::h4(tr('quality.section_qc', lang)), shiny::tableOutput(ns('qc_table')))
        })
        output$qc_table <- shiny::renderTable(tables()$qc)

        output$dilution <- shiny::renderUI({
            if (is.null(tables()$dilution)) return(NULL)
            shiny::tagList(shiny::h4(tr('quality.section_dilution', lang)),
                           shiny::tableOutput(ns('dilution_table')))
        })
        output$dilution_table <- shiny::renderTable(tables()$dilution)

        tables
    })
}
