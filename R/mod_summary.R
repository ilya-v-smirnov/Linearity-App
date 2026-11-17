#' Result Summary Step Module
#'
#' The sidebar holds the grouping columns (with the number of distinct values
#' of each column, quick choices and a preview of the number of rows), the
#' statistics to show, the acceptance criteria of the samples and the
#' download. The grouping defines the replicates: the precision of each group
#' and the dilution linearity of the samples are assessed with it (see
#' [assess_samples()]). The main area has two tabs: the aggregated table with
#' the dilution linearity and the quality controls, and the infographics of
#' the samples.
#'
#' @param id Module id.
#' @param lang Language code.
#' @param curve List of reactives returned by [mod_curve_server()].
#' @param rounding Reactive from [mod_rounding_server()].
#'
#' @return `mod_summary_server()` returns a list of reactives: `table` (the
#'   aggregated table shown), `assessment` (see [assess_samples()]) and
#'   `warnings` (translated warnings of the samples).
#' @name mod_summary
#' @keywords internal
NULL

#' Columns Available for Grouping
#'
#' All columns of the result table except the measurements, the flag and the
#' well, in display order (see [result_columns()]).
#'
#' @param result Result table from [calculate()].
#'
#' @return Character vector.
#' @keywords internal
grouping_columns <- function(result) {
    exclude <- c('type', 'well', 'OD', 'conc', 'conc.lower', 'conc.upper', 'flag')
    setdiff(result_columns(names(result)), exclude)
}


#' Statistics of the Summary Table
#' @keywords internal
summary_statistics <- function() c('n', 'n.quantified', 'mean', 'sd', 'cv', 'min', 'max', 'flag')


#' Summary Table for Display
#'
#' @param samples `samples` element of [assess_samples()].
#' @param group_vars Grouping columns.
#' @param stats Statistics to keep (see [summary_statistics()]).
#' @param round_fun,round_par Rounding of the concentrations.
#' @param lang Language code of the flags.
#'
#' @return A data.frame, or `NULL` without groups.
#' @keywords internal
summary_table <- function(samples, group_vars, stats = summary_statistics(), round_fun = signif, round_par = 3,
                          lang = 'en') {
    if (is.null(samples) || length(group_vars) == 0) return(NULL)
    out <- samples
    for (col in c('mean', 'sd', 'min', 'max')) out[[col]] <- round_fun(out[[col]], round_par)
    out$cv <- round(out$cv, 1)
    out$flag <- translate_flags(out$flag, lang)
    out[, c(group_vars, intersect(summary_statistics(), stats)), drop = FALSE]
}


#' @rdname mod_summary
mod_summary_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    stats <- summary_statistics()

    sidebar <- bslib::sidebar(
        width = 330,
        sidebar_group(
            with_tooltip(t('summary.group_title'), t('summary.group_help')), 'layer-group',
            shiny::div(class = 'summary-presets',
                       shiny::actionButton(ns('preset_samples'), t('summary.preset_samples'),
                                           class = 'btn-sm btn-outline-primary'),
                       shiny::actionButton(ns('preset_dilutions'), t('summary.preset_dilutions'),
                                           class = 'btn-sm btn-outline-primary')),
            shiny::checkboxGroupInput(ns('group_vars'), NULL, choices = character(0)),
            shiny::uiOutput(ns('preview'))
        ),
        sidebar_group(
            t('summary.stats_title'), 'calculator',
            shiny::checkboxGroupInput(ns('stats'), NULL,
                                      choices = stats::setNames(stats, vapply(paste0('summary.stat_', stats), t, '')),
                                      selected = stats)
        ),
        mod_criteria_ui('criteria', lang, 'samples'),
        sidebar_group(
            t('results.export'), 'file-excel',
            shiny::downloadButton(ns('download'), t('summary.download'), icon = shiny::icon('download'),
                                  class = 'btn-primary w-100')
        )
    )

    main <- bslib::navset_card_underline(
        full_screen = TRUE,
        bslib::nav_panel(
            t('summary.tab_table'), value = 'table',
            shiny::div(class = 'step-view',
                       shiny::uiOutput(ns('warnings')),
                       shiny::uiOutput(ns('empty')),
                       DT::DTOutput(ns('table')),
                       shiny::uiOutput(ns('dilution')),
                       shiny::uiOutput(ns('qc')))
        ),
        bslib::nav_panel(
            t('summary.tab_infographics'), value = 'infographics',
            shiny::div(class = 'step-view', shiny::uiOutput(ns('infographics')))
        )
    )

    bslib::layout_sidebar(sidebar = sidebar, main)
}

#' @rdname mod_summary
mod_summary_server <- function(id, lang, curve, rounding) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        t <- function(key, ...) tr(key, lang, ...)
        result <- shiny::reactive(curve$result()$result)
        columns <- shiny::reactive(grouping_columns(result()))

        # Default grouping: one row per sample (name, date and additional columns)
        samples <- shiny::reactive(intersect(columns(), sample_columns(result())))

        # Choices show the number of distinct values; the user's choice is kept
        known <- shiny::reactiveVal(character(0))
        shiny::observeEvent(result(), {
            cols <- columns()
            res <- result()
            labels <- lapply(cols, function(col) {
                shiny::span(col, shiny::span(class = 'text-muted small',
                                             t('summary.values', n = length(unique(res[[col]])))))
            })
            selected <- if (length(known()) == 0) samples() else
                c(intersect(cols, input$group_vars), setdiff(intersect(cols, samples()), known()))
            known(union(known(), cols))
            shiny::updateCheckboxGroupInput(session, 'group_vars', choiceNames = labels, choiceValues = cols,
                                            selected = intersect(cols, selected))
        })
        shiny::observeEvent(input$preset_samples,
                            shiny::updateCheckboxGroupInput(session, 'group_vars', selected = samples()))
        shiny::observeEvent(input$preset_dilutions, {
            shiny::updateCheckboxGroupInput(session, 'group_vars',
                                            selected = intersect(columns(), c(samples(), 'dilution')))
        })

        group_vars <- shiny::reactive({
            vars <- if (is.null(input$group_vars) && length(known()) == 0) samples() else input$group_vars
            intersect(columns(), vars)
        })

        # The groups of the summary are the replicates of the quality assessment
        assessment <- shiny::reactive({
            vars <- group_vars()
            if (length(vars) == 0) return(NULL)
            r <- curve$result()
            assess_samples(r$result, r$calibration_range, r$quality$criteria, vars)
        })
        warnings <- shiny::reactive({
            a <- assessment()
            if (is.null(a)) character(0) else message_text(a$warnings, lang)
        })

        table <- shiny::reactive({
            a <- assessment()
            if (is.null(a)) return(NULL)
            r <- rounding()
            # No choice (before the first answer of the browser, or nothing ticked): all statistics
            stats <- if (is.null(input$stats)) summary_statistics() else input$stats
            summary_table(a$samples, group_vars(), stats, r$fun, r$par, lang)
        })
        dilution_tables <- function(highlight) {
            a <- assessment()
            if (is.null(a)) return(NULL)
            quality_tables(list(dilution = a$dilution, criteria = curve$result()$quality$criteria), lang,
                           highlight = highlight)$dilution
        }
        tables <- shiny::reactive({
            list(dilution = dilution_tables(FALSE),
                 qc = quality_tables(list(qc = curve$result()$quality$qc), lang)$qc)
        })
        # Values failing the acceptance criteria are highlighted in the app
        dilution_marked <- shiny::reactive(dilution_tables(TRUE))

        output$preview <- shiny::renderUI({
            shiny::req(result())
            n_rows <- if (is.null(table())) 0 else nrow(table())
            shiny::div(class = 'summary-preview', shiny::icon('arrow-right'), ' ',
                       t('summary.preview', wells = nrow(result()), rows = n_rows))
        })

        empty_state <- function() {
            shiny::div(class = 'empty-state', shiny::icon('layer-group', class = 'fa-2x'),
                       shiny::p(t('summary.choose')))
        }
        output$empty <- shiny::renderUI(if (is.null(table())) empty_state())
        output$warnings <- shiny::renderUI(notes_ui(warnings(), lang))

        output$table <- DT::renderDT({
            shiny::req(table())
            results_datatable(table(), lang)
        })

        output$dilution <- shiny::renderUI({
            if (is.null(tables()$dilution)) return(NULL)
            criteria <- curve$result()$quality$criteria
            shiny::tagList(shiny::h5(class = 'mt-4', with_tooltip(t('summary.dilution_table'),
                                                                  t('summary.dilution_table_help',
                                                                    bias = criteria$dilution_bias,
                                                                    cv = criteria$dilution_cv))),
                           shiny::tableOutput(ns('dilution_table')))
        })
        output$dilution_table <- shiny::renderTable(dilution_marked(), na = '',
                                                    sanitize.text.function = function(x) x)

        output$qc <- shiny::renderUI({
            if (is.null(tables()$qc)) return(NULL)
            shiny::tagList(shiny::h5(class = 'mt-4', t('quality.section_qc')), shiny::tableOutput(ns('qc_table')))
        })
        output$qc_table <- shiny::renderTable(tables()$qc, na = '')

        # Infographics ---------------------------------------------------------
        figure <- shiny::reactive({
            a <- assessment()
            if (is.null(a)) return(NULL)
            sample_figure_data(result(), a$samples, group_vars())
        })

        output$infographics <- shiny::renderUI({
            a <- assessment()
            if (is.null(a)) return(empty_state())
            qc <- curve$result()$quality$qc
            n_groups <- nrow(a$samples)
            shiny::tagList(
                samples_tiles_ui(a, qc, lang),
                quantification_ui(result()$flag, lang),
                shiny::div(class = 'infographic',
                           shiny::div(class = 'infographic-title', t('samples.figure_title')),
                           shiny::plotOutput(ns('groups_plot'), height = paste0(groups_plot_height(n_groups), 'px')),
                           samples_legend_ui(lang)),
                shiny::fluidRow(
                    shiny::column(6, shiny::div(
                        class = 'infographic',
                        shiny::div(class = 'infographic-title', with_tooltip(t('quality.section_dilution'),
                                                                             t('summary.dilution_help'))),
                        if (is.null(a$dilution)) shiny::p(class = 'text-muted small mb-0', t('samples.no_dilution'))
                        else shiny::plotOutput(ns('dilution_plot'), height = '300px'))),
                    shiny::column(6, shiny::div(
                        class = 'infographic',
                        shiny::div(class = 'infographic-title', t('quality.section_qc')),
                        if (is.null(qc)) shiny::p(class = 'text-muted small mb-0', t('samples.no_qc'))
                        else shiny::plotOutput(ns('qc_plot'), height = '300px')))
                )
            )
        })
        output$groups_plot <- shiny::renderPlot({
            shiny::req(figure())
            plot_sample_groups(figure(), lang)
        })
        output$dilution_plot <- shiny::renderPlot({
            a <- assessment()
            shiny::req(a$dilution)
            plot_dilution_linearity(a$dilution, curve$result()$quality$criteria, lang)
        })
        output$qc_plot <- shiny::renderPlot({
            qc <- curve$result()$quality$qc
            shiny::req(qc)
            plot_qc_accuracy(qc, lang)
        })

        output$download <- shiny::downloadHandler(
            filename = function() paste0(Sys.Date(), '_summary.xlsx'),
            content = function(file) {
                sheets <- list(table())
                names(sheets) <- t('tab.summary')
                if (!is.null(tables()$dilution)) sheets[[t('quality.section_dilution')]] <- tables()$dilution
                if (!is.null(tables()$qc)) sheets[[t('quality.section_qc')]] <- tables()$qc
                openxlsx::write.xlsx(sheets, file, rowNames = FALSE, overwrite = TRUE)
            }
        )

        list(table = table, assessment = assessment, warnings = warnings)
    })
}
