#' Template Step Module
#'
#' Step 1 of the analysis. The sidebar holds the parameters of the plate
#' layout and the download of the template (a plate maps workbook, see
#' [plate_maps_format], or a long table); the main area shows the
#' concentrations of the standard and the plate layout, which can be edited by
#' selecting wells and choosing their type.
#'
#' @param id Module id.
#' @param lang Language code.
#'
#' @return `mod_template_server()` returns a list of reactives: `layout` (the
#'   plate layout, a character matrix), `conc` (concentrations of the standard
#'   levels), `qc_conc` (nominal concentrations of the quality controls),
#'   `params` (all template parameters) and `by` (filling direction).
#' @name mod_template
#' @keywords internal
NULL

#' Group of Sidebar Inputs
#' @keywords internal
sidebar_group <- function(title, icon, ...) {
    shiny::div(class = 'sidebar-group',
               shiny::div(class = 'sidebar-group-title', shiny::icon(icon), ' ', title),
               ...)
}

#' Text with a Tooltip and an Info Icon
#' @keywords internal
with_tooltip <- function(text, tip, placement = 'right') {
    bslib::tooltip(shiny::span(text, ' ', shiny::icon('circle-info', class = 'text-muted')),
                   tip, placement = placement)
}

#' @rdname mod_template
mod_template_ui <- function(id, lang) {
    ns <- shiny::NS(id)
    t <- function(key) tr(key, lang)
    choices <- function(values, keys) stats::setNames(values, vapply(keys, t, ''))

    sidebar <- bslib::sidebar(
        width = 330,
        sidebar_group(
            t('template.plate'), 'table-cells',
            shiny::radioButtons(ns('size'), t('template.plate_size'), choices = c(96, 384),
                                selected = 96, inline = TRUE),
            shiny::radioButtons(ns('by'), t('template.by'),
                                choices = choices(c('column', 'row'), c('template.by_column', 'template.by_row')),
                                selected = 'column', inline = TRUE)
        ),
        sidebar_group(
            t('template.standards'), 'vial',
            shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns('start'), t('template.start'),
                                                     value = 100, min = 0, step = 10)),
                shiny::column(6, shiny::numericInput(ns('step'), t('template.step'),
                                                     value = 2, min = 1.1, step = 1))
            ),
            shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns('levels'), t('template.levels'),
                                                     value = 7, min = 3, step = 1)),
                shiny::column(6, shiny::numericInput(ns('replicates'), t('template.replicates'),
                                                     value = 2, min = 1, step = 1))
            ),
            shiny::radioButtons(ns('position'), t('template.position'),
                                choices = choices(c('before', 'after'),
                                                  c('template.position_before', 'template.position_after')),
                                selected = 'before', inline = TRUE)
        ),
        sidebar_group(
            t('template.samples_title'), 'flask',
            shiny::numericInput(ns('samples'), t('template.samples'), value = 10, min = 0, step = 1)
        ),
        sidebar_group(
            with_tooltip(t('template.qc_title'), t('template.qc_help')), 'clipboard-check',
            shiny::numericInput(ns('qc_levels'), t('template.qc_levels'), value = 0, min = 0, max = 9, step = 1),
            shiny::uiOutput(ns('qc_inputs'))
        ),
        sidebar_group(
            t('template.export'), 'file-excel',
            shiny::radioButtons(ns('export'), NULL,
                                choiceNames = list(
                                    with_tooltip(t('template.export_maps'), t('template.export_help_maps')),
                                    with_tooltip(t('template.export_long'), t('template.export_help_long'))
                                ),
                                choiceValues = c('maps', 'long'),
                                selected = 'maps'),
            shiny::downloadButton(ns('download'), t('template.download'),
                                  icon = shiny::icon('download'), class = 'btn-primary w-100')
        )
    )

    button <- function(action, key, icon) {
        shiny::tags$button(type = 'button', class = paste0('btn btn-sm btn-well-', action),
                           `data-action` = action, disabled = NA, shiny::icon(icon), ' ', t(key))
    }

    main <- bslib::card(
        full_screen = TRUE,
        bslib::card_header(t('template.preview')),
        bslib::card_body(
            shiny::div(
                class = 'step-view',
                shiny::h5(t('template.standards')),
                shiny::tableOutput(ns('standards')),
                shiny::h5(with_tooltip(t('template.layout'), t('template.edit_hint'), placement = 'bottom')),
                shiny::div(
                    class = 'plate-editor', `data-input` = ns('edit'),
                    shiny::div(
                        class = 'plate-toolbar',
                        button('blank', 'template.edit_blank', 'square'),
                        button('sample', 'template.edit_sample', 'flask'),
                        shiny::uiOutput(ns('qc_tool')),
                        button('empty', 'template.edit_empty', 'eraser'),
                        shiny::span(class = 'plate-selection-count text-muted',
                                    `data-template` = t('template.selected'),
                                    tr('template.selected', lang, n = 0)),
                        shiny::actionButton(ns('reset'), t('template.edit_reset'),
                                            icon = shiny::icon('rotate-left'),
                                            class = 'btn-sm btn-outline-secondary ms-auto')
                    ),
                    shiny::uiOutput(ns('plate')),
                    shiny::uiOutput(ns('shift'))
                ),
                shiny::uiOutput(ns('qc_preview'))
            )
        )
    )

    bslib::layout_sidebar(sidebar = sidebar, main)
}

#' @rdname mod_template
mod_template_server <- function(id, lang) {
    shiny::moduleServer(id, function(input, output, session) {
        notify_error <- function(e) {
            shiny::showNotification(condition_text(e, lang), type = 'error')
            shiny::req(FALSE)
        }
        or <- function(x, default) if (is.null(x)) default else x

        params <- shiny::reactive({
            shiny::req(is.numeric(input$start), is.numeric(input$step), is.numeric(input$levels),
                       is.numeric(input$replicates), is.numeric(input$samples))
            list(size = as.numeric(or(input$size, 96)),
                 by = or(input$by, 'column'),
                 position = or(input$position, 'before'),
                 start = input$start, step = input$step, levels = input$levels,
                 replicates = input$replicates, samples = input$samples)
        })

        conc <- shiny::reactive({
            p <- params()
            with_validation(standard_series(p$start, p$step, p$levels), 'template.preview_error', lang)
        })

        # Plate layout: generated from the parameters, then edited by the user --
        generated <- shiny::reactive({
            p <- params()
            tryCatch(list(grid = plate_layout_codes(p$size, p$levels, p$replicates, p$samples,
                                                    by = p$by, standards = p$position),
                          error = NULL),
                     error = function(e) list(grid = NULL, error = e))
        })
        edited <- shiny::reactiveVal(NULL)
        # New structure of the layout discards the edits; the number of samples does not
        shiny::observeEvent(list(input$size, input$by, input$position, input$levels, input$replicates),
                            edited(NULL), ignoreInit = TRUE)
        shiny::observeEvent(input$reset, edited(NULL))

        layout <- shiny::reactive({
            if (!is.null(edited())) return(edited())
            g <- generated()
            if (!is.null(g$error)) {
                shiny::validate(shiny::need(FALSE, tr('template.preview_error', lang,
                                                      message = condition_text(g$error, lang))))
            }
            g$grid
        })

        # The number of samples follows the edits ...
        set_layout <- function(grid) {
            edited(grid)
            n <- count_samples(grid)
            if (!identical(as.numeric(n), as.numeric(input$samples))) {
                shiny::updateNumericInput(session, 'samples', value = n)
            }
        }

        shiny::observeEvent(input$edit, {
            e <- input$edit
            shiny::req(e$action %in% c('blank', 'qc', 'sample', 'empty'), length(e$wells) > 0)
            level <- suppressWarnings(as.integer(or(e$level, NA)))
            if (e$action == 'qc') shiny::req(!is.na(level), level >= 1, level <= qc_n())
            set_layout(edit_layout(layout(), unlist(e$wells), e$action, by = params()$by, qc_level = level))
        })

        # Quality controls: levels QC1...QCn with nominal concentrations -------
        qc_n <- shiny::reactive({
            n <- input$qc_levels
            if (!is.numeric(n) || length(n) != 1 || is.na(n)) 0L else as.integer(max(0, min(9, n)))
        })
        qc_conc <- shiny::reactive({
            vapply(seq_len(qc_n()), function(k) {
                v <- input[[paste0('qc_conc_', k)]]
                if (is.numeric(v) && length(v) == 1 && !is.na(v) && v > 0) v else NA_real_
            }, 0)
        })
        # Fewer levels remove the wells of the other levels
        shiny::observeEvent(qc_n(), {
            grid <- edited()
            if (!is.null(grid)) edited(trim_qc(grid, qc_n()))
        })

        output$qc_inputs <- shiny::renderUI({
            n <- qc_n()
            if (n == 0) return(NULL)
            inputs <- lapply(seq_len(n), function(k) {
                id <- session$ns(paste0('qc_conc_', k))
                shiny::div(
                    class = 'qc-conc',
                    shiny::tags$label(`for` = id, paste0('QC', k, ':')),
                    shiny::numericInput(id, NULL, value = shiny::isolate(input[[paste0('qc_conc_', k)]]), min = 0)
                )
            })
            shiny::tagList(shiny::tags$label(class = 'control-label', tr('template.qc_nominal', lang)), inputs)
        })

        output$qc_tool <- shiny::renderUI({
            n <- qc_n()
            if (n == 0) return(NULL)
            shiny::tagList(
                shiny::tags$button(type = 'button', class = 'btn btn-sm btn-well-qc', `data-action` = 'qc',
                                   disabled = NA, shiny::icon('clipboard-check'), ' ', tr('template.edit_qc', lang)),
                shiny::tags$select(class = 'form-select form-select-sm plate-qc-level', `data-qc-level` = NA,
                                   `aria-label` = tr('template.qc_title', lang),
                                   lapply(seq_len(n), function(k) shiny::tags$option(value = k, paste0('QC', k))))
            )
        })

        output$qc_preview <- shiny::renderUI({
            if (qc_n() == 0) return(NULL)
            shiny::tagList(shiny::h5(tr('template.qc_title', lang)), shiny::tableOutput(session$ns('qc_table')))
        })
        output$qc_table <- shiny::renderTable({
            values <- vapply(qc_conc(), function(x) if (is.na(x)) 'X' else format(signif(x, 4)), '')
            stats::setNames(as.data.frame(as.list(values)), paste0('QC', seq_along(values)))
        })

        # ... and the edited layout follows the number of samples
        shiny::observeEvent(input$samples, ignoreInit = TRUE, {
            grid <- edited()
            n <- input$samples
            if (is.null(grid) || !is.numeric(n) || is.na(n) || n < 0 || count_samples(grid) == n) return()
            resized <- tryCatch(resize_samples(grid, n, by = params()$by), error = function(e) {
                shiny::showNotification(condition_text(e, lang), type = 'error')
                NULL
            })
            if (is.null(resized)) {
                shiny::updateNumericInput(session, 'samples', value = count_samples(grid))
            } else {
                edited(resized)
            }
        })

        # Shift of the whole layout along the filling direction
        directions <- shiny::reactive(if (params()$by == 'column') c('left', 'right') else c('up', 'down'))

        output$shift <- shiny::renderUI({
            grid <- layout()
            icons <- c(left = 'arrow-left', right = 'arrow-right', up = 'arrow-up', down = 'arrow-down')
            buttons <- lapply(directions(), function(d) {
                b <- shiny::actionButton(session$ns(paste0('shift_', d)), tr(paste0('template.shift_', d), lang),
                                         icon = shiny::icon(icons[[d]]), class = 'btn-sm btn-outline-secondary')
                if (is.null(shift_layout(grid, d))) b <- shiny::tagAppendAttributes(b, disabled = NA)
                b
            })
            shiny::div(class = 'plate-shift', shiny::span(class = 'text-muted', tr('template.shift', lang)), buttons)
        })

        for (d in c('left', 'right', 'up', 'down')) {
            local({
                direction <- d
                shiny::observeEvent(input[[paste0('shift_', direction)]], {
                    shifted <- shift_layout(layout(), direction)
                    if (!is.null(shifted)) edited(shifted)
                })
            })
        }

        # Download ---------------------------------------------------------------
        export <- shiny::reactive(or(input$export, 'maps'))

        output$download <- shiny::downloadHandler(
            filename = function() {
                paste0(Sys.Date(), if (export() == 'maps') '_plate_maps.xlsx' else '_long_table.xlsx')
            },
            content = function(file) {
                tryCatch({
                    if (export() == 'maps') {
                        make_plate_maps(file, layout(), conc(), qc_conc = qc_conc())
                    } else {
                        save_xlsx(make_long_table(layout(), conc(), by = params()$by, qc_conc = qc_conc()), file)
                    }
                }, error = notify_error)
            }
        )

        # Preview ---------------------------------------------------------------
        output$standards <- shiny::renderTable({
            values <- vapply(conc(), function(x) format(signif(x, 4)), '')
            stats::setNames(as.data.frame(as.list(values)), paste0('S', seq_along(values)))
        })

        output$plate <- shiny::renderUI(plate_map(layout(), lang = lang, editable = TRUE))

        list(layout = layout, conc = conc, qc_conc = qc_conc, params = params,
             by = shiny::reactive(or(input$by, 'column')))
    })
}
