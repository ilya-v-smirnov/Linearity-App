#' Content of the Report
#'
#' The report is a list of blocks; the same blocks are written to Word
#' ([write_report()]) and shown as a preview in the app ([report_preview()]).
#'
#' * `short`: the standard curve (plots, model and its parameters), the
#'   sample results and the means;
#' * `detailed`: the analysis parameters, the standard curve with the
#'   comparison of models, the excluded points and their effect on the
#'   calibration, the full quality assessment, the results with the quality
#'   controls and the means, and the warnings.
#'
#' With excluded points, the model and its parameters are those of the
#' calibration refitted without them, as accepted by the user; a line says so.
#'
#' The head of the report has the date, the title and, when given, the
#' operator and the plate.
#'
#' @inheritParams save_report
#' @param type `"short"` or `"detailed"`.
#'
#' @return A list of blocks, each a list with `type` (`date`, `title`,
#'   `heading`, `subheading`, `line`, `strong`, `text`, `list`, `plot`,
#'   `table`) and its content.
#' @keywords internal
report_blocks <- function(type = c('short', 'detailed'), date, title, method, scale = NULL, weights = NULL,
                          model_table, result_table, means_table = NULL, qc_table = NULL,
                          exclusions = NULL, comparison = NULL, quality = NULL,
                          notes = character(0), lang = 'en', exclusion_effect = NULL,
                          operator = NULL, plate = NULL) {
    type <- match.arg(type)
    t <- function(key) tr(key, lang)
    b <- function(type, ...) list(type = type, ...)
    has_rows <- function(x) !is.null(x) && nrow(x) > 0
    table <- function(x, caption = NULL) if (has_rows(x)) list(b('table', data = x, caption = caption))

    given <- function(x) length(x) == 1 && !is.na(x) && nzchar(trimws(x))
    head <- c(list(b('date', text = format(as.Date(date), '%d.%m.%Y')),
                   b('title', text = if (is.null(title)) '' else title)),
              if (given(operator)) list(b('line', label = t('report.operator_label'), value = trimws(operator))),
              if (given(plate)) list(b('line', label = t('report.plate_label'), value = trimws(plate))))
    method_lines <- c(list(b('line', label = t('report.method'), value = method)),
                      if (!is.null(scale)) list(b('line', label = t('report.scale'), value = scale)),
                      if (!is.null(weights)) list(b('line', label = t('report.weights'), value = weights)),
                      if (has_rows(exclusions)) {
                          list(b('text', text = tr('report.refit_note', lang, n = nrow(exclusions))))
                      })

    if (type == 'short') {
        return(c(head,
                 list(b('heading', text = t('report.section_curve')), b('plot')),
                 method_lines,
                 table(model_table),
                 list(b('heading', text = t('report.section_result'))), table(result_table),
                 if (has_rows(means_table)) c(list(b('heading', text = t('report.section_means'))),
                                              table(means_table))))
    }

    quality_part <- NULL
    if (!is.null(quality)) {
        s <- quality$summary
        sub <- function(key, x) if (has_rows(x)) c(list(b('subheading', text = t(key))), table(x))
        quality_part <- c(list(b('heading', text = t('report.section_quality')),
                               b('strong', text = s$title)),
                          lapply(c(s$range, s$levels, s$reasons, s$warnings), function(x) b('text', text = x)),
                          sub('quality.section_standards', quality$tables$standards),
                          sub('quality.section_curve', quality$tables$curve),
                          sub('quality.section_qc', quality$tables$qc),
                          sub('quality.section_samples', quality$tables$samples),
                          sub('quality.section_dilution', quality$tables$dilution))
    }

    c(head,
      list(b('heading', text = t('report.section_parameters'))), method_lines,
      list(b('heading', text = t('report.section_curve')), b('plot'),
           b('subheading', text = t('curve.coefs'))),
      table(model_table),
      if (has_rows(comparison)) {
          c(list(b('subheading', text = t('report.section_models'))), table(comparison),
            if (!is.null(attr(comparison, 'note'))) list(b('text', text = attr(comparison, 'note'))))
      },
      if (has_rows(exclusions)) c(list(b('subheading', text = t('report.section_exclusions'))), table(exclusions)),
      if (!is.null(exclusion_effect)) {
          c(list(b('subheading', text = t('effect.title'))),
            list(b('strong', text = t('effect.section_coefs'))), table(exclusion_effect$coefs),
            list(b('strong', text = t('effect.section_summary'))), table(exclusion_effect$summary),
            list(b('strong', text = t('effect.section_back_calc'))), table(exclusion_effect$back_calc))
      },
      quality_part,
      list(b('heading', text = t('report.section_result'))), table(result_table),
      if (has_rows(qc_table)) c(list(b('subheading', text = t('report.section_qc'))), table(qc_table)),
      if (has_rows(means_table)) c(list(b('subheading', text = t('report.section_means'))), table(means_table)),
      if (length(notes) > 0) list(b('heading', text = t('report.section_warnings')), b('list', items = notes)))
}


#' Write the Report to Word
#'
#' @param blocks Output of [report_blocks()].
#' @param file Output `.docx` path.
#' @param plot Plot from [get_plots()] for the `plot` block.
#'
#' @return The path, invisibly.
#' @keywords internal
write_report <- function(blocks, file, plot) {
    font <- function(size, ...) officer::fp_text(font.size = size, font.family = 'Calibri', ...)
    styles <- list(
        date = list(text = font(11, underlined = TRUE), par = officer::fp_par(padding.bottom = 15)),
        title = list(text = font(13, bold = TRUE),
                     par = officer::fp_par(text.align = 'center', padding.bottom = 10, padding.top = 10)),
        heading = list(text = font(12, bold = TRUE), par = officer::fp_par(padding.bottom = 8, padding.top = 14)),
        subheading = list(text = font(11, bold = TRUE, italic = TRUE),
                          par = officer::fp_par(padding.bottom = 6, padding.top = 8))
    )
    norm_text <- font(12)
    bold_text <- font(12, bold = TRUE)
    styled <- function(style, text) {
        officer::fpar(officer::ftext(text, styles[[style]]$text), fp_p = styles[[style]]$par)
    }

    report <- officer::read_docx()
    for (block in blocks) {
        report <- switch(
            block$type,
            date = , heading = , subheading = officer::body_add_fpar(report, styled(block$type, block$text)),
            title = {
                # Each line of a title written on several lines
                lines <- strsplit(block$text, '\r?\n')[[1]]
                if (length(lines) == 0) lines <- ''
                runs <- unlist(lapply(seq_along(lines), function(i) {
                    c(if (i > 1) list(officer::run_linebreak()), list(officer::ftext(lines[i], styles$title$text)))
                }), recursive = FALSE)
                officer::body_add_fpar(report, do.call(officer::fpar, c(runs, list(fp_p = styles$title$par))))
            },
            line = officer::body_add_fpar(report, officer::fpar(officer::ftext(block$label, norm_text),
                                                                officer::ftext(block$value, bold_text))),
            strong = officer::body_add_fpar(report, officer::fpar(officer::ftext(block$text, bold_text))),
            text = officer::body_add_par(report, block$text, style = 'Normal'),
            list = Reduce(function(r, item) officer::body_add_par(r, paste('-', item), style = 'Normal'),
                          block$items, report),
            plot = officer::body_add_gg(report, plot, width = 6.5, height = 3),
            table = officer::body_add_table(officer::body_add_par(report, '', style = 'Normal'),
                                            block$data, style = 'table_template'),
            report
        )
    }
    print(report, target = file)
    invisible(file)
}


#' Preview of the Report
#'
#' An HTML page with the blocks of the report; long tables are cut.
#'
#' @param blocks Output of [report_blocks()].
#' @param plot_output UI of the plot (e.g. a [shiny::plotOutput()]).
#' @param lang Language code.
#' @param max_rows Rows of a table shown in the preview.
#'
#' @return A shiny tag.
#' @keywords internal
report_preview <- function(blocks, plot_output, lang = 'en', max_rows = 8) {
    html_table <- function(data) {
        shown <- utils::head(data, max_rows)
        cell <- function(x) if (is.na(x)) '' else format(x)
        shiny::tagList(
            shiny::div(class = 'report-table-wrap', shiny::tags$table(
                class = 'table table-sm report-table',
                shiny::tags$thead(shiny::tags$tr(lapply(names(shown), shiny::tags$th))),
                shiny::tags$tbody(lapply(seq_len(nrow(shown)), function(i) {
                    shiny::tags$tr(lapply(shown[i, , drop = TRUE], function(x) shiny::tags$td(cell(x))))
                }))
            )),
            if (nrow(data) > max_rows) {
                shiny::p(class = 'report-more', tr('report.more_rows', lang, n = nrow(data) - max_rows))
            }
        )
    }
    parts <- lapply(blocks, function(block) {
        switch(block$type,
               date = shiny::p(class = 'report-date', block$text),
               title = shiny::div(class = 'report-title',
                                  lapply(strsplit(block$text, '\r?\n')[[1]], shiny::div)),
               heading = shiny::div(class = 'report-heading', block$text),
               subheading = shiny::div(class = 'report-subheading', block$text),
               line = shiny::p(block$label, shiny::strong(block$value)),
               strong = shiny::p(shiny::strong(block$text)),
               text = shiny::p(block$text),
               list = shiny::tags$ul(lapply(block$items, shiny::tags$li)),
               plot = plot_output,
               table = html_table(block$data),
               NULL)
    })
    shiny::div(class = 'report-page', parts)
}


#' Save the Word Report
#'
#' @param file Output `.docx` path.
#' @param date Analysis date (`"YYYY-MM-DD"`).
#' @param title Experiment title; it may have several lines.
#' @param operator,plate Optional operator and plate, shown under the title.
#' @param plot Plot from [get_plots()].
#' @param method Calibration method (text shown in the report).
#' @param scale Scale of the fit (text shown in the report).
#' @param weights Weighting (text shown in the report).
#' @param exclusions Optional table of standard points excluded by the user.
#' @param exclusion_effect Optional tables comparing the calibration on all
#'   standards with the refitted one (see [exclusion_effect_tables()]).
#' @param comparison Optional table comparing the calibration models.
#' @param quality Optional quality assessment: a list with `summary` (from
#'   `quality_summary()`) and `tables` (from [quality_tables()]).
#' @param model_table Coefficients table from [get_coef_ci()].
#' @param result_table Sample results.
#' @param means_table Optional aggregated results.
#' @param qc_table Optional quality control results.
#' @param notes Character vector of warnings.
#' @param lang Language of the report headings.
#' @param type `"detailed"` (default) or `"short"` (see [report_blocks()]).
#'
#' @return Called for its side effect.
#' @export
save_report <- function(file,
                        date,
                        title,
                        plot,
                        method,
                        scale = NULL,
                        weights = NULL,
                        model_table,
                        result_table,
                        means_table = NULL,
                        qc_table = NULL,
                        exclusions = NULL,
                        comparison = NULL,
                        quality = NULL,
                        notes = character(0),
                        lang = 'en',
                        type = 'detailed',
                        exclusion_effect = NULL,
                        operator = NULL,
                        plate = NULL) {
    blocks <- report_blocks(type, date = date, title = title, method = method, scale = scale, weights = weights,
                            model_table = model_table, result_table = result_table, means_table = means_table,
                            qc_table = qc_table, exclusions = exclusions, comparison = comparison,
                            quality = quality, notes = notes, lang = lang, exclusion_effect = exclusion_effect,
                            operator = operator, plate = plate)
    write_report(blocks, file, plot)
}
