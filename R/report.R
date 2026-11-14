#' Save the Word Report
#'
#' @param file Output `.docx` path.
#' @param date Analysis date (`"YYYY-MM-DD"`).
#' @param title Experiment title.
#' @param plot Plot from [get_plots()].
#' @param method Calibration method (text shown in the report).
#' @param scale Scale of the fit (text shown in the report).
#' @param weights Weighting (text shown in the report).
#' @param exclusions Optional table of standard points excluded by the user.
#' @param comparison Optional table comparing the calibration models.
#' @param model_table Coefficients table from [get_coef_ci()].
#' @param result_table Sample results.
#' @param means_table Optional aggregated results.
#' @param qc_table Optional quality control results.
#' @param notes Character vector of warnings.
#' @param lang Language of the report headings.
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
                        notes = character(0),
                        lang = 'en') {
    date_text_style <- officer::fp_text(font.size = 11, underlined = TRUE, font.family = 'Calibri')
    date_par_style <- officer::fp_par(padding.bottom = 15)
    title_text_style <- officer::fp_text(font.size = 13, bold = TRUE, font.family = 'Calibri')
    title_par_style <- officer::fp_par(text.align = 'center', padding.bottom = 10, padding.top = 10)
    section_text_style <- officer::fp_text(font.size = 12, bold = TRUE, font.family = 'Calibri')
    section_par_style <- officer::fp_par(padding.bottom = 10, padding.top = 10)
    norm_text <- officer::fp_text(font.size = 12, font.family = 'Calibri')
    bold_text <- officer::fp_text(font.size = 12, bold = TRUE, font.family = 'Calibri')

    section <- function(key) {
        officer::fpar(officer::ftext(tr(key, lang), section_text_style), fp_p = section_par_style)
    }
    labelled_line <- function(key, value) {
        officer::fpar(officer::ftext(tr(key, lang), norm_text),
                      officer::ftext(if (is.null(value)) '' else value, bold_text))
    }

    date <- format(as.Date(date), '%d.%m.%Y')

    report <- officer::read_docx()
    report <- officer::body_add_fpar(report, officer::fpar(officer::ftext(date, date_text_style),
                                                           fp_p = date_par_style))
    report <- officer::body_add_fpar(report, officer::fpar(officer::ftext(title, title_text_style),
                                                           fp_p = title_par_style))
    report <- officer::body_add_fpar(report, section('report.section_curve'))
    report <- officer::body_add_gg(report, plot, width = 6.5, height = 3)
    report <- officer::body_add_fpar(report, labelled_line('report.method', method))
    report <- officer::body_add_fpar(report, labelled_line('report.scale', scale))
    if (!is.null(weights)) {
        report <- officer::body_add_fpar(report, labelled_line('report.weights', weights))
    }
    report <- officer::body_add_par(report, '', style = 'Normal')
    report <- officer::body_add_table(report, model_table, style = 'table_template')

    if (!is.null(exclusions) && nrow(exclusions) > 0) {
        report <- officer::body_add_fpar(report, section('report.section_exclusions'))
        report <- officer::body_add_table(report, exclusions, style = 'table_template')
    }
    if (!is.null(comparison)) {
        report <- officer::body_add_fpar(report, section('report.section_models'))
        report <- officer::body_add_table(report, comparison, style = 'table_template')
    }
    report <- officer::body_add_fpar(report, section('report.section_result'))
    report <- officer::body_add_table(report, result_table, style = 'table_template')

    if (!is.null(qc_table)) {
        report <- officer::body_add_fpar(report, section('report.section_qc'))
        report <- officer::body_add_table(report, qc_table, style = 'table_template')
    }

    if (length(notes) > 0) {
        report <- officer::body_add_fpar(report, section('report.section_warnings'))
        for (note in notes) {
            report <- officer::body_add_par(report, paste('-', note), style = 'Normal')
        }
    }

    if (!is.null(means_table)) {
        report <- officer::body_add_fpar(report, section('report.section_means'))
        report <- officer::body_add_table(report, means_table, style = 'table_template')
    }

    print(report, target = file)
    invisible(file)
}
