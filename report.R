
library(officer)

# Styles
date_text_style <- fp_text(font.size = 11, underlined = TRUE, font.family = 'Calibri')
date_par_style <- fp_par(padding.bottom = 15)

title_text_style <- fp_text(font.size = 13, bold = TRUE, font.family = 'Calibri')
title_par_style <- fp_par(text.align = 'center', padding.bottom = 10, padding.top = 10)

section_text_style <- fp_text(font.size = 12, bold = TRUE, italic = FALSE, font.family = 'Calibri')
section_par_style <- fp_par(padding.bottom = 10, padding.top = 10)

norm_text <- fp_text(font.size = 12, bold = FALSE, font.family = 'Calibri')
bold_text <- fp_text(font.size = 12, bold = TRUE, font.family = 'Calibri')

# Plot theme
report_theme <-
    theme(axis.title = element_text(size = 9, color = "black"),
          axis.text = element_text(size = 8, color = "black"))

# Save report function
save_report <- function(file,
                        date,
                        title,
                        plot,
                        method = 'test',
                        model_table,
                        result_table,
                        means_table = NULL) {
    
    # Date, title and files
    date <- format(as.Date(date), '%d.%m.%Y')
    date_text <- fpar(ftext(date, date_text_style), fp_p = date_par_style)
    title_text <- fpar(ftext(title, title_text_style), fp_p = title_par_style)
    
    calib_text <- fpar(ftext('Standard curve', section_text_style), fp_p = section_par_style)
    result_text <- fpar(ftext('Result', section_text_style), fp_p = section_par_style)
    means_text <- fpar(ftext('Means', section_text_style), fp_p = section_par_style)
    
    # Standard plot
    calib_method_text_title <- ftext('Calibration method: ', norm_text)
    calib_method_text <- ftext(method, bold_text)
    calib_method_line <- fpar(calib_method_text_title, calib_method_text)
    
    report <- read_docx() %>%
        body_add_fpar(date_text) %>%
        body_add_fpar(title_text) %>%
        #
        body_add_fpar(calib_text) %>%
        body_add_gg(plot, width = 6.5, height = 3) %>%
        body_add_fpar(calib_method_line) %>%
        # Add visual space after the calibration method line
        body_add_par("", style = "Normal") %>%
        #
        body_add_table(model_table, style = 'table_template') %>%
        #
        body_add_fpar(result_text) %>%
        body_add_table(result_table, style = 'table_template')
    
    if (!is.null(means_table)) {
        report <- 
            report %>% 
            body_add_fpar(means_text) %>%
            body_add_table(means_table, style = 'table_template')
    }
    # 
    print(report, target = file)
}
