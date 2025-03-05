
library(shiny)


# Load external functions for core computations and reporting
source('./linearity.R')
source('./report.R')


ui <- fluidPage(
    
    # Set the title of the Shiny app
    title = 'Linearity App',
    
    # Main title panel with centered heading
    titlePanel(title = h1('Linearity App', align = 'center')),
    
    sidebarLayout(
        
        # SIDEBAR: Contains controls for generating a template, importing data, and rounding options    
        sidebarPanel(
            
            # --- Template Parameters Section ---
            wellPanel(
                h4('Template Parameters'),
                
                # Input row for standard sample parameters
                fluidRow(
                    column(width = 6,
                           numericInput('standard.start',
                                        label = 'Start',
                                        value = 100,
                                        min = 0, step = 10)),
                    column(width = 6,
                           numericInput('standard.step',
                                        label = 'Step',
                                        value = 2,
                                        min = 2, step = 1))),
                
                # Input row for the number of standards and samples
                fluidRow(
                    column(width = 6,
                           numericInput('standard.n',
                                        label = 'Standard columns',
                                        value = 2,
                                        min = 1, step = 1)),
                    column(width = 6,
                           numericInput('samples.n',
                                        label = 'N samples',
                                        value = 10,
                                        min = 1, step = 1))),
                
                # File type selection and CSV-specific options (shown conditionally)
                fluidRow(
                    column(width = 8,
                           selectInput('template_filetype',
                                       label = 'File type',
                                       choices = c('Excel' = 'xlsx',
                                                   'CSV' = 'csv'),
                                       selected = 'xlsx',
                                       width = '100%')
                    ),
                    column(width = 4,
                           conditionalPanel(condition = "input.template_filetype == 'csv'",
                                            selectInput('csv_dialect',
                                                        label = 'Sep',
                                                        choices = c(',' = 'comma',
                                                                    ';' = 'semicolon'),
                                                        selected = 'comma', width = '100%'))
                    )),
                
                # Download button for template file generation
                downloadButton('download_template', 'Download Template', icon = icon('file-excel')),
                
            ),
            
            # --- Import Data Panel ---
            wellPanel(
                
                h4('Import Data'),
                fileInput('import_data', label = 'Choose file:', accept = c('.csv', '.xlsx'))
                
            ),
            
            # --- Rounding Options ---
            
            wellPanel(
                h4('Rounding Options'),
                fluidRow(
                    column(width = 7,
                           selectInput('round_fun_select',
                                       label = 'Rounding function',
                                       choices = c('Significant' = 'signif',
                                                   'Round' = 'round'),
                                       selected = 'signif')),
                    column(width = 5,
                           numericInput('round_par',
                                        label = 'Number',
                                        value = 3, step = 1, min = 1, max = 7))
                )
            ),
            
            width = 3),
        
        
        # MAIN PANEL: Contains tabbed outputs for data preview, plotting, results, means, and reporting
        mainPanel(
            
            navbarPage('Steps:', id = 'tabs',
                       
                       # Tab: Display imported data in a table format
                       tabPanel('Imported data',
                                tableOutput('imported_table')
                       ),
                       
                       # Tab: Standard Curve - plots, coefficient table, and model selection
                       tabPanel('Standard curve',
                                
                                plotOutput('two_plots', width = '800px'),
                                br(),
                                tableOutput('coefs_table'),
                                fluidRow(
                                    column(width = 4,
                                           radioButtons('model',
                                                        'Choose regression model',
                                                        choices = c('Linear regression model' = 'LRM',
                                                                    'Quadratic regression model' = 'QRM',
                                                                    'Deming regression' = 'deming'),
                                                        selected = 'LRM')),
                                    column(2,
                                           checkboxInput('linear_scale', label = 'Linear scales', value = FALSE), )
                                ),
                                
                                br(),
                                fluidRow(
                                    column(width = 4,
                                           textInput('xtitle',
                                                     'X-axis title:',
                                                     value = 'Concentration',
                                                     width = '100%')),
                                    column(width = 2,
                                           selectInput('xunits',
                                                       'X units:',
                                                       choices = c('',
                                                                   'None' = '',
                                                                   'pg/ml' = ', pg/ml',
                                                                   'ng/ml' = ', ng/ml',
                                                                   'ug/ml' = ', μg/ml',
                                                                   'mg/ml' = ', mg/ml',
                                                                   'ng/ul' = ', ng/μl',
                                                                   'nM'    = ', nM',
                                                                   'uM'    = ', μM',
                                                                   'mM'    = ', mM'),
                                                       selected = '',
                                                       width = '100%')),
                                    column(width = 4,
                                           textInput('ytitle',
                                                     'Y-axis title:',
                                                     value = 'Optical density',
                                                     width = '100%'))),
                                
                                downloadButton('save_plot', 'Save plot')
                                
                       ),
                       
                       # Tab: Results - shows the calculated results table and offers download functionality
                       tabPanel('Results',
                                tableOutput('result_table'),
                                fluidRow(
                                    column(width = 2,
                                           selectInput('result_format',
                                                       label = 'File type',
                                                       choices = c('Excel' = 'xlsx',
                                                                   'CSV' = 'csv'),
                                                       selected = 'Excel')),
                                    column(width = 1,
                                           conditionalPanel(condition = "input.result_format == 'csv'",
                                                            selectInput('res_csv_dialect',
                                                                        label = 'Sep',
                                                                        choices = c(',' = 'comma',
                                                                                    ';' = 'semicolon'),
                                                                        selected = 'comma', width = '100%'))
                                    )
                                ),
                                downloadButton('download_results', 'Download results', icon = icon('file-excel')),
                                br(),
                                br()
                       ),
                       
                       # Tab: Result Summary - allows aggregation of results with selectable grouping parameters
                       tabPanel('Result Summary',
                                selectizeInput('aggr_par',
                                               label = 'Select aggregation parameters:',
                                               choices = c('sample.name', 'sample.date', 'dilution'),
                                               selected = 'sample.name',
                                               multiple = TRUE),
                                tableOutput('means_table'),
                                fluidRow(
                                    column(width = 2,
                                           selectInput('result_summary_format',
                                                       label = 'File type',
                                                       choices = c('Excel' = 'xlsx',
                                                                   'CSV' = 'csv'),
                                                       selected = 'Excel')),
                                    column(width = 1,
                                           conditionalPanel(condition = "input.result_summary_format == 'csv'",
                                                            selectInput('rs_csv_dialect',
                                                                        label = 'Sep',
                                                                        choices = c(',' = 'comma',
                                                                                    ';' = 'semicolon'),
                                                                        selected = 'comma', width = '100%'))
                                           )
                                ),
                                downloadButton('download_means',
                                               label = 'Download Summary Table',
                                               icon = icon('file-excel'))
                                
                       ),
                       
                       # Tab: Report - generates a downloadable report with title and embedded plot
                       tabPanel('Report',
                                textInput('title', 'Title:', placeholder = 'Experiment name',
                                          width = '100%'),
                                downloadButton('download_report', label = 'Download report', icon = icon('file-word'))
                       ),
                       
                       tabPanel('User Manual',
                                
                                tags$iframe(style = "height:800px; width:100%;scrolling=yes",
                                            src = 'User Manual.pdf?raw=1', type="application/pdf"),
                                
                                icon = icon('file-pdf')
                       ),
                       
                       tabPanel('About',
                                
                                h4("This application was designed by Ilya Smirnov."), br(),
                                h4('The source code is published in my GitHub account:'),
                                h4(a('https://github.com/ilya-v-smirnov/Linearity-App', href = 'https://github.com/ilya-v-smirnov/Linearity-App')), br(),
                                h4('Please, report problems or suggestions for improvement by email:'),
                                h4(a('smirnov.iv.mail@gmail.com', href = 'mailto:smirnov.iv.mail@gmial.com')),
                                
                       )
            ),
            width = 9
        )
    )
)


server <- function(input, output, session) {
    
    # List mapping rounding function names to the actual functions
    r_fun <- list('signif' = signif, 'round' = round)
    
    ### Sidebar Panel Handlers ###
    
    output$download_template <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_raw.data.', input$template_filetype)
        },
        content = function(file) {
            # Generate template using user-specified parameters
            template <- make_template(start = input$standard.start,
                                      step = input$standard.step,
                                      n.standard = input$standard.n,
                                      n.sample = input$samples.n)
            
            # Save template as CSV or Excel based on file type selection
            if (input$template_filetype == 'csv') {
                save_csv(template, file, input$csv_dialect)
            } else {
                write.xlsx(template, file, row.names = FALSE)
            }
        }
    )
    
    # Observer to update the rounding parameter input based on the selected rounding function
    observeEvent(input$round_fun_select, {
        if (input$round_fun_select == 'round') {
            updateNumericInput(session, 'round_par', value = 2, min = -7, max = 7)
        } else {
            updateNumericInput(session, 'round_par', value = 3, min = 1, max = 7)
        }
    })
    
    
    ### Imported Data Tab ###
    
    # Reactive expression to read and validate the uploaded data file
    import_data_table <- reactive({
        data_file <- input$import_data
        req(data_file)  # Ensure a file is uploaded
        read_table(data_file$datapath) %>%
            check_input_file() %>% 
            format_input_table()
    })
    
    # Render the imported data as a table in the UI
    output$imported_table <- renderTable({
        import_data_table()
    }, digits = 4)
    
    
    ### standard Curve Tab ###
    
    
    # Reactive expression to perform calculations using the imported data
    result <- reactive({
        calculate(data = import_data_table(),
                  model = input$model,
                  linear = input$linear_scale,
                  round_fun = r_fun[[input$round_fun_select]],
                  round_par = input$round_par)
    })
    
    # Reactive expression to generate the combined plot (standard curve and residuals)
    plots <- reactive({
        get_plots(result = result(),
                  xtitle = paste0(input$xtitle, input$xunits),
                  ytitle = input$ytitle)
    })
    
    # Render the combined plot in the UI
    output$two_plots <- renderPlot({
        plots()
    })
    
    # Create a reactive expression to extract coefficients and confidence intervals
    coef_table <- reactive({
        get_coef_ci(result()$model_direct,
                    round_fun = r_fun[[input$round_fun_select]],
                    round_par = input$round_par)
    })
    
    # Render the coefficients table in the UI
    output$coefs_table <- renderTable({
        coef_table()
    })
    
    # Download handler to save the current plot as a JPEG image
    output$save_plot <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_plot.jpeg')
        },
        content = function(file) {
            ggsave(file,
                   plots(),
                   width = 20, height = 10, units = 'cm', dpi = 600)
        }
    )
    
    
    ### Results Tab ###
    
    # Render the results table in the UI
    output$result_table <- renderTable({
        result()$result
    }, digits = 4)
    
    # Download handler to save the result table as an Excel or CSV file
    output$download_results <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_result_table.', input$result_format)
        },
        content = function(file) {
            rtab <- result()$result
            # Save result summary table as CSV or Excel based on file type selection
            if (input$result_format == 'csv') {
                save_csv(rtab, file, input$res_csv_dialect)
            } else {
                write.xlsx(rtab, file, row.names = FALSE)
            }
        }
    )
    
    
    ### Result Summary Tab ###
    
    # Observer to update aggregation parameter choices based on the results columns
    observe({
        column_names <-
            colnames(result()$result) %>% 
            setdiff(., c('type', 'conc', 'OD'))
        
        updateSelectizeInput(session, 'aggr_par',
                             choices = column_names,
                             selected = 'sample.name')
    })
    
    # Reactive expression to calculate aggregated means based on user-selected grouping variables
    means_table <- reactive({
        aggregate_result(result = result()$result,
                         group_vars = input$aggr_par,
                         round_fun = r_fun[[input$round_fun_select]],
                         round_par = input$round_par)
    })
    
    # Render the aggregated means table in the UI
    output$means_table <- renderTable({
        means_table()
    }, digits = 4)
    
    # Download handler to save the aggregated means as an Excel or CSV file
    output$download_means <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_summary.', input$result_summary_format)
        },
        content = function(file) {
            # Save result summary table as CSV or Excel based on file type selection
            if (input$result_summary_format == 'csv') {
                save_csv(means_table(), file, input$rs_csv_dialect)
            } else {
                write.xlsx(means_table(), file, row.names = FALSE)
            }
        }
    )
    
    
    ### Report Tab ###
    
    # Download handler to generate and save a report document
    output$download_report <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_report.docx')
        },
        content = function(file) {
            save_report(file,
                        date = as.character(Sys.Date()),
                        title = input$title,
                        plot = get_plots(result(),
                                         xtitle = paste0(input$xtitle, input$xunits),
                                         ytitle = input$ytitle,
                                         plot.theme = report_theme), # report_theme is defined in report.R
                        method = switch(input$model,
                                        'LRM' = 'Linear regression model',
                                        'QRM' = 'Quadratic regression model',
                                        'deming' = 'Deming regression'),
                        model_table = coef_table(),
                        result_table = result()$result,
                        means_table = means_table())
        }
    )
    
    
}


# Launch the Shiny app
shinyApp(ui, server)
