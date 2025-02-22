
library(shiny)

source('./linearity.R')
source('./report.R')


ui <- fluidPage(
    
    title = 'Linearity App',
    
    titlePanel(title = h1('Linearity App', align = 'center')),
    
    sidebarLayout(
        
        # SIDEBAR    
        sidebarPanel(
            
            # MAKE TEMPLATE SECTION
            wellPanel(
                h4('Make template'),
                
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
                
                fluidRow(
                    column(width = 6,
                           numericInput('standard.n',
                                        label = 'Standard col',
                                        value = 2,
                                        min = 1, step = 1)),
                    column(width = 6,
                           numericInput('samples.n',
                                        label = 'N samples',
                                        value = 10,
                                        min = 1, step = 1))),
                
                fluidRow(
                    column(width = 8,
                           selectInput('template_filetype',
                                       label = 'File type',
                                       choices = c('Excel' = 'xlsx',
                                                   'CSV' = 'csv'),
                                       selected = 'Excel',
                                       width = '100%')
                    ),
                    column(width = 4,
                           conditionalPanel(condition = "input.template_filetype == 'csv'",
                                            selectInput('csv_dialect',
                                                        label = 'Sep',
                                                        choices = c(',' = 'comma',
                                                                    ';' = 'semicolon'),
                                                        selected = ',.', width = '100%'))
                    )),
                
                downloadButton('download_template', 'Download Template', icon = icon('file-excel')),
                
            ),
            
            # IMPORT DATA PANEL
            wellPanel(
                
                h4('Import data'),
                fileInput('import_data', label = 'Choose file:', accept = c('.csv', '.xlsx'))
                
            ),
            
            fluidRow(
                column(width = 12,
                selectInput('round_fun_select',
                            label = 'Rounding function',
                            choices = c('Significant' = 'signif',
                                        'Round' = 'round'),
                            selected = 'Significant', width = 200))
            ),
            
            fluidRow(
                column(width = 6,
                numericInput('round_par',
                             label = 'Number',
                             value = 3, step = 1, min = 1, max = 7))    
            ),
            
            width = 3),
        
        
        # MAIN PANEL
        mainPanel(
            
            navbarPage('Steps:', id = 'tabs',
                       
                       tabPanel('Imported data',
                                
                                tableOutput('imported_table')
                                
                       ),
                       
                       tabPanel('Standard curve',
                                
                                plotOutput('two_plots', width = '800px'),
                                br(),
                                
                                tableOutput('coefs_table'),
                                
                                radioButtons('model',
                                             'Choose regression model',
                                             choices = c('Linear regression model' = 'LRM',
                                                         'Quadratic regression model' = 'QRM',
                                                         'Deming regression' = 'deming'),
                                             selected = 'LRM'),
                                br(),
                                
                                fluidRow(
                                    column(4,
                                           textInput('xtitle',
                                                     'X-axis title:',
                                                     value = 'Concentration',
                                                     width = '100%')),
                                    column(2,
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
                                    column(4,
                                           textInput('ytitle',
                                                     'Y-axis title:',
                                                     value = 'Optical density',
                                                     width = '100%'))),
                                
                                downloadButton('save_plot', 'Save plot')
                                
                       ),
                       
                       tabPanel('Results',
                                
                                tableOutput('result_table'),
                                
                                downloadButton('download_results', 'Download results', icon = icon('file-excel')),
                                
                       ),
                       
                       tabPanel('Means',
                                
                                selectizeInput('aggr_par',
                                               label = 'Select aggregation parameters:',
                                               choices = c('sample.name', 'sample.date', 'dilution'),
                                               selected = 'sample.name',
                                               multiple = TRUE),
                                
                                tableOutput('means_table'),
                                
                                downloadButton('download_means', 'Download means', icon = icon('file-excel')),
                       ),
                       
                       tabPanel('Report',
                                
                                textInput('title', 'Title:', placeholder = 'Experiment name',
                                          width = '100%'),
                                
                                downloadButton('download_report', label = 'Downoload report', icon = icon('file-word'))
                       )
                       
            ),
            width = 9)
    )
)

server <- function(input, output, session) {
    
    ### Sidebar Panel ####
    
    output$download_template <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_raw.data.', input$template_filetype)
        },
        content = function(file) {
            
            template <- make_template(start = input$standard.start,
                                      step = input$standard.step,
                                      n.standard = input$standard.n,
                                      n.sample = input$samples.n)
            
            if (input$template_filetype == 'csv') {
                save_csv(template, file, input$csv_dialect)
            } else {
                write.xlsx(template, file, row.names = FALSE)
            }
        }
    )
    
    observeEvent(input$round_fun_select, {
        if (input$round_fun_select == 'round') {
            updateNumericInput(session, 'round_par', value = 2, min = -7, max = 7)
        } else {
            updateNumericInput(session, 'round_par', value = 3, min = 1, max = 7)
        }
    })
    
    
    ### Imported Data Tab ###
    
    import_data_table <- reactive({
        data_file <- input$import_data
        req(data_file)
        read_table(data_file$datapath) %>% check_input_file()
    })
    
    output$imported_table <- renderTable({
        import_data_table()
    }, digits = 4)
    
    
    
    ### standard Curve ###
    
    r_fun <- list('signif' = signif, 'round' = round)
    
    result <- reactive({
        
        calculate(data = import_data_table(),
                  model = input$model,
                  round_fun = r_fun[[input$round_fun_select]],
                  round_par = input$round_par)
    })
    
    plots <- reactive({
        get_plots(result = result(),
                  xtitle = paste0(input$xtitle, input$xunits),
                  ytitle = input$ytitle)
    })
    
    output$two_plots <- renderPlot({
        plots()
    })
    
    coef_table <- reactive({
        get_coef_ci(result()$model_direct)
    })
    
    output$coefs_table <- renderTable({
        coef_table()
    })
    
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
    
    
    ### Results ###
    
    
    output$result_table <- renderTable({
        result()$result
    }, digits = 4)
    
    
    output$download_results <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_result_table.', input$template_filetype)
        },
        content = function(file) {
            rtab <- result()$result %>% select(-OD)
            write.xlsx(rtab, file, row.names = FALSE)
        }
    )
    
    
    
    ### Means ###
    
    observe({
        column_names <-
            colnames(result()$resul) %>% 
            setdiff(., c('type', 'conc', 'OD'))
        
        updateSelectizeInput(session, 'aggr_par',
                             choices = column_names,
                             selected = 'sample.name')
    })
    
    means_table <- reactive({
        aggregate_result(result = result()$result,
                         aggr.pars = input$aggr_par,
                         round_fun = r_fun[[input$round_fun_select]],
                         round_par = input$round_par)
    })
    
    output$means_table <- renderTable({
        means_table()
    }, digits = 4)
    
    output$download_means <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_means.', input$template_filetype)
        },
        content = function(file) {
            write.xlsx(means_table(), file, row.names = FALSE)
        }
    )
    
    
    
    ### Report ###
    
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
                                         plot.theme = report_theme),
                        method = switch(input$model,
                                        'LRM' = 'Linear regression model',
                                        'QRM' = 'Quadric regression model',
                                        'deming' = 'Deming regression'),
                        model_table = coef_table(),
                        result_table = result()$result,
                        means_table = means_table())
        }
    )
    
    
}

shinyApp(ui, server)