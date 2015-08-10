library(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t')),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'")),
      selectInput("list_type", "List Type", choices=c("","Leads", "Opportunities")),
      uiOutput("getList"),
      selectInput("score_name", "Score Variable", 
                  choices=c("ssv3.2", "ssv3.12", "ssv3.32",
                            "ssv3.100001", "ssv3.100002", "ssv3.100012", "ssv3.100032",
                            "msv1.700001", "msv1.700002")),
      #textInput("conversion", "Conversion Variable", "isCon"),
      textInput("created_date", "Date Variable", "cre"),
      dateInput("date_constraint", "Date Constraint", value = "2015-01-01", format = "yyyy-mm-dd"),
      selectInput("type", "Type",
                  choices=c("Decile","Quartile","Score","Grade"))
      
    ),
    mainPanel(
      tableOutput('dataframe'),
      plotOutput('conversion_graph'),
      plotOutput('pie_chart'),
      plotOutput('show_all')
    )
  )
))