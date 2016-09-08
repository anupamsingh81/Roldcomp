library(shiny)
library(car)

ui <- fluidPage(

    ui = pageWithSidebar(
      headerPanel('Analysis of Variance'),
      sidebarPanel(
        fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        checkboxInput("header", "Header", TRUE),
        radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
        selectInput('type', 'Please select Sums of Squares type', 
                    c(I = 'type1', II = 'type2', III = 'type3'), 'type1')
        ,uiOutput('var') 
      )
      , mainPanel(    
        h3('ANOVA Table'),
        tableOutput('aovSummary')
      )
    )
    ,