source("grading-function.R")
library(shiny)
library(tidyverse)
library(reshape2)


ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Teammate Appraisal Scores"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    selectInput("var", "Select the stream:", 
                c("Whio" = "Whio",
                  "Ruru" = "Ruru",
                  "Hihi" = "Hihi",
                  "Kaka" = "Kaka")),
    
    checkboxInput("customise", "Input Your Values", F),
    conditionalPanel(
      condition = "input.customise == 1",
      numericInput(inputId = "group",
                   label = "Group Mark:",
                   value = 80,
                   min = 0, max = 100,
                   step = 1),
      sliderInput("i1", "Please select the individual score for student 1",
                  min = 1, max = 5, value = 1, step = 0.1
      ),
      sliderInput("i2", "Please select the individual score for student 2",
                  min = 1, max = 5, value = 2, step = 0.1
      ),
      sliderInput("i3", "Please select the individual score for student 3",
                  min = 1, max = 5, value = 3, step = 0.1
      ),
      sliderInput("i4", "Please select the individual score for student 4",
                  min = 1, max = 5, value = 4, step = 0.1
      ),
      sliderInput("i5", "Please select the individual score for student 5",
                  min = 1, max = 5, value = 5, step = 0.1
      )
    ),
      
  ),
  
  
  
  # Main panel for displaying outputs ----
  mainPanel(
    h2(textOutput("stream")),
    column(downloadButton("downloadData", "Download Data"), align = 'right', width = 12),
    plotOutput("Plot1", width = "100%", height = "350px"),
    br(),
    br(),
    br(),
    br(),
    column(tableOutput("sum1"), align = 'right', width = 8),
    
  )
)

server <- function(input, output) {
  
  stream <- reactive({
    paste0('Stream ', input$var)
  })
  
  output$stream <- renderText({
    stream()
  })
  
  output$Plot1 <- renderPlot({
    group = 100
    individual = c(1,2,3,4,5)
    
    if (input$customise == 1){
      group = input$group
      individual = c(input$i1,input$i2,input$i3,input$i4,input$i5)
      # print(individual)
    }
    
    data <- switch(input$var, 
                   "Whio" = calculate.grades(group, individual, 'Whio'),
                   "Kaka" = calculate.grades(group, individual, 'Kaka'),
                   "Hihi" = calculate.grades(group, individual, 'Hihi'),
                   "Ruru" = calculate.grades(group, individual, 'Ruru'))
    
    download = melt(data) %>%
      mutate('group_mark' = group,
             'individual_score' = rep(individual, length = nrow(melt(data))),
             'stream' = input$var) %>%
      select(-1) %>%
      rename('team' = 1, 'final_score' = 2) %>%
      select(stream, team, group_mark, individual_score, final_score)
    
    write_csv(download, 'download.csv')
    
    ggplot(melt(data)) + 
      geom_boxplot(aes(x = Var2, y = value), color = "#007bc2", pch = 19) + 
      geom_point(aes(x = Var2, y = value, color = factor(Var1)), pch = 17, size = 5) + 
      labs(title = paste0('The output of teammate appraisal function in stream ', input$var),
           x = paste0('Teams in ', input$var),
           y = 'Final mark') + 
      scale_color_discrete(name = paste0('Group Mark = ', group, "\nIndividual Score:"), labels = individual) + 
      scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
      guides(color = guide_legend(reverse = TRUE)) + 
      theme_bw() +
      theme(axis.text = element_text(face="bold", size = 13),
            axis.title = element_text(face="bold", size = 18),
            legend.text = element_text(face="bold", size = 13),
            legend.title = element_text(face="bold", size = 18),
            plot.title = element_text(face="bold", size = 20))
  })
  output$downloadData <- downloadHandler(
    
    filename = function() {
      individual = c(1,2,3,4,5)
      
      if (input$customise == 1){
        individual = c(input$i1,input$i2,input$i3,input$i4,input$i5)
      }
      
      paste("data-", input$var, '-', ifelse(input$customise == 1, input$group, 100), '-', paste0(individual, collapse = '_'), ".csv",
            sep="")
    },
    content = function(file) {
      if('download.csv' %in% list.files()){
        download = read_csv('download.csv', show_col_types = F)
        write.csv(download, file, row.names = FALSE)
        unlink("download.csv")
      }
    }
  )

  output$sum1 <- renderTable({
    group = 100
    individual = c(1,2,3,4,5)
    
    if (input$customise == 1){
      group = input$group
      individual = c(input$i1,input$i2,input$i3,input$i4,input$i5)
      # print(individual)
    }
    
    data <- switch(input$var, 
                   "Whio" = calculate.grades(group, individual, 'Whio'),
                   "Kaka" = calculate.grades(group, individual, 'Kaka'),
                   "Hihi" = calculate.grades(group, individual, 'Hihi'),
                   "Ruru" = calculate.grades(group, individual, 'Ruru'))
    
    tibble::tibble(!!!summary(as.vector(data)))
  })
  
  
  
}

shinyApp(ui, server)
