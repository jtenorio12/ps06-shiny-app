library(shiny)
library(tidyverse)
library(plotly)

totalPop <- read_delim("data1/Region-Table 1.csv")

clean_duration <- read_delim("data1/Duration of homelessness-Table 1.csv")
clean <- clean_duration[1:4]

causes <- read_delim('data1/Cause-Table 1.csv')

ui <- fluidPage(tabsetPanel(
  tabPanel( "Introduction",
            titlePanel("Homelessness in Washington State Seattle/KingCounty"),
            p("The data below displays the", em("duration"), "of", strong("homelessness"), "percentage and count for each year."),
            br(),
            p("The Data Set contains information given to The Department of Health and Human Services by KingCounty WA's, All Home Lead Agency."),
            tableOutput("sampletable")),
  
  tabPanel("Data Plot",
           titlePanel("Causes for Homelessness"),
           sidebarLayout(
             sidebarPanel(
               fluidRow( p("The Data below displays the main causes for Homelessness within Washington State "),
                         p("Further, the data is organized yearly census data that refers to the total count of individuals"),
                 column(6,
                        radioButtons("color", "Choose color",
                                     choices = c("skyblue", "pink", "beige","green", 
                                                          "purple1", "steelblue3", "lightgrey"))
                 ),
                 column(6, 
                        uiOutput("checkboxShelter")
                 )
               )
             ),
             mainPanel(
               plotOutput("plot"),
               textOutput("years")
             )
           )),
  tabPanel("Display Table",
           titlePanel("Homeless Percentage/Count for each Region in Washington State"),
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 p("The Data below displays the Homelessness percentage within each Region in Washington State."),
                 p("Further, the data is organized by each year for both unsheltered and sheltered indivudals."),
                 column(6,
                        uiOutput("checkboxRegion")))
             ),
             mainPanel(tableOutput("rdata"),textOutput("counts")
             )
           ))
))
server <- function(input, output) {
  ## For the about
  output$sampletable <- renderTable({
    clean %>%  
      sample_n(6)
  })
  ## For the plot
  cleaned_causes <- causes %>% 
    select_if(~!any(is.na(.)))
  
  cleaned_causes[rowSums(is.na(cleaned_causes)) == 0, ]
  
  cleaned_causes %>% 
    filter(!is.na(Count), !is.na(Percent))
  
  output$checkboxShelter <- renderUI({
    radioButtons("Cause", "Causes of homelessness",
                 choices = unique(cleaned_causes$Cause),
    )
  })
  sample2 <- reactive({
    s2 <- cleaned_causes %>%
      filter(Cause == input$Cause)
  })
  
  output$plot <- renderPlot({
    p <- sample2() %>%
      ggplot(aes(x = factor(Year), y = Count, fill = factor(Cause))) +
      geom_col() +
      labs(x = "Year", y = "Count", fill = "Cause") +
      scale_fill_manual(values = input$color)
    
    if(nrow(sample2()) == 0) {
      p <- p + labs(title = "Please select a cause")
    } 
    p
  })
  output$years <- renderText({
    years <- sample2() %>% pull(Year) %>% unique()
    if (length(years) == 1) {
      paste("This data is from", years)
    } else {
      paste("The data ranges from the year", min(years), "to the maximum year", max(years))
    }
  })
  
  ## For the table
  output$checkboxRegion <- renderUI({
    radioButtons("Region", "Choose region in Washington State",
                 choices = unique(totalPop$Region),
                 selected = NULL)
    
  })
  sample <- reactive({
    s1 <- totalPop
    filter(Region %in% input$Region)
    s1
  })
  
  output$rdata <- renderTable({
    totalPop %>% 
      filter(Region == (input$Region))
    
  })
  output$counts <- renderText({
    counts <- sample3() %>% pull(Count) %>% unique()
    if (length(counts) == 1) {
      paste("This data has a count", counts)
    } else {
      paste("The data has a minimum population count of", min(counts), "and a maximum population count", max(counts))
    }
  })
  sample3 <- reactive({
    s3 <- totalPop %>%
      filter(Region == (input$Region))
})
}

shinyApp(ui = ui, server = server)