library(shiny)
library(tidyverse)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("About", 
      sidebarPanel(h1("Vaccinations in the United States on 2/8/2023 Data"), 
                   p("Data are reported on ", strong("CDC COVID Data Tracker")),
                   p("The dataset contains 3283 observations and 57 variables"), 
                   img(alt = "vaccination", src = 
                         "https://www.news-medical.net/images/news/ImageForNews_734493_16717867507843586.jpg", 
                       height="80%", width="80%"), 
                   p("Here is small random sample of data: ")), 
      mainPanel(dataTableOutput("sample"))),
    
    tabPanel("Plots", 
      sidebarPanel(p("You can analyze the relationship between the percent of 
                     people who completed primary series and the Percent of 
                     people completed primary series and booster based on ", 
                     strong("Metro status")), 
                   p("Select the metro status you are interested in"), 
        uiOutput("checkboxMetro")
      ), 
      mainPanel(plotOutput("plot"))), 
    
    tabPanel("Tables", 
      sidebarPanel(p("You can choose the ", strong("state "), "you are 
                     interested in"), 
                   p("The table on the right will show the average percent of 
                     the people who received primary series and the percent of 
                     the people who received both of the primary series and 
                     booster dose of the state you choose"), 
        uiOutput("checkboxState")
      ), 
      mainPanel(tableOutput("table")))
  )
)

server <- function(input, output) {
  data <- read_delim("project_data.csv")
  data <- na.omit(data)
  
  output$sample <- renderDataTable({
    data %>% 
      sample_n(6)
    })
  output$checkboxMetro <- renderUI({
    checkboxGroupInput("metro", "Choose Metro", 
                       choices = unique(data$Metro_status))
  })
  select_data <- reactive({
    data %>% 
      filter(Metro_status %in% input$metro)
  })
  
  output$plot <- renderPlot({
    select_data() %>% 
      ggplot(aes(Series_Complete_Pop_Pct, Booster_Doses_Vax_Pct), col=input$metro) + 
      geom_point() + 
      labs(x = "Percent of people completed primary series", 
           y = "Percent of people completed primary series and booster") + 
      ggtitle("Percent of completed primary series versus both primary series and booster")
  })
  
  output$checkboxState <- renderUI({
    checkboxGroupInput("states", "Choose State", 
                       choices = unique(data$Recip_State))
  })
  
  select_state <- reactive({
    data %>% 
      filter(Recip_State %in% input$states)
  })
  
  output$table <- renderTable({
    select_state() %>% 
      summarize(averge_primary_series  = mean(Series_Complete_Pop_Pct), 
                averge_booster_series  = mean(Booster_Doses_Vax_Pct))
      
  })
}

shinyApp(ui = ui, server = server)
