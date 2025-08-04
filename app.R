library(shiny)
library(tidyverse)
library(olsrr)
library(scales)
library(bslib)

#Load data
train <- read_csv("train.csv") %>% rename_with(~ make.names(.x, unique = TRUE)) %>%
mutate(across(where(is.character), as.factor))

#UI
ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "darkly"), titlePanel("Century 21 Ames Housing Explorer"),
  sidebarLayout(sidebarPanel(selectInput("hood", "Neighborhood", choices = c("All", levels(train$Neighborhood))),
      selectInput("ext", "Exterior quality", choices = c("All", levels(train$ExterQual))),
      selectInput("salecond", "Sale condition", choices = c("All", levels(train$SaleCondition))),
      sliderInput("yr", "Year built", min   = min(train$YearBuilt), max   = max(train$YearBuilt), value = range(train$YearBuilt), sep   = ""),
      checkboxInput("reg", "Show regression line", TRUE)),
    mainPanel(tabsetPanel(tabPanel("Scatter", plotOutput("scatter", height = 500)),
        tabPanel("Distribution",  plotOutput("hist", height = 500)),
        tabPanel("Summary stats", tableOutput("summ_tbl"))))))

#server
server <- function(input, output, session){
  
  #filtered data
  df <- reactive({
    d <- train %>% filter(between(YearBuilt, input$yr[1], input$yr[2]))
    if(input$hood != "All") d <- filter(d, Neighborhood == input$hood)
    if(input$ext != "All") d <- filter(d, ExterQual == input$ext)
    if(input$salecond != "All") d <- filter(d, SaleCondition == input$salecond)
    d})
  
  #scatter plot
  output$scatter <- renderPlot({
    g <- ggplot(df(), aes(GrLivArea, SalePrice)) +
      geom_point(alpha = 0.6, color = "deepskyblue") +
      labs(x = "Finished living area (sq ft)", y = "Sale price", title = paste("Scatter –", input$hood,"ExtQual:", input$ext, "SaleCond:", input$salecond)) +
      scale_y_continuous(labels = dollar) +  #formatted y-axis
      scale_x_continuous(labels = comma)     #formatted x-axis
    if(input$reg) g <- g + geom_smooth(method = "lm", se = FALSE, colour = "orange", linewidth = 1.2)
    g
  })
  
  #histogram
  output$hist <- renderPlot({
    ggplot(df(), aes(SalePrice)) +
      geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
      labs(x = "Sale price", y = "Count",
           title = paste("Price distribution –", input$hood)) +
      scale_x_continuous(labels = dollar)})
  
  #summary
  output$summ_tbl <- renderTable({
    d <- df()
    tibble(Count = nrow(d),
      MeanPrice = dollar(mean(d$SalePrice, na.rm = TRUE)),
      MedianPrice = dollar(median(d$SalePrice, na.rm = TRUE)),
      MinPrice = dollar(min(d$SalePrice, na.rm = TRUE)),
      MaxPrice = dollar(max(d$SalePrice, na.rm = TRUE)))})}

#run
shinyApp(ui, server)

