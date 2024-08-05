#
# Nutritional Analysis of Starbucks Drinks
#

library(shiny)
library(tidyverse)
library(colourpicker)

sb <- read.csv('starbucks_drinks.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Nutritional Information about Starbucks Drinks"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            #Select box for variable:
            selectInput("selectvar", label = h3("Choose a Variable"), 
                        choices=list("Calories"=1, "Cholesterol"=2, "Sodium"=3, "Sugars"=4, "Total Carbohydrates"=5), 
                        selected = 1),
            selectInput("selectgen", label = h3("Choose a Category"), 
                        choices=list("All Drinks"=1, "Classic Espresso Drinks"=2, "Frappuccino® Blended Coffee"=3, 
                                     "Tazo® Tea Drinks"=4), 
                        selected = 1),
            colourInput("col","Select a Color", "darkseagreen"),
            
            # Slider input for number of bins
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
        #option to show mean
        checkboxInput("checkbox1", label="Display Mean", value=FALSE),
        
        
        #option to show sd
        checkboxInput("checkbox2", label="Display Standard Deviation", value=FALSE),
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           hr(),
           p('Mean:'),
           fluidRow(column(5, verbatimTextOutput("mean"))),
           p('Standard deviation:'),
           fluidRow(column(5, verbatimTextOutput("sd"))),
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
#1st variable Histograms     
        if(input$selectvar == 1 & input$selectgen == 1){
            hist(sb$Calories, breaks = input$bins, main='Distribution of Calories in All Drinks',xlab='Amount of Calories',col = input$col, border = 'black')
        }
      if(input$selectvar == 1 & input$selectgen == 2){
        hist(sb$Calories[sb$Beverage_category == "Classic Espresso Drinks"], breaks = input$bins, main='Distribution of Calories in Classic Espresso Drinks',xlab='Amount of Calories',col = input$col, border = 'black')
      }
      if(input$selectvar == 1 & input$selectgen == 3){
        hist(sb$Calories[sb$Beverage_category == "Frappuccino® Blended Coffee"], breaks = input$bins, main='Distribution of Calories in Frappuccino® Blended Coffee',xlab='Amount of Calories',col = input$col, border = 'black')
      }
      if(input$selectvar == 1 & input$selectgen == 4){
        hist(sb$Calories[sb$Beverage_category == "Tazo® Tea Drinks"], breaks = input$bins, main='Distribution of Calories in Tazo® Tea Drinks',xlab='Amount of Calories',col = input$col, border = 'black')
      }
#2nd Variable Histograms 
      if(input$selectvar == 2 & input$selectgen == 1){
        hist(sb$Cholesterol..mg., breaks = input$bins, main='Distribution of Cholesterol in All Drinks',xlab='Amount of Cholesterol in Milligrams',col = input$col, border = 'black')
      }
      if(input$selectvar == 2 & input$selectgen == 2){
        hist(sb$Cholesterol..mg.[sb$Beverage_category == "Classic Espresso Drinks"], breaks = input$bins, main='Distribution of Cholesterol in Classic Espresso Drinks',xlab='Amount of Cholesterol in Milligrams',col = input$col, border = 'black')
      }
      if(input$selectvar == 2 & input$selectgen == 3){
        hist(sb$Cholesterol..mg.[sb$Beverage_category == "Frappuccino® Blended Coffee"], breaks = input$bins, main='Distribution of Cholesterol in Frappuccino® Blended Coffee',xlab='Amount of Cholesterol in Milligrams',col = input$col, border = 'black')
      }
      if(input$selectvar == 2 & input$selectgen == 4){
        hist(sb$Cholesterol..mg.[sb$Beverage_category == "Tazo® Tea Drinks"], breaks = input$bins, main='Distribution of Cholesterol in Tazo® Tea Drinks',xlab='Amount of Cholesterol in Milligrams',col = input$col, border = 'black')
      }
#3rd Variable Histograms 
      if(input$selectvar == 3 & input$selectgen == 1){
        hist(sb$Sodium..mg., breaks = input$bins, main='Distribution of Sodium in All Drinks',xlab='Amount of Sodium in Milligrams',col = input$col, border = 'black')
      }
      if(input$selectvar == 3 & input$selectgen == 2){
        hist(sb$Sodium..mg.[sb$Beverage_category == "Classic Espresso Drinks"], breaks = input$bins, main='Distribution of Sodium in Classic Espresso Drinks',xlab='Amount of Sodium in Milligrams',col = input$col, border = 'black')
      }
      if(input$selectvar == 3 & input$selectgen == 3){
        hist(sb$Sodium..mg.[sb$Beverage_category == "Frappuccino® Blended Coffee"], breaks = input$bins, main='Distribution of Sodium in Frappuccino® Blended Coffee',xlab='Amount of Sodium in Milligrams',col = input$col, border = 'black')
      }
      if(input$selectvar == 3 & input$selectgen == 4){
        hist(sb$Sodium..mg.[sb$Beverage_category == "Tazo® Tea Drinks"], breaks = input$bins, main='Distribution of Sodium in Tazo® Tea Drinks',xlab='Amount of Sodium in Milligrams',col = input$col, border = 'black')
      }
#4th Variable Histograms 
      if(input$selectvar == 4 & input$selectgen == 1){
        hist(sb$Sugars..g., breaks = input$bins, main='Distribution of Sugar in All Drinks',xlab='Amount of Sugar in Grams',col = input$col, border = 'black')
      }
      if(input$selectvar == 4 & input$selectgen == 2){
        hist(sb$Sugars..g.[sb$Beverage_category == "Classic Espresso Drinks"], breaks = input$bins, main='Distribution of Sugar in Classic Espresso Drinks',xlab='Amount of Sugar in Grams',col = input$col, border = 'black')
      }
      if(input$selectvar == 4 & input$selectgen == 3){
        hist(sb$Sugars..g.[sb$Beverage_category == "Frappuccino® Blended Coffee"], breaks = input$bins, main='Distribution of Sugar in Frappuccino® Blended Coffee',xlab='Amount of Sugar in Grams',col = input$col, border = 'black')
      }
      if(input$selectvar == 4 & input$selectgen == 4){
        hist(sb$Sugars..g.[sb$Beverage_category == "Tazo® Tea Drinks"], breaks = input$bins, main='Distribution of Sugar in Tazo® Tea Drinks',xlab='Amount of Sugar in Grams',col = input$col, border = 'black')
      } 
#5th Variable Histograms 
      if(input$selectvar == 5 & input$selectgen == 1){
        hist(sb$Total.Carbohydrates..g., breaks = input$bins, main='Distribution of Total Carbohydrates in All Drinks',xlab='Amount of Carbohydrates in Grams',col = input$col, border = 'black')
      }
      if(input$selectvar == 5 & input$selectgen == 2){
        hist(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Classic Espresso Drinks"], breaks = input$bins, main='Distribution of Total Carbohydrates in Classic Espresso Drinks',xlab='Amount of Carbohydrates in Grams',col = input$col, border = 'black')
      }
      if(input$selectvar == 5 & input$selectgen == 3){
        hist(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Frappuccino® Blended Coffee"], breaks = input$bins, main='Distribution of Total Carbohydrates in Frappuccino® Blended Coffee',xlab='Amount of Carbohydrates in Grams',col = input$col, border = 'black')
      }
      if(input$selectvar == 5 & input$selectgen == 4){
        hist(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Tazo® Tea Drinks"], breaks = input$bins, main='Distribution of Total Carbohydrates in Tazo® Tea Drinks',xlab='Amount of Carbohydrates in Grams',col = input$col, border = 'black')
      }
      
    })
        
  
    #Display mean if selected
    output$mean <- renderPrint({ 
#1st variable means
       if(input$checkbox1 == TRUE & input$selectvar == 1 & input$selectgen == 1){
            mean(sb$Calories, na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 1 & input$selectgen == 2) {
        mean(sb$Calories[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 1 & input$selectgen == 3) {
        mean(sb$Calories[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 1 & input$selectgen == 4) {
        mean(sb$Calories[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}
#2nd variable means
      else if(input$checkbox1 == TRUE & input$selectvar == 2 & input$selectgen == 1){
        mean(sb$Cholesterol..mg., na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 2 & input$selectgen == 2) {
        mean(sb$Cholesterol..mg.[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 2 & input$selectgen == 3) {
        mean(sb$Cholesterol..mg.[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 2 & input$selectgen == 4) {
        mean(sb$Cholesterol..mg.[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}      
#3rd variable means
      else if(input$checkbox1 == TRUE & input$selectvar == 3 & input$selectgen == 1){
        mean(sb$Sodium..mg., na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 3 & input$selectgen == 2) {
        mean(sb$Sodium..mg.[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 3 & input$selectgen == 3) {
        mean(sb$Sodium..mg.[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 3 & input$selectgen == 4) {
        mean(sb$Sodium..mg.[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}
#4th variable means
      else if(input$checkbox1 == TRUE & input$selectvar == 4 & input$selectgen == 1){
        mean(sb$Sugars..g., na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 4 & input$selectgen == 2) {
        mean(sb$Sugars..g.[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 4 & input$selectgen == 3) {
        mean(sb$Sugars..g.[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 4 & input$selectgen == 4) {
        mean(sb$Sugars..g.[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}
#5th variable means
      else if(input$checkbox1 == TRUE & input$selectvar == 5 & input$selectgen == 1){
        mean(sb$Total.Carbohydrates..g., na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 5 & input$selectgen == 2) {
        mean(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 5 & input$selectgen == 3) {
        mean(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox1 == TRUE & input$selectvar == 5 & input$selectgen == 4) {
        mean(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}
        })
    
    #Display sd if selected
    output$sd <- renderPrint({ 
#1st variable sd
      if(input$checkbox2 == TRUE & input$selectvar == 1 & input$selectgen == 1){
        sd(sb$Calories, na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 1 & input$selectgen == 2) {
        sd(sb$Calories[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 1 & input$selectgen == 3) {
        sd(sb$Calories[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 1 & input$selectgen == 4) {
        sd(sb$Calories[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}
#2nd variable sd
      else if(input$checkbox2 == TRUE & input$selectvar == 2 & input$selectgen == 1){
        sd(sb$Cholesterol..mg., na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 2 & input$selectgen == 2) {
        sd(sb$Cholesterol..mg.[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 2 & input$selectgen == 3) {
        sd(sb$Cholesterol..mg.[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 2 & input$selectgen == 4) {
        sd(sb$Cholesterol..mg.[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}      
#3rd variable sd
      else if(input$checkbox2 == TRUE & input$selectvar == 3 & input$selectgen == 1){
        sd(sb$Sodium..mg., na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 3 & input$selectgen == 2) {
        sd(sb$Sodium..mg.[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 3 & input$selectgen == 3) {
        sd(sb$Sodium..mg.[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 3 & input$selectgen == 4) {
        sd(sb$Sodium..mg.[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}
#4th variable sd
      else if(input$checkbox2 == TRUE & input$selectvar == 4 & input$selectgen == 1){
        sd(sb$Sugars..g., na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 4 & input$selectgen == 2) {
        sd(sb$Sugars..g.[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 4 & input$selectgen == 3) {
        sd(sb$Sugars..g.[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 4 & input$selectgen == 4) {
        sd(sb$Sugars..g.[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}
#5th variable sd
      else if(input$checkbox2 == TRUE & input$selectvar == 5 & input$selectgen == 1){
        sd(sb$Total.Carbohydrates..g., na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 5 & input$selectgen == 2) {
        sd(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Classic Espresso Drinks"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 5 & input$selectgen == 3) {
        sd(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Frappuccino® Blended Coffee"], na.rm=TRUE)}
      else if(input$checkbox2 == TRUE & input$selectvar == 5 & input$selectgen == 4) {
        sd(sb$Total.Carbohydrates..g.[sb$Beverage_category == "Tazo® Tea Drinks"], na.rm=TRUE)}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
