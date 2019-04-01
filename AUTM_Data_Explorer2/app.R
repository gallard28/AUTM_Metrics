#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

load("data_shiny_df.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AUTM Metrics Explorer - Beta"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        #Select university 
        selectInput(inputId = "institution", 
                    label = "Institution",
                    choices = unique(data_shiny_df$INSTITUTION), 
                    selected = "Albert Einstein College of Medicine Inc."),
        
        #Select variable for y-axis
        selectInput(inputId = "metric", 
                    label = "Metric",
                    choices = c("Total Research Expenditure"="Tot.Res.Exp",
                    "Total Disclosures Licensed"="Tot.Discl.Lic",
                  "Gross Licensing Income"="Gross.Lic.Inc",  
                 "Disclosures Received"="INVDISRES",
                  "New Patent Applications Filed"="New.Pat.App.Fld",
                  "Start Ups Formed"="St.Ups.Formed"),
                    selected = "Tot.Res.Exp")

        
       
      ),
        # Show a plot of the generated distribution
      mainPanel(
         plotOutput("areaplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  # Create scatterplot object the plotOutput function is expecting
  output$areaplot <- renderPlot({
    req(input$institution)
      data_from_selected_inst<-data_shiny_df %>% 
        filter(INSTITUTION %in% input$institution)
      ggplot(data=data_from_selected_inst,aes_string(x=data_from_selected_inst$YEAR, y=input$metric)) +
      geom_area(alpha=.5)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

