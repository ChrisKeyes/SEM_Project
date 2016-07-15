library(shiny)

bcl <- Parklist

ui <- fluidPage(
  titlePanel("SEM_Project"),
  sidebarLayout(
    sidebarPanel(
      selectInput("parkInput", "PARKname",
                  choices = c("CUVA","YOSE")),
      # radioButtons("parkInput", "ParkName",
      #                 choices = c("CUVA","YOSE"),
      #                    # choices = c("CUVA" = "CUVA","GATE" = "GATE","YOSE"= "YOSE"),
      #                 selected = "CUVA"),
      
      uiOutput("parkOutput")
    ),
    mainPanel("Selection")
  )
)



server <- function(input, output) {
  output$parkOutput <- renderUI({
    selectInput("parkInput", "PARKname")
  })
}

shinyApp(ui = ui, server = server)
