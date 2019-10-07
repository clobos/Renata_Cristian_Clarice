library(shiny)
ui <- fluidPage(
  titlePanel("Meu primeiro App em Shiny")
)
server <- function(input, output){}
shinyApp(ui = ui, server = server)
