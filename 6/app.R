
library(shiny)
ui <- fluidPage(
  titlePanel("Meu primeiro App em Shiny"),
  sidebarLayout(
    sidebarPanel("Os inputs aparecerão aqui"),
    mainPanel("Os resultados aparecerão aqui")
  )
)
server <- function(input, output){}
shinyApp(ui = ui, server = server)


