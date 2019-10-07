
library(shiny)
ui <- fluidPage(
  titlePanel("Meu primeiro App em Shiny"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "texto", 
                label = "Texto", 
                value = "Meu texto")
    ),
    mainPanel(
      verbatimTextOutput("meu.texto")
    )
  )
)
server <- function(input, output) {
  output$meu.texto <- renderText({ input$texto })
}
shinyApp(ui, server)
