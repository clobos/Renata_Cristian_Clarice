library(shiny)
library(shinythemes)
ui<- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("radioButtons()"),
  sidebarPanel(
    radioButtons(inputId = "distribuicao", 
                 label = "Escolher uma distribuição", 
                 choices = c("Normal", 
                             "Exponencial"))
  ),
  mainPanel( 
    plotOutput("grafico")  
  )
)

server<- function(input,output){ 
  output$grafico<- renderPlot({
    
    resposta <- reactive({
      switch(input$distribuicao,
             "Normal" = rnorm(100),
             "Exponencial" = rexp(100))
    })
    ggplot(, aes(resposta())) +
      geom_histogram(aes(y=..density..)) +
      geom_density() +
      ggtitle(input$distribuicao)
  })
}
shinyApp(ui=ui, server=server)
