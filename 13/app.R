
library(shiny)
ui<- fluidPage(
  titlePanel("Exercício"),
  sidebarPanel(
    numericInput(inputId = "n", 
                 label = "Tamanho da amostra", 
                 value = 100),
    numericInput(inputId = "k",
                 label = "Número de classes",
                 value = 30),
    numericInput(inputId = "mean",
                 label = "Média",
                 value = 0),
    numericInput(inputId = "sd",
                 label = "Desvio-padrão",
                 value = 1),
    textInput(inputId = "title",
              label = "Título do gráfico",
              value = "título")
  ),
  mainPanel( 
    plotOutput("grafico")  
  )
)
server<- function(input,output){ 
  output$grafico<- renderPlot({
    
    resposta <- reactive({
      rnorm(n = input$n, input$mean, input$sd)
    })
    ggplot(, aes(resposta())) +
      geom_histogram(aes(y=..density..), bins = input$k) +
      geom_density() +
      xlab("variável") +
      ylim(0,1) + 
      ggtitle(input$title)
  })
}
shinyApp(ui=ui, server=server)

