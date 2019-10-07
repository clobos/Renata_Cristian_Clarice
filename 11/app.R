
library(shiny)
library(ggplot2)
ui<- fluidPage(
  titlePanel("numericInput()"),
  sidebarPanel(
    selectInput(inputId = "distribuicao", 
                label = "Escolher uma distribuição", 
                choices = c("Normal", 
                            "Exponencial")),
    numericInput(inputId = "n",
                 label = "Tamanho da amostra",
                 value = 100),
    actionButton(inputId = "update",
                 label = "Atualizar")
  ),
  mainPanel( 
    plotOutput("grafico")  
  )
)

server<- function(input,output){ 
  p <- eventReactive(input$update, {
    resposta <- switch(input$distribuicao,
                       "Normal" = rnorm(input$n),
                       "Exponencial" = rexp(input$n))
    ggplot(, aes(resposta)) +
      geom_histogram(aes(y=..density..), bins = sqrt(input$n)) +
      geom_density() +
      ggtitle(input$distribuicao) +
      xlab("variável") +
      ylim(0,1)
    
  }, ignoreNULL = FALSE)
  
  output$grafico<- renderPlot({
    p()
  })
} 


shinyApp(ui=ui, server=server)
