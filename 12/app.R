library(shiny)
library(ggplot2)
ui<- fluidPage(
  titlePanel("sliderInput()"),
  sidebarPanel(
    selectInput(inputId = "distribuicao", 
                label = "Escolher uma distribuição", 
                choices = c("Normal", 
                            "Exponencial")),
    sliderInput(inputId = "n",
                label = "Tamanho da amostra",
                min = 50,
                max = 10000,
                step = 50,
                value = 100),
    numericInput(inputId = "k",
                 label = "Número de classes",
                 value = 30)
  ),
  mainPanel( 
    plotOutput("grafico")  
  )
)

server<- function(input,output){ 
  output$grafico<- renderPlot({
    resposta <- switch(input$distribuicao,
                       "Normal" = rnorm(input$n),
                       "Exponencial" = rexp(input$n))
    ggplot(, aes(resposta)) +
      geom_histogram(aes(y=..density..), bins = input$k) +
      geom_density() +
      ggtitle(input$distribuicao) +
      xlab("variável") +
      ylim(0,1)
  })
} 


shinyApp(ui=ui, server=server)
