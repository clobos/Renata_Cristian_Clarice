
library(shiny)
ui<- fluidPage(
  titlePanel("conditionalPanel()"),
  sidebarPanel(
    selectInput(inputId = "distribuicao", 
                label = "Selecione uma distribuição", 
                choices = c("Normal", 
                            "Exponencial")),
    
    
    numericInput(inputId = "n", 
                 label = "Selecione o tamanho da amostra",
                 value=100),
    
    conditionalPanel(condition = "input.distribuicao == 'Normal' ",
                     numericInput(inputId = "mu", 
                                  label = "Média", 
                                  value = 10),
                     numericInput(inputId = "sd", 
                                  labe = "Devio-padrão", 
                                  value = 3)),
    
    conditionalPanel(condition = "input.distribuicao == 'Exponencial' ",
                     numericInput(inputId = "lambda", 
                                  label = "Taxa", 
                                  value = 2))
  ),
  mainPanel( plotOutput("grafico") )
)

server<- function(input,output){ 
  output$grafico<- renderPlot({
    resposta <- function(distribuicao) {
      switch(input$distribuicao,
             Normal = rnorm(input$n, 
                            input$mu, 
                            input$sd),
             Exponencial = rexp(input$n,
                                input$lambda))
    }
    ggplot(, aes(resposta())) +
      geom_histogram(aes(y=..density..)) +
      # geom_density() +
      xlab("variável") +
      ylim(0,1)
  })
}

shinyApp(ui=ui, server=server)
