library(shiny)
library(ggplot2)
ui<- fluidPage(
  titlePanel("actionButton()"),
  sidebarPanel(
    selectInput(inputId = "distribuicao", 
                label = "Escolher uma distribuição", 
                choices = c("Normal", 
                            "Exponencial")),
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
                       "Normal" = rnorm(100),
                       "Exponencial" = rexp(100))
    ggplot(, aes(resposta)) +
      geom_histogram(aes(y=..density..), bins = 10) +
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
