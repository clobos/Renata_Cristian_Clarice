library(shiny)
ui<- fluidPage(
  titlePanel("selectInput() e renderPlot()"),
  sidebarPanel(
    selectInput(inputId = "distribuicao", 
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
      ggtitle(input$distribuicao)+xlim(c(0,10))
  })
}
shinyApp(ui=ui, server=server)
