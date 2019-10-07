library(shiny)
library(nlme)
library(shinythemes)


data("Orange")

ui<- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Modelo não linear"),
  sidebarPanel(
    sliderInput("Asym","Asym",min=120,max=220,
                step=1, value = 130),
    sliderInput("xmid","xmid",min=400,max=1200,
                step=1, value = 450),
    sliderInput("scal","scal",min=250,max=450,
                step=0.2,value = 280)
  ),
  mainPanel(
    plotOutput("grafico")
  ),
  helpText("Modelo: y = Asym/(1 + exp(-(x - xmid)/scal))")
)
server<- function(input,output){ 
  
  output$grafico<- renderPlot({
    
    logist <- function(x, Asym = input$Asym, xmid = input$xmid, scal = input$scal) {
      Asym/(1 + exp(-(x - xmid)/scal))
    }
    
    ggplot(Orange, aes(x = age, y = circumference))+
      geom_point() + 
      stat_function(fun = logist) +
      xlab("Tempo (dias)") +
      ylab("CAP (mm)")
    
    
  })  
}
shinyApp(ui=ui, server=server)
