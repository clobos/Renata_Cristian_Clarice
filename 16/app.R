
chuva <- read.csv2("Climatologicos_MM.csv")
# str(chuva)
chuva <- transform(chuva,
                   MES = factor(MES,
                                levels = c("JAN", "FEV", "MAR", 
                                           "ABR", "MAI","JUN", 
                                           "JUL", "AGO", "SET", 
                                           "OUT", "NOV", "DEZ")),
                   MES.num = as.numeric(MES))

# str(chuva)
# 
# as.numeric(chuva$MES)

library(ggplot2)
library(shiny)
library(plotly)



ui<- fluidPage(
  titlePanel("Temperatura média"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "mes",
                  label = "Selecione o mês",
                  choices = c("Janeiro" = "JAN", 
                              "Fevereiro" = "FEV", 
                              "Março"  = "MAR", 
                              "Abril"  = "ABR", 
                              "Maio" = "MAI", 
                              "Junho" = "JUN", 
                              "Julho" = "JUL", 
                              "Agosto" = "AGO", 
                              "Setembro" = "SET", 
                              "Outubro" = "OUT", 
                              "Novembro" = "NOV", 
                              "Dezembro" = "DEZ"))
    ),
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Gráfico", 
                           plotlyOutput(outputId = "ggplots")),
                  tabPanel(title = "Resumo", 
                           verbatimTextOutput("summary"))
      )
    )
  )
)




server <- function(input, output){
  
  "plt" <- function(mes){
    
    dados <- subset(chuva, MES == mes)
    plt0 <- ggplot(dados,
                   aes(x = ANO,
                       y = TEMPER.MEDIA))+
      geom_line()+
      xlab("ano") +
      ylab("temperatura média (ºC)")+
      theme_bw() 
    return(plt0)
    return(dados)
  }
  
  
  output$ggplots <- renderPlotly({
    
    
    ggplotly(plt(mes = input$mes))
    
  })
  
  output$summary <- renderPrint({
    summary(subset(chuva, MES == input$mes)$TEMPER.MEDIA)
  })
  
}

shinyApp(ui = ui, server = server)
