chuva <- read.csv2("Climatologicos_MM.csv")
# str(chuva)
chuva <- transform(chuva,
                   MES = factor(MES,
                                levels = c("JAN", "FEV", "MAR", 
                                           "ABR", "MAI","JUN", 
                                           "JUL", "AGO", "SET", 
                                           "OUT", "NOV", "DEZ")),
                   MES.num = as.numeric(MES))
library(ggplot2)
library(shiny)
library(plotly)



ui<- fluidPage(
  titlePanel("Dados Climatológicos"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "y", 
                   label = "Variável resposta",
                   choices = c("Temperatura média" = "TEMPER.MEDIA",
                               "Temperatura máxima" = "TEMPER.MAX",
                               "Temperatura mínima" = "TEMPER.MIN")),
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
                              "Dezembro" = "DEZ")),
      checkboxInput("tendencia", "Adicionar tendência (temperatura)", FALSE),
      checkboxInput("precipitacao", "Adicionar precipitação", FALSE),
      sliderInput("intervalo", label = "Intervalo para avaliação", min = 0, 
                  max = 50, value = c(0, 45))
    ),
    # mainPanel(
    #   plotOutput(outputId = "ggplots")
    # )
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Gráfico", 
                           plotOutput(outputId = "ggplots")),
                  tabPanel(title = "Resumo (temperatura)", 
                           verbatimTextOutput("summary"))
      )
    )
  )
)



server <- function(input, output){
  
  "plt" <- function(dados = chuva, 
                    mes, 
                    y,
                    x,
                    y.min,
                    y.max,
                    tendencia,
                    precipitacao){
    
    dados$y<-dados[, which(names(dados) == as.character(y))]
    dadosN <- subset(dados, MES == mes &
                       y >= y.min &
                       y <= y.max)
    
    plt0 <- ggplot(data = dadosN,
                   aes_string(x = x,
                              y = y))+
      geom_line(aes(color = "Temperatura"))+
      xlab("ano") +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    if(tendencia == TRUE) {
      plt0 <- plt0 +
        geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "red")
    }
    if(precipitacao == TRUE) {
      plt0 <- plt0 +
        geom_line(aes(x = ANO, y = PRECIPITACAO/15, color = "Precipitação")) +
        scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Precipitação (mm)")) +
        scale_color_manual(breaks = c("Precipitação", "Temperatura"),
                           values = c("blue", "red"))
      
    }
    if(y == "TEMPER.MAX") {
      plt0 <- plt0 + ylab("Temperatura máxima (ºC)")
    }
    if(y == "TEMPER.MIN") {
      plt0 <- plt0 + ylab("Temperatura mínima (ºC)")
    }
    if(y == "TEMPER.MEDIA") {
      plt0 <- plt0 + ylab("Temperatura média (ºC)")
    }
    return(plt0)
    return(dadosN)
  }
  
  
  output$ggplots <- renderPlot({
    
    
    print(plt(dados = chuva, 
              mes = input$mes,
              x = "ANO",
              y = input$y,
              y.min = min(input$intervalo),
              y.max = max(input$intervalo),
              tendencia = input$tendencia,
              precipitacao = input$precipitacao))
    
  })
  
  output$summary <- renderPrint({
    summary(plt(dados = chuva, 
                mes = input$mes,
                x = "ANO",
                y = input$y,
                y.min = min(input$intervalo),
                y.max = max(input$intervalo),
                tendencia = input$tendencia,
                precipitacao = input$precipitacao)[[1]]$y)
  })
  
}

shinyApp(ui = ui, server = server)
