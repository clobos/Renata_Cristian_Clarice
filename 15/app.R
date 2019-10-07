```


### Gráfico e Tabela

```{r, echo=TRUE}
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
```

### checkboxInput()

```{r, echo=TRUE}
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
                              "Dezembro" = "DEZ")),
      checkboxInput("tendencia", "Adicionar tendência", FALSE)
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
    if(input$tendencia == TRUE){
      plt0 <- plt0+
        geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "red")
    }
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
```


chuva <- read.csv2("Climatologicos_MM.csv")
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
      checkboxInput("tendencia", "Adicionar tendência", FALSE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Gráfico", 
                           plotOutput(outputId = "ggplots")),
                  tabPanel(title = "Resumo", 
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
                    tendencia){
    
    dados$y<-dados[, which(names(dados) == as.character(y))]
    dadosN <- subset(dados, MES == mes)
    
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
              tendencia = input$tendencia))
    
  })
  
  output$summary <- renderPrint({
    summary(plt(dados = chuva, 
                mes = input$mes,
                x = "ANO",
                y = input$y,
                tendencia = input$tendencia)[[1]]$y)
  })
  
}

shinyApp(ui = ui, server = server)
