
library(readr)
library(tidyverse)
library(dplyr)

vih <- read_csv("notificacion-casos-vih-sexo-jurisdiccion-2010-2019-argentina_1-1.csv", 
                                                                             locale = locale(encoding = "ISO-8859-1"))

# selecciono las variables que necesito para el análisis

vih<-vih%>%select(2,4,5,6)

# convierto en factor las variables para gráficar 

vih$sexo<-as.factor(vih$sexo)

#install.packages(c("shiny", "shinyWidgets", "dslabs",
#                   "tidyverse", "plotly"))

library(shiny)
library(shinyWidgets)
library(dslabs)
library(tidyverse)
library(plotly)


library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Contagios de VIH en Argentina 2010-2019 "),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("jurisdiccionInput", "Jurisdicción",
                     choices = unique(vih$jurisdiccion),  
                     selected="ARG",
                     multiple =FALSE),
      checkboxGroupInput("sexoInput", "Sexo",
                         choices = c("mujeres",
                                     "varones",
                                     "ambos_sexos"),
                         selected = c("mujeres", "varones")),
      sliderInput("anioInput", "Año", min=2010, max=2019, 
                  value=c(2010, 2013), sep="")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("vihPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive({
    filtered <-
      vih %>%
      filter(jurisdiccion %in% input$jurisdiccionInput,
             sexo %in% input$sexoInput,
             anio >= input$anioInput[1],
             anio <= input$anioInput[2])   
    
  }) 
  
  output$vihPlot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    ggplot(d(), 
           aes(x=as.factor(anio), 
               y=casos_vih,
               color=sexo,
               group=sexo)) +
      geom_point()+
      geom_line()+
      theme_bw()+
      xlab("Año") +
      ylab("Número de contagios") +
      ggtitle("Contagios a lo largo del tiempo")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
