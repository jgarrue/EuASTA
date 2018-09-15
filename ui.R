#
# Ejemplo de app shiny para master UNED Big Data
# Módulo visualización avanzada
#
# Ejemplo para ejercicio de fin de módulo
#
# Basado en parte en el ejemplo de Joe Cheng
# https://gist.github.com/jcheng5/3239667
# Basado en parte en el ejemplo de Pedro Concejero
# https://gist.github.com/pedroconcejero

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

#Listas
countries <- c('Todos',levels(as.factor(Airport_Traffic$STATE_NAME)))
years <- levels(as.factor(Airport_Traffic$YEAR))
months_ES <- c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre')

shinyUI(
  navbarPage("European Airport Space Traffic Analysis",
                   tabPanel("Descripción del trabajo",
                            mainPanel(
                              h1("European Airport Space Traffic Analysis", align = "center"),
                              h2("Estudio del tráfico aéreo en los aeropuertos europeos", align = "center"),
                              h2("autor: Javier Gómez-Arrue Azpiazu", align = "center"),
                              h3("Datos obtenidos de la página del repositorio Pan-Europeo de desempeño de Seguridad en Navegación Aérea de la agencia EUROCONTROL",align = "center"),
                              h3("http://ansperformance.eu/data/performancearea/",align = "center"),
                              h3("Código disponible en:",align = "center"),
                              h3("https://github.com/jgarrue/EuASTA",align = "center"),
                              p("Mediante ésta sencilla aplicación puede analizarse el tráfico aéreo de los distintos aeropuertos europeos."),
                              p("La primera pestaña (esta) describe el contenido y el autor."),
                              p("La segunda pestaña presenta una serie de gráficos interactivos en los que puede explorarse el tráfico de los distintos aeropuertos europeos entre 2014 y julio del 2018."),
                              p("Pueden identificarse los países con mayor número de vuelos en europa, que corresponden a Reino Unido, Alemania, España y Francia."),
                              p("Mediante la modificación del inicio y final del periodo revisado, puede verse como se ha ido modificando éste orden."),
                              p("Puede observarse como España ha adelantado a Francia, separándose de ella en el último año. Y cómo el Reino Unido ha desbancado a Alemania como principal 'HUB' europeo."),
                              p("También llama la atención la distribución estacional del tráfico de los aeropuertos más grandes, en los que se observa un acusado descenso en invierno frente a los vuelos en verano."),
                              p("Como curiosidad en el caso de España, podemos observar que casi todos los aeropuertos principales son de poblaciones turísticas en los que hay un fuerte ascenso de tráfico en verano. Sin embargo, en el caso concreto de Madrid, Barcelona y Bilbao, a pesar de existir un aumento del tráfico en verano, también se observa un leve descenso en Agosto, motivado quizá por no ser destinos de costa. Este descenso es más acusado en Madrid y Bilbao, en los que el tráfico de agosto está por debajo del tráfico de julio y septiembre."),
                              p("(Nota:Tras un análisis inicial de los datos, se han agregado las salidas y las llegadas al no existir prácticamente diferencia entre una y otra.)")
                            )),
                   tabPanel("Funcionamiento diario del aeropuerto",
                            sidebarPanel(
                              
                                #selectInput('targetgraph','Indica la gráfica que quieres', graphs_ES),
                                selectInput('targetstate','Elige qué país quieres analizar', countries, countries[1]),
                                conditionalPanel(
                                  condition = "input.targetstate != 'Todos'",
                                  radioButtons('targetStateOptions','Elige qué gráfica quieres ver para este país',c('Vuelos totales agregados','Comparativa de aeropuertos'))
                                ),
                                uiOutput("AirportOnDemand"),
                                conditionalPanel(
                                  condition = "input.targetairport != 'Todos'",
                                  radioButtons('targetAirportOptions','Elige qué gráfica quieres ver para este aeropuerto',c('Gráfico de vuelos en un periodo','Agregado por meses'))
                                ),
                                selectInput('targetmonth1','Indica el inicio del periodo a analizar', months_ES, months_ES[1]),
                                selectInput('targetyear1',NULL, choices=years, selected=years[1]),
                                selectInput('targetmonth2','Indica el final del periodo a analizar', months_ES, months_ES[12]),
                                selectInput('targetyear2',NULL, choices=years, selected=tail(years, n=1))
                              
                            ),
                            
                            mainPanel(
                              plotOutput('plot',
                                         height=500)
                              
                            )
                   ),
                   tabPanel("Retrasos",
                            h1("Actualmente en proceso de análisis de los datos", align = "center")
                   )
  
))

