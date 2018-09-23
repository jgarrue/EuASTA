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
                              p("La tercera pestaña muestra una serie de gráficos interactivos en los que puede explorarse los retrasos producidos en las llegadas entre 2014 y julio del 2018."),
                              p("Pueden identificarse los países con mayor número de vuelos en europa, que corresponden a Reino Unido, Alemania, España y Francia."),
                              p("Mediante la modificación del inicio y final del periodo revisado, puede verse como se ha ido modificando éste orden."),
                              p("Puede observarse como España ha adelantado a Francia, separándose de ella en el último año. Y cómo el Reino Unido ha desbancado a Alemania como principal 'HUB' europeo."),
                              p("También llama la atención la distribución estacional del tráfico de los aeropuertos más grandes, en los que se observa un acusado descenso en invierno frente a los vuelos en verano."),
                              p("Como curiosidad en el caso de España, podemos observar que casi todos los aeropuertos principales son de poblaciones turísticas en los que hay un fuerte ascenso de tráfico en verano. Sin embargo, en el caso concreto de Madrid, Barcelona y Bilbao, a pesar de existir un aumento del tráfico en verano, también se observa un leve descenso en Agosto, motivado quizá por no ser destinos de costa. Este descenso es más acusado en Madrid y Bilbao, en los que el tráfico de agosto está por debajo del tráfico de julio y septiembre."),
                              p("(Nota:Tras un análisis inicial de los datos, se han agregado las salidas y las llegadas al no existir prácticamente diferencia entre una y otra.)")
                            )),
                   tabPanel("Funcionamiento diario del aeropuerto",
                            sidebarPanel(
                              
                                selectInput('targetstate','Elige qué país quieres analizar', countries, countries[1]),
                                conditionalPanel(
                                  condition = "input.targetstate == 'Todos' && input.targetairport == 'Todos'",
                                  radioButtons('HistogramOrMap','Elige el tipo de gráfica',c('Ver histograma','Ver mapa'))
                                ),
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
                                selectInput('targetyear2',NULL, choices=years, selected=tail(years, n=1)),
                                conditionalPanel(
                                  condition = "(input.targetstate != 'Todos' && input.targetairport == 'Todos' && input.targetStateOptions == 'Vuelos totales agregados') || (input.targetairport != 'Todos' && input.targetAirportOptions == 'Gráfico de vuelos en un periodo')",
                                  checkboxInput('Sh_LM', 'Marca para mostrar la línea de regresión lineal', value = FALSE, width = NULL),
                                  checkboxInput('Sh_LOESS', 'Marca para mostrar la línea de regresión LOESS', value = FALSE, width = NULL),
                                  checkboxInput('Sh_GAM', 'Marca para mostrar el modelo GAM', value = TRUE, width = NULL)
                                )
                                
                            ),
                            
                            mainPanel(
                              plotOutput('plot',
                                         height=500)
                              
                            )
                   ),
                   tabPanel("Retrasos",
                            sidebarPanel(
                              
                              selectInput('Delay_targetstate','Elige qué país quieres analizar', countries, countries[1]),
                              uiOutput("Delay_AirportOnDemand"),
                              conditionalPanel(
                                condition = "input.Delay_targetairport == 'Todos'",
                                checkboxInput('Delay_distribution', 'Indica si quieres mostrar la distribución de los retrasos', value = FALSE, width = NULL),
                                conditionalPanel(
                                  condition = "input.Delay_distribution != true",
                                  selectInput('Delay_targetdelay','Indica el tipo de retraso que quieres analizar', Delay_codes[,2], Delay_codes[1,2]),
                                  checkboxInput('Delay_adjusted', 'Indica si quieres ajustar los retrasos al número de vuelos', value = FALSE, width = NULL)
                                )
                              ),
                              selectInput('Delay_targetmonth1','Indica el inicio del periodo a analizar', months_ES, months_ES[1]),
                              selectInput('Delay_targetyear1',NULL, choices=years, selected=years[1]),
                              selectInput('Delay_targetmonth2','Indica el final del periodo a analizar', months_ES, months_ES[12]),
                              selectInput('Delay_targetyear2',NULL, choices=years, selected=tail(years, n=1))
                              
                            ),
                            
                            mainPanel(
                              plotOutput('Delay_plot',
                                         height=500)
                              
                            )
                   )
  
))

