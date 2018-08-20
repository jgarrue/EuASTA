library(shiny)
library(ggplot2)
library(dplyr)

#Carga de datos
get(load(url("https://github.com/jgarrue/EuASTA/blob/master/EuroAirportTraffic.rda?raw=true")))

shinyServer(function(input, output) {

  output$plot <- renderPlot({
    
    #Variables
    months_ES <- c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre')
    months_trans <- c(1:12)
    names(months_trans) <- months_ES
    targetstate   <- input$targetstate
    targetairport <- input$targetairport
    targetyear1   <- input$targetyear1
    targetyear2   <- input$targetyear2
    targetmonth1  <- months_trans[input$targetmonth1]
    targetmonth2  <- months_trans[input$targetmonth2]
    targetgraph   <- input$targetgraph
    MaxDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    targetdate1   <- as.POSIXct.Date(as.Date(paste(targetyear1,targetmonth1,'01',sep='-')),tz='CET')
    targetdate2   <- as.POSIXct.Date(as.Date(paste(targetyear2,targetmonth2,MaxDays[targetmonth2],sep='-')),tz='CET')

    if (targetstate == 'Todos' & targetairport == 'Todos') {
      by_countries <- filter(Airport_Traffic,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)%>%group_by(STATE_NAME)%>%summarise(DEPS=sum(FLT_DEP_1),ARR=sum(FLT_ARR_1))%>%select(STATE_NAME,DEPS,ARR)%>%arrange(desc(DEPS+ARR))
      ggplot(by_countries,aes(x=reorder(STATE_NAME,DEPS+ARR),y=DEPS+ARR)) + geom_col(color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Países',y='Vuelos Totales',title=paste('Tráfico en los aeropuertos europeos entre',targetyear1,'y',targetyear2,sep=' ')) + scale_y_continuous(expand = c(0, 0))
    }
    else if (targetstate != 'Todos' & targetairport == 'Todos') {
      if (input$targetStateOptions == 'Vuelos totales agregados') {
          by_evolution <- filter(Airport_Traffic,STATE_NAME==targetstate,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)%>%group_by(FLT_DATE)%>%summarise(DEPS=sum(FLT_DEP_1),ARR=sum(FLT_ARR_1))
          ggplot(by_evolution,aes(x=FLT_DATE,y=DEPS+ARR)) + geom_point(color="#41ae76") + geom_smooth(color="#084594") + theme(axis.text.x=element_text(angle=0, size=8)) + labs(x='Tiempo',y='Vuelos Totales',title=paste('Tráfico agregado de los aeropuertos de',targetstate,sep=' ')) + scale_y_continuous(expand = c(0, 0))
      }
      else {
          by_airport <- filter(Airport_Traffic,STATE_NAME==targetstate,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)%>%group_by(APT_NAME)%>%summarise(DEPS=sum(FLT_DEP_1),ARR=sum(FLT_ARR_1))%>%select(APT_NAME,DEPS,ARR)
          ggplot(by_airport,aes(x=reorder(APT_NAME,DEPS+ARR),y=DEPS+ARR)) + geom_col(color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Países',y='Vuelos Totales',title='Tráfico en los aeropuertos') + scale_y_continuous(expand = c(0, 0))
      }

    }
    else if (targetairport != 'Todos') {
      if (input$targetAirportOptions == 'Gráfico de vuelos en un periodo') {
          by_interval <- filter(Airport_Traffic,APT_NAME==targetairport,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)
          ggplot(by_interval,aes(x=FLT_DATE,y=FLT_DEP_1+FLT_ARR_1)) + geom_point(color="#41ae76") + geom_smooth(color="#084594") + labs(x='Tiempo',y='Vuelos',title=paste('Tráfico en el aeropuerto de',targetairport,sep=' '))
      }
      else {
          by_aggregate <- filter(Airport_Traffic,APT_NAME==targetairport,YEAR>=targetyear1,YEAR<=targetyear2)%>%group_by(MONTH_NUM)
          ggplot(by_aggregate,aes(x=MONTH_NUM,y=FLT_DEP_1+FLT_ARR_1)) + geom_boxplot(color="#005824",fill="#41ae76") + labs(x='Tiempo',y='Vuelos',title=paste('Tráfico agregado por meses en el aeropuerto de',targetairport,'( entre',targetyear1,'y',targetyear2,')',sep=' '))
      }
    }
    else {}
    
  })
  
  output$AirportOnDemand <- renderUI({
    SelectedStateAirports <- filter(Airport_Traffic,STATE_NAME==input$targetstate)
    if (input$targetstate == 'Todos') {AirportsOnDemand <- c('Todos',levels(as.factor(Airport_Traffic$APT_NAME)))}
    else {AirportsOnDemand <- c('Todos',levels(as.factor(SelectedStateAirports$APT_NAME)))}
    selectInput("targetairport", "Elige qué aeropuerto quieres analizar", AirportsOnDemand)
  })

  
})
