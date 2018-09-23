shinyServer(function(input, output) {

#Funcionamiento general del aeropuerto
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
    Lin_LM <- geom_smooth(color="#ffa800",method="lm")
    Lin_LOESS <- geom_smooth(color="#993333",method="loess")
    Lin_GAM <- geom_smooth(color="#084594",method="gam",formula=y ~ s(x, bs = "cs"))

    if (targetstate == 'Todos' & targetairport == 'Todos') {
      if (input$HistogramOrMap == 'Ver histograma') {
        by_countries <- filter(Airport_Traffic,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)%>%group_by(STATE_NAME)%>%summarise(DEPS=sum(FLT_DEP_1),ARR=sum(FLT_ARR_1))%>%select(STATE_NAME,DEPS,ARR)%>%arrange(desc(DEPS+ARR))
        ggplot(by_countries,aes(x=reorder(STATE_NAME,DEPS+ARR),y=DEPS+ARR)) + geom_col(color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Países',y='Vuelos Totales',title=paste('Tráfico en los aeropuertos europeos entre',targetyear1,'y',targetyear2,sep=' ')) + scale_y_continuous(expand = c(0, 0))
      } else {
        by_countries <- filter(Airport_Traffic,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)%>%group_by(STATE_NAME_2)%>%summarise(FLTS=sum(FLT_TOT_1))%>%select(STATE_NAME_2,FLTS)%>%arrange(desc(FLTS))
        by_countries2 <- as.data.frame(cbind(by_countries$STATE_NAME_2,by_countries$FLTS),row.name = by_countries$STATE_NAME_2)
        if (NUTS_Lvl == 0) {
          geodata$FLTS <- by_countries2[geodata$geo,2]
        } else if (NUTS_Lvl == 2) {
          geodata$FLTS <- by_countries2[substr(geodata$geo, 1, 2),2]
        }
        map1 <- tmap::tm_shape(geodata) +
          tmap::tm_fill("lightgrey") +
          tmap::tm_shape(geodata) +
          tmap::tm_grid() +
          tmap::tm_polygons("FLTS", title = paste("Vuelos de la UE entre",format(as.Date(targetdate1), format="%d %B %Y"),'y',format(as.Date(targetdate2), format="%d %B %Y"),sep=' '),  
                            palette = "Greens") +
          tmap::tm_format("World",legend.outside = FALSE, legend.bg.color = TRUE, legend.frame = TRUE, legend.width = -0.4, legend.height = -0.2, attr.outside = TRUE)
        tmap_mode("plot")
        map1
      }
    }
    else if (targetstate != 'Todos' & targetairport == 'Todos') {
      if (input$targetStateOptions == 'Vuelos totales agregados') {
          by_evolution <- filter(Airport_Traffic,STATE_NAME==targetstate,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)%>%group_by(FLT_DATE)%>%summarise(DEPS=sum(FLT_DEP_1),ARR=sum(FLT_ARR_1))
          P_by_evolution <- ggplot(by_evolution,aes(x=FLT_DATE,y=DEPS+ARR)) + geom_point(color="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + labs(x='Tiempo',y='Vuelos Totales',title=paste('Tráfico agregado de los aeropuertos de',targetstate,sep=' ')) + scale_y_continuous(expand = c(0, 0))
          if (input$Sh_GAM == TRUE) {P_by_evolution <- P_by_evolution + Lin_GAM} else {}
          if (input$Sh_LM == TRUE) {P_by_evolution <- P_by_evolution + Lin_LM} else {}
          if (input$Sh_LOESS == TRUE) {P_by_evolution <- P_by_evolution + Lin_LOESS} else {}
          P_by_evolution
      }
      else {
          by_airport <- filter(Airport_Traffic,STATE_NAME==targetstate,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)%>%group_by(APT_NAME)%>%summarise(DEPS=sum(FLT_DEP_1),ARR=sum(FLT_ARR_1))%>%select(APT_NAME,DEPS,ARR)
          ggplot(by_airport,aes(x=reorder(APT_NAME,DEPS+ARR),y=DEPS+ARR)) + geom_col(color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Países',y='Vuelos Totales',title='Tráfico en los aeropuertos') + scale_y_continuous(expand = c(0, 0))
      }

    }
    else if (targetairport != 'Todos') {
      if (input$targetAirportOptions == 'Gráfico de vuelos en un periodo') {
          by_interval <- filter(Airport_Traffic,APT_NAME==targetairport,FLT_DATE >= targetdate1,FLT_DATE <= targetdate2)
          P_by_interval <- ggplot(by_interval,aes(x=FLT_DATE,y=FLT_DEP_1+FLT_ARR_1)) + geom_point(color="#41ae76") + labs(x='Tiempo',y='Vuelos',title=paste('Tráfico en el aeropuerto de',targetairport,sep=' '))
          if (input$Sh_GAM == TRUE) {P_by_interval <- P_by_interval + Lin_GAM} else {}
          if (input$Sh_LM == TRUE) {P_by_interval <- P_by_interval + Lin_LM} else {}
          if (input$Sh_LOESS == TRUE) {P_by_interval <- P_by_interval + Lin_LOESS} else {}
          P_by_interval
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

#Estudio de los retrasos
  output$Delay_plot <- renderPlot({
    
    #Variables
    months_ES <- c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre')
    months_trans <- c(1:12)
    names(months_trans) <- months_ES
    Delay_targetstate   <- input$Delay_targetstate
    Delay_targetairport <- input$Delay_targetairport
    Delay_targetyear1   <- input$Delay_targetyear1
    Delay_targetyear2   <- input$Delay_targetyear2
    Delay_targetmonth1  <- months_trans[input$Delay_targetmonth1]
    Delay_targetmonth2  <- months_trans[input$Delay_targetmonth2]
    MaxDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    Delay_targetdate1   <- as.POSIXct.Date(as.Date(paste(Delay_targetyear1,Delay_targetmonth1,'01',sep='-')),tz='CET')
    Delay_targetdate2   <- as.POSIXct.Date(as.Date(paste(Delay_targetyear2,Delay_targetmonth2,MaxDays[Delay_targetmonth2],sep='-')),tz='CET')
    targetdelay <- as.vector(Delay_codes[which(Delay_codes$V2 == input$Delay_targetdelay),1])
    if (Delay_targetstate == 'Todos' & Delay_targetairport == 'Todos') {
      #Histogramas de los retrasos acumulados por países
      FLT_by_country <- filter(Airport_Delay,FLT_DATE >= Delay_targetdate1,FLT_DATE <= Delay_targetdate2)%>%group_by(STATE_NAME)%>%summarise(ARR=sum(FLT_ARR_1))
      DLY_by_country <- as.data.frame(filter(Airport_Delay,FLT_DATE >= Delay_targetdate1,FLT_DATE <= Delay_targetdate2,DLY_APT_ARR_1>0)%>%group_by(STATE_NAME)%>%summarise(DLY_APT_ARR_1=sum(DLY_APT_ARR_1),DLY_APT_ARR_A_1=sum(DLY_APT_ARR_A_1),DLY_APT_ARR_C_1=sum(DLY_APT_ARR_C_1),DLY_APT_ARR_D_1=sum(DLY_APT_ARR_D_1),DLY_APT_ARR_E_1=sum(DLY_APT_ARR_E_1),DLY_APT_ARR_G_1=sum(DLY_APT_ARR_G_1),DLY_APT_ARR_I_1=sum(DLY_APT_ARR_I_1),DLY_APT_ARR_M_1=sum(DLY_APT_ARR_M_1),DLY_APT_ARR_N_1=sum(DLY_APT_ARR_N_1),DLY_APT_ARR_O_1=sum(DLY_APT_ARR_O_1),DLY_APT_ARR_P_1=sum(DLY_APT_ARR_P_1),DLY_APT_ARR_R_1=sum(DLY_APT_ARR_R_1),DLY_APT_ARR_S_1=sum(DLY_APT_ARR_S_1),DLY_APT_ARR_T_1=sum(DLY_APT_ARR_T_1),DLY_APT_ARR_V_1=sum(DLY_APT_ARR_V_1),DLY_APT_ARR_W_1=sum(DLY_APT_ARR_W_1),DLY_APT_ARR_NA_1=sum(DLY_APT_ARR_NA_1)))
      rownames(DLY_by_country) <- DLY_by_country$STATE_NAME
      FLT_by_country$DLY <- DLY_by_country[FLT_by_country$STATE_NAME,targetdelay]
      FLT_by_country$ADJ_DLY <- FLT_by_country$DLY / FLT_by_country$ARR
      FLT_by_country <- FLT_by_country %>% mutate(DLY = replace(DLY, is.na(DLY), 0)) %>% mutate(ADJ_DLY = replace(ADJ_DLY, is.na(ADJ_DLY), 0))
      if (input$Delay_distribution != FALSE) {
        ddLong_by_country <- melt(data = DLY_by_country[,c(1,seq(3,18))], id.vars = "STATE_NAME")
        ddLong_by_country$variable <- as.factor(Delay_codes[as.vector(ddLong_by_country$variable),2])
        #Histograma por tipo de retraso en porcentaje
        ggplot(ddLong_by_country, aes(STATE_NAME, value, fill = variable)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_y_continuous(labels = percent, expand = c(0, 0)) + coord_flip() + labs(x='Aeropuertos',y='Distribución de los retrasos',title=paste('Distribución de los retrasos en los aeropuertos entre',format(as.Date(targetdate1), format="%d %B %Y"),'y',format(as.Date(targetdate2), format="%d %B %Y"),sep=' '))
      } else if (input$Delay_adjusted == TRUE) {
        ggplot(FLT_by_country,aes(x=reorder(STATE_NAME,ADJ_DLY),y=ADJ_DLY)) + geom_col(color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Países',y='Minutos de retraso por vuelo',title=paste('Retrasos en los países (',Delay_codes[targetdelay,2],') [Ajustado entre número de vuelos]',sep='')) + scale_y_continuous(expand = c(0, 0))
      } else {
        ggplot(FLT_by_country,aes(x=reorder(STATE_NAME,DLY),y=DLY)) + geom_col(color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Países',y='Minutos de retraso',title=paste('Retrasos en los países (',Delay_codes[targetdelay,2],')',sep='')) + scale_y_continuous(expand = c(0, 0))
      }
    }
    else if (Delay_targetstate != 'Todos' & Delay_targetairport == 'Todos') {
      FLT_by_airport <- filter(Airport_Delay,STATE_NAME==Delay_targetstate,FLT_DATE >= Delay_targetdate1,FLT_DATE <= Delay_targetdate2)%>%group_by(APT_NAME)%>%summarise(ARR=sum(FLT_ARR_1))
      DLY_by_airport <- as.data.frame(filter(Airport_Delay,STATE_NAME==Delay_targetstate,FLT_DATE >= Delay_targetdate1,FLT_DATE <= Delay_targetdate2,DLY_APT_ARR_1>0)%>%group_by(APT_NAME)%>%summarise(DLY_APT_ARR_1=sum(DLY_APT_ARR_1),DLY_APT_ARR_A_1=sum(DLY_APT_ARR_A_1),DLY_APT_ARR_C_1=sum(DLY_APT_ARR_C_1),DLY_APT_ARR_D_1=sum(DLY_APT_ARR_D_1),DLY_APT_ARR_E_1=sum(DLY_APT_ARR_E_1),DLY_APT_ARR_G_1=sum(DLY_APT_ARR_G_1),DLY_APT_ARR_I_1=sum(DLY_APT_ARR_I_1),DLY_APT_ARR_M_1=sum(DLY_APT_ARR_M_1),DLY_APT_ARR_N_1=sum(DLY_APT_ARR_N_1),DLY_APT_ARR_O_1=sum(DLY_APT_ARR_O_1),DLY_APT_ARR_P_1=sum(DLY_APT_ARR_P_1),DLY_APT_ARR_R_1=sum(DLY_APT_ARR_R_1),DLY_APT_ARR_S_1=sum(DLY_APT_ARR_S_1),DLY_APT_ARR_T_1=sum(DLY_APT_ARR_T_1),DLY_APT_ARR_V_1=sum(DLY_APT_ARR_V_1),DLY_APT_ARR_W_1=sum(DLY_APT_ARR_W_1),DLY_APT_ARR_NA_1=sum(DLY_APT_ARR_NA_1)))
      rownames(DLY_by_airport) <- DLY_by_airport$APT_NAME
      if (input$Delay_distribution != FALSE) {
        ddLong_by_delay <- melt(data = DLY_by_airport[,c(1,seq(3,18))], id.vars = "APT_NAME")
        ddLong_by_delay$variable <- as.factor(Delay_codes[as.vector(ddLong_by_delay$variable),2])
        #Histograma por tipo de retraso en porcentaje
        ggplot(ddLong_by_delay, aes(APT_NAME, value, fill = variable)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_y_continuous(labels = percent,expand = c(0, 0)) + coord_flip() + labs(x='Aeropuertos',y='Distribución de los retrasos',title=paste('Distribución de los retrasos en los aeropuertos de',Delay_targetstate,'entre',format(as.Date(targetdate1), format="%d %B %Y"),'y',format(as.Date(targetdate2), format="%d %B %Y"),sep=' '))
      } else {
        #Histogramas de retrasos de todos los aeropuertos de un país
        FLT_by_airport$DLY <- DLY_by_airport[FLT_by_airport$APT_NAME,targetdelay]
        FLT_by_airport$ADJ_DLY <- FLT_by_airport$DLY / FLT_by_airport$ARR
        FLT_by_airport <- FLT_by_airport %>% mutate(DLY = replace(DLY, is.na(DLY), 0)) %>% mutate(ADJ_DLY = replace(ADJ_DLY, is.na(ADJ_DLY), 0))
        if (input$Delay_adjusted != TRUE) {
          ggplot(FLT_by_airport,aes(x=reorder(APT_NAME,DLY),y=DLY)) + geom_col(color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Aeropuertos',y='Minutos de retraso',title=paste('Retrasos en los aeropuertos de ',Delay_targetstate,' (',Delay_codes[targetdelay,2],')',sep='')) + scale_y_continuous(expand = c(0, 0))
        } else {
          ggplot(FLT_by_airport,aes(x=reorder(APT_NAME,ADJ_DLY),y=ADJ_DLY)) + geom_col(color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Aeropuertos',y='Minutos de retraso por vuelo',title=paste('Retrasos en los aeropuertos de ',Delay_targetstate,' (',Delay_codes[targetdelay,2],') [Ajustado entre número de vuelos]',sep='')) + scale_y_continuous(expand = c(0, 0))
        }
      }
      
    }
    else if (Delay_targetairport != 'Todos') {
      ddLong_by_delay <- melt(data = DLY_by_airport[,c(1,seq(3,18))], id.vars = "APT_NAME")
      ddLong_by_delay$variable <- as.factor(Delay_codes[as.vector(ddLong_by_delay$variable),2])
      ggplot(ddLong_by_delay%>%filter(APT_NAME==Delay_targetairport), aes(x = reorder(variable,value), y = value)  ) +
        geom_bar(stat = "identity",color="#005824",fill="#41ae76") + theme(axis.text.x=element_text(angle=0, size=8)) + coord_flip() + labs(x='Retrasos',y='Minutos de retraso',title=paste('Retrasos en el aeropuerto de',Delay_targetairport,'entre',format(as.Date(targetdate1), format="%d %B %Y"),'y',format(as.Date(targetdate2), format="%d %B %Y"),sep=' ')) + scale_y_continuous(expand = c(0, 0))
    }
    else {}
    
  })
  
  output$Delay_AirportOnDemand <- renderUI({
    Delay_SelectedStateAirports <- filter(Airport_Traffic,STATE_NAME==input$Delay_targetstate)
    if (input$Delay_targetstate == 'Todos') {Delay_AirportsOnDemand <- c('Todos',levels(as.factor(Airport_Traffic$APT_NAME)))}
    else {Delay_AirportsOnDemand <- c('Todos',levels(as.factor(Delay_SelectedStateAirports$APT_NAME)))}
    selectInput("Delay_targetairport", "Elige qué aeropuerto quieres analizar", Delay_AirportsOnDemand)
  })

  
})
