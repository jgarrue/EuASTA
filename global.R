# Ejemplo de app shiny para master UNED Big Data
# Trabajo fin de Máster
#
# Basado en parte en el ejemplo de Joe Cheng
# https://gist.github.com/jcheng5/3239667
# Basado en parte en el ejemplo de Pedro Concejero
# https://gist.github.com/pedroconcejero


library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(data.table)
library(scales)
library(eurostat)
library(sf)
library(tmap)

#Descarga de datos de la aplicación
Mode <- "ANSPerformance"

if (Mode == "Github") {
  get(load(url("https://github.com/jgarrue/EuASTA/blob/master/EuroAirportTraffic.rda?raw=true")))
} else if (Mode == "ANSPerformance") {
  url <- "http://ansperformance.eu/data/set/apt_flt/Airport_Traffic.xlsm"
  download.file(url,destfile='localfile.xlsm', method='curl')
  read_excel("localfile.xlsm")
  Airport_Traffic <- read_excel("localfile.xlsm",sheet = "DATA")
  file.remove("localfile.xlsm")
  url <- "http://ansperformance.eu/data/set/apt_dly/Airport_Arrival_ATFM_Delay.xlsm"
  download.file(url,destfile='localfile.xlsm', method='curl')
  read_excel("localfile.xlsm")
  Airport_Delay <- read_excel("localfile.xlsm",sheet = "DATA")
  file.remove("localfile.xlsm")
}

# Datos espaciales de GISCO (Eurostat)
NUTS_Lvl <- 0
geodata <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = NUTS_Lvl, year="2013")

remove(url,Mode)

#Variables comunes
Delay_codes <- as.data.frame(cbind(
                                    c("DLY_APT_ARR_1","DLY_APT_ARR_A_1","DLY_APT_ARR_C_1","DLY_APT_ARR_D_1","DLY_APT_ARR_E_1","DLY_APT_ARR_G_1","DLY_APT_ARR_I_1","DLY_APT_ARR_M_1","DLY_APT_ARR_N_1","DLY_APT_ARR_O_1","DLY_APT_ARR_P_1","DLY_APT_ARR_R_1","DLY_APT_ARR_S_1","DLY_APT_ARR_T_1","DLY_APT_ARR_V_1","DLY_APT_ARR_W_1","DLY_APT_ARR_NA_1")
                                    ,
                                    c("Airport ATFM arrival delay","A - Accident/Incident - AD","C - ATC Capacity - AD","D - De-icing - AD","E - Equipment (non-ATC) - AD","G - Aerodrome Capacity - AD","I - Industrial Action (ATC) - AD","M - Airspace Management - AD","N - Industrial Action (non-ATC) - AD","O - Other - AD","P - Special Event - AD","R - ATC Routeing - AD","S - ATC Staffing - AD","T - Equipment (ATC) - AD","V - Environmental Issues - AD","W - Weather - AD","NA - Not specified - AD")
                                    ,
                                    c("","AD Disruptions","AD Capacity (ATC)","AD Weather","AD Disruptions","AD Capacity","AD Disruptions (ATC)","AD Capacity","AD Disruptions","AD Disruptions","AD Events","AD Capacity","AD Staffing (ATC)","AD Disruptions (ATC)","AD Capacity","AD Weather","AD Disruptions")
                                  )
                              ,
                              row.names = c("DLY_APT_ARR_1","DLY_APT_ARR_A_1","DLY_APT_ARR_C_1","DLY_APT_ARR_D_1","DLY_APT_ARR_E_1","DLY_APT_ARR_G_1","DLY_APT_ARR_I_1","DLY_APT_ARR_M_1","DLY_APT_ARR_N_1","DLY_APT_ARR_O_1","DLY_APT_ARR_P_1","DLY_APT_ARR_R_1","DLY_APT_ARR_S_1","DLY_APT_ARR_T_1","DLY_APT_ARR_V_1","DLY_APT_ARR_W_1","DLY_APT_ARR_NA_1")
                              ,
                              col.names = c("Field","Case","Reason Group")
                            )
Country_codes <- as.data.frame(cbind(c("AL","AM","AT","BA","BG","CY","CZ","CH","BE","DE","DK","EE","EL","ES","HR","HU","FI","FR","GE","IE","LI","LT","LU","LV","MD","ME","MK","MT","NL","IS","IT","PT","RO","NO","PL","SE","SK","TR","UA","UK","SI","RS"),c("Albania","Armenia","Austria","Bosnia and Herzegovina","Bulgaria","Cyprus","Czech Republic","Switzerland","Belgium","Germany","Denmark","Estonia","Greece","Spain","Croatia","Hungary","Finland","France","Georgia","Ireland","Liechtenstein","Lithuania","Luxembourg","Latvia","Moldova","Montenegro","The former Yugoslav Republic of Macedonia","Malta","Netherlands","Iceland","Italy","Portugal","Romania","Norway","Poland","Sweden","Slovakia","Turkey","Ukraine","United Kingdom","Slovenia","Serbia")),row.name = c("Albania","Armenia","Austria","Bosnia and Herzegovina","Bulgaria","Cyprus","Czech Republic","Switzerland","Belgium","Germany","Denmark","Estonia","Greece","Spain","Croatia","Hungary","Finland","France","Georgia","Ireland","Liechtenstein","Lithuania","Luxembourg","Latvia","Moldova","Montenegro","The former Yugoslav Republic of Macedonia","Malta","Netherlands","Iceland","Italy","Portugal","Romania","Norway","Poland","Sweden","Slovakia","Turkey","Ukraine","United Kingdom","Slovenia","Serbia"))

# Columna de códigos en Airport_Traffic en función del país
Airport_Traffic$STATE_NAME_2 <- Country_codes[Airport_Traffic$STATE_NAME,1]