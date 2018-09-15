library(readxl)

#Descarga de datos de la aplicación
Mode <- "Github"
if (Mode == "Github") {
  get(load(url("https://github.com/jgarrue/EuASTA/blob/master/EuroAirportTraffic.rda?raw=true")))
} else if (Mode == "ANSPerformance") {
  url <- "http://ansperformance.eu/data/set/apt_flt/Airport_Traffic.xlsm"
  download.file(url,destfile='localfile.xlsm', method='curl')
  read_excel("localfile.xlsm")
  Airport_Traffic <- read_excel("localfile.xlsm",sheet = "DATA")
  file.remove("localfile.xlsm")
}

remove(url,Mode)
