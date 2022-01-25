cov<-read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
view(cov)

### in this project we are going to work with the region Campania, Intensive Care Units (terapia_intensiva) )

library(tidyverse)

covfilter<-filter(cov,denominazione_regione=="Campania")

nrow(covfilter)

names(covfilter)

view(covfilter)

covselect<-select(covfilter,data,ricoverati_con_sintomi,terapia_intensiva,ingressi_terapia_intensiva,totale_ospedalizzati,isolamento_domiciliare,totale_positivi,
                  nuovi_positivi,dimessi_guariti,deceduti,totale_casi,tamponi,casi_testati)

view(covselect)

sum(is.na(covselect$data))

covselect$data<-as.Date(covselect$data)
view(covselect)

Cov<-filter(covselect,covselect$data>="2020-10-01",covselect$data<="2021-02-01")
view(Cov)

nrow(Cov)
sum(is.na(Cov$ingressi_terapia_intensiva))

Data_t<-select(Cov, -ingressi_terapia_intensiva)
view(Data_t)
