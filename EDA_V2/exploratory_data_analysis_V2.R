###############################################################################
###############################################################################
#####                      STATISTICS PROJECT                             #####
###############################################################################
###############################################################################

# load libraries
library(tidyverse)
library(corrplot)
library(PerformanceAnalytics)
library(car)
library(ggplot2)

# upload and prepare the dataset cov
cov<-read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
#view(cov)

### in this project we are going to work with the region Campania, Intensive Care Units (terapia_intensiva) )
# select the variables/period of interest
cov<-filter(cov,denominazione_regione=="Campania")
nrow(cov)
names(cov)
#view(cov)
cov<-select(cov,data,ricoverati_con_sintomi,terapia_intensiva,ingressi_terapia_intensiva,
                  totale_ospedalizzati,isolamento_domiciliare,totale_positivi,
                  nuovi_positivi,dimessi_guariti,deceduti,totale_casi,tamponi,casi_testati)
#view(cov)
sum(is.na(cov$data))
cov$data<-as.Date(cov$data)
#view(cov)
cov<-filter(cov,cov$data>="2020-10-01",cov$data<="2021-02-01")
#view(cov)
nrow(cov)
sum(is.na(cov$ingressi_terapia_intensiva))
cov<-select(cov, -ingressi_terapia_intensiva)
#view(cov)

# upload and prepare the dataset vax and add it to cov
vax <- read_csv("Vax.csv")
names(vax)
vax <- subset(vax,vax$nome_area == "Campania")
vax <- select(vax,data_somministrazione,totale,prima_dose,seconda_dose,pregressa_infezione)
vax <- subset(vax,vax$data_somministrazione <= '2021-02-01')
vax <- rename(vax,dayly_vax = totale)
vax$totale_vax <- cumsum(vax$dayly_vax)
cov_vax <- merge(cov,vax, by.x = "data", by.y = "data_somministrazione", all.x = TRUE)
cov_vax[is.na(cov_vax)] <- 0


# plot the data
x11()
pairs(cov)

# plot the dependencies between ricoverati_con_sintomi terapia_intensiva totale_ospedalizzati
x11()
plot(cov$data,cov$ricoverati_con_sintomi,ylim = c(0,2500), col = "green")
lines(cov$data,cov$terapia_intensiva, col = "blue")
lines(cov$data,cov$totale_ospedalizzati, col = "red")

p1 <- ggplot(cov, aes(x=data)) +
  geom_line(aes(y = ricoverati_con_sintomi, colour = "ric")) +
  geom_line(aes(y = terapia_intensiva, colour = "ter")) +
  geom_line(aes(y = totale_ospedalizzati, colour = "tot")) +
  geom_density(adjust=2) +
  theme_ipsum()

x11()
p1

# we can discard totale_ospedalizzati

# same with totale_ospedalizzati isolamento_domiciliare totale_positivi
p2 <- ggplot(cov, aes(x=data)) +
  geom_line(aes(y = totale_ospedalizzati, colour = "osp")) +
  geom_line(aes(y = isolamento_domiciliare, colour = "casa")) +
  geom_line(aes(y = totale_positivi, colour = "tot")) +
  geom_density(adjust=2) +
  theme_ipsum()

x11()
p2

# we can discard totale_positivi

# should we take a look at all the variables wrt date?

# plot terapia_intensiva wrt all the variables
x11()
par(mfrow=c(3,4))
plot(cov$data,cov$terapia_intensiva)
plot(cov$ricoverati_con_sintomi,cov$terapia_intensiva)
#plot(cov$totale_ospedalizzati,cov$terapia_intensiva)
plot(cov$isolamento_domiciliare,cov$terapia_intensiva)
#plot(cov$totale_positivi,cov$terapia_intensiva)
plot(cov$nuovi_positivi,cov$terapia_intensiva)
plot(cov$dimessi_guariti,cov$terapia_intensiva)
plot(cov$deceduti,cov$terapia_intensiva)
plot(cov$totale_casi,cov$terapia_intensiva)
#plot(cov$tamponi,cov$terapia_intensiva) # explain
plot(cov$casi_testati,cov$terapia_intensiva)

# we notice an outlier
watchout_point <- max(cov$terapia_intensiva)

x11()
par(mfrow=c(3,4))
plot(cov$data,cov$terapia_intensiva)
points(cov$data[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$ricoverati_con_sintomi,cov$terapia_intensiva)
points(cov$ricoverati_con_sintomi[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$totale_ospedalizzati,cov$terapia_intensiva)
points(cov$totale_ospedalizzati[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$isolamento_domiciliare,cov$terapia_intensiva)
points(cov$isolamento_domiciliare[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$totale_positivi,cov$terapia_intensiva)
points(cov$totale_positivi[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$nuovi_positivi,cov$terapia_intensiva)
points(cov$nuovi_positivi[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$dimessi_guariti,cov$terapia_intensiva)
points(cov$dimessi_guariti[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$deceduti,cov$terapia_intensiva)
points(cov$deceduti[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$totale_casi,cov$terapia_intensiva)
points(cov$totale_casi[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$tamponi,cov$terapia_intensiva)
points(cov$tamponi[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)
plot(cov$casi_testati,cov$terapia_intensiva)
points(cov$casi_testati[which(cov$terapia_intensiva == watchout_point)],watchout_point, col = "red", pch = 16)

# now we have to choose if we want to exclude the outlier. in my opinion yes


# correlation analysis
corr <- cor(select(cov, -data))
corr
x11()
corrplot(corr, method = "ellipse")
x11()
chart.Correlation(cov[,c("ricoverati_con_sintomi","terapia_intensiva","isolamento_domiciliare",
                          "nuovi_positivi","dimessi_guariti","deceduti","totale_casi",
                            "tamponi","casi_testati")])



# # fit the complete linear model
# lm1 <- lm(terapia_intensiva ~ ricoverati_con_sintomi+totale_ospedalizzati+isolamento_domiciliare+
#          totale_positivi+nuovi_positivi+dimessi_guariti+deceduti+totale_casi+tamponi+casi_testati,
#          data = cov)
# lm1
# 
# #VIF
# vif(lm1)
# # we ran into perfect collinearity
# 
# x11()
# avPlots(lm1,layout=c(2,5))
# 
# 
# # we still have some correlation. We do a stepwise procedure to choose the model
# 
# lm2 <- lm(terapia_intensiva ~ totale_ospedalizzati+
#             totale_positivi+nuovi_positivi+dimessi_guariti+totale_casi,
#           data = cov)
# summary(lm2)
# 
# 
# 








