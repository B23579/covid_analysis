cov<-read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
view(cov)
setwd("D:/University of Trieste/statistical Data Science/Final project/covid_analysis")

### in this project we are going to work with the region Campania, Intensive Care Units (terapia_intensiva) )

library(tidyverse)

covfilter<-filter(cov,denominazione_regione=="Campania")

nrow(covfilter)

names(covfilter)

view(covfilter)

covselect<-select(covfilter,data,ricoverati_con_sintomi,terapia_intensiva,ingressi_terapia_intensiva,totale_ospedalizzati,isolamento_domiciliare,totale_positivi,
                  nuovi_positivi,dimessi_guariti,deceduti,totale_casi,tamponi,casi_testati)

# check the missing value

sum(is.na(covselect$data))

covselect$data<-as.Date(covselect$data)
#view(covselect)

Cov<-filter(covselect,covselect$data>="2020-10-01",covselect$data<="2021-02-01")
view(Cov)

#nrow(Cov)
sum(is.na(Cov$ingressi_terapia_intensiva))

Data_t<-as_tibble(select(Cov, -ingressi_terapia_intensiva))

str(Data_t)
# let's check the outlier using the box plot 

# total population estimation in campagna on 2021-01-01 is 5,679,759

summary(Data_t)
## visualization



p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=casi_testati,color="casi_testati"))+
  geom_line(mapping =aes(x=data, y=tamponi,color="tamponi"))
p
 
ggsave(p, filename = "visualization/casi_testi_tamponi.png")

p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=terapia_intensiva))
p
ggsave(p, filename = "visualization/intensive_terapia.png")



boxplot(Data_t)  

dar<-select(Data_t, -casi_testati, -tamponi)  
boxplot(dar)  


# feature importance using buruto 

library(Boruta)
library(mlbench)

library(randomForest)
set.seed(111)
Dat<-select(Data_t,-data)
boruto<-Boruta(terapia_intensiva~.,data = Dat,doTrace=2, maxRuns=100)
print(boruto)
p<-plot(boruto,las=2,cex.axis=0.5)
ggsave(p, filename = "visualization/feature_importance.png")
P<-plotImpHistory(boruto)
bor<-TentativeRoughFix(boruto)
bor
getNonRejectedFormula(boruto)

###########

### Let's split the data in train and test data set###
### our test data should be the last 15 day of the selected data set

train<-filter(Cov,Cov$data<="2021-01-16")
test<-filter(Cov,Cov$data>"2021-01-16")

view(test)
plot(test$data,test$terapia_intensiva)


library(mgcv)

library(PerformanceAnalytics)
chart.Correlation(Dat)
names(Dat)

### Model GAM : Generalized additive model ##

mod_lm <- gam(terapia_intensiva ~ s(ricoverati_con_sintomi) + s(totale_ospedalizzati)+  
                s(isolamento_domiciliare)+ s(totale_positivi)+ s(nuovi_positivi)+        
                s(dimessi_guariti)+ s(deceduti)+ (totale_casi)+s(tamponi), data=train)
summary(mod_lm)
plot(mod_lm,residuals = TRUE, pages = 1, pch = 19)

## plot the prediction and the target on the same plot####

pred<-predict(mod_lm, test)
pred[2]


new<-mutate(test,fittes=pred)

####
view(new)

p<-ggplot(new)+
  geom_line(aes(data,terapia_intensiva,,color="True"))+
  geom_point(aes(data,fittes,color= "Predicted"))
p

ggsave(p, filename = "visualization/GAM_result.png")
     

