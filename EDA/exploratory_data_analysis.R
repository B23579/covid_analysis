cov<-read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
view(cov)
setwd("D:/University of Trieste/statistical Data Science/Final project/covid_analysis")

### in this project we are going to work with the region Campania, Intensive Care Units (terapia_intensiva) )

library(tidyverse)

covfilter<-filter(cov,denominazione_regione=="Campania")

nrow(covfilter)

# totale_positivi is the Total amount of current positive cases
#(Hospitalised patients + Home confinement). so in our analysis, we will remove it

#nuovi_positivi New amount of current positive cases 
# (totale_casi current day - totale_casi previous day), so we will use only new case 


covselect<- as_tibble(select(covfilter,data,variazione_totale_positivi,ricoverati_con_sintomi,terapia_intensiva,ingressi_terapia_intensiva,totale_ospedalizzati,isolamento_domiciliare,
                  nuovi_positivi,dimessi_guariti,deceduti,totale_casi,tamponi,casi_testati,-ingressi_terapia_intensiva))

view(covselect)
# check the missing value

sum(is.na(covselect$data))

covselect$data<-as.Date(covselect$data)
#view(covselect)

Data_t<-filter(covselect,covselect$data>="2020-10-01",covselect$data<="2021-02-15")
view(Data_t)

str(Data_t)
# let's check the outlier using the box plot 

# total population estimation in campagna on 2021-01-01 is 5,679,759

summary(Data_t)

## From the summary, we can notice the the total cases are 4.2 % of the population, 
## the number of people teste are 33.06% of the population, tamponi respresent 47.71 %
## deceduti represent 0.07 % , dimessi_guariti 3 %
## nuovi_positivi 0.0081 %

## totale_positivi 1.84 %, isolamento_domiciliare 1.8, 0.003996648, terapia_intensiva 0.0039 %



## visualization

p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=casi_testati,color="Total number of people tested"))+
  geom_line(mapping =aes(x=data, y=tamponi,color="Tests performed"))+
  labs(x=" Periode", y= " Count ", title = "Train of Covid-19 test Oct-2020-Feb-2021")+
  theme(plot.title = element_text(hjust = 0.5))
p


 
ggsave(p, filename = "visualization/casi_testi_tamponi.png")

p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=terapia_intensiva,color="intensive care"))+
  geom_line(mapping =aes(x=data, y=totale_ospedalizzati,color="Total hospitalised patients"))+
   labs(x=" Periode", y= " Number of people in intensive care ", title = " Patients in intensive care and hospitalised patients during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ people_in_intensive_care_and_hospitalize.png")


p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=terapia_intensiva,color="intensive care"))+
  geom_line(mapping =aes(x=data, y=totale_ospedalizzati,color="Total hospitalised patients"))+
  geom_line(mapping =aes(x=data, y=isolamento_domiciliare,color="Home confinement"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = "  Home confinement during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ Home_confinement.png")





p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=terapia_intensiva))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Patients in intensive care during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ people_in_intensive_care.png")


# feature importance using buruto (to check)

#variazione_totale_positivi New amount of current positive cases

#(totale_positivi current day - totale_positivi previous day)


#nuovi_positivi New amount of current positive cases
#(totale_casi current day - totale_casi previous day)

l<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=nuovi_positivi), color="red")+
  labs(x=" Periode", y= " New amount of current positive cases ", title = "New amount of current positive Covid-19 during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(l, filename = "visualization/New_amount_of_current_positive_cases.png")

l<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=deceduti), color="red")+
  labs(x=" Periode", y= " Nomber of death ", title = "Cumulative number of death due to Covid-19 during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

l

ggsave(l, filename = "visualization/death_cases.png")



## in the next step, we are interested in the number of new New amount of current positive cases

boxplot(Data_t)  

dar<-select(Data_t,-deceduti,-terapia_intensiva, -data, -casi_testati,-dimessi_guariti, -tamponi,-totale_casi,-variazione_totale_positivi,-totale_ospedalizzati,-isolamento_domiciliare)  

ggplot(dar)+
  geom_boxplot(aes(nuovi_positivi))+
  geom_boxplot(aes(ricoverati_con_sintomi,color="red"))
  
boxplot(dar)  

# from the boxplot, we can identifier the outliers for our explanatory variable, 
# so we should remove the outliers  
### Model ####

Dataset<-mutate(dar,data = Data_t$data,terapia_intensiva=Data_t$terapia_intensiva)

 view(Dataset)

p<-ggplot(Dataset,mapping = aes(ricoverati_con_sintomi,nuovi_positivi))+
  geom_point(color="blue")+ 
  labs(x=" Hospitalised patients with symptoms", y= " New amount of current positive cases ")
p

ggsave(p, filename = "visualization/corpoint.png")

### Let's split the data in train and test data set###
### our test data should be the last 15 day of the selected data set

train<-filter(Dataset,Dataset$data<="2021-02-01")
test<-filter(Dataset,Dataset$data>"2021-02-01")



library(mgcv)

library(PerformanceAnalytics)
chart.Correlation(Dat)


### Model GAM : Generalized additive model ##

mod_lm <- gam(terapia_intensiva ~ s(ricoverati_con_sintomi) + s(nuovi_positivi), data=train)
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
  geom_line(aes(data,fittes,color= "Predicted"))
p

ggsave(p, filename = "visualization/GAM_result.png")
     

