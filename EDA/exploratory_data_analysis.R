cov<-read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
setwd("D:/University of Trieste/statistical Data Science/Final project/covid_analysis")

### in this project we are going to work with the region Campania, Intensive Care Units (terapia_intensiva) )

library(tidyverse)
library(mice)
covfilter<-filter(cov,denominazione_regione=="Campania")

nrow(covfilter)
view(covfilter)

# totale_positivi is the Total amount of current positive cases
#(Hospitalised patients + Home confinement). so in our analysis, we will remove it

#nuovi_positivi New amount of current positive cases 
# (totale_casi current day - totale_casi previous day), so we will use only new case 


covselect<- as_tibble(select(covfilter,data,variazione_totale_positivi,ricoverati_con_sintomi,terapia_intensiva,totale_ospedalizzati,isolamento_domiciliare,
                  nuovi_positivi,dimessi_guariti,deceduti,totale_casi,tamponi,casi_testati,ingressi_terapia_intensiva,totale_positivi))

view(covselect)
# check the missing value

sum(is.na(covselect$data))


covselect$data<-as.Date(covselect$data)
#view(covselect)

Data_t<-filter(covselect,covselect$data>="2020-10-01",covselect$data<="2021-02-15")
view(Data_t)
nrow(Data_t)

sum(is.na(Data_t$ingressi_terapia_intensiva))

# Daily admissions to intensive care has 63 missing value around 45.6 % of raw data 

# let's handle missing value using imputation method because removing 
# will reduce the size of the dataset 

# Let's handle missing value 
# we have two possibility 
# 1- simply exclude cases with missing data from the analysis, this will end up with droping 


# 2- apply an imputation mechanism to fill in the gaps

t <- mice(Data_t, m=3, maxit = 4, method = 'cart', seed = 500)
Data_t<-complete(t)

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

a<-c(0)

for (i in 1:nrow(Data_t)) {
  if(i!=1){
    a<-c(a,abs(Data_t$deceduti[i]-Data_t$deceduti[i-1]))
  }
}

Data_t<-mutate(Data_t,death=a)

## visualization

p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=casi_testati,color="Total number of people tested"))+
  geom_line(mapping =aes(x=data, y=tamponi,color="Tests performed"))+
  geom_line(mapping =aes(x=data, y=totale_positivi,color="Current positive cases"))+
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

###
p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=terapia_intensiva,color="Intensive care"))+
  geom_line(mapping =aes(x=data, y=death,color="Death"))+
  geom_line(mapping =aes(x=data, y=dimessi_guariti,color="Recovered"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Patients in intensive care and hospitalised patients during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ people_in_intensive_care_death_and_hospitalize.png")

###
p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=deceduti,color="Cumulated death values"))+
  geom_line(mapping =aes(x=data, y=dimessi_guariti,color="Recovered"))+
  labs(x=" Periode", y= " Number of people ", title = " Patients in intensive care and hospitalised patients during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ recover.png")

###
p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=deceduti,color="Cumulated death values"))+
  labs(x=" Periode", y= " Number of people ", title = " Cumulative death during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ cumulative_death.png")





####
p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=terapia_intensiva,color="intensive care"))+
  geom_line(mapping =aes(x=data, y=ingressi_terapia_intensiva,color="Daily admissions to intensive care"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Patients in intensive care during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ people_in_intensive_care_and_Daily_admission.png")



p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=terapia_intensiva,color="intensive care"))+
  geom_line(mapping =aes(x=data, y=totale_ospedalizzati,color="Total hospitalised patients"))+
  geom_line(mapping =aes(x=data, y=ricoverati_con_sintomi,color="Hospitalised patients with symptoms"))+
  geom_line(mapping =aes(x=data, y=isolamento_domiciliare,color="Home confinement"))+
  geom_line(mapping =aes(x=data, y=totale_positivi,color="current positive cases"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = "  Home confinement during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ Home_confinement.png")

p<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=terapia_intensiva,color="intensive care"))+
  geom_line(mapping =aes(x=data, y=totale_ospedalizzati,color="Total hospitalised patients"))+
  geom_line(mapping =aes(x=data, y=ricoverati_con_sintomi,color="Hospitalised patients with symptoms"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Hospitalised patients during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

p

ggsave(p, filename = "visualization/ Hospitalised patients.png")





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
l
ggsave(l, filename = "visualization/New_amount_of_current_positive_cases.png")

l<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=deceduti), color="red")+
  labs(x=" Periode", y= " Nomber of death ", title = "Cumulative number of death due to Covid-19 during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

l

ggsave(l, filename = "visualization/death_cases.png")


l<-ggplot(Data_t)+
  geom_line(mapping =aes(x=data, y=death), color="red")+
  labs(x=" Periode", y= " Nomber of death ", title = "Death in intensive care due to Covid-19 during October 2020 and February 2021.")+
  theme(plot.title = element_text(hjust = 0.5))

l
ggsave(l, filename = "visualization/death_cases1.png")


## in the next step, we are interested in the number of new New amount of current positive cases

boxplot(Data_t)  

dar<-select(Data_t,-deceduti,-terapia_intensiva, -data, -casi_testati,-dimessi_guariti,-totale_positivi, -tamponi,-totale_casi,-variazione_totale_positivi,-isolamento_domiciliare)  

## deceduti is a cumulative variable, let's remove the cumulative funtion



view(dar)


boxplot(dar,horizontal = TRUE, cex.names=1,las=2)  

par(mar=c(5,11,1,1))
boxplot(dar,horizontal = TRUE, cex.names=1,las=2)

# from the boxplot, we can identifier the outliers for our explanatory variable, 
# so we should remove the outliers  
### Model ####

Dataset<-mutate(dar,data = Data_t$data,terapia_intensiva=Data_t$terapia_intensiva)


p<-ggplot(Dataset,mapping = aes(ricoverati_con_sintomi,nuovi_positivi))+
  geom_point(color="blue")+ 
  labs(x=" Hospitalised patients with symptoms", y= " New amount of current positive cases ")
p

ggsave(p, filename = "visualization/corpoint.png")


### Let's split the data in train and test data set###
### our test data should be the last 15 day of the selected data set

train<-filter(Dataset,Dataset$data<="2021-02-01")
test<-filter(Dataset,Dataset$data>"2021-02-01")

nrow(train)

## outliers 

outlier_norm <- function(x){
  qntile <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qntile[1] - H)] <- caps[1]
  x[x > (qntile[2] + H)] <- caps[2]
  return(x)
}
train$nuovi_positivi=outlier_norm(train$nuovi_positivi)
boxplot(train$nuovi_positivi)

train$ricoverati_con_sintomi=outlier_norm(train$ricoverati_con_sintomi)
boxplot(train$ricoverati_con_sintomi)

#############
library(PerformanceAnalytics)
chart.Correlation(Dat)

names(train)
# destribution of covariate 
ggplot(train)+
  geom_density(aes(ricoverati_con_sintomi, fill="ricoverati_con_sintomi"))+
  geom_density(aes(nuovi_positivi, fill="nuovi_positivi"))

ggplot(train)+
  geom_density(aes(totale_ospedalizzati, fill="totale_ospedalizzati"))+
  geom_density(aes(death, fill="death"))
  #geom_density(aes(ingressi_terapia_intensiva, fill="ingressi_terapia_intensiva"))

ggplot(train)+
  geom_density(aes(death, fill="death"))+
  geom_density(aes(ingressi_terapia_intensiva, fill="ingressi_terapia_intensiva"))

  

plot(train$nuovi_positivi, train$terapia_intensiva)
plot(train$ricoverati_con_sintomi,train$terapia_intensiva)
plot(train$death,train$terapia_intensiva)

names(train)

library(data.table)

fwrite(train, "NEW_data/train.csv")
fwrite(test, "NEW_data/test.csv")

