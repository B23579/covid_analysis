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
library(hrbrthemes)
library(ggpubr)
library(dplyr)
library(MASS)


###############################################################################
#                          DATASET PREPARATION                                #
###############################################################################

# upload and prepare the dataset cov
#cov<-read.csv("https://raw.githubusercontent.com/pcm-dpc/
#COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
##view(cov)
#write.csv(cov,"Covid19.csv")

# import the dataset
cov_orig <- read.csv("Covid19.csv")

### in this project we are going to work with the region Campania, Intensive Care Units (terapia_intensiva) )
# select the variables/period of interest
cov<-filter(cov_orig,denominazione_regione=="Campania")
nrow(cov)
names(cov)
#view(cov)
cov<-dplyr::select(cov,data,ricoverati_con_sintomi,terapia_intensiva,ingressi_terapia_intensiva,
                  totale_ospedalizzati,isolamento_domiciliare,totale_positivi,
                  nuovi_positivi,dimessi_guariti,deceduti,totale_casi,tamponi,casi_testati)
#view(cov)
sum(is.na(cov$data))
cov$data<-as.Date(cov$data)
#view(cov)
cov<-filter(cov,cov$data>="2020-09-30",cov$data<="2021-02-15")
#view(cov)
nrow(cov)
sum(is.na(cov$ingressi_terapia_intensiva))
cov<-dplyr::select(cov, -ingressi_terapia_intensiva)
#view(cov)
cov <- cov[order(cov$data),]
cov<-filter(cov,cov$data>="2020-10-01",cov$data<="2021-02-15")


# upload and prepare the dataset vax and add it to cov
# https://github.com/pcm-dpc/COVID-19 ???
vax_orig <- read_csv("Vax.csv")
names(vax_orig)
vax <- subset(vax_orig,vax_orig$nome_area == "Campania")
vax <- dplyr::select(vax,data_somministrazione,totale)
vax <- subset(vax,vax$data_somministrazione <= '2021-02-15')
vax <- rename(vax,dayly_vax = totale)
vax <- vax[order(vax$data_somministrazione),]
vax$tot_vax <- cumsum(vax$dayly_vax)
pop_campania = 5624260 #https://www.tuttitalia.it/campania/statistiche/popolazione-andamento-demografico/
vax$perc_vax <- vax$tot_vax/pop_campania*100
cov <- merge(cov,vax, by.x = "data", by.y = "data_somministrazione", all.x = TRUE)
cov[is.na(cov)] <- 0
# for now I choose to consider only %of vax, then we can choose what we prefer
cov <- dplyr::select(cov, -tot_vax, -dayly_vax)

# upload ad add colore dataset
# https://github.com/imcatta/restrizioni_regionali_covid
color_orig <- read_csv("color.csv")
color <- subset(color_orig,color_orig$denominazione_regione == "Campania")
names(color)
color <- dplyr::select(color,"data","colore")
color <- subset(color,color$data <= '2021-02-15')
color <- subset(color,color$data >= '2020-10-01')
color <- color[order(color$data),]
cov <- merge(cov,color, by.x = "data", by.y = "data", all.x = TRUE)
cov[is.na(cov)] <- "pre_decreto"
cov$colore <- as.factor(cov$colore)

cov_completo <- cov

# Translate in english
levels(cov$colore) <- c('Orange', 'Yellow', 'Pre.decreto', 'Red')

names(cov) 

# data,ricoverati_con_sintomi,terapia_intensiva,totale_ospedalizzati,isolamento_domiciliare,
# totale_positivi,nuovi_positivi,dimessi_guariti,deceduti,totale_casi,tamponi,
# casi_testati,perc_vax,colore

# translation
names(cov)  <- c('Date'
                ,'Hospitalized.with.symptoms'
                ,'ICU'
                ,'Total.Hospitalized'
                ,'People.at.home'
                ,'Total.positives'
                ,'New.positives'
                ,'Discharged.healed'
                ,'Deceased'
                ,'Total.cases'
                ,'Covid.tests'
                ,'Cases.tested'
                ,'Percentage.vaccinated'
                ,'Color')

###############################################################################
#            SHOW THE VARIABLES AND CHOOSE WHICH ONE TO CONSIDER              #
###############################################################################

# plot the dependencies between Hospitalized.with.symptoms ICU Total.Hospitalized
p1 <- ggplot(cov) +
  geom_line(mapping = aes(x=Date, y = Hospitalized.with.symptoms, color = "Hospitalized.with.symptoms")) +
  geom_line(mapping = aes(x=Date, y = ICU, color = "ICU")) +
  geom_line(mapping = aes(x=Date, y = Total.Hospitalized, color = "Total.Hospitalized")) +
  theme_ipsum()

#x11()
p1
# we can discard Total.Hospitalized


# same with Total.Hospitalized People.at.home Total.positives
p2 <- ggplot(cov) +
  geom_line(mapping = aes(x=Date, y = Total.Hospitalized, color = "Total.Hospitalized")) +
  geom_line(mapping = aes(x=Date, y = People.at.home, color = "People.at.home")) +
  geom_line(mapping = aes(x=Date, y = Total.positives, color = "Total.positives")) +
  theme_ipsum()

#x11()
p2

# we can discard Total.positives 
cov <- dplyr::select(cov, -Total.Hospitalized, -Total.positives)

# let's take a look at all the variables wrt date
names(cov)
date_TI <- ggplot(cov, aes(x=Date, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_RCS <- ggplot(cov, aes(x=Date, y=Hospitalized.with.symptoms, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_ID <- ggplot(cov, aes(x=Date, y=People.at.home, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_NP <- ggplot(cov, aes(x=Date, y=New.positives, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_DG <- ggplot(cov, aes(x=Date, y=Discharged.healed, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_D <- ggplot(cov, aes(x=Date, y=Deceased, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_TC <- ggplot(cov, aes(x=Date, y=Total.cases, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_T <- ggplot(cov, aes(x=Date, y=Covid.tests, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_CT <- ggplot(cov, aes(x=Date, y=Cases.tested, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
date_PV <- ggplot(cov, aes(x=Date, y=Percentage.vaccinated, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))

x11()
ggarrange(date_TI,date_RCS,date_ID,date_NP,date_DG,date_D,date_TC,
          date_T,date_CT,date_PV,
          ncol = 3, nrow = 4)


# we notice some outliers
watchout_point_TI <- max(cov$ICU)

# we choose to discard them
cov <- cov[-which.max(cov$ICU),]

# ploot again

# plot ICU wrt all the variables
names(cov)
date_TI <- ggplot(cov, aes(x=Date, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
RCS_TI <- ggplot(cov, aes(x=Hospitalized.with.symptoms, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
ID_TI <- ggplot(cov, aes(x=People.at.home, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
NP_TI <- ggplot(cov, aes(x=New.positives,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
DG_TI <- ggplot(cov, aes(x=Discharged.healed,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
D_TI <- ggplot(cov, aes(x=Deceased,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
PV_TI <- ggplot(cov, aes(x=Percentage.vaccinated,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
TC_TI <- ggplot(cov, aes(x=Total.cases,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
T_TI <- ggplot(cov, aes(x=Covid.tests,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))
CT_TI <- ggplot(cov, aes(x=Cases.tested,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum(axis_text_size = 8,plot_margin = ggplot2::margin(10, 10, 10, 10))

x11()
ggarrange(RCS_TI,ID_TI,NP_TI,DG_TI,D_TI ,PV_TI, TC_TI, T_TI, CT_TI,
          ncol = 3, nrow = 3)





# we also choose not to consider Total.cases and Covid.tests and Cases.tested
# because they are cumulative variables that we are not particularly interested in
cov <- dplyr::select(cov, -Total.cases, -Covid.tests, -Cases.tested)


test_cov<-filter(cov,cov$Date>="2021-02-02",cov$Date<="2021-02-15")

cov<-filter(cov,cov$Date>="2020-10-01",cov$Date<="2021-02-01")

#write.csv(cov,"cov_post_EDA.csv")
#write.csv(test_cov,"test_cov.csv")


###############################################################################
#                              SECOND OPTION                                  #
###############################################################################

# we know that Hospitalized.with.symptoms + ICU = Total.Hospitalized
# and Total.Hospitalized + People.at.home = Total.positives
# till now we condidered the Date divided in categories. Now I want to see what
# happens if we consider the grouped Date: Total.positives
###
###names(cov_completo)
###cov_group <- dplyr::select(cov_completo, -Hospitalized.with.symptoms, -Total.Hospitalized, -People.at.home, -Covid.tests,
###                           -Cases.tested, 
###                           -ICU_ieri, -New.positives_norm, -Total.cases)
###
###names(cov_group)
###
#### plot ICU wrt all the variables
###date_TI <- ggplot(cov_completo, aes(x=Date, y=ICU, color=Color)) + 
###  geom_point(size=3) +
###  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
###  theme_ipsum()
###TP_TI <- ggplot(cov_completo, aes(x=Total.positives, y=ICU, color=Color)) + 
###  geom_point(size=3) +
###  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
###  theme_ipsum()
###NP_TI <- ggplot(cov_completo, aes(x=New.positives,y=ICU, color=Color)) + 
###  geom_point(size=3) +
###  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
###  theme_ipsum()
###DG_TI <- ggplot(cov_completo, aes(x=Discharged.healed,y=ICU, color=Color)) + 
###  geom_point(size=3) +
###  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
###  theme_ipsum()
###D_TI <- ggplot(cov_completo, aes(x=Deceased,y=ICU, color=Color)) + 
###  geom_point(size=3) +
###  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
###  theme_ipsum()
###PV_TI <- ggplot(cov_completo, aes(x=Percentage.vaccinated,y=ICU, color=Color)) + 
###  geom_point(size=3) +
###  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
###  theme_ipsum()
###
###
###x11()
###ggarrange(date_TI,TP_TI,NP_TI,DG_TI,D_TI ,PV_TI,
###          ncol = 3, nrow = 2)
###
#### discard outliers
###cov_group <- cov_group[-which.max(cov_group$ICU),]
###
###
###
###write.csv(cov_group,"cov_group_post_EDA.csv")
###
###
###
###
###
###