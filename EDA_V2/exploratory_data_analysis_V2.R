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
  geom_line(mapping = aes(x=Date, y = Hospitalized.with.symptoms, color = "Hospitalized.with.symptoms"),size=1.1) +
  geom_line(mapping = aes(x=Date, y = ICU, color = "ICU"),size=1.1) +
  geom_line(mapping = aes(x=Date, y = Total.Hospitalized, color = "Total.Hospitalized"),size=1.1) +
  ggtitle('Hospitalized.with.symptoms, ICU and Total.Hospitalized over time') +
  theme(axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5,size=18),
        legend.title=element_text(size=14))

x11()
p1

ggsave(p1, filename = "img/Tot_hosp_wrt_Date.png")

# we can discard Total.Hospitalized


# same with Total.Hospitalized People.at.home Total.positives
p2 <- ggplot(cov) +
  geom_line(mapping = aes(x=Date, y = Total.Hospitalized, color = "Total.Hospitalized"),size=1.1) +
  geom_line(mapping = aes(x=Date, y = People.at.home, color = "People.at.home"),size=1.1) +
  geom_line(mapping = aes(x=Date, y = Total.positives, color = "Total.positives"),size=1.1) +
  ggtitle('Hospitalized.with.symptoms, People.at.home and Total.positives over time') +
  theme(axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5,size=18),
        legend.title=element_text(size=14))

x11()
p2

ggsave(p2, filename = "img/Tot_pos_wrt_Date.png")

# we can discard Total.positives 
cov <- dplyr::select(cov, -Total.Hospitalized, -Total.positives)

# let's take a look at all the variables wrt date
names(cov)
date_TI <- ggplot(cov, aes(x=Date, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_RCS <- ggplot(cov, aes(x=Date, y=Hospitalized.with.symptoms, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_ID <- ggplot(cov, aes(x=Date, y=People.at.home, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_NP <- ggplot(cov, aes(x=Date, y=New.positives, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_DG <- ggplot(cov, aes(x=Date, y=Discharged.healed, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_D <- ggplot(cov, aes(x=Date, y=Deceased, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_TC <- ggplot(cov, aes(x=Date, y=Total.cases, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_T <- ggplot(cov, aes(x=Date, y=Covid.tests, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_CT <- ggplot(cov, aes(x=Date, y=Cases.tested, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_PV <- ggplot(cov, aes(x=Date, y=Percentage.vaccinated, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),,legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))

p <- ggarrange(date_TI,date_RCS,date_ID,date_NP,date_DG,date_D,date_TC,
          date_T,date_CT,date_PV,
          ncol = 3, nrow = 4)
x11()
p

ggsave(p, filename = "img/all_wrt_Date.png")

# we notice some outliers
watchout_point_TI <- max(cov$ICU)

# we choose to discard them
cov <- cov[-which.max(cov$ICU),]

# ploot again

date_TI <- ggplot(cov, aes(x=Date, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_RCS <- ggplot(cov, aes(x=Date, y=Hospitalized.with.symptoms, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_ID <- ggplot(cov, aes(x=Date, y=People.at.home, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_NP <- ggplot(cov, aes(x=Date, y=New.positives, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_DG <- ggplot(cov, aes(x=Date, y=Discharged.healed, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_D <- ggplot(cov, aes(x=Date, y=Deceased, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_TC <- ggplot(cov, aes(x=Date, y=Total.cases, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_T <- ggplot(cov, aes(x=Date, y=Covid.tests, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_CT <- ggplot(cov, aes(x=Date, y=Cases.tested, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
date_PV <- ggplot(cov, aes(x=Date, y=Percentage.vaccinated, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))

p <- ggarrange(date_TI,date_RCS,date_ID,date_NP,date_DG,date_D,date_TC,
          date_T,date_CT,date_PV,
          ncol = 3, nrow = 4)

x11()
p

ggsave(p, filename = "img/all_wrt_Date_no_outliers.png")


# plot ICU wrt all the variables
names(cov)
date_TI <- ggplot(cov, aes(x=Date, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
RCS_TI <- ggplot(cov, aes(x=Hospitalized.with.symptoms, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
ID_TI <- ggplot(cov, aes(x=People.at.home, y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
NP_TI <- ggplot(cov, aes(x=New.positives,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
DG_TI <- ggplot(cov, aes(x=Discharged.healed,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
D_TI <- ggplot(cov, aes(x=Deceased,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
PV_TI <- ggplot(cov, aes(x=Percentage.vaccinated,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
TC_TI <- ggplot(cov, aes(x=Total.cases,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
T_TI <- ggplot(cov, aes(x=Covid.tests,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))
CT_TI <- ggplot(cov, aes(x=Cases.tested,y=ICU, color=Color)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme(axis.text = element_text(size = 8),legend.position = "none",
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour= "grey"),
        panel.background = element_rect(fill = 'white', colour = 'white'))

p <- ggarrange(RCS_TI,ID_TI,NP_TI,DG_TI,D_TI ,PV_TI, TC_TI, T_TI, CT_TI,
          ncol = 3, nrow = 3)

x11()
p

ggsave(p, filename = "img/ICU_wrt_all.png")


##prova
#x11()
#ggplot(cov, aes(x=log(Discharged.healed),y=ICU, color=Color)) + 
#  geom_point(size=3) +
#  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
#  theme(axis.text = element_text(size = 8),legend.position = "none",
#        panel.grid.major = element_line(colour = "grey"),
#        panel.grid.minor = element_line(colour= "grey"),
#        panel.background = element_rect(fill = 'white', colour = 'white'))
#



# we also choose not to consider Total.cases and Covid.tests and Cases.tested
# because they are cumulative variables that we are not particularly interested in
cov <- dplyr::select(cov, -Total.cases, -Covid.tests, -Cases.tested)


test_cov<-filter(cov,cov$Date>="2021-02-02",cov$Date<="2021-02-15")

cov<-filter(cov,cov$Date>="2020-10-01",cov$Date<="2021-02-01")

#write.csv(cov,"cov_post_EDA.csv")
#write.csv(test_cov,"test_cov.csv")
