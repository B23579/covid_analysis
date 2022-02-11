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
cov<-select(cov,data,ricoverati_con_sintomi,terapia_intensiva,ingressi_terapia_intensiva,
                  totale_ospedalizzati,isolamento_domiciliare,totale_positivi,
                  nuovi_positivi,dimessi_guariti,deceduti,totale_casi,tamponi,casi_testati)
#view(cov)
sum(is.na(cov$data))
cov$data<-as.Date(cov$data)
#view(cov)
cov<-filter(cov,cov$data>="2020-09-30",cov$data<="2021-02-01")
#view(cov)
nrow(cov)
sum(is.na(cov$ingressi_terapia_intensiva))
cov<-select(cov, -ingressi_terapia_intensiva)
#view(cov)
cov <- cov[order(cov$data),]
cov$deceduti_daily <- c(cov$deceduti[1],diff(cov$deceduti))
cov$dimessi_guariti_dayly <- c(cov$dimessi_guariti[1],diff(cov$dimessi_guariti))
cov$casi_testati_daily <- c(cov$casi_testati[1],diff(cov$casi_testati))
cov<-filter(cov,cov$data>="2020-10-01",cov$data<="2021-02-01")
cov$nuovi_positivi_norm <- cov$nuovi_positivi/cov$casi_testati_daily


# upload and prepare the dataset vax and add it to cov
# https://github.com/pcm-dpc/COVID-19 ???
vax_orig <- read_csv("Vax.csv")
names(vax_orig)
vax <- subset(vax_orig,vax_orig$nome_area == "Campania")
vax <- select(vax,data_somministrazione,totale)
vax <- subset(vax,vax$data_somministrazione <= '2021-02-01')
vax <- rename(vax,dayly_vax = totale)
vax <- vax[order(vax$data_somministrazione),]
vax$tot_vax <- cumsum(vax$dayly_vax)
pop_campania = 5624260 #https://www.tuttitalia.it/campania/statistiche/popolazione-andamento-demografico/
vax$perc_vax <- vax$tot_vax/pop_campania*100
cov <- merge(cov,vax, by.x = "data", by.y = "data_somministrazione", all.x = TRUE)
cov[is.na(cov)] <- 0
# for now I choose to consider only %of vax, then we can choose what we prefer
cov <- select(cov, -tot_vax, -dayly_vax)

# upload ad add colore dataset
# https://github.com/imcatta/restrizioni_regionali_covid
color_orig <- read_csv("color.csv")
color <- subset(color_orig,color_orig$denominazione_regione == "Campania")
names(color)
color <- select(color,"data","colore")
color <- subset(color,color$data <= '2021-02-01')
color <- subset(color,color$data >= '2020-10-01')
color <- color[order(color$data),]
cov <- merge(cov,color, by.x = "data", by.y = "data", all.x = TRUE)
cov[is.na(cov)] <- "pre_decreto"
cov$colore <- as.factor(cov$colore)

cov_completo <- cov


###############################################################################
#            SHOW THE VARIABLES AND CHOOSE WHICH ONE TO CONSIDER              #
###############################################################################

# plot the dependencies between ricoverati_con_sintomi terapia_intensiva totale_ospedalizzati
p1 <- ggplot(cov) +
  geom_line(mapping = aes(x=data, y = ricoverati_con_sintomi, color = "ricoverati_con_sintomi")) +
  geom_line(mapping = aes(x=data, y = terapia_intensiva, color = "terapia_intensiva")) +
  geom_line(mapping = aes(x=data, y = totale_ospedalizzati, color = "totale_ospedalizzati")) +
  theme_ipsum()

x11()
p1
# we can discard totale_ospedalizzati


# same with totale_ospedalizzati isolamento_domiciliare totale_positivi
p2 <- ggplot(cov) +
  geom_line(mapping = aes(x=data, y = totale_ospedalizzati, color = "totale_ospedalizzati")) +
  geom_line(mapping = aes(x=data, y = isolamento_domiciliare, color = "isolamento_domiciliare")) +
  geom_line(mapping = aes(x=data, y = totale_positivi, color = "totale_positivi")) +
  theme_ipsum()

x11()
p2

# we can discard totale_positivi 
cov <- select(cov, -totale_ospedalizzati, -totale_positivi)

# let's take a look at all the variables wrt date
names(cov)
date_TI <- ggplot(cov, aes(x=data, y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_RCS <- ggplot(cov, aes(x=data, y=ricoverati_con_sintomi, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_ID <- ggplot(cov, aes(x=data, y=isolamento_domiciliare, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_NP <- ggplot(cov, aes(x=data, y=nuovi_positivi, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_DG <- ggplot(cov, aes(x=data, y=dimessi_guariti, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_D <- ggplot(cov, aes(x=data, y=deceduti, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_TC <- ggplot(cov, aes(x=data, y=totale_casi, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_T <- ggplot(cov, aes(x=data, y=tamponi, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_CT <- ggplot(cov, aes(x=data, y=casi_testati, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_PV <- ggplot(cov, aes(x=data, y=perc_vax, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()

#date_TI
#date_RCS
#date_ID
#date_NP
#date_DG
#date_D 
#date_TC
#date_T 
#date_CT
#date_PV

x11()
ggarrange(date_TI,date_RCS,date_ID,date_NP,date_DG,date_D,date_TC,
          date_T,date_CT,date_PV, 
          ncol = 3, nrow = 4)

# we also choose not to consider totale_casi and tamponi
# (see Walter Work to see how to consider the casi_testati variable)
# because they are cumulative variables that we are not particularly interested in
cov <- select(cov, -totale_casi, -tamponi, -casi_testati)

# we see what happens if we consider deceduti and dimessi_guariti on a daily basis
date_DGD <- ggplot(cov, aes(x=data, y=dimessi_guariti_dayly, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
date_DD <- ggplot(cov, aes(x=data, y=deceduti_daily, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()


x11()
ggarrange(date_TI,date_RCS,date_ID,date_NP,date_DGD,date_DD,date_PV, 
          ncol = 3, nrow = 3)

# should we consider nuovi_positivi or nuovi_positivi normalized?
date_NPN <- ggplot(cov, aes(x=data, y=nuovi_positivi_norm, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
x11()
ggarrange(date_NP,date_NPN,
          ncol = 2, nrow = 1)




# plot terapia_intensiva wrt all the variables
names(cov)
date_TI <- ggplot(cov, aes(x=data, y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
RCS_TI <- ggplot(cov, aes(x=ricoverati_con_sintomi, y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
ID_TI <- ggplot(cov, aes(x=isolamento_domiciliare, y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
NP_TI <- ggplot(cov, aes(x=nuovi_positivi,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
DG_TI <- ggplot(cov, aes(x=dimessi_guariti,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
D_TI <- ggplot(cov, aes(x=deceduti,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
PV_TI <- ggplot(cov, aes(x=perc_vax,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()

x11()
ggarrange(date_TI,RCS_TI,ID_TI,NP_TI,DG_TI,D_TI ,PV_TI, 
          ncol = 3, nrow = 3)

# we notice an outlier
watchout_point <- max(cov$terapia_intensiva)
# now we have to choose if we want to exclude the outlier. in my opinion yes

# we see what happens if we consider deceduti and dimessi_guariti on a daily basis
DGD_TI <- ggplot(cov, aes(x=dimessi_guariti_dayly,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
DD_TI <- ggplot(cov, aes(x=deceduti_daily,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()

x11()
ggarrange(date_TI,RCS_TI,ID_TI,NP_TI,DGD_TI,DD_TI ,PV_TI, 
          ncol = 3, nrow = 3)

# should we consider nuovi_positivi or nuovi_positivi normalized?
NPN_TI <- ggplot(cov, aes(x=nuovi_positivi_norm,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
x11()
ggarrange(NP_TI,NPN_TI, 
          ncol = 2, nrow = 1)
# Discuss together these results




###############################################################################
#                         CORRELATION ANALYSIS                                #
###############################################################################


# correlation analysis
#corr <- cor(select(cov, -data, -colore))
#corr
#x11()
#corrplot(corr, method = "ellipse")
#names(cov)
x11()
chart.Correlation(cov[,c("ricoverati_con_sintomi","terapia_intensiva","isolamento_domiciliare",
                          "nuovi_positivi","dimessi_guariti","deceduti","perc_vax")])
x11()
chart.Correlation(cov[,c("ricoverati_con_sintomi","terapia_intensiva","isolamento_domiciliare",
                         "nuovi_positivi","dimessi_guariti_dayly","deceduti_daily","perc_vax")])

# Discuss together these results


###############################################################################
#                               FIT LM                                        #
###############################################################################




###############################################################################
#                               FIT GLM                                       #
###############################################################################

# Remark: the number of people in intensive care is always greater than 0: 
# it is inappropriate to consider a linear model since it can also predict
# negative value -> Poisson regression

watchout_point <- which.max(cov$terapia_intensiva)
cov <- cov[-watchout_point, ] # Removing the outlier

# Trying out GLM with different combination
fit_glm_all <- glm(terapia_intensiva ~ . - data, data = cov, family = poisson)
fit_glm_no_color <- glm(terapia_intensiva ~ . - colore - data, data = cov, 
                        family = poisson)
fit_glm_high_correlation <- glm(terapia_intensiva ~ ricoverati_con_sintomi + 
                                nuovi_positivi + deceduti, data = cov, 
                                family = poisson)

fit_glm_all # AIC: 950.6 <- Best
fit_glm_no_color # AIC: 991.5
fit_glm_high_correlation # AIC: 1009 <- Worse

# Easiest one to plot
fitted_values_high_correlation <- exp(fit_glm_high_correlation$coefficients[1] + 
                                      fit_glm_high_correlation$coefficients[2] * cov$ricoverati_con_sintomi +
                                      fit_glm_high_correlation$coefficients[3] * cov$nuovi_positivi +  
                                      fit_glm_high_correlation$coefficients[4] * cov$deceduti)

plot(cov$data, cov$terapia_intensiva, ylim = c(0, 250))
points(cov$data, fitted_values_high_correlation, type = 'l')

###############################################################################
#                               FIT GAM                                       #
###############################################################################









###############################################################################
#                              SECOND OPTION                                  #
###############################################################################

# we know that ricoverati_con_sintomi + terapia_intensiva = totale_ospedalizzati
# and totale_ospedalizzati + isolamento_domiciliare = totale_positivi
# till now we condidered the data divided in categories. Now I want to see what
# happens if we consider the grouped data: totale_positivi

names(cov_completo)
cov_group <- dplyr::select(cov_completo, -ricoverati_con_sintomi, -totale_ospedalizzati, -isolamento_domiciliare, -tamponi,
                           -casi_testati, -deceduti_daily, -dimessi_guariti_dayly, -casi_testati_daily,
                           -terapia_intensiva_ieri, -nuovi_positivi_norm, -totale_casi)

names(cov_group)

# plot terapia_intensiva wrt all the variables
date_TI <- ggplot(cov_completo, aes(x=data, y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
TP_TI <- ggplot(cov_completo, aes(x=totale_positivi, y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
NP_TI <- ggplot(cov_completo, aes(x=nuovi_positivi,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
DG_TI <- ggplot(cov_completo, aes(x=dimessi_guariti,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
D_TI <- ggplot(cov_completo, aes(x=deceduti,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()
PV_TI <- ggplot(cov_completo, aes(x=perc_vax,y=terapia_intensiva, color=colore)) + 
  geom_point(size=3) +
  scale_color_manual(values=c("orange", "yellow", "grey", "red")) +
  theme_ipsum()


x11()
ggarrange(date_TI,TP_TI,NP_TI,DG_TI,D_TI ,PV_TI,
          ncol = 3, nrow = 2)

# discard outliers
cov_group <- cov_group[-which.max(cov_group$terapia_intensiva),]



write.csv(cov_group,"cov_group_post_EDA.csv")





