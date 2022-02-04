# STATISTICS PROJECT ##########################################################
# In this project we are going to work with the region Campania, Intensive Care
# Units ('terapia_intensiva')

# Load libraries
library(tidyverse)
library(corrplot)
library(PerformanceAnalytics)
library(car)
library(ggplot2)
library(hrbrthemes)
library(ggpubr)
library(dplyr)
library(MASS)

# DATASET PREPARATION #########################################################

# Upload and prepare the dataset cov
# Import the dataset
cov_orig <- read.csv("Covid19.csv")

# Select the variables/period of interest
cov<-filter(cov_orig,denominazione_regione=="Campania")
nrow(cov)
names(cov)
cov<-dplyr::select(cov,data, ricoverati_con_sintomi, terapia_intensiva, 
                   ingressi_terapia_intensiva, totale_ospedalizzati,
                   isolamento_domiciliare, totale_positivi, nuovi_positivi,
                   dimessi_guariti, deceduti, totale_casi, tamponi, 
                   casi_testati)
sum(is.na(cov$data))
cov$data<-as.Date(cov$data)
cov<-filter(cov,cov$data>="2020-09-30",cov$data<="2021-02-01")
nrow(cov)
sum(is.na(cov$ingressi_terapia_intensiva))
cov<-dplyr::select(cov, -ingressi_terapia_intensiva)
cov <- cov[order(cov$data),]
cov$deceduti_daily <- c(cov$deceduti[1],diff(cov$deceduti))
cov$dimessi_guariti_dayly <- c(cov$dimessi_guariti[1], 
                               diff(cov$dimessi_guariti))
cov$casi_testati_daily <- c(cov$casi_testati[1], diff(cov$casi_testati))
cov$terapia_intensiva_ieri <- c(NA, head(cov$terapia_intensiva, 
                                         length(cov$terapia_intensiva) - 1))
cov<-filter(cov,cov$data>="2020-10-01",cov$data<="2021-02-01")
cov$nuovi_positivi_norm <- cov$nuovi_positivi/cov$casi_testati_daily

# Upload and prepare the dataset vax and add it to cov
# https://github.com/pcm-dpc/COVID-19 (?)
vax_orig <- read_csv("Vax.csv")
names(vax_orig)
vax <- subset(vax_orig,vax_orig$nome_area == "Campania")
vax <- dplyr::select(vax,data_somministrazione,totale)
vax <- subset(vax,vax$data_somministrazione <= '2021-02-01')
vax <- rename(vax,dayly_vax = totale)
vax <- vax[order(vax$data_somministrazione),]
vax$tot_vax <- cumsum(vax$dayly_vax)
pop_campania = 5624260 # https://www.tuttitalia.it/campania/statistiche/popolazione-andamento-demografico/
vax$perc_vax <- vax$tot_vax/pop_campania*100
cov <- merge(cov,vax, by.x = "data", by.y = "data_somministrazione",
             all.x = TRUE)
cov[is.na(cov)] <- 0
# For now I choose to consider only %of vax, then we can choose what we prefer
cov <- dplyr::select(cov, -tot_vax, -dayly_vax)

# upload ad add colore dataset
# https://github.com/imcatta/restrizioni_regionali_covid
color_orig <- read_csv("color.csv")
color <- subset(color_orig,color_orig$denominazione_regione == "Campania")
names(color)
color <- dplyr::select(color,"data","colore")
color <- subset(color,color$data <= '2021-02-01')
color <- subset(color,color$data >= '2020-10-01')
color <- color[order(color$data),]
cov <- merge(cov,color, by.x = "data", by.y = "data", all.x = TRUE)
cov[is.na(cov)] <- "pre_decreto"
cov$colore <- as.factor(cov$colore)

# SHOW THE VARIABLES AND CHOOSE WHICH ONE TO CONSIDER #########################

# we can discard totale_positivi 
cov <- dplyr::select(cov, -totale_ospedalizzati, -totale_positivi)

# we also choose not to consider totale_casi and tamponi
# (see Walter Work to see how to consider the casi_testati variable)
# because they are cumulative variables that we are not particularly interested in
cov <- dplyr::select(cov, -totale_casi, -tamponi, -casi_testati)

# we notice some outliers
watchout_point_TI <- max(cov$terapia_intensiva)
watchout_point_TII <- max(cov$terapia_intensiva_ieri)
# we choose to discard them
cov <- cov[-which.max(cov$terapia_intensiva),]
cov <- cov[-which.max(cov$terapia_intensiva_ieri),]

# Discuss together these results
# we prefer to keep the original variables, because we don't have any improvement after this trasformations
cov <- dplyr::select(cov, -deceduti_daily,-dimessi_guariti_dayly, -casi_testati_daily, -nuovi_positivi_norm)

#write.csv(cov,"cov_post_EDA.csv")

# SLIDING WINDOWS: INFO #######################################################
# Rees, E.M., Nightingale, E.S., Jafari, Y. et al. COVID-19 length of hospital 
# stay: a systematic review and data synthesis. BMC Med 18, 270 (2020). 
# https://doi.org/10.1186/s12916-020-01726-3
# --> FROM 4 TO 21 DAYS IN ICU (outside china) <--
# We can consider (21 + 4) / 2 ~ 12 days in the sliding windows: to discuss (?)
# For now I'll consider only 7

# https://cran.r-project.org/web/packages/runner/vignettes/apply_any_r_function.html
# --> COSTANT SLIDING WINDOWS <--
# Type of windows that are these commonly known as running, rolling, moving, 
# sliding windows: this types of windows moves along the index
# To obtain constant sliding windows we do the following

# SLIDING WINDOWS: TOY EXAMPLE ################################################
library(runner)

# Example: sum of 4 elements considering the array [1, 2, 3, ..., 14, 15]
runner(
  1:15,
  k = 4,
  f = sum
)

# Example: slope from 'lm'
df <- data.frame(
  a = 1:15,                # First column of the 'data.frame' structure
  b = 3 * 1:15 + rnorm(15) # Second column 
)

runner(
  x = df,
  k = 5,
  f = function(x) {
    model <- lm(b ~ a, data = x)
    coefficients(model)["a"]
  }
)

## SLINDING WINDOWS: WINDOWS DEPENDING ON DATE ################################
# By default runner calculates on assumption that index increments by one, but 
# sometimes data points in dataset are not equally spaced (missing weekends, 
# holidays, other missings) and thus window size should vary to keep expected 
# time frame
# If one specifies 'idx' argument, then running functions are applied on
#  windows depending on date rather on a sequence 1 - n
# 'idx' should be the same length as 'x' and should be of type 'Date', 'POSIXt'
# or integer
# Example below illustrates window of size 'k = 5' lagged by 'lag = 1'
# Note that one can specify also 'k = "5 days"' and 'lag = "day"' as in 
# 'seq.POSIXt'

idx <- c(4, 6, 7, 13, 17, 18, 18, 21, 27, 31, 37, 42, 44, 47, 48)

# Mean
runner::runner(
  x = idx, 
  k = 5, # 5 - days window
  lag = 1,
  idx = idx,
  f = function(x) mean(x)
)

# Use 'date' or 'datetime' sequences
runner::runner(
  x = idx, 
  k = "5 days", # 5-days window
  lag = 1,
  idx = Sys.Date() + idx,
  f = function(x) mean(x)
)

# SLIDING WINDOWS: ICU ########################################################

# Let's add to our dataframe a feature that each day computes the means of the 
# previous 7 days
sliding_ICU <- runner(
  cov$terapia_intensiva,
  k = 7,
  f = mean
)

cov$sliding_ICU <- sliding_ICU

# Seems to work: let's plot it with respect to the date to see what I got
ggplot(cov, aes(x = data, y = sliding_ICU, color = colore)) + 
  scale_color_manual(values = c("orange", "green", "grey", "red")) +
  geom_path(aes(group = 1)) + 
  geom_point(size = 3, colour = "white") +
  geom_point(size = 2.5) + theme_ipsum() # <- PLOT THIS

# Cool! There's seem to be a more smooth behavior with respect to before,
# especially on January
# Let's try to 'estimate' the daily new entries in ICU using this new result
cov$new_ICU_entries <- cov$terapia_intensiva - cov$sliding_ICU

ggplot(cov, aes(x = data, y = new_ICU_entries, color = colore)) + 
  scale_color_manual(values = c("orange", "yellow", "grey", "red")) +
  geom_point(size = 2,) + geom_path(aes(group = 1)) + theme_ipsum() 

# Not a good idea: I guess we miss some more information to do such a thing
cov <- dplyr::select(cov, -new_ICU_entries)

# Right, so, we have to predict 'terapia_intensiva', obviously we can't use 
# 'sliding_ICU' to predict the number of people in ICU in a certain day since
# in the current 'sliding_ICU' variable we're considering also that day to 
# compute the mean
# Indeed, if we take a look at the correlation between this two variables is
# quite high
cor(cov$terapia_intensiva, cov$sliding_ICU)

# We can try to consider the 7 previous days (this implies that I have to
# exclude the first point)
shifted_sliding_ICU <- runner(
  cov$terapia_intensiva,
  k = 7,
  lag = 1,
  f = mean
)

cov$shifted_sliding_ICU <- shifted_sliding_ICU

cov <- cov[-1, ] # Excluding the first point

# As we expect they are similar to the 'sliding_ICU' values and still retain an
# high correlation with 'terapia_intensiva' (lower than before though), but we 
# now can use this variable for prediction
cor(cov$terapia_intensiva, cov$shifted_sliding_ICU)
