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

## SHOW THE VARIABLES AND CHOOSE WHICH ONE TO CONSIDER ########################

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

## TOY EXAMPLE #################################################################
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

## SLINDING WINDOWS ############################################################
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
# previous 6 days
sliding_ICU <- runner(
  cov$terapia_intensiva,
  k = 6,
  f = mean
)

cov$sliding_ICU <- sliding_ICU

# Seems to work: let's plot it with respect to the date to see what I got
ggplot(cov, aes(x = data, y = sliding_ICU, color = colore)) + 
  scale_color_manual(values = c("orange", "light blue", "grey", "red")) +
  geom_path(aes(group = 1)) + 
  geom_point(size = 3, colour = "white") +
  geom_point(size = 2.5) + theme_ipsum() # <- PLOT THIS 
  # (Remark: now yellow is light blue because I can't see yellow)

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

# We can try to consider the 6 previous days (this implies that I have to
# exclude the first point)
shifted_sliding_ICU <- runner(
  cov$terapia_intensiva,
  k = 6,
  lag = 1,
  f = mean
)

cov$shifted_sliding_ICU <- shifted_sliding_ICU
cov <- dplyr::select(cov, -sliding_ICU)

cov <- cov[-1, ] # Excluding the first point

# As we expect they are similar to the 'sliding_ICU' values and still retain an
# high correlation with 'terapia_intensiva' (lower than before though), but we 
# now can use this variable for prediction
cor(cov$terapia_intensiva, cov$shifted_sliding_ICU)

## FITTING A LINEAR MODEL ######################################################
chart.Correlation(dplyr::select(cov, -colore, -data,),  
                  histogram = TRUE, method = "pearson")

# Taking a look at the following correlation plot I think I'll consider only
# the 4 variables in the chart to fit the linear model since it seems that they
# are the only linearly correlated with 'terapia_intensiva' and I am already
# considering 'terapia_intensiva_ieri' in some way in the 'sliding_ICU'
chart.Correlation(dplyr::select(cov, -colore, -data, -perc_vax, 
                                -dimessi_guariti, -terapia_intensiva_ieri,
                                -deceduti, -isolamento_domiciliare),  
                  histogram = TRUE, method = "pearson")

# To warm up a bit and gain some confidence I start by considering a single
# predictor: that is 'shifted_sliding_ICU'
plot(cov$shifted_sliding_ICU, cov$terapia_intensiva, pch=19, xlab="Num", ylab="time")
lm_ICU <- lm(terapia_intensiva ~ shifted_sliding_ICU, data = cov)
par(mfrow = c(2, 2))
plot(lm_ICU)

# Not so good but whatever
# Now I'll plot the fitted values
par(mfrow = c(1, 1))
with(cov, plot(shifted_sliding_ICU, terapia_intensiva, pch = 19))
abline(coef(lm_ICU), col="red", lty="solid")

text(70, 170, expression(terapia_intensiva == hat(beta)[0] + 
                       hat(beta)[1] * shifted_sliding_ICU), col="red")

points(cov$shifted_sliding_ICU, predict(lm_ICU), col = "red", 
       pch = 19, cex = 0.8)

segments(cov$shifted_sliding_ICU, cov$terapia_intensiva, 
         cov$shifted_sliding_ICU, fitted(lm_ICU), lty="dashed")

# Pretty nice plot, a final thing to notice
summary(lm_ICU) # <- R^2 = 0.925

# Indeed
Tot_SS <- with(cov, sum((terapia_intensiva - mean(terapia_intensiva))^2)) 
Res_SS <- with(cov, sum((predict(lm_ICU) - terapia_intensiva)^2))
R_sq <- 1 - Res_SS/Tot_SS # <- 0.925

# We can even visualize the results with respect to the date
plot(cov$data, fitted(lm_ICU), type = "b", pch = 19, cex = 0.8, col = "red",
     ylim = c(40, 220))
points(cov$data, cov$terapia_intensiva,  type = "b", pch = 19, 
       cex = 0.8, col = "black")
segments(cov$data, cov$terapia_intensiva, 
         cov$data, fitted(lm_ICU), lty = "dashed")

# It seems to be shifted, this could be caused by the fact that I'm basing the
# analysis on the measures of the previous days (?)

### MULTIPLE LINEAR REGRESSION ################################################
# We already visualized the correlation chart so now time to use the chosen
# variable to get some goodies 
lm_fantanstic_three <- lm(terapia_intensiva ~ nuovi_positivi +
                          ricoverati_con_sintomi + shifted_sliding_ICU, 
                          data = cov)
par(mfrow = c(2,2))
plot(lm_fantanstic_three)

# Surely better than before, there are a couple of outliers though
# Let's take a look at the R^2 for curiosity
summary(lm_fantanstic_three) # R^2 --> 0.9657, yes: better

# Obviously now we can't visualize a plot in 4 dimensions but we can still 
# visualize it with respect to the date
plot(cov$data, fitted(lm_fantanstic_three), type = "b", pch = 19, cex = 0.8,
     col = "red", ylim = c(40, 220))
points(cov$data, cov$terapia_intensiva,  type = "b", pch = 19, 
       cex = 0.8, col = "black")
segments(cov$data, cov$terapia_intensiva, 
         cov$data, fitted(lm_fantanstic_three), lty = "dashed")

# It better yes, but maybe it over fits the data a bit (?) 
# In any case, I'm still using only a linear model so I can't expect much better
# Just out of curiosity, let's also remove 'ricoverati_con_sintomì'
lm_last <- lm(terapia_intensiva ~ nuovi_positivi + shifted_sliding_ICU, 
              data = cov)
plot(cov$data, fitted(lm_last), type = "b", pch = 19, cex = 0.8,
     col = "red", ylim = c(40, 220))
points(cov$data, cov$terapia_intensiva,  type = "b", pch = 19, 
       cex = 0.8, col = "black")
segments(cov$data, cov$terapia_intensiva, 
         cov$data, fitted(lm_last), lty = "dashed")

# And the R^2 is..
summary(lm_last) # R^2 --> 0.9656, like the previous

# Plotting the residuals against the common explanatory variables in the last 2
# cases:  they're pretty much the same
par(mfrow = c(2, 2))
plot(cov$shifted_sliding_ICU, lm_last$residuals, ylab = 'last')
abline(h = 0, lty = "dashed")
plot(cov$shifted_sliding_ICU, lm_fantanstic_three$residuals, ylab = 'three')
abline(h = 0, lty = "dashed")
plot(cov$nuovi_positivi, lm_last$residuals, ylab = 'last')
abline(h = 0, lty = "dashed")
plot(cov$nuovi_positivi, lm_fantanstic_three$residuals, ylab = 'three')
abline(h = 0, lty = "dashed")

# Rad, this is even better than the one with 3 features, later I will compare
# the models with better techniques
# Really the last thing for this section: extending the simple linear regression 
# model by modelling the explanatory variable dist as a polynomial of degree two
# for 'nuovi_positivi', because looking at the correlation chart maybe there's
# something (?)
lm_polynomial <- lm(terapia_intensiva ~ nuovi_positivi + shifted_sliding_ICU + 
                    I(nuovi_positivi^2), data = cov)

par(mfrow = c(1, 1))
lm_last <- lm(terapia_intensiva ~ nuovi_positivi + shifted_sliding_ICU, 
              data = cov)
plot(cov$data, fitted(lm_polynomial), type = "b", pch = 19, cex = 0.8,
     col = "red", ylim = c(40, 220))
points(cov$data, cov$terapia_intensiva,  type = "b", pch = 19, 
       cex = 0.8, col = "black")
segments(cov$data, cov$terapia_intensiva, 
         cov$data, fitted(lm_polynomial), lty = "dashed")

summary(lm_polynomial) # R^2 --> 0.9687
# It is better, but is it correct?

### AIC #######################################################################
# AIC
AIC <- rbind(extractAIC(lm_ICU)[2], extractAIC(lm_fantanstic_three)[2],
             extractAIC(lm_last)[2], extractAIC(lm_polynomial)[2])

AIC
# The last model yields the lower AIC

### RIDGE REGRESSION ##########################################################
# Doing things randomly
lm_ridge<- lm.ridge(terapia_intensiva ~ nuovi_positivi + shifted_sliding_ICU,
                    lambda = 0, data = cov)

# Remark: is a good idea to use different windows length for a different number
# of people in ICU? 

