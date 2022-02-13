# SLIDING WINDOWS  #############################################################
# In this project we are going to work with the region Campania, Intensive Care
# Units ('terapia_intensiva')

# Loadin' necessary libraries
library(tidyverse)
library(corrplot)
library(PerformanceAnalytics)
library(car)
library(ggplot2)
library(hrbrthemes)
library(ggpubr)
library(dplyr)
library(MASS)
library(regclass)
library(RColorBrewer)
library(runner)
library(pracma)

# DATASET PREPARATION ##########################################################
# Upload and prepare the dataset cov
# Import the dataset
cov_orig <- read.csv("EDA_V2/Covid19.csv")

# Selecting the variables/period of interest
upper.date <- "2021-01-31"
lower.date <- "2020-09-01"
actual.lower.date <- "2020-10-01"
region <- "Campania"
cov <- filter(cov_orig,denominazione_regione == region)
cov <- cov %>% dplyr::select(data, terapia_intensiva,  deceduti, totale_casi,
                             totale_positivi, nuovi_positivi)
# Dimessi_guariti produces NAs coefficents when fitted (?)
cov$data <- as.Date(cov$data)
cov <- filter(cov, cov$data >= lower.date, cov$data <= upper.date)

# Uploading ad adding the color dataset
color_orig <- read_csv("EDA_V2/color.csv")
color <- subset(color_orig, color_orig$denominazione_regione == region)
names(color)
color <- dplyr::select(color, "data", "colore")
color <- subset(color, color$data <= upper.date)
color <- subset(color, color$data >= lower.date)
color <- color[order(color$data),]
cov <- merge(cov,color, by.x = "data", by.y = "data", all.x = TRUE)
cov[is.na(cov)] <- "pre_decreto"
cov$colore <- as.factor(cov$colore)
levels(cov$colore) <- c('Orange', 'Yellow', 'Pre.decreto', 'Red')

# Reordering levels
cov$colore <- relevel(cov$colore, "Red")
cov$colore <- relevel(cov$colore, "Orange")
cov$colore <- relevel(cov$colore, "Yellow")
cov$colore <- relevel(cov$colore, "Pre.decreto")

# Unclassing colors
cov$colore <- unclass(cov$colore)
levels(cov$colore)

# Upload and prepare the dataset vax and add it to cov
vax_orig <- read_csv("EDA_V2/Vax.csv")
vax <- subset(vax_orig,vax_orig$nome_area == region)
vax <- dplyr::select(vax, data_somministrazione, totale)
vax <- subset(vax,vax$data_somministrazione <= upper.date)
vax <- rename(vax,dayly_vax = totale)
vax <- vax[order(vax$data_somministrazione),]
vax$tot_vax <- cumsum(vax$dayly_vax)
pop_campania = 5624260 
vax$perc_vax <- vax$tot_vax/pop_campania * 100
cov <- merge(cov,vax, by.x = "data", by.y = "data_somministrazione",
             all.x = TRUE)
cov <- dplyr::select(cov, -tot_vax, -dayly_vax)
cov[is.na(cov)] <- 0

# We notice some outliers and e choose to discard them
cov <- cov[-which.max(cov$terapia_intensiva), ]

# Renaiming
names(cov) <- c('Date', 'ICU', 'Deceased', 'Total.cases', 'Total.positives', 
                'New.positives', 'Color', 'Percentage.vaccinated') 

untouched.cov <- cov
# SLIDING WINDOWS: INFO ########################################################
# Rees, E.M., Nightingale, E.S., Jafari, Y. et al. COVID-19 length of hospital 
# stay: a systematic review and data synthesis. BMC Med 18, 270 (2020). 
# https://doi.org/10.1186/s12916-020-01726-3
# FROM 4 TO 21 DAYS IN ICU (outside china)

# https://cran.r-project.org/web/packages/runner/vignettes/apply_any_r_function.html
# COSTANT SLIDING WINDOWS

# LM ###########################################################################
# We have to predict 'terapia_intensiva'
# We can try to consider the 7 previous days (this implies that I have to
# exclude the dates not of interest)

cov <- untouched.cov

sliding.ICU <- runner(
  cov$ICU,
  k = 7,
  lag = 1,
  f = mean # Taking the mean
)

cov$Sliding.ICU <- sliding.ICU
cov <- filter(cov, cov$Date >= actual.lower.date)

# High correlation with 'terapia_intensiva' 
cor(cov$ICU, cov$Sliding.ICU) # Pretty high as expected

names(cov)
str(cov)


cov<-filter(cov,New.positives,Sliding.ICU)

# Fit complete LM with original variables plus added variable
sliding.lm1 <- lm(ICU ~. -Date, data = cov)
summary(sliding.lm1)
extractAIC(sliding.lm1) # AIC: 529

# Fit complete LM with original variables plus added variable
sliding.lm1 <- lm(ICU ~. -Date,-, data = cov)
summary(sliding.lm1)
extractAIC(sliding.lm1) # AIC: 529
                  
## CORRELATION ANALYSIS ########################################################

correlation <- cor(dplyr::select(cov, -Date))
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD",
                          "#4477AA"))
p.mat <- cor.mtest(cov %>% dplyr::select(-Date))

# A pretty cool correlation plot, try to plot it!
x11()
corrplot(correlation, method = "color", col = col(200), diag = FALSE,
         order = "hclust",  addCoef.col = "black", tl.col = "black",
         type = "upper", tl.srt = 45)

# Interesting relationship between Color and Sliding.ICU/ICU
chart.Correlation(cov %>% dplyr::select(-Date),  
                  histogram = TRUE, method = "pearson")

vif(sliding.lm1) # (?)

## REDUCING THE MODEL: STEPWISE PROCEDURE ######################################

step.sliding.lm1 <- step(sliding.lm1)
summary(step.sliding.lm1)
extractAIC(step.sliding.lm1)

par(mfrow = c(2, 2))
plot(step.sliding.lm1) # Hypotheses are respected
par(mfrow = c(1, 1))

# Manually
summary(sliding.lm1)
extractAIC(sliding.lm1)

manual.sliding.lm1 <- update(sliding.lm1, . ~ . -Color)
extractAIC(manual.sliding.lm1) # Better: 527
summary(manual.sliding.lm1)

manual.sliding.lm1 <- update(manual.sliding.lm1, . ~ . -Total.cases)
extractAIC(manual.sliding.lm1) # Better: 526
summary(manual.sliding.lm1)

manual.sliding.lm1 <- update(manual.sliding.lm1, . ~ . -Percentage.vaccinated)
extractAIC(manual.sliding.lm1) # Better: 525
summary(manual.sliding.lm1)

manual.sliding.lm1 <- update(manual.sliding.lm1, . ~ . -Deceased)
extractAIC(manual.sliding.lm1) # Better: 523
summary(manual.sliding.lm1)

manual.sliding.lm1 <- update(manual.sliding.lm1, . ~ . -Total.positives)
extractAIC(manual.sliding.lm1) # Worse: 526
summary(manual.sliding.lm1)

# Reversing: final one
manual.sliding.lm1 <- update(manual.sliding.lm1, . ~ . +Total.positives)
extractAIC(manual.sliding.lm1)
summary(manual.sliding.lm1)

par(mfrow = c(2, 2))
plot(manual.sliding.lm1) # Identical as the one obtained with the automatical
                         # step procedure
par(mfrow = c(1, 1))

AIC1 <- setNames(c(extractAIC(sliding.lm1)[2], extractAIC(step.sliding.lm1)[2],
                  extractAIC(manual.sliding.lm1)[2]),
                c('All features', 'Step', 'Manual'))

AIC1

## RIDGE REGRESSSION ###########################################################

# Considering only the variables mantained in the manual step procedure
sliding.lm1.ridge <- lm.ridge(ICU ~ Total.positives + New.positives + 
                              Sliding.ICU, lambda = seq(0, 1.5, 0.001),
                              data = cov)
select(sliding.lm1.ridge)
selected.lambda <- sliding.lm1.ridge$lambda[which(sliding.lm1.ridge$GCV == 
                                                  min(sliding.lm1.ridge$GCV))]

# Visualization  of  ridge  regression: trace  plots
mycol <- brewer.pal(9, 'Set1')

matplot(sliding.lm1.ridge$lambda, t(sliding.lm1.ridge$coef), type = "l",
        xlab = expression(lambda), ylab = expression(hat(beta)),
        main = "Ridge Traceplot", col = mycol, lwd = 2, lty = 1)
legend('right', rownames(sliding.lm1.ridge$coef), col = mycol,
       lwd = 2, lty = 1 ) 
abline(h = 0, lwd = 2, lty = 2)
abline(v = 0.33, lty = 2, col = 'red')

# We now compare the estimates obtained with OLS ( ?? = 0 ) with the ones 
# obtained with the ridge regression

sliding.lm1.ridge.GCV <- lm.ridge(ICU ~ Total.positives + 
                                  New.positives + Sliding.ICU,
                                  lambda = selected.lambda, data = cov)

coef(sliding.lm1.ridge.GCV)
coef(manual.sliding.lm1)

## VISUALIZING THE RESULTS #####################################################
plot(cov$Date, fitted(manual.sliding.lm1), type = "b", pch = 19, cex = 0.8,
     col = "red", ylim = c(40, 220))
points(cov$Date, cov$ICU,  type = "b", pch = 19, 
       cex = 0.8, col = "black")
segments(cov$Date, cov$ICU, 
         cov$Date, fitted(manual.sliding.lm1), lty = "dashed")

# Good but it seem to overfit the data
# How to improve? 

## IMPROVEMENT: SLIDING POSITIVES ##############################################
cov <- untouched.cov

sliding.new.positives <- runner(
  cov$New.positives,
  k = 7,
  lag = 1,
  f = mean # Taking the mean
)

which.max(table(cov$Color))

cov <- cov %>% dplyr::select(-New.positives)
cov$Sliding.new.positives <- sliding.new.positives
cov$Sliding.ICU <- sliding.ICU
cov <- filter(cov, cov$Date >= actual.lower.date)

# High correlation with 'terapia_intensiva' 
cor(cov$ICU, cov$Sliding.new.positives) # Pretty high as expected

# Fit complete LM with original variables plus added variable
sliding.lm2 <- lm(ICU ~. -Date, data = cov)
summary(sliding.lm2)
extractAIC(sliding.lm2) # AIC: 522 (529 the one of before)

###  CORRELATION ANALYSIS ######################################################

# Interesting relationship between Color and Sliding.ICU/ICU
chart.Correlation(cov %>% dplyr::select(-Date),  
                  histogram = TRUE, method = "pearson")

### REDUCING THE MODEL: STEPWISE PROCEDURE #####################################

step.sliding.lm2 <- step(sliding.lm2)
summary(step.sliding.lm2)
extractAIC(step.sliding.lm2) # 519

par(mfrow = c(2, 2))
plot(step.sliding.lm2) # Similar to the previous
par(mfrow = c(1, 1))

# Manually
summary(sliding.lm2)
extractAIC(sliding.lm2)

manual.sliding.lm2 <- update(sliding.lm2, . ~ . -Color)
extractAIC(manual.sliding.lm2) # Better: 521
summary(manual.sliding.lm2)

manual.sliding.lm2 <- update(manual.sliding.lm2, . ~ . -Total.positives )
extractAIC(manual.sliding.lm2) # Better: 520
summary(manual.sliding.lm2)

manual.sliding.lm2 <- update(manual.sliding.lm2, . ~ . -Deceased)
extractAIC(manual.sliding.lm2) # Worse: 533
summary(manual.sliding.lm2)

# Reversing
manual.sliding.lm2 <- update(manual.sliding.lm2, . ~ . +Deceased)

# Trying another
manual.sliding.lm2 <- update(manual.sliding.lm2, . ~ . -Percentage.vaccinated)
extractAIC(manual.sliding.lm2) # Worse: 530 
summary(manual.sliding.lm2)

# Reversing: final one
manual.sliding.lm2 <- update(manual.sliding.lm2, . ~ . +Percentage.vaccinated)
par(mfrow = c(2, 2))
plot(manual.sliding.lm2) # Identical as the one obtained with the automatical
# step procedure
par(mfrow = c(1, 1))

SSE.lm2 <- sum((fitted(manual.sliding.lm2) - cov$ICU)^2)
SSE.lm2

# Do you think we should stick with a model with a loss less features but an
# higher AIC? 
# I mean, probably the deceased are somehow correlated but the percentage of
# vaccinated people? The vaccine should take 2 weeks to be effective and the
# vaccination campaign started in the end of December, moreover the first
# couple of weeks the percentage is to small to have some real effect

AIC2 <- setNames(c(extractAIC(sliding.lm2)[2], extractAIC(step.sliding.lm2)[2],
                   extractAIC(manual.sliding.lm2)[2]),
                 c('All features', 'Step', 'Manual'))

AIC1
AIC2

### VISUALIZING THE RESULTS ###################################################
plot(cov$Date, fitted(manual.sliding.lm2), type = "b", pch = 19, cex = 0.8,
     col = "red", ylim = c(40, 220))
points(cov$Date, cov$ICU,  type = "b", pch = 19, 
       cex = 0.8, col = "black")
segments(cov$Date, cov$ICU, 
         cov$Date, fitted(manual.sliding.lm2), lty = "dashed")

# The model doesn't seem to overfit the data and the AIC is even better than
# before

# But..
# Let's take a look a the following plot
plot(cov$Sliding.new.positives, cov$ICU)
# There's seem to be a non linear relation

## IMPROVEMENT: INTRODUCING A NON LINEAR TERM ##################################

cov <- untouched.cov
cov$Sliding.ICU <- sliding.ICU
cov$Sliding.new.positives <- sliding.new.positives
cov <- cov %>% dplyr::select(-New.positives)
cov <- filter(cov, cov$Date >= actual.lower.date)

### CHOOSING BETWEEN SQUARE ROOT AND LOGARITHM ################################

new.lm.log <- lm(ICU ~ log(Sliding.new.positives), data = cov)
new.lm.sqrt <- lm(ICU ~ sqrt(Sliding.new.positives), data = cov)
summary(new.lm.log)
summary(new.lm.sqrt)
extractAIC(new.lm.log)
extractAIC(new.lm.sqrt) # AIC: 515 (better than I was expecting)

AIC3 <- setNames(c(extractAIC(new.lm.log)[2], extractAIC(new.lm.sqrt)[2]),
                 c('Logarithm', 'Square root'))

AIC3 # The logarithm wins

# Better with or without the linear term as well?
new.lm.linear.log <- lm(ICU ~ Sliding.new.positives + 
                              log(Sliding.new.positives), data = cov)
extractAIC(new.lm.linear.log) 

sliding.lm.log <- lm(ICU ~ . -Date+log(Sliding.new.positives), data = cov)
summary(sliding.lm.log)
extractAIC(sliding.lm.log) # 506, good, real good

### REDUCING THE MODEL: STEPWISE PROCEDURE #####################################

step.sliding.lm.log <- step(sliding.lm.log)
summary(step.sliding.lm.log)
extractAIC(step.sliding.lm.log) # 503

par(mfrow = c(2, 2))
plot(step.sliding.lm.log) # Similar to the previous
par(mfrow = c(1, 1))

# Manually
summary(sliding.lm.log)
extractAIC(sliding.lm.log)

manual.sliding.lm.log <- update(sliding.lm.log, . ~ . -Total.positives)
extractAIC(manual.sliding.lm.log) # Better: 505
summary(manual.sliding.lm.log)

manual.sliding.lm.log <- update(manual.sliding.lm.log, . ~ . -Color)
extractAIC(manual.sliding.lm.log) # Better: 504
summary(manual.sliding.lm.log)

manual.sliding.lm.log <- update(manual.sliding.lm.log, . ~ . -Total.cases)
extractAIC(manual.sliding.lm.log) # Equal: 504
summary(manual.sliding.lm.log)

manual.sliding.lm.log <- update(manual.sliding.lm.log, . ~ . -Deceased)
extractAIC(manual.sliding.lm.log) # Better: 503
summary(manual.sliding.lm.log)

manual.sliding.lm.log <- update(manual.sliding.lm.log, . ~ . -Percentage.vaccinated)
extractAIC(manual.sliding.lm.log) # Worse: 506
summary(manual.sliding.lm.log)

# Reversing: final one
manual.sliding.lm.log <- update(manual.sliding.lm.log, . ~ . +Percentage.vaccinated)
extractAIC(manual.sliding.lm.log)
summary(manual.sliding.lm.log)

# Comparing nested models
anova(manual.sliding.lm.log, sliding.lm.log, test = "Chisq")

SSE.lm.lg <- sum((fitted(manual.sliding.lm.log) - cov$ICU)^2)
SSE.lm.lg

par(mfrow = c(2, 2))
plot(manual.sliding.lm.log) 
par(mfrow = c(1, 1))

# Do you think we should stick with a model with a loss less features but an
# higher AIC? 
# I mean, probably the deceased are somehow correlated but the percentage of
# vaccinated people? The vaccine should take 2 weeks to be effective and the
# vaccination campaign started in the end of December, moreover the first
# couple of weeks the percentage is to small to have some real effect

AIC3 <- setNames(c(extractAIC(sliding.lm.log)[2], 
                   extractAIC(step.sliding.lm.log)[2],
                   extractAIC(manual.sliding.lm.log)[2]),
                 c('All features', 'Step', 'Manual'))

AIC1
AIC2
AIC3

### VISUALIZING THE RESULTS ###################################################
plot(cov$Date, fitted(manual.sliding.lm.log), type = "b", pch = 19, cex = 0.8,
     col = "red", ylim = c(40, 220))
points(cov$Date, cov$ICU,  type = "b", pch = 19, 
       cex = 0.8, col = "black")
segments(cov$Date, cov$ICU, 
         cov$Date, fitted(manual.sliding.lm.log), lty = "dashed")

# Beautiful: doesn't describe well the rapid changes of February though

### ANALYSIS OF THE INFLUENTIAL POINTS (?) #####################################
# Ask Ceci

# GLM ##########################################################################

cov <- untouched.cov
cov$Sliding.ICU <- sliding.ICU
cov$Sliding.new.positives <- sliding.new.positives
cov <- cov %>% dplyr::select(-New.positives)
cov <- filter(cov, cov$Date >= actual.lower.date)

# Fit complete GLM with all variables keeping the non linear relation between
# the sliding new positives and the ICU
sliding.glm <- glm(ICU ~. -Date+log(Sliding.new.positives),
                   data = cov, family = poisson)
summary(sliding.glm)
extractAIC(sliding.glm) # 877: a lot worse

# Before starting to interpret results, let's check whether the model has
# over-dispersion or under-dispersion
# If the Residual Deviance is greater than the degrees of freedom, then
# over-dispersion exists
# The Null deviance shows how well the response variable is predicted by a 
# model that includes only the intercept (grand mean) whereas residual with 
# the inclusion of independent variables
# (https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/)

# These results are somehow reassuring. First, the null deviance is high, which
# means it makes sense to use more than a single parameter for fitting the 
# model
# Second, the residual deviance is relatively low (and close to the number of 
# degrees of freedom), which indicates that the log likelihood of our model is 
# close to the log likelihood of the saturated model
# (https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/)

# We perform a test to confirm this theory
# To calculate the p-value for the deviance goodness of fit test we simply
# calculate the probability to the right of the deviance value for the 
# chi-squared distribution on 113 degrees of freedom
pchisq(sliding.glm$deviance, df = sliding.glm$df.residual, lower.tail = FALSE)

with(sliding.glm, cbind(res.deviance = deviance,
                                         df = df.residual,
                 p = pchisq(deviance, df.residual, lower.tail = FALSE)))

# 0.3119235
# The null hypothesis is that our model is correctly specified, 
# and we have strong evidence to accept that hypothesis
# (https://thestatsgeek.com/2014/04/26/deviance-goodness-of-fit-test-for-poisson-regression/)

par(mfrow=c(2, 2))
plot(sliding.glm)
par(mfrow=c(1, 1))

ggplot(cov) +
  geom_point(aes(x = Date, y = ICU)) +
  geom_line(aes(x = Date,y = fitted(sliding.glm)))

plot(cov$Date, fitted(sliding.glm), type = "b", pch = 19, cex = 0.8,
     col = "red", ylim = c(40, 220))
points(cov$Date, cov$ICU,  type = "b", pch = 19, 
       cex = 0.8, col = "black")
segments(cov$Date, cov$ICU, 
         cov$Date, fitted(sliding.glm), lty = "dashed")

### REDUCING THE MODEL: STEPWISE PROCEDURE #####################################

# Stepwise procedure
step.sliding.glm <- step(sliding.glm)
summary(step.sliding.glm)
extractAIC(step.sliding.glm) # 873

pchisq(step.sliding.glm$deviance, df = step.sliding.glm$df.residual, 
       lower.tail=FALSE)

with(step.sliding.glm, cbind(res.deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))


x11()
par(mfrow=c(2, 2))
plot(step.sliding.glm)
par(mfrow=c(1, 1))

# I think that we should do this analysis for each model and then use the tools 
# that we have to compare the nested models.

# Also consider to use an offset (casi giornalieri)

x11()
ggplot(cov_glm1) +
  geom_point(aes(x = data, y = terapia_intensiva)) +
  geom_line(aes(x = data,y = fitted(glm1_step)))

# Comparing nested models

anova(step.sliding.glm, sliding.glm, test = "Chisq")

# The excluded variables were not statistically significant predictors hence
# we choose the reduced model