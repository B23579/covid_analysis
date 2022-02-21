###############################################################################
#                             LINEAR MODEL                                    #
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
library(regclass)
library(RColorBrewer)

#setwd("C:/Users/Cecilia/OneDrive/Desktop/Ceci/UniTS/Statistical_Methods_for_Data_Science/Progetto_new/covid_analysis/model_prediction")

# upload data created with script EDA_V2
cov <- read.csv("cov_post_EDA.csv")
cov <- dplyr::select(cov,-X)
cov$Color <- as.factor(cov$Color)
cov$Date <- as.Date(cov$Date)
names(cov)
str(cov)
#x11()
#plot(cov)

#info <- subset(info, data == c("2020-11-04","2020-11-05","2020-11-06","2020-11-07","2020-12-05"))

# Fit complete LM with original variables

cov_lm1 <- cov
names(cov_lm1)
lm1 <- lm(ICU ~. -Date , data = cov_lm1)
lm1
summary(lm1)
extractAIC(lm1)
# I think we should use extractAIC (https://stats.stackexchange.com/questions/43733/what-is-the-difference-between-aic-and-extractaic-in-r)

x11()
par(mfrow=c(2,2))
plot(lm1)

#summary(lm1)$adj.r.squared

tab_lm <- data.frame(matrix(ncol = 3, nrow = 0))
new_row <- c("complete_lm",round(summary(lm1)$adj.r.squared, digits = 3),round(extractAIC(lm1)[2], digits = 3))
tab_lm <- rbind(tab_lm, new_row)
colnames(tab_lm) <- c("Model","adj.r.squared","AIC")

# Normality and Heteroschedacity look ok

###############################################################################
#                         CORRELATION ANALYSIS                                #
###############################################################################

corr <- cor(dplyr::select(cov_lm1, -Date, -Color))
corr
x11()
corrplot(corr, method = "ellipse")
#names(cov_lm1)
x11()
chart.Correlation(cov_lm1[,c("Hospitalized.with.symptoms","ICU","People.at.home",
                         "New.positives","Discharged.healed","Deceased","Percentage.vaccinated")])

#vif -> bisogna guardare la colonna GVIF
vif(lm1)

#by hand
#lm_RCS<- lm(Hospitalized.with.symptoms ~. -ICU , data=cov_lm1)
#1/(1-summary(lm_RCS)$r.squared)
#lm_D<- lm(Deceased ~. -ICU , data=cov_lm1)
#1/(1-summary(lm_D)$r.squared)

# Both the plots and the vifs suggest the presence of multicollinearity

# lm1 seems a very good model: p-value < 2.2e-16, Adjusted R-squared 0.966. However, there are some 
# non-signi???cant covariates (Discharged.healed, Percentage.vaccinated).
# Having a look at the plots, we understand why Percentage.vaccinated is not important.
# Instead, the non relevance of Discharged.healed This can be due to the linear dependence 

# We try to ???x collinearity problems as follows:

###############################################################################
#                REDUCING THE MODEL -> STEPWISE PROCEDURE                     #
###############################################################################

lm1_step <- step(lm1)
summary(lm1_step)
extractAIC(lm1)
extractAIC(lm1_step)
x11()
par(mfrow=c(2,2))
plot(lm1_step)
# Hypotheses are respected

new_row <- c("lm_auto_step",round(summary(lm1_step)$adj.r.squared, digits = 3),round(extractAIC(lm1_step)[2], digits = 3))
tab_lm <- rbind(tab_lm, new_row)

# lm1_step has better AIC and better R^2

# try manualy
summary(lm1)

#remove Percentage.vaccinated
lm1_manual <- update(lm1, . ~ . -Percentage.vaccinated)
summary(lm1_manual)
extractAIC(lm1_manual)

#remove Discharged.healed
lm1_manual <- update(lm1_manual, . ~ . -Discharged.healed)
summary(lm1_manual)
extractAIC(lm1_manual)

x11()
ggplot(cov_lm1) +
  geom_point(aes(x = Date, y = ICU)) +
  geom_line(aes(x = Date,y = fitted(lm1_manual)),color="blue", size=1.2)

# R^2 and AIC are almost the same -> we prefer to consider the simplest model

new_row <- c("lm_manual_step",round(summary(lm1_manual)$adj.r.squared, digits = 3),round(extractAIC(lm1_manual)[2], digits = 3))
tab_lm <- rbind(tab_lm, new_row)

###############################################################################
#                            RIDGE REGRESSION                                 #
###############################################################################
##
### in this case we still exclude Percentage.vaccinated
##
##lm2 <- lm(ICU ~ . -Percentage.vaccinated, data = cov_lm1)
##lm1_ridge <- lm.ridge(ICU ~ . -Percentage.vaccinated,lambda=seq(0,1.5,0.001), data = cov_lm1)
##select(lm1_ridge)
##lambda_selected<-lm1_ridge$lambda[which(lm1_ridge$GCV==min(lm1_ridge$GCV))]
##
### Visualization  of  ridge  regression:   trace  plots
##mycol <- brewer.pal(9,'Set1')
##x11()
##matplot( lm1_ridge$lambda, t(lm1_ridge$coef), type = "l", xlab = expression(lambda),
##         ylab = expression(hat(beta)),
##         main = "Ridge Traceplot", col = mycol, lwd = 2, lty = 1)
##legend('topright', rownames(lm1_ridge$coef), col = mycol, lwd = 2, lty = 1 ) 
##abline(h = 0, lwd = 2, lty = 2)
##abline(v = 0.33, lty = 2, col = 'red')
##
##
### comment the results together
##
##
###We now compare the estimates obtained with OLS ( ?? = 0 ) with the ones obtained with the ridge regression
##
##lm1_ridge_GCV <- lm.ridge(ICU ~ . -Percentage.vaccinated,lambda=lambda_selected, data = cov_lm1)
##
##coef(lm1_ridge_GCV)
##coef(lm2)
##
##

###############################################################################
#                     ANALYSIS OF INFLUENTIAL POINTS                          #
###############################################################################

# I do it with lm1_manual, then we can do it whit the model that we choose at the end

lev <- hatvalues(lm1_manual)
lev
sum(lev)
dim(cov_lm1)
n <- dim(cov_lm1)[1]
p <- dim(cov_lm1)[2]

length(lm1_manual$fitted.values)

x11()
plot( lm1_manual$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", pch = 16, col = 'black' ) 
abline( h = 2 * p/n, lty = 2, col = 'red' )
watchout_points_lev = lev[ which( lev > 2 * p/n  ) ] 
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( lm1_manual$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

lm1_manual$call

# fit the model without the influential points:
lm1_no_inf <- lm(formula = ICU ~ Hospitalized.with.symptoms + 
                   People.at.home + New.positives + Deceased + Color, 
                 data = cov_lm1, subset = lev < 2 * p/n)
summary(lm1_no_inf)

# investigate the differences in the coef
abs( ( lm1_manual$coefficients - lm1_no_inf$coefficients ) / lm1_manual$coefficients )

#The leverages affect the estimate heavily: exept for New.positives, there is a variation of 18% at least

x11()
colors = rep( 'black', nrow( cov_lm1 ) )
colors[ watchout_ids_lev ] = c('red', 'blue', 'green', 'orange', 'yellow', 'purple')
pairs( cov_lm1[ , c( 'ICU','Date','Hospitalized.with.symptoms', 
                     'People.at.home','New.positives','Deceased','Color' ) ],
       pch = 16, col = colors, cex = 1 + 0.5 * as.numeric( colors != 'black' )    )

#x11()
#pairs( cov_lm1[ , c( 'ICU','Date', ) ],
#       pch = 16, col = colors, cex = 1 + 0.5 * as.numeric( colors != 'black' )    )

# the influential points are the values of the 4, 5, 6, 7 November and 5 December

# discard 5 December and do again all the models
cov_lm2<-filter(cov,cov$Date != "2020-12-05")

lm2 <- lm(ICU ~. -Date , data = cov_lm2)
summary(lm2)
extractAIC(lm2)

x11()
par(mfrow=c(2,2))
plot(lm2)

new_row <- c("complete_lm2",round(summary(lm1)$adj.r.squared, digits = 3),round(extractAIC(lm1)[2], digits = 3))
tab_lm <- rbind(tab_lm, new_row)

#step
lm2_step <- step(lm2)
summary(lm2_step)
extractAIC(lm2_step)
x11()
par(mfrow=c(2,2))
plot(lm2_step)
# Hypotheses are respected

new_row <- c("lm2_auto_step",round(summary(lm2_step)$adj.r.squared, digits = 3),round(extractAIC(lm1_step)[2], digits = 3))
tab_lm <- rbind(tab_lm, new_row)

# manual
lm2_manual <- update(lm2_step, . ~ . -Discharged.healed)
summary(lm2_manual)
extractAIC(lm2_manual)

new_row <- c("lm2_manual",round(summary(lm2_manual)$adj.r.squared, digits = 3),round(extractAIC(lm1_step)[2], digits = 3))
tab_lm <- rbind(tab_lm, new_row)

x11()
ggplot(cov_lm1) +
  geom_point(aes(x = Date, y = ICU)) +
  geom_line(aes(x = Date,y = fitted(lm1_manual)),color="blue", size=1.2)


###############################################################################
#                                PREDICTION                                   #
###############################################################################
##https://www.youtube.com/watch?v=ahDFXHAdZRU
#test_cov <- read.csv("test_cov.csv")
#test_cov <- dplyr::select(test_cov,-X)
#test_cov$Color <- as.factor(test_cov$Color)
#test_cov$Date <- as.Date(test_cov$Date)
#names(test_cov)
#str(test_cov)
#
#new_ICU <- predict( lm2_manual, test_cov, interval = "prediction", level = 0.95 )
#
#
##library(boot)
#boot_fun <- function(data_local, indices) {
#  d_train=data_local[indices,]
#  model = lm(formula = ICU ~ Hospitalized.with.symptoms + People.at.home + 
#               New.positives + Deceased + Color, data = d_train)
#  
#  pred <- predict(model,test_cov, se.fit=FALSE,type="response")
#  
#  return (pred) 
#}
#
#b <- boot(cov_lm2, boot_fun, R=5000,ncpus=8)
#x11()
#
