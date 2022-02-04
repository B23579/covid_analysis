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

#setwd("C:/Users/Cecilia/OneDrive/Desktop/Ceci/UniTS/Statistical_Methods_for_Data_Science/Progetto/covid_analysis/model_prediction")

# upload data created with script EDA_V2
cov <- read.csv("cov_post_EDA.csv")
cov <- dplyr::select(cov,-X)
cov$colore <- as.factor(cov$colore)
cov$data <- as.Date(cov$data)
names(cov)
str(cov)
#x11()
#plot(cov)


# Fit complete LM with original variables

cov_lm1 <- dplyr::select(cov,-terapia_intensiva_ieri)
names(cov_lm1)
lm1 <- lm(terapia_intensiva ~. , data = cov_lm1)
lm1
summary(lm1)
extractAIC(lm1)
# I think we should use extractAIC (https://stats.stackexchange.com/questions/43733/what-is-the-difference-between-aic-and-extractaic-in-r)

x11()
par(mfrow=c(2,2))
plot(lm1)

# Normality and Heteroschedacity look ok


###############################################################################
#                         CORRELATION ANALYSIS                                #
###############################################################################

corr <- cor(dplyr::select(cov_lm1, -data, -colore))
corr
x11()
corrplot(corr, method = "ellipse")
#names(cov_lm1)
x11()
chart.Correlation(cov_lm1[,c("ricoverati_con_sintomi","terapia_intensiva","isolamento_domiciliare",
                         "nuovi_positivi","dimessi_guariti","deceduti","perc_vax")])

#vif -> bisogna guardare la colonna GVIF
vif(lm1)

#by hand
#lm_RCS<- lm(ricoverati_con_sintomi ~. -terapia_intensiva , data=cov_lm1)
#1/(1-summary(lm_RCS)$r.squared)
#lm_D<- lm(deceduti ~. -terapia_intensiva , data=cov_lm1)
#1/(1-summary(lm_D)$r.squared)

# Both the plots and the vifs suggest the presence of multicollinearity

# lm1 seems a very good model: p-value < 2.2e-16, Adjusted R-squared 0.966. However, there are some 
# non-signi???cant covariates (dimessi_guariti, perc_vax).
# Having a look at the plots, we understand why perc_vax is not important.
# Instead, the non relevance of dimessi_guariti This can be due to the linear dependence 

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

# lm1_step has better AIC and better R^2


###############################################################################
#                            RIDGE REGRESSION                                 #
###############################################################################

# in this case we still exclude perc_vax

lm2 <- lm(terapia_intensiva ~ . -perc_vax, data = cov_lm1)
lm1_ridge <- lm.ridge(terapia_intensiva ~ . -perc_vax,lambda=seq(0,1.5,0.001), data = cov_lm1)
select(lm1_ridge)
lambda_selected<-lm1_ridge$lambda[which(lm1_ridge$GCV==min(lm1_ridge$GCV))]

# Visualization  of  ridge  regression:   trace  plots
mycol <- brewer.pal(9,'Set1')
x11()
matplot( lm1_ridge$lambda, t(lm1_ridge$coef), type = "l", xlab = expression(lambda),
         ylab = expression(hat(beta)),
         main = "Ridge Traceplot", col = mycol, lwd = 2, lty = 1)
legend('topright', rownames(lm1_ridge$coef), col = mycol, lwd = 2, lty = 1 ) 
abline(h = 0, lwd = 2, lty = 2)
abline(v = 0.33, lty = 2, col = 'red')


# comment the results together


#We now compare the estimates obtained with OLS ( ?? = 0 ) with the ones obtained with the ridge regression

lm1_ridge_GCV <- lm.ridge(terapia_intensiva ~ . -perc_vax,lambda=lambda_selected, data = cov_lm1)

coef(lm1_ridge_GCV)
coef(lm2)



###############################################################################
#                     ANALYSIS OF INFLUENTIAL POINTS                          #
###############################################################################

# I do it with lm1_step, then we can do it whit the model that we choose at the end

lev <- hatvalues(lm1_step)
lev
sum(lev)
dim(cov_lm1)
n <- dim(cov_lm1)[1]
p <- dim(cov_lm1)[2]

length(lm1_step$fitted.values)

#x11()
plot( lm1_step$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", pch = 16, col = 'black' ) 
abline( h = 2 * p/n, lty = 2, col = 'red' )
watchout_points_lev = lev[ which( lev > 2 * p/n  ) ] 
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( lm1_step$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

lm1_step$call

# fit the model without the influential points:
lm1_no_inf <- lm(formula = terapia_intensiva ~ data + ricoverati_con_sintomi + 
                   isolamento_domiciliare + nuovi_positivi + deceduti + colore, 
                 data = cov_lm1, subset = lev < 2 * p/n)
summary(lm1_no_inf)

# investigate the differences in the coef
abs( ( lm1_step$coefficients - lm1_no_inf$coefficients ) / lm1_step$coefficients )

#The leverages affect the estimate heavily: exept for nuovi_positivi, there is a variation of 18% at least

#x11()
colors = rep( 'black', nrow( cov_lm1 ) )
colors[ watchout_ids_lev ] = c('red', 'blue', 'green', 'orange', 'yellow', 'purple')
pairs( cov_lm1[ , c( 'terapia_intensiva','data','ricoverati_con_sintomi', 
                     'isolamento_domiciliare','nuovi_positivi','deceduti','colore' ) ],
       pch = 16, col = colors, cex = 1 + 0.5 * as.numeric( colors != 'black' )    )


# if we want to expand this section, we can also consider to compute the nfluential point WRT cook's distance







###############################################################################
#                        BOX-COX TRANSFORMATION                               #
###############################################################################

###x11()
##b <- boxCox(lm1)
##names(b)
##best_lambda_ind = which.max( b$y )
##best_lambda = b$x[ best_lambda_ind ]
##best_lambda
###0.6666667
##
### we know that for lambda = 0.5 we transform Y in Sqrt(Y) and for lamda = 1 we keep Y as it is
### check if trasforming terapia_intensiva in sqrt(terapia_intensiva) the normality gets better
##
##lm_sqrt <- lm(sqrt(terapia_intensiva) ~. , data = cov_lm1)
##lm_sqrt
##summary(lm_sqrt)
###x11()
##par(mfrow=c(2,2))
##plot(lm_sqrt)
##
### we don't have an improvement in the normality hypotesis, so we keep our variable
##
### oppure
##powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
##  
##  boxcoxTrans <- function(x, lam1, lam2 = NULL) {
##    
##    # if we set lambda2 to zero, it becomes the one parameter transformation
##    lam2 <- ifelse(is.null(lam2), 0, lam2)
##    
##    if (lam1 == 0L) {
##      log(y + lam2)
##    } else {
##      (((y + lam2)^lam1) - 1) / lam1
##    }
##  }
##  
##  switch(method
##         , boxcox = boxcoxTrans(y, lambda1, lambda2)
##         , tukey = y^lambda1
##  )
##}
##
##
### re-run with transformation
##lm1_bct <- lm(powerTransform(terapia_intensiva, best_lambda) ~ . , data = cov_lm1)
##
### QQ-plot
###x11()
##par(mfrow=c(1,2))
##qqnorm(lm1$residuals); qqline(lm1$residuals)
##qqnorm(mnew$residuals); qqline(mnew$residuals)
##
###x11()
##par(mfrow=c(2,2))
##plot(mnew)
##
### we don't have an improvement in the normality hypotesis, so we keep our variable



###################################################################################



### what happens if we consider also terapia_intensiva_ieri (in this case, should we discard data?)
##names(cov)
##cov_lm2 <- cov
##names(cov_lm2)
##lm2 <- lm(terapia_intensiva ~. , data = cov_lm2)
##lm2
##summary(lm2)
###AIC(lm1)
##
###x11()
##par(mfrow=c(2,2))
##plot(lm2)
##
### we do a stepwise procedure to select the best lm
##lm2_step <- step(lm2)
##summary(lm2_step)
###x11()
##par(mfrow=c(2,2))
##plot(lm2_step)
##
##lm1_step$call
##lm2_step$call
##
##extractAIC(lm1)
##extractAIC(lm1_step)
##extractAIC(lm2)
##extractAIC(lm2_step)


