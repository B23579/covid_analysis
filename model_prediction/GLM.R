###############################################################################
#                                GLM                                          #
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
library(stats)
library(jtools)

library(sjPlot)
library(sjlabelled)
library(sjmisc)


#devtools::install_github("cardiomoon/ggiraphExtra")

#setwd("C:/Users/Cecilia/OneDrive/Desktop/Ceci/UniTS/Statistical_Methods_for_Data_Science/Progetto/covid_analysis/model_prediction")

# upload data created with script EDA_V2
cov <- read.csv("cov_post_EDA.csv")
cov <- dplyr::select(cov,-X)
cov$Color <- as.factor(cov$Color)
cov$Date <- as.Date(cov$Date)
names(cov)
str(cov)
#x11()
#plot(cov)


# Fit complete GLM with original variables
cov_glm1 <- cov
glm1 <- glm(ICU ~. -Date, data = cov_glm1, family = poisson)
summary(glm1)
extractAIC(glm1)

tab_Glm <- data.frame(matrix(ncol = 2, nrow = 0))
new_row <- c("complete_Glm",round(extractAIC(glm1)[2], digits = 3))
tab_Glm <- rbind(tab_Glm, new_row)
colnames(tab_Glm) <- c("Model","AIC")

# Before starting to interpret results, let's check whether the model has over-dispersion
# or under-dispersion.
# If the Residual Deviance is greater than the degrees of freedom, then over-dispersion exists.
# The Null deviance shows how well the response variable is predicted by a model that includes 
# only the intercept (grand mean) whereas residual with the inclusion of independent variables.
# (https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/)

# These results are somehow reassuring. First, the null deviance is high, which means it makes sense to use more
# than a single parameter for fitting the model. Second, the residual deviance is relatively low (and close to the number
# of degrees of freedom), which indicates that the log likelihood of our model is close to the log likelihood of the 
# saturated model.
# (https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/)

# we perform a test to confirm this theory:
# To calculate the p-value for the deviance goodness of fit test we simply calculate the 
# probability to the right of the deviance value for the chi-squared distribution on 113 
# degrees of freedom:
pchisq(glm1$deviance, df=glm1$df.residual, lower.tail=FALSE)

with(glm1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# 0.3119235
# The null hypothesis is that our model is correctly specified, 
# and we have strong evidence to accept that hypothesis
# (https://thestatsgeek.com/2014/04/26/deviance-goodness-of-fit-test-for-poisson-regression/)

x11()
par(mfrow=c(2,2))
plot(glm1)


x11()
ggplot(cov_glm1) +
  geom_point(aes(x = Date, y = ICU)) +
  geom_line(aes(x = Date,y = fitted(glm1)),color="blue", size=1.2)



####################################################################################################


# Stepwise procedure
glm1_step <- step(glm1)
summary(glm1_step)
extractAIC(glm1_step)

new_row <- c("Glm_auto_step",round(extractAIC(glm1_step)[2], digits = 3))
tab_Glm <- rbind(tab_Glm, new_row)

# the reduced method has a better AIC, so we choose this one

# Before starting to interpret results, let's check whether the model has over-dispersion
# or under-dispersion.
# If the Residual Deviance is greater than the degrees of freedom, then over-dispersion exists.
# The Null deviance shows how well the response variable is predicted by a model that includes 
# only the intercept (grand mean) whereas residual with the inclusion of independent variables.
# (https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/)

# These results are somehow reassuring. First, the null deviance is high, which means it makes sense to use more
# than a single parameter for fitting the model. Second, the residual deviance is relatively low (and close to the number
# of degrees of freedom), which indicates that the log likelihood of our model is close to the log likelihood of the 
# saturated model.
# (https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/)

# we perform a test to confirm this theory:
# To calculate the p-value for the deviance goodness of fit test we simply calculate the 
# probability to the right of the deviance value for the chi-squared distribution on 113 
# degrees of freedom:
pchisq(glm1_step$deviance, df=glm1_step$df.residual, lower.tail=FALSE)

with(glm1_step, cbind(res.deviance = deviance, df = df.residual,
                 p = pchisq(deviance, df.residual, lower.tail=FALSE)))


# 0.3273179
# The null hypothesis is that our model is correctly specified, 
# and we have strong evidence to accept that hypothesis
# (https://thestatsgeek.com/2014/04/26/deviance-goodness-of-fit-test-for-poisson-regression/)

x11()
par(mfrow=c(2,2))
plot(glm1_step)

# I think that we should do this analysis for each model and then use the tools that we have to compare the nested models.

# People.at.home has a p-value of 7%, what happens if I discard it?
glm1_manual <- update(glm1_step, . ~ . -People.at.home)
summary(glm1_manual)
extractAIC(glm1_manual)

new_row <- c("Glm_manual_step",round(extractAIC(glm1_manual)[2], digits = 3))
tab_Glm <- rbind(tab_Glm, new_row)

# The AIC increases again, so we choose to consider the automatic step model

x11()
ggplot(cov_glm1) +
  geom_point(aes(x = Date, y = ICU)) +
  geom_line(aes(x = Date,y = fitted(glm1_step)),color="blue", size=1.2)


# comparing nested models

anova(glm1_step,glm1,test="Chisq")

# the excluded variables were not statistically significant predictors -> we choose the reduced model!

