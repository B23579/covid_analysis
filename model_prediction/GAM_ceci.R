
library(tidyverse)
library(mgcv)
library(mlr)

# upload data created with script EDA_V2
cov <- read.csv("covid_analysis/EDA_V2/cov_post_EDA.csv")
cov <- dplyr::select(cov,-X)
names(cov)
cov$Color <- as.factor(cov$Color)
cov$Date <- as.Date(cov$Date)
names(cov)
str(cov)
#x11()
#plot(cov)
cov$Color <- as_factor(cov$Color)
names(cov)
str(cov)

#https://noamross.github.io/gams-in-r-course/
### Model GAM : Generalized additive model ##

gam1 <- gam(ICU ~ +s(Hospitalized.with.symptoms) +s(People.at.home) +s(New.positives)
            +s(Discharged.healed) +s(Deceased) +s(Percentage.vaccinated) +Color , data=cov)
# s within the formula denote the smooth terms

summary(gam1)
extractAIC(gam1)
plot(gam1,residuals = TRUE, pages = 1, pch = 19)

tab_GAM_ceci <- data.frame(matrix(ncol = 2, nrow = 0))
new_row <- c("Complete_GAM",round(extractAIC(gam1)[2], digits = 3))
tab_GAM_ceci <- rbind(tab_GAM_ceci, new_row)
colnames(tab_GAM_ceci) <- c("Model","AIC")



# I see what happens if I remove Color, the less informative variable
gam2 <- gam(ICU ~ +s(Hospitalized.with.symptoms) +s(People.at.home) +s(New.positives)
            +s(Discharged.healed) +s(Deceased) +s(Percentage.vaccinated)  , data=cov)
# s within the formula denote the smooth terms

summary(gam2)
extractAIC(gam2)
plot(gam2,residuals = TRUE, pages = 1, pch = 19)

new_row <- c("gam2",round(extractAIC(gam2)[2], digits = 3))
tab_GAM_ceci <- rbind(tab_GAM_ceci, new_row)

# anova

anova(gam2,gam1, test="Chisq")

# I see what happens if I remove Deceased, the less informative variable
gam3 <- gam(ICU ~ +s(Hospitalized.with.symptoms) +s(People.at.home) +s(New.positives)
            +s(Discharged.healed) +s(Percentage.vaccinated)  , data=cov)
summary(gam3)
extractAIC(gam3)
new_row <- c("gam2",round(extractAIC(gam3)[2], digits = 3))
tab_GAM_ceci <- rbind(tab_GAM_ceci, new_row)
anova(gam3,gam2, test="Chisq")

# the result is worse, so I choose gam2

BIC(gam1)
BIC(gam2)
BIC(gam3)

# the gam2 is better, plot:
x11()
ggplot(cov) +
  geom_point(aes(x = Date, y = ICU)) +
  geom_line(aes(x = Date,y = fitted(gam2)),color="blue", size=1.2)

# in my opinion this looks even too sensible

# prediction 

test_cov <- read.csv("covid_analysis/EDA_V2/test_cov.csv")
test_cov <- dplyr::select(test_cov,-X)
test_cov$Color <- as.factor(test_cov$Color)
test_cov$Date <- as.Date(test_cov$Date)
names(test_cov)
str(test_cov)
test_cov <- dplyr::select(test_cov,-Color)

pred<-predict(gam2,test_cov,type="response")


test2<-mutate(test_cov,pre=pred)
view(test2)

p<-ggplot(test2)+
  geom_point(aes(Date,ICU))+
  geom_line(aes(Date,pred),color="red", size=1.2)+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Model fitting ")+
  theme(plot.title = element_text(hjust = 0.5))
x11()
p


