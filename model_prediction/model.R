
library(tidyverse)
library(mgcv)
library(mlr)
library(flexmix)
# upload data created with script EDA_V2
cov <- read.csv("cov_post_EDA.csv")
#test <- read.csv("covEDA.csv")
#view(test)
cov <- dplyr::select(cov,-X)
#test <- dplyr::select(test,-X)
cov$colore <- as.factor(cov$colore)
cov$data <- as.Date(cov$data)
#test$data <- as.Date(test$data)
#names(test)
names(cov)
str(cov)
#x11()
#plot(cov)
cov<- dplyr::select(cov,-terapia_intensiva_ieri)
#test<-filter(test,test$data>="2021-02-02")
view(cov)

view(test)

plot(cov$data,cov$deceduti)
plot(cov$data,cov$isolamento_domiciliare)
plot(cov$data,cov$nuovi_positivi)
plot(cov$data,cov$ricoverati_con_sintomi)

cov<-mutate(cov,death=a)

cov$colore <- as_factor(cov$colore)
names(cov)
str(cov)

#View(test)

#view(test)
train<- select(cov,-data)
#test1<- select(test,ricoverati_con_sintomi,perc_vax, deceduti, isolamento_domiciliare)
#view(test1)
names(train)

### Model GAM : Generalized additive model ##

### for this model ingressi_terapia_intensiva, even death covariate don't have effect on on the model

mod_1 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) +s(deceduti) + s(nuovi_positivi)
             +s(dimessi_guariti)+s(isolamento_domiciliare)+colore, data=train)
# s within the formula denote the smooth terms

summary(mod_1)
plot(mod_1,residuals = TRUE, pages = 1, pch = 19)

mod_2 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) + s(nuovi_positivi)
             +s(dimessi_guariti)+s(isolamento_domiciliare), data=train)
# s within the formula denote the smooth terms

summary(mod_2)
plot(mod_2,residuals = TRUE, pages = 1, pch = 19)

#It appears we have statistical effects for other covariate, but not for dimessi_guariti,

mod_3 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) + s(nuovi_positivi)
             +s(dimessi_guariti)+s(isolamento_domiciliare), data=train)

summary(mod_3)

#It appears we have statistical effects for other covariate, but not for nuovi_positivi,

plot(mod_3,residuals = TRUE, pages = 1, pch = 19)

mod_4<- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax)+s(dimessi_guariti)+s(isolamento_domiciliare), data=train)


summary(mod_4)
#It appears we have a strong statistical effects for other covariates, but not for perc_vax,
plot(mod_4,residuals = TRUE, pages = 1, pch = 19)

A<-AIC(mod_1,mod_2,mod_3,mod_4)
B<-BIC(mod_1,mod_2,mod_3,mod_4)
A<-mutate(A,BIC=B$BIC)
A


anova(mod_1,mod_2, test="Chisq")

anova(mod_2,mod_3, test="Chisq")
anova(mod_3,mod_4, test="Chisq")



new<-mutate(cov,fittes=mod_2$fitted.values,fittes1=mod_3$fitted.values,fittes3=mod_4$fitted.values,fittes4=mod_4$fitted.values)

#### 

p<-ggplot(new)+
  geom_point(aes(data,terapia_intensiva,color="True"))+
  geom_point(aes(data,fittes,color= "Predicted model 2"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Model fitting ")+
  theme(plot.title = element_text(hjust = 0.5))
p

ggsave(p, filename = "Result//GAM_result_mod2.png")

p<-ggplot(new)+
  geom_point(aes(data,terapia_intensiva,color="True"))+
  geom_point(aes(data,fittes1,color= "Predicted model 3"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Model fitting ")+
  theme(plot.title = element_text(hjust = 0.5))
p

ggsave(p, filename = "Result/GAM_result_mod3.png")

# next, do more on model comparison 

p<-ggplot(new)+
  geom_point(aes(data,terapia_intensiva,color="True"))+
  geom_point(aes(data,fittes3,color= "Predicted model 4"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Model fitting ")+
  theme(plot.title = element_text(hjust = 0.5))
p

ggsave(p, filename = "Result/GAM_result4.png")

# prediction 

pred<-predict(mod_1,test,type="response")

test2<-mutate(test,pre=pred)
view(test2)

p<-ggplot(test2)+
  geom_point(aes(data,terapia_intensiva,color="True"))+
  geom_point(aes(data,pred,color= "Predicted model 4"))+
  labs(x=" Periode", y= " Number of people in intensive care ", title = " Model fitting ")+
  theme(plot.title = element_text(hjust = 0.5))
p

ggsave(p, filename = "Result/GAM_predict.png")





