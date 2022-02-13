
library(tidyverse)
library(mgcv)
library(mlr)

# upload data created with script EDA_V2
cov <- read.csv("cov_post_EDA.csv")
cov <- dplyr::select(cov,-X)
cov$colore <- as.factor(cov$colore)
cov$data <- as.Date(cov$data)
names(cov)
str(cov)
#x11()
#plot(cov)
cov<- dplyr::select(cov,-terapia_intensiva_ieri)
cov$colore <- as_factor(cov$colore)
names(cov)
str(cov)

train<- select(cov,-data)
names(train)

### Model GAM : Generalized additive model ##

### for this model ingressi_terapia_intensiva, even death covariate don't have effect on on the model

mod_1 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) +s(deceduti) + s(nuovi_positivi)
             +s(dimessi_guariti)+s(isolamento_domiciliare)+s(colore,bs="re"), data=train)
# s within the formula denote the smooth terms

summary(mod_1)
plot(mod_1,residuals = TRUE, pages = 1, pch = 19)

mod_2 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) +s(deceduti) + s(nuovi_positivi)
             +s(dimessi_guariti)+s(isolamento_domiciliare), data=train)
# s within the formula denote the smooth terms

summary(mod_2)
plot(mod_2,residuals = TRUE, pages = 1, pch = 19)

#It appears we have statistical effects for other covariate, but not for dimessi_guariti,

mod_3 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) +s(deceduti) + s(nuovi_positivi)
             +s(isolamento_domiciliare), data=train)

summary(mod_3)

#It appears we have statistical effects for other covariate, but not for nuovi_positivi,

plot(mod_3,residuals = TRUE, pages = 1, pch = 19)

mod_4<- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) +s(deceduti) 
             +s(isolamento_domiciliare), data=train)


summary(mod_4)
#It appears we have a strong statistical effects for other covariates, but not for perc_vax,
plot(mod_4,residuals = TRUE, pages = 1, pch = 19)

mod_5<- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(deceduti) 
            +s(isolamento_domiciliare), data=train)

summary(mod_5)
#It appears we have a strong statistical effects for all the covariates
plot(mod_4,residuals = TRUE, pages = 1, pch = 19)


AIC(mod_1,mod_2,mod_3,mod_4,mod_5)

anova(mod_1,mod_2,mod_3,mod_4, test="Chisq")



new<-mutate(cov,fittes=mod_2$fitted.values,fittes1=mod_4$fitted.values)

#### 

p<-ggplot(new)+
  geom_point(aes(data,terapia_intensiva,color="True"))+
  geom_line(aes(data,fittes,color= "Predicted_m2"))+
  geom_line(aes(data,fittes1,color= "Predicted_m4"))
p

ggsave(p, filename = "Result//GAM_result1.png")

p<-ggplot(new)+
  geom_point(aes(data,terapia_intensiva,color="True"))+
  geom_point(aes(data,fittes,color= "Predicted_m2"))
p

ggsave(p, filename = "Result/GAM_result2.png")

# next, do more on model comparison 






