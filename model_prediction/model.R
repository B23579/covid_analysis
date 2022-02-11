
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

names(cov)
str(cov)

train<- select(cov,-data,-colore)
names(train)

### Model GAM : Generalized additive model ##

### for this model ingressi_terapia_intensiva, even death covariate don't have effect on on the model

mod_1 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) +s(deceduti) + s(nuovi_positivi)
             +s(dimessi_guariti)+s(isolamento_domiciliare), data=train)
# s within the formula denote the smooth terms

summary(mod_1)
plot(mod_1,residuals = TRUE, pages = 1, pch = 19)

#It appears we have statistical effects for other covariate, but not for dimessi_guariti,

mod_2 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) +s(deceduti) + s(nuovi_positivi)
             +s(isolamento_domiciliare), data=train)

summary(mod_2)

#It appears we have statistical effects for other covariate, but not for nuovi_positivi,

plot(mod_2,residuals = TRUE, pages = 1, pch = 19)

mod_3<- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(perc_vax) +s(deceduti) 
             +s(isolamento_domiciliare), data=train)


summary(mod_3)
#It appears we have a strong statistical effects for other covariates, but not for perc_vax,
plot(mod_3,residuals = TRUE, pages = 1, pch = 19)

mod_4<- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(deceduti) 
            +s(isolamento_domiciliare), data=train)

summary(mod_4)
#It appears we have a strong statistical effects for all the covariates
plot(mod_4,residuals = TRUE, pages = 1, pch = 19)


AIC(mod_1,mod_2,mod_3,mod_4)

anova(mod_1,mod_2,mod_3,mod_4, test="Chisq")


# Let's check whether the model have some muticolinearity 

library(car)

vif(mod_1)

## plot the prediction and the target on the same plot####

pred<-predict(mod_1, test)

new<-mutate(test,fittes=pred)

#### 

p<-ggplot(new)+
  geom_line(aes(data,terapia_intensiva,color="True"))+
  geom_line(aes(data,fittes,color= "Predicted"))
p

ggsave(p, filename = "visualization/GAM_result.png")

# in this step, we should check the variable inflation


train$data<-as.Date(train$data)
s









########### polynomial Linear model###
plot(train)

####### GLM

model_glm = glm(terapia_intensiva ~ ricoverati_con_sintomi+ingressi_terapia_intensiva+ totale_ospedalizzati + death, data=train
                , family = "gaussian")
coef(model_glm)
head(predict(model_glm))

## plot the prediction and the target on the same plot####

pred<-predict(model_glm, test)

new<-mutate(test,fittes=pred)

#### 

p<-ggplot(new)+
  geom_line(aes(data,terapia_intensiva,color="True"))+
  geom_line(aes(data,fittes,color= "Predicted"))
p

ggsave(p, filename = "visualization/GLM_result.png")


