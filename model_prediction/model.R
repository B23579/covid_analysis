
library(tidyverse)
library(mgcv)
library(mlr)

train<-as_tibble(read.csv("NEW_data/train.csv"))
test<-as_tibble(read.csv("NEW_data/test.csv"))

test$data<-as.Date(test$data)

### Model GAM : Generalized additive model ##

### for this model ingressi_terapia_intensiva covariate don't have effect on on the model

mod_1 <- gam(terapia_intensiva ~s(ricoverati_con_sintomi)+s(totale_ospedalizzati)+ s(death), data=train)
summary(mod_1)
plot(mod_1,residuals = TRUE, pages = 1, pch = 19)

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

########### polynomial Linear model###
plot(train)


