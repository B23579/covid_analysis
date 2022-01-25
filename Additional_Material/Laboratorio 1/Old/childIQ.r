
############################################
######      Analisi dati CHILD.IQ      #####
############################################

# The child.iq folder contains the following variables
#	ppvt     -> children’s test scores at age 3 
#	mmage    -> mother’s age at the time she gave birth 
#	educ_cat -> mom's education where 1= no hs education, 2= hs grad, 3= some college, and 4= college grad
# The sample consists of 400 children. 
# The data are a Stata file which you can read in the following way:

library ("foreign")
iq.data <- read.dta ("child.iq.dta")

names(iq.data)
dim(iq.data)

attach(iq.data)

# GOALS:
#	(a) Fit a regression of child test scores on mother’s age, display the data and
#	    fitted model, check assumptions, and interpret the slope coefficient. When do
#	    you recommend mothers should give birth? What are you assuming in making
#	    these recommendations?
#	(b) Repeat this for a regression that further includes mother’s education, interpreting
#	    both slope coefficients in this model. Have your conclusions about the timing of 
#	    birth changed?
#	(c) Now create an indicator variable reflecting whether the mother has completed
#	    high school or not. Consider interactions between the high school completion
#	    and mother’s age in family. Also, create a plot that shows the separate regression
#	    lines for each high school completion status group.
#	(d) Finally, fit a regression of child test scores on mother’s age and education level
#	    for the first 200 children and use this model to predict test scores for the next
#	    200. Graphically display comparisons of the predicted and actual scores for the final
#	    200 children

#---------------------------------------------------------------------------------------------#


# Analisi preliminare

plot(iq.data)



###########
#   (a)   #
###########

mod <- lm(ppvt ~ momage)
summary(mod)

plot(momage, ppvt, xlab = "mothers' age", ylab = "ppvt", main = "Retta di regressione per i dati del file \"child.iq.txt\"")
abline(mod,col="red")



###########
#   (b)   #
###########

mod2 <- lm(ppvt ~ momage + educ_cat)
summary(mod2)



###########
#   (c)   #
###########

hs.grd <- NULL
for(i in 1:length(educ_cat)){
	ifelse(educ_cat[i]==1,hs.grd[i]<-0,hs.grd[i]<-1)}

mod3 <- lm(ppvt ~ momage*hs.grd)
summary(mod3)

dati.lowerEdu <- iq.data[hs.grd==0,]
dati.upperEdu <- iq.data[hs.grd==1,]

mod4.1 <- lm(dati.lowerEdu$ppvt ~ dati.lowerEdu$momage)
mod4.2 <- lm(dati.upperEdu$ppvt ~ dati.upperEdu$momage)

summary(mod4.1)
summary(mod4.2)

par(mfrow=c(1,2))
plot(dati.lowerEdu$momage, dati.lowerEdu$ppvt, xlab = "mothers' age", ylab = "ppvt", main="lower level education")
abline(mod4.1,col="red")
plot(dati.upperEdu$momage, dati.upperEdu$ppvt, xlab = "mothers' age", ylab = "ppvt", main="upper level education")
abline(mod4.2,col="red")

detach(iq.data)


###########
#   (d)   #
###########

mod5 <- lm(iq.data$ppvt[1:200] ~ iq.data$momage[1:200] + iq.data$educ_cat[1:200])

nuovidati <- data.frame(momage=iq.data$momage[201:400],educ_cat=iq.data$educ_cat[201:400])
pred1 <- predict(mod5, nuovidati, interval = 'confidence', level = .95)
pred2 <- predict(mod5, nuovidati, interval = 'prediction', level = .95)

par(mfrow=c(2,2))
plot(iq.data$momage[201:400],as.numeric(pred1[,1]),main="CI")
#points(iq.data$momage[1:400],iq.data$ppvt[1:200],col="red")
plot(iq.data$momage[201:400],as.numeric(pred2[,1]),main="PI")
points(iq.data$ppvt[1:200],col="red")
plot(iq.data$educ_cat[201:400],as.numeric(pred1[,1]),main="CI")
points(iq.data$ppvt[1:200],col="red")
plot(iq.data$educ_cat[201:400],as.numeric(pred2[,1]),main="PI")
points(iq.data$ppvt[1:200],col="red")


