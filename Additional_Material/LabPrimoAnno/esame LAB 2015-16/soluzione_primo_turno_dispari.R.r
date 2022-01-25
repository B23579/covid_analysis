#D0
dati = read.table("data1.txt", header=TRUE)
colnames(dati)
attach(dati)
gradazione =v1
temperatura =v2
durata =v3
tipo =v4

ls()
detach(dati)
search()

#D1
table(tipo)
barplot(table(tipo))
moda_tipo = 0 # perché più numerosa 

#D2

summary(temperatura)

#D3
b = seq(2,36,2)
b

hist(temperatura, breaks = b) # bimodale ma simmetrica, quasi una mistura di due normali

#D4
#varianza campionaria
tapply(temperatura, tipo, var)
#deviazione standard
tapply(temperatura, tipo, sd)

# coefficiente di variazione
tapply(temperatura, tipo, sd) / tapply(temperatura, tipo, mean)

#IQR
tapply(temperatura, tipo, IQR)
#range
tapply(temperatura, tipo, max) - tapply(temperatura, tipo, min)

# D5
boxplot(temperatura ~ tipo) # diverso centro, il secondo ha un outlier troppo grande rispetto al resto della popolazione, ma stessa variabilità ed entrambi simmetrici.

# D6

cor(gradazione, temperatura)

# positivo, non trascurabile ma non altissimo : 

cor(gradazione, temperatura)^2 # in un eventuale modello di regressione lineare semplice di gradazione su temperatura avrei un R2 \simeq 0.31, cioè basso

# D7

modello_completo = lm(gradazione ~ temperatura + tipo + durata)
summary(modello_completo)
# temperatura non è assolutamente significativa, tutte le altre lo sono, perché hanno p-value simeq 0.  il modello complessivamente è un buon modello  (p-value: < 2.2e-16) dell' F-test

# D8
modello_tipo_durata = lm(gradazione ~  tipo + durata)
summary(modello_tipo_durata)

#E' il modello più semplice cui arrivo
names(summary(modello_tipo_durata))

summary(modello_tipo_durata)$coefficients # valori dei coefficienti significativi: F-statistic: 510.8 on 2 and 182 DF,  p-value: < 2.2e-16 il modello rimane complessivamente ottimo


#D9 
res_std = summary(modello_tipo_durata)$residuals / summary(modello_tipo_durata)$sigma

# normalità
shapiro.test(res_std)
qqnorm(res_std)
abline(a=0, b=1, col="red") # perfetto
# non rifiuto  l'ipotesi di normali 0,1

# omoschedasticità: 
names(modello_tipo_durata)
plot(modello_tipo_durata$fitted.values, res_std)
abline(a=-2, b=0, col="red")
abline(a=2, b=0, col="red")
# andamento dei residui standardizzati casuale al variare dei dati stimati e tutti compresi in (-2,2) come mi aspetto al 95% sei i dati sono norm(0,1)

# D10

summary(modello_tipo_durata)$coefficients[1,1] + summary(modello_tipo_durata)$coefficients[2,1] * 0 + summary(modello_tipo_durata)$coefficients[3,1]*10

# 18.97191

