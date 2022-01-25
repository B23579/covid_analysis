# Analisi dei dati contenuti nel file "creatinina.txt"

# I dati consistono in osservazioni di 4 variabili:
# 1) pressione: pressione arteriosa sanguigna.
# 2) riduzione_st: riduzione dello slivellamento del tratto ST
#    dell'elettrocardiogramma a un'ora dell'intervento di angioplastica.
#    E' una variabile categorica che vale 1 se vi è stata la riduzione e
#    0 altrimenti.
# 3) creatinina_in: valore della creatinina prima dell'operazione
# 4) creatinina_out: valore della creatinina dopo l'operazione

# DOMANDE (durante l'esame ricordatevi che dovrete riportare sul foglio
# i comandi utilizzati per rispondere alle domande e i valori numerici
# delle risposte)

# 0) Importare il dataset contenuto nel file "creatinina.txt" in R
dati = read.table('creatinina.txt', header = T)

# Analisi della variabile "riduzione_st"
rid = as.factor(dati$riduzione_st)
head(rid)

# 1) Creare le tabelle di frequenze assolute e relative e visualizzare 
# il diagramma a barre e il diagramma a torta

freq_ass <- table(rid)
prop.table(freq_ass)
barplot( freq_ass )
pie(table(rid))

# 2) Identificare la classe modale
table(rid) # la 1
which.max(table(rid) )

## in realtà basta guardare la tabella delle frequenze e prendere la classe con frequenza
## maggiore. Nel caso in cui ad occhio non si riuscisse ad individuare, si fa il which.max

# Analisi della variabile "pressione"
pres = dati$pressione

# 3) Calcolare gli indici di posizione per la variabile "pressione":
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.65
summary(pres)
quantile(pres, probs = 0.65)

# 4) Calcolare gli indici di dispersione per la variabile "pressione":
#    range, varianza, deviazione standard e range interquartile
diff(range(pres))
var(pres)
sd(pres)
IQR(pres)

# 5) Visualizzare istogramma e boxplot per la variabile "pressione":
#    quali considerazioni possiamo fare?
istogramma <- hist(pres, prob = TRUE )
boxplot(pres)

# Consideriamo ora la variabile "pressione" alla luce della variabile
# categorica "riduzione_st"

# 6) Calcolare media, mediana, varianza e quantile di ordine 0.4 della
#    variabile "pressione" per ognuno dei due gruppi identificati dalla
#    variabile "riduzione_st"


tapply(pres, rid, mean)
tapply(pres, rid, median)
tapply(pres, rid, var)
tapply(pres, rid, quantile, probs = 0.4)

# 7) Visualizzare istogrammi e boxplot della variabile "pressione" per
#    ognuno dei due gruppi identificati dalla variabile "riduzione_st"
#    e commentare.
par(mfrow=c(2,1))

hist(pres[which(rid==1)], xlim=range(pres), col = 'blue', prob = T, 
     xlab = 'Pressione', main = 'Istogramma della pressione per rid=1')
hist(pres[which(rid==0)], xlim=range(pres), col = 'forestgreen', prob=T,
     xlab = 'Pressione', main = 'Istogramma della pressione per rid=0')

dev.off()

boxplot(pres~rid)
# Relazioni tra le variabili

# 8) Calcolare covarianza e correlazione tra le variabili "pressione" e
#    "creatinina_in" e tra le variabili "creatinina_in" e "creatinina_out".
#    Trarre le dovute considerazioni in base ai valori degli indici calcolati.

## grafico in più
pairs(dati)

cov(pres, dati$creatinina_in)
cor(pres, dati$creatinina_in)
cov(dati$creatinina_in, dati$creatinina_out)
cor(dati$creatinina_in, dati$creatinina_out)

# Regressione lineare

# Un medico ipotizza una relazione tra le variabili "creatinina_in" e
# "creatinina_out" del tipo:
# creatinina_out = a + b*creatinina_in + e

# 9) Fornire una stima dei coefficienti a e b. I coefficienti vanno considerati
#    nel modello?

## di solito, fare il plot in un modello lineare è molto utile perchè vi permette di avere un'idea
## della relazione (lineare) tra le due variabili
plot(dati$creatinina_in, dati$creatinina_out)

# # X <- dati$creatinina_in
# # Y <- dati$cretinina_out
# # lm(Y~X)

mod = lm(dati$creatinina_out ~ dati$creatinina_in)
summary(mod)

# 10) Calcolare il coefficiente di determinazione (R^2) e la deviazione
#     standard dei residui. Cosa possiamo dire della nostra regressione?

names(summary(mod))
summary(mod)$r.squared
summary(mod)$sigma

## li potete vedere anche solo da summary(mod), senza accedere ai campi specifici 
## ma guardando il tutto

# 11) Sono soddisfatte le ipotesi di normalità e omoschedasticità
#     dei residui?

res.std = mod$residuals / summary(mod)$sigma

par( mfrow = c(1,3))
plot( mod$fitted.values, res.std, xlab = 'y_hat', ylab = 'Residui standardizzati' )
abline( h = 2, col = 'red', lwd = 2, lty = 2 )
abline( h = 0, col = 'grey', lwd = 2, lty = 2 )
abline( h = -2, col = 'red', lwd = 2, lty = 2 )

hist( res.std, prob = T, ylim = c(0,0.4) )
#lines( sort( res.std ), dnorm( sort(res.std), 0,  1), col = 'red', lwd = 2 )

qqnorm( res.std)
qqline(res.std, col = 'red')

## l'ipotesi di normalità dei residui sembra verificata (istogramma e qqplot dei residui),
## mentre l'ipotesi di omoschedasticità no (la nuvola di punti non è omogenea su tutti i valori di y_hat).
