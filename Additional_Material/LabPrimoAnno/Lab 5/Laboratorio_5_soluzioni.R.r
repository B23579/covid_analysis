#################### LABORATORIO 5 ##################

##########################
## ESERCIZIO DI COMPITO ##
##########################

# consideriamo dati relativi a misurazioni effettuate su un campione 
# di 82 cozze provenienti dalla Nuova Zelanda e raccolte nel dataset cozze_specie.txt.

# 0) Importare il dataset

# 1) Creare la tabella delle frequenze assolute e relative della variabile "Specie" e 
#    disegnare il grafico a barre e a torta. Qual è la moda?

# 2) Calcolare i principali indici di posizione e di dispersione della variabile "Lunghezza", il
#    quantile di ordine 0.7 e disegnare l'istogramma e il boxplot di Lunghezza. 
#    Cosa potete dire sulla distribuzione dei dati?

# 3) Calcolare i principali indici di posizione e di dispersione della variabile "Massa_commestibile" e il
#    quantile di ordine 0.7 distinguendo le due Specie. Confrontare poi gli istogrammi e i boxplot 
#    delle due distribuzioni, commentando i risultati.

# 4) calcolare le matrici di covarianza e correlazione delle prime tre variabili del dataset. 
##   Rappresentare graficamente le relazioni tra le variabili
#    (colorando i punti a seconda della specie) e commentare i risultati.


# 5) Si vuole studiare la relazione tra la massa commestibile della cozza 
# (Massa_commestibile) misurata in grammi e altre misure che definiscono 
# le caratteristiche del mollusco:
# lunghezza della conchiglia (Lunghezza) espressa in mm, 
# massa totale (Massa_totale) espressa in g
# specie (Specie) (1 e 2 per i diversi tipi)

# Si definisca il modello inziale (più articolato possibile)
# e si effettuino le opportune semplificazioni

# il modello finale è significativo? sono soddisfatte le ipotesi sui residui?

# 0) Importare il dataset

cozze = read.table( 'cozze_specie.txt', header = T )

names( cozze )
head( cozze )
str( cozze ) # la quarta grandezza è quella categorica

# 1) Creare la tabella delle frequenze assolute e relative della variabile "Specie" e 
#    disegnare il grafico a barre e a torta. Qual è la moda?

specie = cozze$Specie

f_ass = table(specie)
f_ass

f_rel = prop.table(f_ass)
f_rel

barplot(f_ass)
barplot(f_rel)

pie(f_ass)
pie(f_rel)

## entrambe le categorie hanno la stessa frequenza (assoluta o relativa) quindi la variabile ha
## due valori modali. [Nota bene: nel caso di dati continui si parla di bimodalità in presenza di due
## classi con frequenze elevate simili (due massimi locali nell'istogramma)]

# 2) Calcolare i principali indici di posizione e di dispersione della variabile "Lunghezza", il
#    quantile di ordine 0.7 e disegnare l'istogramma e il boxplot di Lunghezza. 
#    Cosa potete dire sulla distribuzione dei dati?

lunghezza = cozze$Lunghezza
summary(lunghezza)
quantile(lunghezza, probs = 0.7)

var(lunghezza)
sd(lunghezza)
IQR(lunghezza)
diff(range(lunghezza))

hist(lunghezza)
abline(v = median(lunghezza), col ='red')
abline(v = mean(lunghezza), col ='green')

boxplot(lunghezza)

## Commenti: la distribuzione sembra simmetrica (media e mediana molto vicine). Dal boxplot si individua
## un otlier inferiore (infatti, media < mediana).


# 3) Calcolare i principali indici di posizione e di dispersione della variabile "Massa_commestibile" e il
#    quantile di ordine 0.7 distinguendo le due Specie. Confrontare poi gli istogrammi e i boxplot 
#    delle due distribuzioni, commentando i risultati.

massac = cozze$ Massa_commestibile

tapply(massac, specie, summary)
tapply(massac, specie, quantile, probs = 0.7)
tapply(massac, specie, var)
tapply(massac, specie, sd)
tapply(massac, specie, IQR)
diff(tapply(massac, specie, range)$'1')
diff(tapply(massac, specie, range)$'2')


par(mfrow = c(2,1))
hist(massac[which(specie=='1')], prob = T, breaks = seq(min(massac), max(massac),length=10),
     xlab = 'massa commestibile in g',main = 'Istogramma specie 1')
hist(massac[which(specie=='2')], prob = T, breaks = seq(min(massac), max(massac),length=10),
     xlab = 'massa commestibile in g',main = 'Istogramma specie 2')

## oppure, più semplicemente ma in modo meno preciso:
par(mfrow = c(2,1))
hist(massac[which(specie=='1')], prob = T, xlim= range(massac), 
     xlab = 'massa commestibile in g',main = 'Istogramma specie 1')
hist(massac[which(specie=='2')], prob = T, xlim= range(massac), 
     xlab = 'massa commestibile in g',main = 'Istogramma specie 2')

dev.new()
boxplot(massac ~ specie)

## Sia dall'istogramma che dal boxplot si vede che la massa commestibile è mediamente maggiore
## nella specie 2 che nella 1, mentre hanno simile dispersione. Inoltre, la distribuzione nella specie 1 è sbilanciata e 
## particolarmente concentrata su valori bassi. La distribuzione nella specie 2, seppur più simmetrica
## rispetto alla prima, ha una coda a destra più pesante che a sinistra.
## Entrambe le distribuzioni presentano un outlier superiore.


# 4) calcolare le matrici di covarianza e correlazione delle prime tre variabili del dataset. 
##   Rappresentare graficamente le relazioni tra le variabili
#    (colorando i punti a seconda della specie) e commentare i risultati.

round(cov(cozze[,1:3]),2)
round(cor(cozze[,1:3]),2)

pairs( cozze[,1:3], col = cozze[,4] ) 

## Dal grafico si nota che massa commestibile e massa totale hanno una forte dipendenza
## lineare l'una dall'altra (infatti il loro indice di correlazione vale 0.79).
## la dipendenza lineare tra lunghezza e le altre due variabili è più debole ( indici di correlazione minori)


# 5) Si vuole studiare la relazione tra la massa commestibile della cozza 
# (Massa_commestibile) misurata in grammi e altre misure che definiscono 
# le caratteristiche del mollusco:
# massa totale (Massa_totale) espressa in g
# specie (Specie) (1 e 2 per i diversi tipi)

# Si definisca il modello iniziale (più articolato possibile)
# e si effettuino le opportune semplificazioni

# il modello finale è significativo? sono soddisfatte le ipotesi sui residui?

y = cozze$Massa_commestibile
x1 = cozze$Massa_totale
s = cozze$Specie-1  ## rendiamo la variabile binaria 0-1


reg = lm( y ~ x1+s + x1:s)
summary( reg )

## il p-value di x1:s è alto, quindi togliamo questa interazione

reg_2 = lm( y ~ x1 + s)
summary( reg_2 )

## tutti i regressori sono significativi ( p-values bassi) e R^2 è alto (0.9) quindi il modello
## spiega circa il 90% della variabilità di y (molto bene!)

dev.new()
plot( x1, y, col = s +1 , pch = 19)
abline( reg_2$coefficients[1]+ reg_2$coefficients[3],reg_2$coefficients[2], col = 'red' , lwd=2)
abline( reg_2$coefficients[1],reg_2$coefficients[2], col = 'black', lwd=2 )


## diagnostica modello

residui = reg_2$residuals
res.std = residui/summary(reg_2)$sigma

dev.new()
par( mfrow = c( 1,3 ) )
hist( residui, prob = TRUE, breaks = 20 )
plot( y, res.std )
abline( h = 0, col='grey', lty = 2 )
abline( h = 2, col='red', lty = 2 )
abline( h= -2, col='red', lty = 2 )
qqnorm( residui )
qqline( residui, col='red' )

shapiro.test(res.std)

## l'omoschedasticità non sembra essere verificata, come si vede dallo scatterplot dei residui
## in cui appaiono andamenti anomali dei residui (la varianza aumenta all'aumentare del valore di y).
## Inoltre, l'ipotesi di normalità, come si vede dal qqplot e dal basso p-value dello shapiro.test 
## non è verificata. Il modello dunque non è valido, seppur buono e significativo, perché non rispetta 
## le ipotesi alla base della regressione.
