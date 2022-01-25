###############################
#####    CORSO DI FSSB    #####
## per INGEGNERIA BIOMEDICA ###
###############################


###############################################
################ LABORATORIO 5 ################
####### REGRESSIONE LINEARE PARTE 2 ###########
################ E PREVISIONE #################
###############################################

# Riferimenti bibliografici per la teoria: 
# Ross, Capitolo 12 sulla regressione lineare multipla


################################################ 
# REGRESSIONE LINEARE CON VARIABILE CATEGORICA #
################################################ 
## Dati contenuti nel file lavoro.txt.
## Obiettivo dello studio: capire se è possibile utilizzare la variabile
## relativa al sesso (predittore categorico 'Sex'), insieme agli anni
## di servizio (predittore continuo 'Years_Service'), per prevedere con un
## modello lineare il punteggio medio ottenuto dall'individuo nel questionario
## (risposta 'Average_Score').
## Obiettivo: Stimare un modello di regressione lineare che spieghi il punteggio in funzione
## del numero di anni di servizio tenendo in considerazione la variabile 
## categorica sesso.

rm(list=ls())
graphics.off()
# importazione del dataset
dati <- read.table('lavoro.txt', header=T)
dati

dim(dati)
names(dati)
str(dati)

n <- dim(dati)[1]
attach(dati)

# diamo dei nomi diversi alle variabili per semplicità di notazione
Y <- Average_Score
X <- Years_Service
C <- Sex

detach(dati)

## Obiettivo: costruire un modello di regressione in funzione del numero di anni di servizio e della 
##            variabile categorica Sesso

#ha senso usare la variabile X come predittore di Y?
# qualitativamente: esplorazione grafica
x11()
plot(X, Y, main='Scatterplot di Y vs X', lwd=2,
     xlab='Years of Service', ylab='Average Score')
# sì, sembra esserci una qualche relazione tra X e Y

# quantitativamente: effettuiamo una regressione lineare semplice di Y rispetto a X
result <- lm(Y~X)
summary(result)
# l'output sembra confermare le intuizioni avute con l'esplorazione, anche se R^2 è piuttosto basso 
abline(result,lwd=2)

# dato che R^2 è basso, vogliamo inserire ulteriori informazioni nel modello

# proviamo a vedere le differenze tra uomini e donne:
# due colori diversi per il plot
col <- rep('blue',n)
femmine <- which(C=='Female')
maschi <- which(C=='Male')
col[femmine] <- 'red'

x11()
plot(X, Y, main='Scatterplot di Y vs X', lwd=2,
     xlab='Years of Service', ylab='Average Score', col = col)

## REGRESSIONE LINEARE CON PREDITTORE CATEGORICO
# In questo caso la procedura ottimale è costruire un unico modello di regressione
# che tenga in considerazione anche la variabile categorica. 

# Intuitivamente, guardando la distribuzione dei punti nei gruppi, possiamo osservare
# comportamenti differenti:
# 1) i gruppi di punti sono traslati e paralleli -> avremo intercette diverse nel modello di regressione
# 2) i gruppi di punti hanno incrementi relativi differenti -> avremo pendenze diverse
# 3) caso 1) + caso 2) -> diverse intercette e diverse pendenze nel modello di regressione
# si veda per la casistica il grafico 'effetti_var_categorica.pdf'

# Consideriamo un modello completo con intercetta e coefficiente angolare 
# diverso per uomini e donne
# L'intercetta comune è inserita in automatico nel modello

result1 <- lm(Y ~  X + C + X:C)
summary(result1)

# Notare che la variabile categorica C=Sex è passata direttamente al modello come factor
# e la funzione lm() si occupa di trasformare C in una variabile binaria ({0,1}).
# Dal summary, deduciamo che il gruppo maschile corrisponde ad 1, mentre le femmine corrispondono a 0 (baseline)

# Il termine di interazione tra X e il predittore categorico non è significativo
# dunque il coeff angolare delle due rette non è significativamente diverso.
# Possiamo stimare un nuovo modello senza questo termine e otterremo due rette
# con intercetta diversa ma stesso coefficiente angolare:

result2 <- lm(Y ~ C + X)
summary(result2)

# Tutti i predittori ora sono significativamente diversi da 0 
# Il valore di R^2 è molto più alto del caso precedente senza variabile categorica

# Come interpretare le stime del modello:
# modello DONNE:  Y = 7.035 + 0.097 X
# modello UOMINI: Y = 7.035 - 2.591 + 0.097 X 
#                   = 4.444 + 0.097 X

x11()
plot(X, Y, main='Scatterplot di Y vs X', lwd=2,
     xlab='Years of Service', ylab='Average Score', col = col)
coef=result2$coef

# donne
abline(coef[1],coef[3],lwd=2,col='red')
# uomini
abline(coef[1]+coef[2],coef[3],lwd=2,col='blue')

legend(23, 4.5, legend=c('femmine','maschi'),
       col=c('red','blue'), lwd=2, cex=0.85)

# Diagnostica dei residui

# 1) grafici di dispersione dei residui

# estraggo i residui del modello lineare
res <- result2$residuals
res

Ycapp <- result2$fitted

x11()
plot(X, res, main='Resuidui vs X', lwd=2,
     xlab='X', ylab='Residui')
abline(h=0, lwd=2,col='red')

x11()
plot(Ycapp, res, main='Resuidui vs Y stimati', lwd=2,
     xlab='Y stimati dal modello', ylab='Residui')
abline(h=0, lwd=2,col='red')

# si possono considerare gli errori omoschedastici

# 2) Normal Probability Plot dei residui

# verifica dell'ipotesi di Normalità dei residui

x11()
qqnorm(res, main='QQ plot dei residui del modello',pch=19)
qqline(res,lwd=2,col='red')

shapiro.test(res)

# si possono considerare i residui normali


########################
# ESERCIZIO DI COMPITO
########################

# consideriamo dati relativi a misurazioni effettuate su un campione 
# di 82 cozze provenienti dalla Nuova Zelanda e raccolte nel dataset cozze_specie.txt.

cozze = read.table( 'cozze_specie.txt', header = T )

# L'obiettivo è studiare la relazione tra la massa commestibile della cozza 
# (Massa_commestibile) misurata in grammi e altre misure che definiscono 
# le caratteristiche del mollusco:
# massa totale (Massa_totale) espressa in g
# specie (Specie) (1 e 2 per i diversi tipi)

# Si definisca il modello iniziale (più articolato possibile)
# e si effettuino le opportune semplificazioni

# il modello finale è significativo? sono soddisfatte le ipotesi sui residui?
