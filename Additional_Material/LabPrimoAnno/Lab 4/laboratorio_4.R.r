###############################
#####    CORSO DI FSSB    #####
## per INGEGNERIA BIOMEDICA ###
###############################

###############################################
################ LABORATORIO 4 ################
############ REGRESSIONE LINEARE ##############
###############################################

# Riferimenti bibliografici per la teoria: 
# Ross, Capitolo 12 sulla regressione lineare

##########################################
######### Regressione lineare ############
##########################################

#### L'idea della regressione: ####
# I Dati di Galton: che legame esiste tra l'altezza dei figli e quella
# dei genitori? Il legame è del tipo y = x o differente?
# galton = read.table( 'galton_cm.txt', header = TRUE )
# names( galton )
# head( galton ) 
# plot( galton$Child, galton$Midparent, asp = T, pch = 19, 
#       xlim = c( 160, 190 ), ylim = c( 160, 190 ), xlab = 'Figli [cm]', ylab = 'Genitori [cm]' )
# rappresentazione grafica dei dati e delle possibili rette 
# per rappresentare la loro relazione:
# GUARDIAMO IL FILE galton.pdf
# Quale retta scegliereste? 
# La rossa o la blu? Perchè?
# La retta blu è la retta per cui la lunghezza complessiva delle distanze dei 
# dati dalle previsioni è minima (y- hat(y)), quindi SCELGO LA RETTA BLU !!


#### Primo esempio di regressione con R ####
## Analisi dei dati quantitativi contenuti 
## nel file "appendiceA.txt"

# Vogliamo indagare sulla relazione tra colesterolo e peso: 
# esiste un legame lineare? Qual è la retta che meglio caratterizza questo legame?

# Importiamo il dataset
dati = read.table( 'appendiceA.txt', header = T )

# Vi ricordate com'è fatto il dataset?
head( dati )


# Vogliamo vedere se esiste una relazione lineare fra colesterolo
# (variabile dati$Colesterolo, che chiameremo Y) e il peso
# (variabile  dati$Peso, che chiameremo X).

plot( dati$Peso, dati$Colesterolo, pch = 16, xlab = 'Peso', ylab = 'Colesterolo' )
plot( dati$Peso, dati$Colesterolo, pch = 16, xlab = 'Peso', ylab = 'Colesterolo',
      col = ifelse( dati$Sesso == 'F', 'pink', 'royalblue' ) )
legend( 'bottomright', fill = c( 'pink', 'royalblue' ), legend = c( 'Femmine', 'Maschi' ) )

# Vogliamo studire solo i maschi, quindi li isoliamo
# e costruiamo il relativo data.frame
maschi = dati[ which( dati$Sesso == 'M' ), ]

# rivediamo il grafico
plot( maschi$Peso, maschi$Colesterolo, pch = 16, xlab = 'Peso', ylab = 'Colesterolo',
      col = 'royalblue')



################################################
# COSTRUZIONE AUTOMATICA DELLA RETTA DI REGRESSIONE
################################################
# Utilizziamo il comando lm(), cioè linear model

# Creiamo le variabili che ci servono per la regressione
Colesterolo = maschi$Colesterolo
Peso = maschi$Peso

# Il simbolo ~ si ottiente con Alt+1+2+6
# lm(Y ~ X) con Y = risposta/variabile dipendente, 
#               X = regressore/predittore/covariata/variabile indipendente
regressione = lm ( Colesterolo ~ Peso ) 

regressione
names( regressione )

regressione$coefficients
regressione$residuals

alpha_cap = regressione$coefficients[1]
alpha_cap

beta_cap = regressione$coefficients[2]
beta_cap

res = regressione$residuals
res

# Ora disegnamo la retta sullo scatterplot
plot( maschi$Peso, maschi$Colesterolo, xlab = "Peso", ylab = "Colesterolo", pch = 19 )
abline( regressione, col = "red", lwd = 2 )


# Dove troviamo R^2 (coefficiente di determinazione)??
# In R, tramite il comando summary(), si ricavano numerose informazioni
# sul modello costruito

summary( regressione )

# In particolare, l'ultima colonna della tabella "Coefficients" ci dice
# se il corrispettivo coefficiente va inserito o meno nella
# regressione. In particolare, se il valore di Pr(>|t|) è piccolo (<0.05)
# allora il coefficiente va considerato diverso da zero, altrimenti
# può essere considerato uguale a 0 e quindi deve essere escluso
# dal modello.

# COMMENTI: R^2 ci dice la percentuale di variabilità dei dati che il 
# nostro modello riesce a spiegare.
# Qui, R^2 ci sta dicendo che il nostro modello spiega
# poco meno della metà della variabilità dei nostri dati.

## ASSUNZIONI ULTERIORI SULL'ERRORE
# In genere, nel modello di regressione, l'assunzione di media nulla non è
# l'unica assunzione che si fa sull'errore.
# Si assume anche che gli errori siano i.i.d. e distribuiti come una variabile
# aleatoria gaussiana di media 0 e varianza sigma^2. Ovvero:
# e_i ~ N (0,sigma^2) e che la varianza sia costante e non dipenda
# dall'osservazione, cioè è la stessa per tutte le osservazioni
# (questa è chiamata proprietà di omoschedasticità).

# Quando si fa una regressione, quindi, è necessario verificare che
# i residui siano effettivamente distribuiti come una normale.
# Per fare questo si possono utilizzare due grafici:
# 1) visualizzazione dell'istogramma dei residui per avere un'idea della forma
#    della distribuzione dei residui, che ci aspettiamo simmetrica
# 2) visualizzazione del QQ plot, ovvero un grafico che rappresenta
#    la relazione tra i quantili empirici dei residui e i quantili
#    teorici di una variabile aleatoria gaussiana. Se i punti di questo
#    grafico si dispongono lungo una retta, questo indica
#    normalità dei residui.


# I residui standardizzati si ottengono dividendo
# i residui del modello per la loro deviazione standard stimata, che nel summary
# si chiama Residual standard error.
res.std = res / summary( regressione )$sigma

par( mfrow = c( 2, 2 ) )
# 1
hist( res, prob = TRUE, breaks = 10, col = "lightblue", main = "Istogramma dei residui",
      ylim = c( 0 , 0.05 ) )
griglia = sort( res )
lines( griglia, dnorm( griglia, mean = 0, sd = sd( res ) ), 
       col = 'royalblue', lwd = 2 )
# 2
qqnorm( res, pch = 16 )
qqline( res, col = "red", lwd = 2 )
# 3
plot( regressione$fitted.values, res, pch = 16,
      xlab = 'y stimata', ylab = 'residui', main = 'Residui' )
abline( h = 0, col = "grey", lwd = 2, lty = 2 )
# 4
plot( regressione$fitted.values, res.std, pch = 16,
      xlab = 'y stimata', ylab = 'residui standardizzati', main = 'Residui standardizzati' )
abline( h = 2, col = "red", lwd = 2, lty = 2 )
abline( h = 0, col = "grey", lwd = 2, lty = 2 )
abline( h = -2, col = "red", lwd = 2, lty = 2 )

shapiro.test(res)

# COMMENTI: dall'istogramma non si vedono particolari asimmetrie e
# i punti del QQ plot sembrano essere disposti lungo una retta (con piccole
# deviazioni agli estremi). 
# Quindi i residui possono essere considerati normali.
# Inoltre, dal grafico dei residui, la varianza sembra essere costante,
# pertanto i residui possono essere considerati omoschedastici.

# STANDARDIZZAZIONE DEI RESIDUI
# Questa procedura permette di analizzare il comportamento dei residui
# per evidenziare effetti di eteroschedasticità e normalità.
# Calcolare i residui standardizzati è molto comodo poichè
# permette un confronto con la normale standard N(0,1).
# Valgono quindi tutte le regole empiriche viste nel precedente laboratorio
# con la semplificazione che sigma è sempre 1 e la media 0.
# In altre parole, si valutano quanti dati rimangono fuori dall'intervallo
# che dovrebbe contenere il 95% dei dati che in questo modo diventa direttamente
# 0±2 (rette rosse del quarto grafico)




# PREVISIONE DI UN NUOVO VALORE DEL COLESTEROLO

# Supponiamo di avere la misura del peso di un nuovo studente.
# Questo studente pesa 210 libbre (circa 95 Kg).
# Quanto sarà il suo colesterolo atteso? 

# costruiamo il nostro nuovo dato e lo inseriamo in un data.frame
# attenzione che la variabile si deve chiamare nello stesso modo
# del data.frame che abbiamo utilizzato per costruire il modello
nuovo = data.frame( Peso = 210 )


# rivediamo il grafico

plot( maschi$Peso, maschi$Colesterolo, pch = 16, xlab = 'Peso', 
      ylab = 'Colesterolo', col = 'royalblue' )
abline( v = nuovo$Peso, col = 'red', lwd = 2, lty = 2 )
abline( regressione, col = "red", lwd = 2 )

# facciamo la previsione utilizzando il nostro modello
prev.punt = predict( regressione, newdata = nuovo )
prev.punt

points( nuovo$Peso, prev.punt, col = 'red', pch = 16, cex = 1.5 )

# Quello che si fa solitamente in statistica è,
# in corrispondenza di un nuovo valore di x,
# fornire una stima puntuale.

# IMPORTANTE: in R il valore della nuova x deve essere inserito in un 
# dataframe, la cui variabile deve avere lo stesso nome della variabile
# indipendente che abbiamo passato al comando lm().

# sarà proprio dato dalla retta di regressione come abbiamo
# visto nel grafico? Proviamo a calcolarlo manualmente
regressione$coefficients
regressione$coefficients[1] + regressione$coefficients[2] * nuovo$Peso
# effettivamente è proprio la stessa cosa!


##############################
###### FAC-SIMILE ESAME ######
##############################

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

# Analisi della variabile "riduzione_st"

# 1) Creare le tabelle di frequenze assolute e relative e visualizzare 
#    il diagramma a barre e il diagramma a torta

# 2) Identificare la classe modale

# Analisi della variabile "pressione"

# 3) Calcolare gli indici di posizione per la variabile "pressione":
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.65

# 4) Calcolare gli indici di dispersione per la variabile "pressione":
#    varianza, deviazione standard e range interquartile

# 5) Visualizzare istogramma e boxplot per la variabile "pressione":
#    quali considerazioni possiamo fare?

# Consideriamo ora la variabile "pressione" alla luce della variabile
# categorica "riduzione_st"

# 6) Calcolare media, mediana, varianza e quantile di ordine 0.4 della
#    variabile "pressione" per ognuno dei due gruppi identificati dalla
#    variabile "riduzione_st"

# 7) Visualizzare istogrammi e boxplot della variabile "pressione" per
#    ognuno dei due gruppi identificati dalla variabile "riduzione_st"
#    e commentare.

# Relazioni tra le variabili

# 8) Calcolare covarianza e correlazione tra le variabili "pressione" e
#    "creatinina_in" e tra le variabili "creatinina_in" e "creatinina_out".
#    Trarre le dovute considerazioni in base ai valori degli indici calcolati.

# Regressione lineare

# Un medico ipotizza una relazione tra le variabili "creatinina_in" e
# "creatinina_out" del tipo:
# creatinina_out = a + b*creatinina_in + e

# 9) Fornire una stima dei coefficienti a e b. I coefficienti vanno considerati
#    nel modello?

# 10) Calcolare il coefficiente di determinazione (R^2) e la deviazione
#     standard dei residui. Cosa possiamo dire della nostra regressione?

# 11) Sono soddisfatte le ipotesi di normalità e omoschedasticità
#     dei residui?


