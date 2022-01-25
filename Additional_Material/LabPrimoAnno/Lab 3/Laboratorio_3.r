###############################
#####    CORSO DI FSSB    #####
## per INGEGNERIA BIOMEDICA ###
###############################

###############################################
################ LABORATORIO 3 ################
###########  STATISTICA DESCRITTIVA ###########
################# (PARTE 3) ###################
###############################################

# Riferimenti bibliografici per gli argomenti trattati in questa lezione: 
# ----- Capitolo 2 del Ross, 2014 -----
# Sezioni 2.2 per i barplot (qua chiamati grafici a 
# barre o a bastoncini; saltare i diagrammi poligonali)
# Sezione 2.3 sugli istogrammi 
# ------ Capitolo 3 del Ross, 2014 -----------
# Sezioni 3.2 (media campionaria),  3.3 (su mediana campionaria),  
# 3.3.1 (su percentili o quantili), 3.4 (per la moda) e 3.5 (per la  varianza campionaria, 
# deviazione standard). 
# 3.6 (Gli insiemi di dati normali e la regola empirica)
# 3.7 Coefficiente di correlazione campionaria.


# NORMALITA' DEI DATI
# Un insieme di dati si dice NORMALE (o GAUSSIANO) se il rispettivo istogramma ha le proprietà seguenti:
# 1. Ha il punto massimo in corrispondenza dell'intervallo centrale
# 2. Ha forma a campana
# 3. L'istogramma è simmetrico rispetto all'intervallo centrale 
# Se l'istogramma di un insieme di dati è vicino a essere un istogramma "normale", 
# allora l'insieme dei dati è approssimativamente normale.

# Dopo aver impostato la cartella di lavoro giusta, 
# importiamo il file di dati 'record_2.txt'
record = read.table( 'record_2.txt', header = T )
head( record )
dim( record )
str( record )


# analizziamo il comportamento dei 100m e dei 1000m
# calcoliamo media e dev. std. di m100 e m1000

# 1. istogramma

hist( record$m100, prob = T, main = 'Istogramma di m100', xlab = 'm100')
hist( record$m1000, prob = T, main = 'Istogramma di m1000', xlab = 'm1000')

# istogramma più fine
hist( record$m100, prob = T, breaks = 12, main = 'Istogramma di m100', xlab = 'm100' )



media.m100 = mean( record$m100 )
media.m100
sd.m100 = sd( record$m100 )
sd.m100

media.m1000 = mean( record$m1000 )
media.m1000
sd.m1000 = sd( record$m1000 )
sd.m1000

# calcoliamo la griglia che serve al campionamento dalla normale
griglia.m100 = seq( min( record$m100 ), max( record$m100 ), length = 100 )
griglia.m1000 = seq( min( record$m1000 ), max( record$m1000 ), length = 100 )

par ( mfrow = c( 1, 2 ) )
hist(record$m100, prob = T, breaks = 10, main = 'Istogramma di m100', xlab = 'm100',
     xlim = c( 10, 13 ) )
lines(griglia.m100, dnorm(griglia.m100, media.m100, sd.m100), col = 'red', lwd = 2 )

hist(record$m1000, prob = T, breaks = 10, main = 'Istogramma di m1000', xlab = 'm1000',
     xlim = c( 0, 700 ) )
lines(griglia.m1000, dnorm(griglia.m1000, media.m1000, sd.m1000), col = 'red', lwd = 2 )

# vediamo ora un altro metodo grafico per sondare la normalità dei dati
# Lo strumento si chiama Normal Probability Plot e rappresenta in ascissa
# i quantili teorici di una normale standard (media = 0, var. = 1) e 
# in ordinata i quantili empirici calcolati a partire dai dati

par ( mfrow = c( 1, 2 ) )
qqnorm( record$m100, pch = 16, main = 'Normal Q-Q Plot - m100' )
qqline( record$m100, lwd = 2, col = 'red' )
qqnorm( record$m1000, pch = 16, main = 'Normal Q-Q Plot - m1000' )
qqline( record$m1000, lwd = 2, col = 'red' )


# LETTURA DEL GRAFICO E OSSERVAZIONI:
# Per dati normali (Gaussiani), i punti del qqplot sono disposti  sulla linea rossa. 
# Più i dati si distribuiscono lungo la retta, maggiore
# è la loro normalità. Quello che cerchiamo, quindi, sono deviazioni
# dalla linea rossa.
# Se osserviamo i due grafici, si nota come m100 sia molto più aderente 
# alla retta rispetto ai dati di m1000 dove si nota un allontanamento dal comportamento
# gaussiano soprattutto nella coda destra dei dati.


# TEST di Shapiro

shapiro.test(record$m100)
shapiro.test(record$m1000)

# La funzione shapiro.test() ci aiuta a indagare la normalità di un campione di dati. 
# In particolare, dell'output ci interessa il numero "p-value = ... ".
# Ricetta per la normalità (da imparare):
#   - se p-value < 0.05 -> i dati NON possono essere considerati normali
#   - se p-value > 0.05 -> i dati possono essere considerati normali

# dato che per m100 abbiamo p-value = 0.2569 -> i dati sono normali
# dato che per m1000 abbiamo p-value = 0.000951 -> i dati NON sono normali

# Tuttavia, se siamo proprio interessati alla normalità possiamo indagare se 
# i dati trasformati con una qualche funzione standard (i.e. esponenziale, log, ecc.)
# sono normali.

# Proviamo quindi a trasformare la variabile m1000 in record con un logaritmo
# costruiamo la nuova variabile 

m1000.log = log ( record$m1000 )

media.m1000.log = mean( m1000.log )
sd.m1000.log = sd( m1000.log )
griglia.m1000.log = seq( min( m1000.log ), max( m1000.log ), len = 100 )

par ( mfrow = c( 1, 2 ) )
hist( record$m1000, prob = T, breaks = 10, main = 'Istogramma di m1000', xlab = 'm1000')
lines( griglia.m1000, dnorm( griglia.m1000, media.m1000, sd.m1000), col = 'red', lwd = 2 )

hist( m1000.log, prob = T, breaks = 10,
     main = 'Istogramma di log(m1000)', xlab = 'log(m1000)' )
lines( griglia.m1000.log, dnorm( griglia.m1000.log, media.m1000.log, sd.m1000.log), col = 'red', lwd = 2 )

# Vediamo i qqplot

par ( mfrow = c( 1, 2 ) )
qqnorm( record$m1000, pch = 16, main = 'Normal Q-Q Plot - m1000' )
qqline( record$m1000, lwd = 2, col = 'red' )
qqnorm( m1000.log, pch = 16, main = 'Normal Q-Q Plot - log(m1000)' )
qqline( m1000.log, lwd = 2, col = 'red' )

dev.off()

shapiro.test(m1000.log)
# p-value = 0.5012 -> i dati trasformati col logaritmo sono normali :)

# Sembra proprio che la trasformazione logaritmica porti
# ad avere una distribuzione sensibilmente più gaussiana
# Si nota, infatti, come l'allontanamento dalla retta ora sia
# molto più contenuto. Rimane tuttavia una coda un po' pesante a sinistra
# Non è comunque detto che applicare trasformazioni a dati non gaussiani
# risolva sempre il problema. Se la distribuzione dei dati è nativamente
# non normale, non ci si può fare molto...


##########################################
########## Analisi bivariata #############
###### (confronto tra variabili) #########
##########################################

## analisi dei dati quantitativi contenuti 
## nel file "record.txt"

plot( record$m100, record$m200, asp=1,  col = "forestgreen", pch = 16,
      main = 'm100 vs. m200', xlab = 'm100', ylab = 'm200' )

# Analizziamo quantitativamente la relazione tra
# le variabili record$m100 e record$m200.

# Un indice per valutare la dipendenza lineare tra due variabili
# è la covarianza.
# Covarianza positiva indica che le variabili sono direttamente proporzionali,
# covarianza negativa, invece, indica che sono inversamente proporzionali.
# Quando la covarianza è uguale a zero le variabili si dicono scorrelate.
# ATTENZIONE: la non correlazione non implica l'indipendenza!!!

# la covarianza campionaria è data dalla seguente formula:
#   cov(x, y) = sum( ( x_i - media.campionaria(x) ) * ( y_i - media.campionaria(y) ) ) / ( n - 1 )

# Per calcolare la covarianza campionaria con R 
# si utilizza il comando cov

cov( record$m100, record$m200 )

# la covarianza non dipende dall'ordine degli argomenti
cov( record$m200, record$m100 )

# NB: Cov(X,X) = Var(X)

cov( record$m100, record$m100 )
var( record$m100 )

# In alternativa si può utilizzare l'indice di correlazione LINEARE che, invece,
# a) è una quantità sempre compresa tra -1 e 1, 
# b) è  uguale a  1 se y = a + b*x dove b è costante positiva, 
# c) è  uguale a -1 se y = a + b*x dove b è costante negativa
# A differenza della covarianza, l'indice di correlazione ha un
# "significato universale", ovvero è possibile confrontare indici
# di correlazione tra variabili diverse.
# L'indice di correlazione compionario tra X e Y si calcola come:
# cov(X,Y)/[sd(X)*sd(Y)] 

cov( record$m100, record$m200 ) / ( sd( record$m100 ) * sd( record$m200 ) )

# Per calcolare l'indice di correlazione LINEARE con un unico comando
cor( record$m100, record$m200 )
# NB: Cor(X,X)=1
cor( record$m100, record$m100 )

# In un dataset multivariato, possiamo riassumere questi indici relativi
# a tutte le coppie di variabili in un'unica matrice

cov( record ) # Matrice di varianza e covarianza
cor( record ) # Matrice di correlazione

# potete usare il comando round( variabile, numero decimali )
# per una visualizzazione più immediata

round( cov( record ), 2 )
round( cor( record ), 2 )

# Rappresentazione grafica di dati multivariati

# Scatterplot
pairs( record )

# Boxplot
boxplot( record, col = c( 2, 3, 4, 5, 6, 7 ) )



##############################
# ESERCIZI DA FARE IN CLASSE #
##############################

##############################
######## ESERCIZIO 1  ########
##############################


# 0) caricare il file dati "arance.txt"
# il dataset contiene 35 osservazioni relative
# alla crescita di alberi di arance. Sono presenti
# 3 variabili: 
#   Tree: identificativo dell'albero
#   age: età dell'albero in giorni dal 31/12/1968
#   circumference: valore della circonferenza in mm

arance = read.table( 'arance.txt', header = T )

# 1) Trasformare le età in mesi considerandoli tutti di 30 giorni 
# approssimandoli all'unità. 
#  Suggerimento: non voglio nessuna cifra decimale

# 2) Trasformare la circonferenza in diametro
# Suggerimento: R ha tante costanti memorizzate

# 3) Calcolare il massimo diametro raggiunto per ogni albero

# 4) Qual è l'albero che ha il diametro più piccolo dopo 22 mesi?

# 5) Quali sono gli alberi che presentano una maggiore crescita?
# Riportare i valori della crescita e mostrare il risultato graficamente



##############################
######## ESERCIZIO 2  ########
##############################

# 0) Caricare il dataset 'airquality.txt'
airquality = read.table( 'airquality.txt', header = T )

# 1) Ci sono degli NA presenti? Se sì, quanti sono?

# 2) Calcolare media e varianza per le prime 4 colonne
# Si utilizzi l'opzione "na.rm = TRUE" nei comandi (senza le "" )

# 3) Costruire un grafico di dispersione unico per le prime 4 
# colonne e calcolare covarianza e correlazione
# Si utilizzi l'opzione use = 'complete.obs' per questi due comandi

# Commentare il grafico e i risultati ottenuti

# 4) Costruire i qqplot per le prime 4 variabili e
# commentare i risultati



##############################
###### ESERCIZI PER VOI ######
##############################

##############################
######## ESERCIZIO 1  ########
##############################

## analisi dei dati quantitativi contenuti 
## nel dataframe "iris" (già contenuto in R)

# 0) Visualizzare alcune informazioni (dimensioni, struttura, prime
#    osservazioni) del dataset "iris".
#    Creare poi un nuovo dataframe chiamato "iris2" contenente solo le
#    prime 4 colonne del dataframe "iris". Eseguire poi 
#    l'attach del nuovo dataframe.

# carico il db in memoria e lo chiamo iris

# 1) Considerare solo le variabili "Petal.Length" e "Petal.Width": calcolare
#    covarianza campionaria e correlazione campionaria fra le due variabili
#    e commentare

# 2) Visualizzare, tramite opportuni grafici, le relazioni tra tutte
#    e quattro le variabili del dataset.

# 3) E' possibile studiare la differenza tra la lunghezza dei petali
# per le diverse specie di fiore? Commentare i risultati fornendo
# alcune statistiche descrittive e opportuni grafici di confronto

# 4) Quale specie risulta essere più grande nelle diverse misure del fiore?


##############################
######## ESERCIZIO 2  ########
##############################

## analisi dei dati quantitativi contenuti 
## nel dataframe "sleep.txt"

# il dataframe contiene 20 osservazioni di 10 pazienti
# a cui sono stati somministrati due differenti farmaci per 
# per agevolare il sonno. 

# DESCRIZIONE DEI DATI: 
# ID: identificativo del paziente
# farmaco: indica il tipo di farmaco somministrato
# ore: ore di sonno in più o in meno rispetto ad un valore di riferimento

# 0) Caricare il dataset in R e chiamarlo sleep

# 1) Fornire i principali indici di dispersione e posizione stratificando
# i dati per tipo di farmaco

# 2) Quale farmaco risulta più efficace nel garantire un maggior
# numero di ore di sonno?

# 3) Identificare il paziente che dorme di meno E quello che dorme di più.

# 4) Fornire indicazioni su eventiali asimmetrie della distribuzione del numero 
# di ore di sonno in funzione del farmaco. Cosa si può notare dai grafici? 
# Notate qualche anomalia nelle misurazioni? Se sì, provate ad eliminare il dato
# e a ripetere l'analisi.




