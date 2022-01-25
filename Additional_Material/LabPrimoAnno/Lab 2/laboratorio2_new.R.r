###############################
#####    CORSO DI FSSB    #####
## per INGEGNERIA BIOMEDICA ###
###############################

###############################################
################ LABORATORIO 2 ################
###########  STATISTICA DESCRITTIVA ###########
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

##########################################
########## Analisi univariata ############
####### (variabili quantitative) #########
##########################################

## analisi dei dati quantitativi contenuti 
## nel file "appendiceA.txt" 

# NOTA BENE: impostare la cartella di lavoro tramite File-->Cambiare directory-->... 
# oppure con il comando setwd('...')

setwd("C:/Users/User/Desktop/Chiara/stat_biomedici/2015_lab_bio/Lab2")

getwd()

# Importazione del dataset:
studenti <- read.table( 'appendiceA.txt', header = T )
head( studenti )
dim( studenti )
names( studenti )

# 1. Estraiamo le studentesse
femmine = studenti[ which( studenti$Sesso == 'F' ) , ]
# quante sono? 
dim( femmine )

########## QUALCHE GRAFICO  #################
# Per aprire una finestra grafica, usare uno dei tre comandi qua sotto,
# in base al sistema operativo usato:
windows()    # windows
# quartz()   # mac
# x11()      # unix, windows
# Scatterplot:
plot( studenti$Peso, studenti$Colesterolo, 
     xlab = 'Peso [lb]', ylab = 'Colesterolo [mg/l]', main = 'Scatterplot Colesterolo vs. Peso' )

# indichiamo in rosso le ragazze
points( femmine$Peso, femmine$Colesterolo, col='red', pch = 19 )

# facciamo uno zoom intorno alle femmine, modificando i range degli assi
# Per prima cosa troviamo minimo e massimo del vettore Peso e Colesterolo per le femmine

min_p = min( femmine$Peso )
min_p
max_p = max( femmine$Peso )
max_p
min_c = min( femmine$Colesterolo )
min_c
max_c = max( femmine$Colesterolo )
max_c

# costruiamo il grafico relativo
plot(studenti$Peso, studenti$Colesterolo, 
     xlab = 'Peso [lb]', ylab = 'Colesterolo [mg/l]', main = 'Scatterplot Colesterolo vs. peso',
     xlim = c( min_p, max_p ), ylim = c( min_c, max_c) )
points( femmine$Peso, femmine$Colesterolo, col='red', pch = 19 )

# provate il comando demo(graphics): rassegna di possibili grafici
# cliccate sul grafico per passare al successivo
# di volta in volta vedremo i comandi per i grafici che ci interessano
windows()
demo(graphics)

# Table:
# calcoliamo le frequenze assolute per genere
f_ass = table( studenti$Sesso )
f_ass

# calcoliamo le frequenze relative:
f_rel = f_ass / length( studenti$Sesso )
f_rel
# metodo alternativo
f_rel2 = prop.table( f_ass )
f_rel2

# Barplot:
# costruiamo un grafico a barre con le frequenze assolute e relative
barplot( f_ass, col = c( 'pink', 'lightblue' ), ylab = 'F. ass' )
barplot( f_rel, col = c( 'pink', 'lightblue' ), ylab = 'F. rel' )

# notiamo che varia solo la scala


##### Calcolare i principali indici di posizione e di dispersione  ------------

# Media dei dati = media campionaria = somma delle osservazioni diviso per n,
# dove n è il numero delle osservazioni

mean( studenti$Peso ) # media campionaria

# Varianza dei dati = somma degli scarti quadratici 
# dalla media campionaria, diviso per (n-1): sum_i(x_i - media campionaria)^2 / (n-1)
# Indica la dispersione del dataset.

varianza <- var( studenti$Peso ) # varianza campionaria
varianza

# deviazione standard campionaria (è la radice 
# quadrata del risultato ottenuto con var)
sd( studenti$Peso ) 
sqrt( varianza )  # deviazione standard della popolazione 

## NB: la funzione range restituisce un vettore contenente max e min!
range( studenti$Peso )

# utilizzando la funzione "diff" otteniamo direttamente la differenza
diff( range( studenti$Peso ) )

# Un altro indice di posizione diverso dalla media è la mediana.
# La mediana è il valore centrale del dataset.
# Ordinando i dati: - se n è dispari corrisponde alla (n+1)/2 - esima 
#                     osservazione
#                   - se n è pari corrisponde alla media tra le osservazioni
#                     in posizione n/2 e (n/2 + 1)

median( studenti$Peso ) # mediana


# La moda è il valore che si verifica con maggiore frequenza nei dati: 
# per trovare la moda si identifica il valore (o i valori) che ha frequenza (assoluta o relativa)
# massima. Le frequenze si calcolano con table()
# Nota: non per forza questo valore è unico

t_freq <- table( studenti$Peso )
t_freq
moda = t_freq[ t_freq == max( t_freq ) ]
moda  # 126 è la moda, appare 11 volte 

## Sarebbe più appropriato dividere i valori della variabile "Peso" in classi (intervalli) e identificare
## la classe con frequenza maggiore (classe modale)

# Quartili
# Primo quartile (Q1):    è quel valore che lascia a sinistra il 25% dei dati
# Secondo quartile (Q2):  è quel valore che lascia a sinistra il 50% dei dati
# Terzo quartile (Q3):    è quel valore che lascia a sinistra il 75% dei dati

quantile( studenti$Peso, probs = 0.25 ) # primo quartile

quantile( studenti$Peso, probs = 0.50 ) # secondo quartile

# NB: coincide con la mediana!!!

quantile( studenti$Peso, probs = 0.75 ) # terzo quartile

# Il range interquartile (IQR) è la differenza tra il terzo
# e il primo quartile

quantile( studenti$Peso, probs = 0.75 ) - quantile( studenti$Peso, probs = 0.25 ) # IQR

# oppure in un comando unico con la funzione IQR()
IQR( studenti$Peso )

# funzione quantile: riassunto di min, max e quartili

quantile( studenti$Peso )

# Quantile di ordine p: valore che lascia a sinistra il 100p% dei dati

p <- 0.85
quantile( studenti$Peso, probs = p )

p <- ( 0:10 ) / 10
quantile( studenti$Peso, probs = p )

# Il comando summary restituisce alcuni valori di sintesi, quali
# la media, il minimo, il massimo e i tre quartili.

summary( studenti$Peso )

# ISTOGRAMMA: un istogramma conta la frequenza dei dati in ogni intervallo di dati prefissato
## COSTRUIRE UN ISTOGRAMMA

# Per costruire un istogramma si effettuano le seguenti operazioni:
# - si divide il range dei dati in classi (intervalli)
# - si calcolano, per ogni classe, frequenze assolute, frequenze relative
#   e densità [densità = (frequenza relativa)/(ampiezza classe)]
# - in corrispondenza di ogni classe si disegnano dei rettangoli di area
#   pari alla frequenza relativa della classe considerata (ovvero di
#   altezza pari alla densità)

# NB: spesso un istogramma viene rappresentato con l'altezza dei rettangoli
# uguale alle frequenza assoluta. In questo caso, però, l'istogramma
# è coerente solo se le classi hanno tutte la stessa ampiezza.

## ora guardiamo la variabile colesterolo

hist( studenti$Colesterolo ) # in ordinata ci sono le frequenze assolute
abline(v= median(studenti$Colesterolo), col = 'red')
abline(v= mean(studenti$Colesterolo), col = 'green')

# dall'istogramma non si rilevano particolari asimmetrie 

## proviamo insieme a disegnare l'istogramma della variabile peso e commentarlo
## ....

hist( studenti$Colesterolo, prob = TRUE ) # in ordinata ci sono le densità

hist( studenti$Colesterolo, prob = TRUE, col = 'orange', 
     main = 'Istogramma del Colesterolo', xlab = 'Colesterolo', ylab = 'Densita' )

# l'argomento breaks serve a scegliere le classi.
# In particolare il numero di classi (equispaziate) è uguale (indicativamente) a breaks se breaks è un numero. 
# Ma breaks può essere un vettore. Se breaks è un vettore fornisce gli estremi di ogni classe.
# R in questo caso sceglie 13 classi in automatico (quindi se scriviamo 
# breaks = 13 non cambia niente rispetto a prima!)

hist( studenti$Colesterolo, prob = TRUE, breaks = 13, col = 'orange',
     main = 'Istogramma del Colesterolo', xlab = 'Colesterolo', ylab = 'Densita' )

# posso giocare con il numero di classi: non esiste un numero di classi
# 'giusto', la scelta sta alla sensibilità dello statistico

hist( studenti$Colesterolo, prob = TRUE, breaks = 7, col = 'orange',
     main = 'Istogramma del Colesterolo', xlab = 'Colesterolo', ylab = 'Densita' )

# Per scegliere il numero di classi si può utilizzare per esempio
# la regola sqrt(n) (radice quadrata di n), oppure la formula 
# [log_2(n) +1], che è quella usata di default da R , se non specificate 
# l'argomento breaks (cioè corrisponde a breaks = "Sturges")

# Si possono imporre classi di ampiezza diversa
# NB: in questo caso il grafico ha comunque area 1!

hist( studenti$Colesterolo, prob = TRUE, col = 'royalblue',
     main = 'Istogramma del Colesterolo', xlab = 'Colesterolo', ylab = 'Densita',
     xlim = c ( min ( studenti$Colesterolo ) - 5, max ( studenti$Colesterolo ) + 5 ),
    breaks = c( 160, 180, 187, 190, 198, 204, 210, 230 ) )

# Tabella di distribuzione di frequenze

# per avere la tabella in automatico è possibile utilizzare ancora hist,
# con l'opzione 'plot' impostata su FALSE. La funzione restituisce:
# breaks: estremi delle classi
# counts: frequenze assolute delle classi
# intensities & density: densità associate alle classi
# mids: valori centrali delle classi
# xname: nome della variabile
# equidist: booleano. Le classi hanno tutte la stessa ampiezza? TRUE o FALSE

hist( studenti$Colesterolo, prob = T, col = 'orange')
hist( studenti$Colesterolo, plot = FALSE )

# naturalmente è possibile salvare la tabella in una variabile di R
# e richiamare le varie informazioni utilizzando il $.

istogramma <- hist( studenti$Colesterolo, plot = FALSE )
estremiclassi <- istogramma$breaks
estremiclassi

frequenzeassolute <- istogramma$counts
frequenzeassolute

totaleosservazioni <- sum( frequenzeassolute )
totaleosservazioni
length( studenti$Colesterolo )

frequenzerelative <- ( frequenzeassolute ) / totaleosservazioni
frequenzerelative

density <- istogramma$density
density

# sapendo che densita = f_rel / (ampiezza classe), ricaviamo:
frequenzerelative_2 = density * diff( estremiclassi )
frequenzerelative_2

## COSTRUIRE UN BOXPLOT

# Il boxplot è uno strumento grafico molto utile per identificare
# eventuali asimmetrie della distribuzione e/o la presenza di eventuali
# valori estremi (outlier).

# Per costruire un boxplot (verticale) si effettuano le seguenti operazioni:
# - si costruisce un rettangolo con basi inferiore e superiore uguali,
#   rispettivamente, al primo e al terzo quartile e che quindi conterrà
#   il 50% centrale delle osservazioni
# - all'interno del rettangolo di traccia una linea in corrispondenza
#   della mediana
# - si considera il limite superiore uguale a Q3 + 1.5*IQR e si traccia
#   un baffo che collega la base superiore del rettangolo all'osservazione
#   più alta contenuta all'interno del limite superiore
# - si considera il limite inferiore uguale a Q1 - 1.5*IQR e si traccia
#   un baffo che collega la base inferiore del rettangolo all'osservazione
#   più bassa contenuta all'interno del limite superiore.
# - eventuali valori maggiori del limite superiore o minori di quello
#   inferiore vengono segnati singolarmente con un cerchio sul grafico
#   e vengono chiamati outlier (superiori o inferiori).

# Con R il boxplot si ottiene tramite il comando "boxplot"

boxplot( studenti$Colesterolo, main = 'Boxplot del Colesterolo' )

# Un boxplot può anche essere rappresentato in orizzontale, anche se è
# più comune rappresentarlo in verticale. Per rappresentarlo in orizzontale scegliere 
# l'argomento in boxplot: horizontal=TRUE (di default horizontal = FALSE)

boxplot( studenti$Colesterolo, horizontal = TRUE, col = 'forestgreen' )

boxplot( studenti$Colesterolo, main = "Boxplot Colesterolo", ylab = "Colesterolo", 
         ylim = c( 150, 250 ) )

# Disegniamo i boxplot per tutti gli studenti insieme 

boxplot( studenti$Colesterolo, col = 'forestgreen', horizontal = TRUE,
         main = 'Boxplot colesterolo studenti m/f')

# poi per le classi distinte dal sesso

boxplot( femmine$Colesterolo, col = 'pink',main = 'Boxplot colesterolo femmine' )
boxplot( studenti$Colesterolo[ studenti$Sesso == 'M' ], col = 'royalblue',main = 'Boxplot colesterolo maschi' )
# ------> questo non va bene! Non è possibile confrontare grafici che hanno scale diverse! 

# usiamo un altro comando per la gestione grafica dei plot

boxplot( studenti$Colesterolo ~ studenti$Sesso, col = c ( 'pink', 'royalblue' ), 
         names = c( 'Femmine', 'Maschi' ), main = 'Distinzione per sesso')


# per uccidere tutti i device grafici e reimpostare il default:
graphics.off()





##########################################
########## Analisi bivariata #############
####### (variabili quantitative) #########
##########################################

# prima di iniziare, eliminiamo tutto tranne il data.frame studenti
rm ( list = ls())

studenti <- read.table( 'appendiceA.txt', header = T )

## indici di posizione e di dispersione

# per calcolare gli indici di posizione e dispersione separatamente
# per ogni sottocampione, individuato dal sesso, è comodo utilizzare 
# la funzione tapply: essa applica una certa funzione (terzo argomento) 
# a ciascuno dei sottoinsiemi - non vuoti - di valori di una variabile 
# (primo argomento) individuati da un fattore di raggruppamento 
# (secondo argomento). Quindi per noi il primo argomento è la 
# variabile Peso, il secondo è la variabile Sesso (categorica!)
# mentre il terzo sarà di volta in volta la funzione che dobbiamo 
# applicare per ottenere l'indice cercato

# NB: attenzione alle funzioni a valori vettoriali (come range) 
# o a più argomenti (come quantile)

tapply( studenti$Peso, studenti$Sesso, mean )  # media del peso degli studenti nei due sottogruppi maschi e femmine
tapply( studenti$Peso, studenti$Sesso, var )   # varianza campionaria del peso degli studenti nei due sottogruppi maschi e femmine
tapply( studenti$Peso, studenti$Sesso, sd )    # deviazione standard del peso degli studenti nei due sottogruppi maschi e femmine
tapply( studenti$Peso, studenti$Sesso, min )   # minimo del peso degli studenti nei due sottogruppi maschi e femmine
tapply( studenti$Peso, studenti$Sesso, max )   # massimo del peso degli studenti nei due sottogruppi maschi e femmine


diff( tapply( studenti$Peso, studenti$Sesso, range )$F ) # range femmine
diff( tapply( studenti$Peso, studenti$Sesso, range )$M ) # range maschi

tapply( studenti$Peso, studenti$Sesso, median ) # mediana

Q <- tapply( studenti$Peso, studenti$Sesso, quantile ) # quartili
Q

Q1 <- c( Q$F[2], Q$M[2] ) # primo quartile [femmine,maschi]
Q1
Q3 <- c( Q$F[4], Q$M[4] ) # terzo quartile [femmine,maschi]
Q3
Q3 - Q1 # IQR

# se la funzione ha ulteriori argomenti, si inseriscono come argomenti 
# di tapply dopo la funzione (quarto,quinto,... argomento).
# Per esempio, se volessi il quantile di ordine 0.9 dei due gruppi

Q_90 <- tapply( studenti$Peso, studenti$Sesso, quantile, probs = 0.9 )
Q_90

# Al comando tapply si può passare anche la funzione summary 

tapply( studenti$Peso, studenti$Sesso, summary )

# sembra che il peso medio delle femmine sia 
# leggermente inferiore a quello dei maschi

## Istogramma
# Traccio i due istogrammi uno sotto l'altro in modo da poter effettuare più
# facilmente un confronto (per poter confrontare i due grafici: stessa scala
# sull'asse delle ascisse e stesse classi!)
windows()
# quartz()
# x11()
par ( mfrow = c( 2, 1 ) )

hist( studenti$Peso[ studenti$Sesso == 'F' ], prob = TRUE,
     main = 'Istogramma del peso delle femmine', xlab = 'Peso [lb]',
   ylab = 'Densita', col = 'pink', xlim = range( studenti$Peso ), 
   breaks = seq( min( studenti$Peso ), max( studenti$Peso ), length = 10 ) )

hist( studenti$Peso[ studenti$Sesso == 'M' ], prob = TRUE,
     main = 'Istogramma del peso dei maschi', xlab = 'Peso [lb]',
   ylab = 'Densita', col = 'royalblue', xlim = range( studenti$Peso ), 
   breaks = seq( min( studenti$Peso ), max( studenti$Peso ), length = 10 ) )

dev.off()

# e se volessimo sovrapporli????
# bisogna specificare l'opzione add = TRUE nei diversi grafici tranne il primo
# in questo caso ne abbiamo solo due
windows()
# quartz()
# x11()
hist(studenti$Peso[ studenti$Sesso == 'F' ], prob = TRUE,
     main = 'Istogramma del peso per genere',xlab = 'Peso [Kg]', ylab = 'Densita', col = 'pink',
     xlim = range( studenti$Peso ), breaks = seq( min( studenti$Peso ), max( studenti$Peso ), length = 10 ), 
     angle = 45, density = 14, ylim =c( 0, 0.03 ) )
hist( studenti$Peso[ studenti$Sesso == 'M' ], prob = TRUE, add = TRUE,
      col = 'royalblue', angle = 135, density = 14,
     xlim = range( studenti$Peso ), breaks = seq( min( studenti$Peso ), max( studenti$Peso ), length = 10 ) )
legend( 'topright', legend = c( 'femmine', 'maschi' ), fill = c( 'pink', 'royalblue' ) )

# Anche l'istogramma evidenzia una tendenza delle femmine ad avere un
# peso inferiore rispetto ai maschi.
# Il confronto tra i due istogrammi affiancati è comunque 
# difficoltoso: meglio confrontare i boxplot.

##########################################
#############   DATI POLIMI  #############
##########################################


dati_poli = read.table(file="Poli_studenti0708.txt", 
                       header=T, quote = "\"",dec =',', sep='\t', fill=T)
dim(dati_poli)
colnames(dati_poli)
head(dati_poli)

attach(dati_poli)
## analizziamo la variabile ISTITUTO_REGIONE

table(ISTITUTO_REGIONE)
prop.table(table(ISTITUTO_REGIONE))


x11()
par(mar=c(10,3,3,3))
barplot(table(ISTITUTO_REGIONE),las = 3)

x11()
pie(table(ISTITUTO_REGIONE))

dati_nolomb = dati_poli[-which(ISTITUTO_REGIONE=="Lombardia"),]
dim(dati_nolomb)

x11()
par(mar=c(10,3,3,3))
barplot(table(factor(dati_nolomb$ISTITUTO_REGIONE)),las = 3)

x11()
pie(table(factor(dati_nolomb$ISTITUTO_REGIONE)))


## Confrontiamo la media degli esami sostenuti del primo anno degli studenti che 
## hanno terminato il corso di studi e quelli che hanno invece abbandonato dopo il 
## primo anno.

abbandoni = MEDIA_ESAMI_1[which(ABBANDONI==2 & MEDIA_ESAMI_1>0)]
no_abbandoni = MEDIA_ESAMI_1[which(ABBANDONI==0 & MEDIA_ESAMI_1>0)]


length(abbandoni)
length(no_abbandoni)

x11()
boxplot(abbandoni, no_abbandoni, names = c('Abbandoni', 'No abbandoni'), 
        col = c('red3', 'forestgreen'), main = "Boxplot media esami primo anno")

boxplot(abbandoni)$stats
boxplot(abbandoni)$out
summary(abbandoni)
summary(no_abbandoni)


x11()
par(mfrow=c(2,1))
hist(abbandoni, prob=T, xlim=range(c(abbandoni, no_abbandoni)), col = 'red3')
hist(no_abbandoni, prob=T, xlim=range(c(abbandoni, no_abbandoni)), col='forestgreen')



detach(dati_poli)




##########################################
########### ESERCIZIO 1 ##################
##########################################
# Esercizio 7 pag.35 del Ross, 2014.

# Considera il livello di colesterolo nel sangue dei primi 
# 100 studenti presenti in 'appendiceA.txt' (il dataset usato finora).
# 1. Suddividere gli studenti in maschi e femmine, costruire una tabella con le 
#    frequenze relative
# 2. Costruire in un unico grafico l'istogramma del colesterolo per i maschi e per le
#    femmine
# 3. Cosa possiamo affermare sul rapporto tra sesso dello studente 
#    e livello di colesterolo?


##########################################
#########  ESERCIZO 2  ###################
##########################################
## Consideriamo adesso i dati contenuti 
## nel file "studenti.txt"

# Nota bene: prima operazione da fare: selezionare il file 'studenti.txt'
# e salvarlo nella directory di lavoro.

# 0) importare in R il dataset "studenti.txt" e salvarlo in un dataframe

# 1) Calcolare gli indici di posizione per la variabile "Taglio":
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.9

# 2) Calcolare gli indici di dispersione visti per la variabile "Taglio":
#    varianza della popolazione, deviazione standard, range
#    e range interquartile

# 3) Costruire un istogramma che illustri le frequenze relative 
#    della variabile "Taglio".
#    Ricavare le seguenti informazioni contenute nella funzione 
#    che ha generato l'istogramma:
#    punti centrali, frequenza relativa e assoluta di ogni classe.
#    Che considerazioni si possono trarre dall'istogramma?

# 4) Costruire un boxplot con le osservazioni della variabile "Taglio".
#    Che considerazioni si possono trarre dal boxplot?

# 5) Calcolare gli indici di posizione per la variabile "Taglio"
#    nei due sottocampioni individuati dal genere:
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.9

# 6) Calcolare gli indici di dispersione per la variabile "Taglio"
#    nei due sottocampioni individuati dal genere:
#    varianza della popolazione, deviazione standard, 
#    range e range interquartile

# 7) Costruire gli istogrammi che illustrino le frequenze relative
#    della variabile "Taglio" nei due sottocampioni individuati dal genere.
#    Che considerazioni si possono fare confrontando i due istogrammi?

# 8) Costruire i boxplot con le osservazioni della variabile "Taglio"
#    per i due sottocampioni individuati dal genere.
#    Che considerazioni si possono fare confrontando i due boxplot?
