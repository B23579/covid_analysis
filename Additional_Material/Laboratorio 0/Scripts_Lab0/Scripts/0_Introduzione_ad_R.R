
#################################################################################
###                  Esercitazione 1 :  Introduzione a R                        ###
#################################################################################


# Argomenti trattati :
# - Basic utilities, help and more...
# - R come calcolatore
# - Comandi base di R ( scalari, vettori, matrici e relative operazioni )
# - Import/Export dataframe
# - Ciclo for
# - Funzioni in R
# - Rappresentazione di grafici ( 1D, 2D e 3D )


#----------------------------------------------------------------------------#

# R :  linguaggio interpretato; e' possibile :
# - ( sconsigliato ) scrivere il codice direttamente sulla console
# - ( preferibile ) scrivere il codice su uno script ( come questo ) e poi eseguirlo
#   in console, una riga per volta oppure una selezione. Per eseguire il codice in
#   console :  ctrl + r oppure bottone |->| nell'interfaccia di R

# Commento :  tutto quanto preceduto da '#' non viene letto dalla Console di R
# e' possibile dunque eseguire indistintamente nella Console comandi e commenti
# senza dover togliere questi ultimi
# ( e' SEMPRE opportuno commentare i propri script come mostrato a laboratorio )

# IMPORTANTE :  come Matlab, R deve avere una DIRECTORY DI LAVORO,
# ovvero una cartella dove di default verranno cercati o salvati i file utilizzati da R
# per selezionare la directory di lavoro :
#  - seleziono la finestra della Console, e poi bottone 'file' -> 'cambia directory',
#  - oppure con un comando da tastiera

# setwd( "percorso/fino/alla/directory" )

setwd('./Lab_0_kickoff/')

# Nota :  il percorso e il modo in cui va scritto possono dipendere dal sistema
# operativo della macchina che state usando.

# se non mi ricordo la directory di lavoro :

getwd()


# Help --------------------------------------------------------------------


# cosa fare quando ci si ricorda nome del comando ma non si ricordano il suo
# scopo oppure i suoi argomenti?
# help( NOMECOMANDO )
help( hist )

# quando invece non ci si ricorda nome del comando si puo' utilizzare
# help.search( "KEYWORD" )
help.search( "histogram" )
# resituisce un elenco di pacchetti :  comandi nel cui help e' contenuta la keyword specificata.
# A destra compare anche una brevissima spiegazione del comando.

# Caso più generale
help.start()    # segnala separatamnte i pacchetti installati oltre a quelli compresi nella standard library



# Pacchetti -----------------------------------------------------------------------------------

# Cosa sono i 'pacchetti' di R?
# in generale, sono degli archivi di file .R ( o file programmati in altri linguaggi come C/C++ o Fortran ),
# e di dati, che permettono di definire particolari tipi di funzioni o di effettuare specifiche
# analisi per cui siano necessarie funzioni piu' sofisticate di quelle di base.

# Si caricano in R tramite il comando nella Console 'Pacchetti' -> 'Installa Pacchetti...'
# ( accedendo direttamente al sito CRAN se si dispone di una connessione ), oppure tramite
# 'Pacchetti' -> 'Installa Pacchetti da file zip locali' se si dispone del pacchetto salvato
# sul proprio pc in formato di archivio zip.
# E' possibile anche usare un comando :
#
#  install.packages( 'package_name' )
#
# A questo punto il pacchetto desiderato e' disponibile nella propria installazione di R.

# Per poter utilizzare le funzioni e i dati disponibili nel pacchetto, e' necessario 'caricarlo' :
library( MASS )

# Salvare -------------------------------------------------------------------------------------


# Selezionare la directory di lavoro, ovvero la cartella dove di default verranno cercati o salvati
# i file utilizzati da R ( attenzione anche qui alla codifica dei percorsi )
# setwd( "./prova" )

# per salvare lo script :
# selezionare la finestra dello script, e cliccare su 'file' -> 'salva' -> ...

# per salvare un grafico :
# selezionare la finestra del grafico, e cliccare su 'file' -> 'salva con nome' -> ...

# per salvare tabelle quali datasets, vettori, matrici, o altri oggetti in un file .txt
# write.table( OGGETTO, 'il-mio-oggetto.txt' )

# oppure e' possibile salvare i dati direttamente come variabili di R, in un file ( binario ) .RData
# save(  ogg1, ogg2, ogg3, ogg4, file = 'le-mie-variabili.RData'  )

# ATTENZIONE :
# la tabella o il file .RData verranno salvati nella directory di lavoro che avete selezionato
# ( se ne avete selezionata una ), altrimenti in quella di default ( bad practice ).
# write.table(  W, 'la-mia-matrice.txt' )
# controllate nella vostra directory!


# Data import/export --------------------------------------------------------------------------

record = read.table( 'record.txt', header = T )
str( record )
record[ 1 : 5, ]

dim( record )
dimnames( record )

# metto come etichette delle righe il contenuto dell'ottava colonna
record = data.frame( record[ , 1 : 7 ], row.names=record[ , 8 ] )
# metto un titolo alle colonne
var.names = c( "m100", "m200", "m400", "m800", "m1500", "m3000", "Marathon" )
dimnames( record )[[ 2 ]] = var.names

write.table( record, file = 'record_mod.txt' )ù




record = read.table( 'record_mod.txt', header=T )
str( record )
record[ 1 : 5, ]


# R come calcolatore --------------------------------------------------------------------------


# E' possibile utilizzare R per eseguire operazioni semplicissime

( 17 * 0.35 )^( 1 / 3 )

# in R sono definite le principali funzioni matematiche
# ( alcune serviranno spesso nell'analisi dei dati! )

log( 10 )
exp( 1 )
3^-1
1 / 3




# Manipolazione di variabili ------------------------------------------------------------------

# scalari
a = 1
a
a = 2
a

b = 3
b

c <- 4
4 -> c

c

a = b
a
b

# vettori
# c()  : serve a concatenare le variabili
v = c( 2, 3, 7, 10 )
v

# vettori costituiti da sequenze ordinate di numeri :
# e' possibile automatizzare la procedura

# posso imporre il passo
u = seq( 0,0.5,0.1 )
u

# oppure la lunghezza del vettore
length( u )
u = seq( 0, 0.5, len = 10 )
u

# passo negativo significa sequenza decrescente
u1 = seq( 0.5, 0, - 0.1 )
u1

# sequenza di passo 1
u2 = 1 : 5
u2

# vettori costituiti da ripetizioni di numeri :
# e' possibile automatizzare la procedura
w = rep( 1,10 )
w

# primo argomento di rep :  valore o vettore di valori che voglio ripetere
# secondo argomento di rep :  valore o vettore di valori che indicano
# come il primo argomento va ripetuto
w1 = rep( c( 1, 2, 3, 4 ), 3 )
w1
# quale sarà la lunghezza di w1?
length( w1 )

# N.B. i comandi rep e seq possono essere annidati
w2 = rep( 1 : 8, rep( 3,8 ) )
w2

w3 = rep( seq( 0, 10, len = 6 ), 1 : 6 )
w3

w4 = rep( c( 5, 9, 1, 3 ), c( 1, 4, 2, 0 ) )
w4


# Matrici
W = matrix( data = c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ), nrow = 4, ncol = 3, byrow = F )
W

# oppure
W = rbind( c( 1, 5, 9 ),c( 2, 6, 10 ),c( 3, 7, 11 ),c( 4, 8, 12 ) )
W

# oppure ( piu' furbo.. )
W = cbind( 1 : 4,5 : 8,9 : 12 )
W

# ATTENZIONE: a differenza di Matlab, in R i vettori non sono matrici con una sola riga o una
# sola colonna

v
# secondo elemento
v[ 2 ]

# piu' elementi
v[ 2 : 3 ]
v[ c( 1,3 ) ]

# tutto il vettore tranne il primo elemento
v[ -1 ]

# tutto il vettore tranne l'ultimo elemento
v[ -length( v ) ]


W
W[ 2, 3 ]
dim( W )
W[ 2 : 4, 1 ]
W[ 4, c( 1, 3 ) ]

# estrazione di righe o colonne di una matrice
W[ 3, ]
W[ , 2 ]

# estrazione di sottomatrici
W[ c( 1, 3, 4 ), 2 : 3 ]




# Operazioni algebriche -----------------------------------------------------------------------

# NB :  R di default effettua le operazioni componente per componente (operazioni vettorizzate)
a = 1
b = 2
c = c( 2, 3, 4 )
d = c( 10, 10, 10 )
e = c( 1, 2, 3, 4 )
f = 1 : 6

W    # dimensioni 4x3 da prima

Z = rbind( rep( 0,3 ), 1 : 3,rep( 10, 3 ), c( 4, 7, 1 ) )
Z    # Z ha le stesse dimensioni di W


# Operazioni su scalari e vettori
a + b    # scalare + scalare
c + d    # vettore + vettore => somma elemento per elemento
a * b    # scalare * scalare
c * d    # vettore * vettore => prod componente per componente
c + a    # vettore + scalare
c^2    # attenzione :  operazioni sono sempre componente per componente
exp( c ) # vedi sopra

c( c, d, e ) # la funzione c() serve a concatenare qualsiasi tipo di oggetti omogenei, anche vettori!

# ATTENZIONE al RECYCLING!
# cosa succede se compio operazioni componente per componente su vettori di dimensione diversa?
# R cerca comunque di eseguire l'opreazione, ciclando eventualmente sulla variabile `piu' corta`
# (il cosiddetto recycling).
#
# A volte gli oggetti hanno dimensioni diverse per errore, tuttavia R può portare a termine le
# operazioni desiderate ugualmente. Bisogna quindi prestare attenzione.

c
e
c + e
# warning ma NON errore :  i due vettori hanno dimensioni diverse ma R calcola comunque la loro somma.
# come? somma per componente fino all'ultimo elemento del piu' corto, poi 'ricicla'
# gli elementi del piu' corto dall'inizio fino a quando non esaurisce il vettore piu' lungo
# quindi :  warning perche' i due vettori non hanno lunghezze una multipla dell'altra

c
f
c + f
# f e' lungo il doppio di c :  R non dà neanche un warning e calcola la somma
# riciclando gli elementi di c


# Operazioni fra matricin
Z + W   # matrice + matrice => componente per componente

Z * W   # matrice * matrice => componente per componente


sum( c )  # somma componenti vettore c
sum( Z )  # somma componenti matrice Z ( somma tutto )

prod( c ) # prodotto componenti vettore c
prod( Z ) # prodotto componenti matrice Z ( come sopra )
colSums( Z )
rowSums( Z )

V = t( W ) # trasposizione di matrice ( V e' una matrice 3x4 )
V

V * W
# matrice * matrice ( componente per componente )
# errore :  le matrici hanno dimensioni diverse


# Moltiplicazione matriciale
dim( V )
dim( W )
V %*% W   # 3x4 * 4x3 = 3x3
W %*% V   # 4x3 * 3x4 = 4x4


# Inversione di matrici
solve( W )   # errore!

C = 3 * diag( 4, 3 )
# inversa di una matrice
solve( C )

library( MASS )
# inversa generalizzata ( in questo caso coincidono visto che la matrice ha rango pieno )
ginv( C )




# Altri tipi di variabili ---------------------------------------------------------------------

# Liste
scritto = list ( corso = 'Metodi e Modelli per l Inferenza Statistica',
            data.esame = '30/06/2010',
            num_iscritti = 25,
            num_consegnati = 23,
            numeri_matricola = as.character( c( 45020, 45679, 46789, 43126, 42345, 47568, 45674 ) ),
            voti = c( 30, 19, 29, 21, 25, 26, 27 )
             )
scritto

# Estrazione di un elemento da una lista
scritto$voti
# oppure
scritto[[ 6 ]]

# Si potrebbe anche fare :
scritto[ 6 ]
# ma e' ancora una lista, infatti:
scritto[[ 6 ]][ 2 ] # ok
scritto[ 6 ][ 2 ]   # non trova nulla.. devo invece scrivere :
scritto[ 6 ]$voti[ 2 ] # da evitare!


# Data frame

# N.B. sembrano matrici ma non lo sono; infatti, i vettori in essi contenuti, se presi per
#      colonna, per R hanno significato di variabili statistiche ( posso associare dei nomi! )

esame = data.frame(
            matricola = as.character( c( 45020, 45679, 46789, 43126, 42345, 47568, 45674 ) ),
            voti_S = c( 30, 19, 29, 21, 25, 26, 27 ),
            voti_O = c( 3, 3, 1, 0, 3, 2, 1 ),
            voti_TOT = c( 30, 22, 30, 21, 28, 28, 28 ) )
esame

# agli elementi di un dataframe si accede come a quelli di una lista
voti_S
esame$voti_S

# e' possibile pero' fare in modo che anche le variabili contenute in un dataframe risultino
# visibili nell'enviroment corrente
attach( esame )
voti_S

# Fare empre il detach, una volta finito di lavorare con le variabili di un data.frame, per evitare
# ambiguita'
detach( esame )
voti_S

# se non ci ricordassimo piu' il formato di un oggetto...
is( W )
#...o se lo volessimo cambiare
as.data.frame( W )



# Grafici -------------------------------------------------------------------------------------


# Aprire una nuova finestra grafica
dev.new()

# Su windows:
#
#   windows()
#
# Su Linux e Mac OS X:
#
# x11()
#
# su Mac OS X:
#
# quartz()

# funzione PLOT :  grafici nel piano cartesiano.
# argomenti :  ascisse e ordinate dei punti da plottare

x = c( 0, 1, 2, 3 )
y = c( 4, 5, 2, 7 )
y
plot( x, y )

x = seq( 0, 3, by = 0.01 )
y = x^2
plot( x, y )
# anziche' punti, tracciare una linea che passa per i vari punti
plot( x, y, type = 'l' )
# aggiungo in rosso il vettore z = x^3
z = x^3
lines( x, z, col = 'red' )
# points( x, z, type = 'l', col = 'green' )


# Se so di voler aggiungere al plot un altro vettore, posso regolare
# i valori limite sull'asse delle ascisse/ordinate in modo che entrambi
# i vettori vengano interamente visualizzati con il comando `range`
plot( x, y, type = 'l', xlim = range( x ), ylim = range( cbind( y, z ) ) )
lines( x, z, col = 'red' )

# provate il comando demo( graphics ) :  rassegna di possibili grafici
# cliccate sul grafico per passare al successivo
# di volta in volta vedremo i comandi per i grafici che ci interessano
demo( graphics )


par ( mfrow = c( 2, 2 ) )
hist( record[ , 1 ], prob = T, main = "Istogramma records 100m", xlab = "sec" )
hist( record[ , 2 ], prob = T, main = "Istogramma records 200m", xlab = "sec" )
boxplot( record, horizontal = TRUE, main = "Boxplot records 100m e 200m", xlab = "sec" )
plot( record[ , 1 ], record[ , 2 ], main = 'Scatter plot records 100m e 200m',
      xlab = "Records 100m", ylab = "Records 200m" )




# Grafici in 2D -------------------------------------------------------------------------------

# Rappresentazione di dati multivariati
pairs( record )

# Boxplot
boxplot( record )

record_sec = record
# riporto nella stessa unit? di misura ( sec ) anche i tempi delle gare piu' lunghe
record_sec[ , 4 : 7 ] = record[ ,4 : 7 ]*60

boxplot( record_sec )

# i tempi sembrano crescere linearmente in scala logaritmica
boxplot( log( record_sec ) )

# Grafici condizionati
coplot( m200 ~ m100 | m400, data = record_sec )

# Starplot
stars( record_sec, col.stars = rep( 'red',55 ) )

# Radarplot
stars( record_sec, draw.segments=T )



# Grafici in 3D -------------------------------------------------------------------------------

x = seq( -4, 4, 0.15 )
y = seq( -4, 4, 0.15 )

# Esempio semplicissimo di funzione :  densita' gaussiana bivariata
nucleo.gaussiano = function( x, y ){exp( -( x^2+y^2+x*y ) )}

w = matrix( NA, length( x ), length( y ) )

# Esempio di ciclo for
# NB :  tendenzialmente i cicli annidati in R sono da EVITARE!
for( i in 1 : length( x ) ){
	for( j in 1 : length( y ) ){
		w[ i, j ] = nucleo.gaussiano( x[ i ], y[ j ] )}
	}

# oppure
w = outer( x, y, nucleo.gaussiano )

image( x, y, w )
contour( x, y, w, add = T )
persp( x, y, w, col = 'red' )
persp( x, y, w, col = 'red', theta = 30, phi = 30, shade = 0.45, zlab = 'densità' )
persp( x, y, w, col = 'red', theta = 30, phi = 30, shade = 0.15, zlab = 'densità' )
# cambio angolo ( rotaz orizzontale )
persp( x, y, w, col = 'red', theta = 30, phi = 10, shade = 0.15, zlab = 'densità' )
# cambio angolo ( rotaz verticale )
persp( x, y, w, col = 'red', theta = 110, phi = 10, shade = 0.15, zlab = 'densità' )


library( rgl )
persp3d( x, y, w, col = 'red', alpha = 1 )
lines3d( x, x, 0, col = 'blue', lty = 2 )
lines3d( x, x, nucleo.gaussiano( x, x ), col = 'blue', lty = 1 )