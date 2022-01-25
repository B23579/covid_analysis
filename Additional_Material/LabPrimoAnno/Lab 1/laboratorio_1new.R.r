###############################
#####    CORSO DI FSSB    #####
## per INGEGNERIA BIOMEDICA ###
###### A.A. 2016 - 2017 #######
###############################

###############################################
################ LABORATORIO 1 ################
## INTRODUZIONE A R & STATISTICA DESCRITTIVA ##
################# (PARTE 1) ###################
###############################################

# ACCESSO AL VIRTUAL DESKTOP POLIMI
# Dai computer del laboratorio cliccare sul men? Start
# e cercare nell'apposita barra Virtual. Cliccare una sola
# volta sul link in cima al men? e attendere l'apertura 
# di una pagina di Internet Explorer in cui dovete
# inserire il vostro codice persona e relativa password.
# Attendete che si carichino tutti i programmi e cercate 
# R for Windows (3.1.1). Apritelo cliccando una sola volta

# DOWNLOAD DEI FILE PER LA LEZIONE DA BEEP
# Accedere al sito beep.metid.polimi.it da Internet Explorer
# Inserire codice persona e password e navigare fino a trovare 
# i file della lezione corrispondente. Assicuratevi di scaricare
# tutti i file preseniti nella cartella. 
# Salvare i file nel seguente percorso: 
# Computer --> Windows --> Users --> USER_LEO --> Desktop
# QUESTO E' L'UNICO MODO CHE AVETE PER VISUALIZZARE I FILE SUL VOSTRO
# DESKTOP

# Riferimenti bibliografici per R:
# TITOLO: Introductory Statistics with R
# AUTORI: Dalgaard, P. (2008)
# EDITORE: Pearson
# available E-book at http://link.springer.com/book/10.1007/978-0-387-79054-1/page/1


# Riferimenti bibliografici per gli argomenti trattati nel corso: 
# TITOLO: Introduzione alla Statistica, Seconda Edizione,
# AUTORI:  Ross, S. M. (2014)
# EDITORE:  Maggioli Editore

# Per scaricare R:   http://www.r-project.org/
# disponibile per Windows, Mac e Linux
# Per maggiori informazioni su R:
# - dall'interfaccia R: 'aiuto' -> 'Guida Html'
#   oppure 'aiuto' -> 'sito CRAN'

# Esiste un frontend di R chiamato RStudio
# che possiede un'interfaccia grafica pi? vantaggiosa
# Per scaricare RStudio: http://www.rstudio.com/products/rstudio/download/
# disponibile per Windows, Mac e Linux

# Argomenti trattati nel laboratorio 1:
# - Comandi base di R (scalari, vettori, matrici e relative operazioni)
# - L'oggetto data.frame, l'import di file di dati e loro esplorazione
# - Esempi di analisi descrittiva di variabili qualitative.
# Riferimento Dalgaard a cap. 1 e 2

# R è un linguaggio interpretato; è possibile:
# - scrivere il codice direttamente sulla console e fare invio
# - (preferibile) scrivere il codice su uno script (come questo) 
#   e poi eseguirlo in console, una riga per volta oppure una selezione. 
#   Per eseguire il codice in console: ctrl + R.

# Es

3

# Per aprire un nuovo script:
# - File -> Nuovo script
# - Archivio -> Nuovo Documento

# Commento: tutto quanto preceduto da '#' non viene letto dalla Console di R
# E' possibile dunque eseguire indistintamente nella Console comandi e commenti
# senza dover togliere questi ultimi
# (? SEMPRE opportuno commentare i propri script come mostrato a laboratorio)

# Esempio
5
# 5

# MOLTO IMPORTANTE:  R deve avere una DIRECTORY DI LAVORO,
# ovvero una cartella dove di default verranno cercati o salvati i file 
# utilizzati da R. Quindi: create immediatamente una cartella 
# in 'C:/USER_DATA', e chiamatela 'nome_cognome_lab1'; 
# salvate poi in questa cartella i file che trovate sul sito del corso
# (pacchetti, dati,..) all'INIZIO di ogni laboratorio

# per selezionare la directory di lavoro:
# seleziono la finestra della Console, e poi bottone 
# 'file' -> 'cambia directory',
# oppure con un comando da tastiera
# setwd('C:/USER_DATA/laboratorio_1')

# se non mi ricordo la directory di lavoro:
getwd()

# per sapere tutti i file presenti nella directory:
dir()

# R COME CALCOLATORE ------------------------------------------------------


# E' possibile utilizzare R per eseguire operazioni semplicissime

# Operatori matematici di base

(17*0.35)+(1/3)-(1/2)

# in R sono definite le principali funzioni matematiche
# (alcune serviranno spesso nell'analisi dei dati!)

log(10) # logaritmo in base naturale

log10(10) #logaritmo in base 10

exp(1)

3^(-1)



# OGGETTI IN R: ASSEGNARE VALORI A VARIABILI ------------------------------

# operatore di assegnamento: <-
# funziona anche '='

# 1. Scalari

a <- 1
a
a = 2
a

b <- 3
b

a+b
a
b

a <- b
a
b

# 2. Vettori

# c() ? la funzione che serve a concatenare: un vettore 
# ? un insieme di numeri concatenati!
v <- c( 2, 3, 7, 10 )
v


# vettori costituiti da sequenze ordinate di numeri:
# ? possibile automatizzare la procedura

# sequenza di passo 1
u <- 1:5
u


# posso imporre il passo
u <- seq( 1, 5, by = 1 )
u

# passo negativo significa sequenza decrescente
u <- seq( 5 , 1, by = -1 )
u

# oppure la lunghezza del vettore
u <- seq( 0, 1, length = 10 )
u

length( u )

# vettori costituiti da ripetizioni di numeri:
# E' possibile automatizzare la procedura

w <- rep( 1, 10 )
w

# Primo argomento di rep: valore o vettore di valori che voglio ripetere
# Secondo argomento di rep: valore o vettore di valori che indicano
# come il primo argomento va ripetuto

w1 <- rep( c( 1, 2, 3, 4 ), 3 )
w1

# quale sar? la lunghezza di w1?
# ...

length(w1)


# usando l'opzione each
w2  <- rep( 1:8, each = 3 )
w2




# 3. Matrici

W <- matrix( data = c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 4, ncol = 3, byrow = FALSE)
W

# oppure
W <- rbind( c(1,5,9), c(2,6,10), c(3,7,11), c(4,8,12) )
W

# oppure (pi? furbo..)
# costruisco la matrice Z le cui colonne sono i vettori v1, v2, v3
v1 = 1:4
v2 = 5:8
v3 = 9:12

Z <- cbind( v1, v2, v3 )
Z

colnames(Z)
colnames(Z)= c("a","b","c")

Z

# ESTRAZIONE DI ELEMENTI DA UN VETTORE DI R: ------------------------------

# Attenzione: in R i vettori non sono matrici n*1 o 1*n!
# fondamentale ricordarselo quando si vuole estrarre un elemento 
# da un vettore
# [] per estrarre elementi da qualunque oggetto di R, mentre () usate nelle funzioni

v
v[ 2 ]
v[ 2:3 ]
v[ c( 1, 3 ) ]
v[ -1 ]         # tutto il vettore tranne il primo elemento
v[ -length(v) ] # tutto il vettore tranne l'ultimo elemento



# 1. La funzione which (per determinare tutte le posizioni di un elemento ripetuto MA NON L'ELEMENTO):
# Operatori logici:
# ==: uguale
# != : non uguale (diverso)
# > : maggiore
# < : minore
# & : and
# |: or

w4 = c(5,9,9,9,9,1,1)

which( w4  == 5 )  
which( w4 != 5 )  

which( w4 > 1 ) 

which( w4 > 1 & w4 <= 9)

which(w4 > 6 | w4 == 5)


# 2. Accesso diretto agli elementi (non restituisce la posizione ma il valore del vettore):

w4 > 1

w4[ w4 > 1 ]

w4[ w4 == 5 ]  ## cosa vi aspettate?


# 3. La funzione unique permette di determinare i valori univoci di un vettore.
w4
unique(w4)
# e se volessimo sapere quanti elementi univoci ci sono in un vettore ?

length( unique( w4 ) ) 



# ESTRAZIONE DI ELEMENTI DA UNA MATRICE: ----------------------------------

W
dim(W) ## differenza tra length e dim

# Accesso diretto a righe e colonne:
W[ 2, 3 ]

W[ 2:4, 1 ]

W[ 4, c( 1, 3 ) ]

# intere colonne o righe
W[ 3, ]

W[ , 2 ]

# estrazione di sottomatrici
W[ c( 1, 3, 4 ) , 2:3 ]



# OPERAZIONI ALGEBRICHE IN R ----------------------------------------------

# NB: R di default effettua le operazioni componente per componente

# introduciamo un nuovo scalare
a <- 5
a

# 1. Operazioni tra scalari e vettori

a + 2 * a   # scalare + scalare
a + w4      # scalare + vettore
a * w4      # scalare * vettore
w4 + 2 * w4 # vettore + (scalare * vettore)
w4^2        # attenzione: operazioni sono sempre componente per componente
exp(w4)     # vedi sopra


# 2. Operazioni tra matrici

W 
Z

# matrice + matrice (componente per componente), CON STESSE DIM
Z + W 

# si pu? moltiplicare W * Z ?
# Non ? il classico prodotto matriciale, ma anche qui compon * compon
P = Z * W 
P

# R fa sempre il prodotto componente per componente, esattamente
# come per i vettori! 

## funzioni che permettono operazioni algebriche in R

sum(w4)   # somma componenti vettore w4
sum(W)    # somma componenti matrice W (somma tutto!!)

prod(w4)  # prodotto componenti vettore w4
prod(W)   # prodotto componenti matrice W (come sopra)

V <- t(W) # trasposizione di matrice
          # V ? una matrice 3x4
V

# matrice * matrice (componente per componente)
V * W 

# Errore: le matrici hanno dimensioni diverse!
dim(V)
dim(W)

# Vera Moltiplicazione matriciale: anche qui bisogna fare 
# attenzione alle dimensioni..

V %*% W
W %*% V 


# Per visualizzare e cancellare le variabili

ls()              # fornisce la lista delle variabili esistenti
rm(a)             # rimuove la variabile a
ls()
rm( list = ls() ) # rimuove tutte le variabili nel workspace
                  # si pu? fare anche con: ctrl + l
ls()


# L'OGGETTO DATA.FRAME ----------------------------------------------------

# I dataframe sono oggetti costituiti da vettori di uguale lunghezza, ma
# non necessariamente dello stesso tipo. Contrariamente alle matrici che 
# richiedono un'unica tipologia di dato, il dataframe ? pi? flessibile 
# e pu? essere utilizzato con dati molto diversi tra loro. Ecco perche 
# ? l'oggetto comunemente pi? usato per analisi statistiche

# N.B. sembrano matrici ma non lo sono; infatti, i vettori in essi contenuti, 
# se presi per colonna, per R hanno significato di "variabili statistiche" 
# (posso associare dei nomi!)

esame <- data.frame(
            matricola = as.character( c( 45020, 45679, 46789, 43126, 42345, 47568, 45674 ) ),
            voti_S = c( 30, 19, 29, NA, 25, 26, 27 ), 
            voti_O = c( 3, 3, 1, NA, 3, 2, NA ), 
            voti_TOT = c( 30, 22, 30, NA, 28, 28, 27 ) )
esame

# purtroppo non ? possibile accedere agli elementi di un dataframe in modo diretto.
# ? necessario utilizzare il comando $ (dollaro) subito dopo il nome del dataframe,
# seguito dal nome della variabile che ci interessa
# Questo ? uno dei due modi per evitare che R ignori il contenuto di un dataframe
# e per fare in modo di poter accedere al contenuto stesso. Vediamo un esempio:

voti_S # guardare l'errore
esame$voti_S

# E' possibile per? fare in modo che anche le variabili contenute 
# in un dataframe risultino visibili direttamente

attach(esame) 
voti_S
search() # per vedere gli attachments, tra il resto
detach(esame)
voti_S
# NB: non stiamo rimuovendo il dataframe, ma solo l'accesso diretto!!
# ATTENZIONE: usate con cautela il comando attach() poich? ci possono
# essere dei problemi di sovrascrizione. Vediamo un esempio: 

# creo una variabile esempio che abbia lo stesso nome di una 
# contenuta in esame
voti_S = 'esempio'
voti_S

# se ora utilizziamo il comando attach(), la variabile
# voti_S contenuta in esame non prender? il posto della nostra appena creata
attach(esame)
voti_S

# come vedete ci restituisce 'esempio', ma non i dati del dataframe !!!

detach(esame)

# ora invece rimuoviamo proprio il dataframe
rm(esame)


# Un dataframe si pu? anche importare in R utilizzando il comando
# read.table(). Prima di importare i dati ? conveniente guardare
# il file che viene importato

# per prima cosa diciamo ad R dove si trova il file da importare! 
#setwd("C:/Users/User/Desktop/Chiara/stat_biomedici/2015_lab_bio/Lab1")


studenti = read.table("appendiceA.txt", header = TRUE, stringsAsFactors = FALSE)


# L'opzione header = TRUE o FALSE indica se il file che viene
# importato contiene i nomi delle variabili nella prima riga
# l'opzione stringAsFactors = FALSE impone ad R di trattare le 
# variabili character come tali (stringhe di caratteri)
# senza convertirle in factor che vedremo tra poco

names(studenti) # Mostra i nomi delle variabili nel dataframe 

head(studenti)  # Mostra le prime sei righe del dataframe
tail(studenti)  # Mostra le ultime sei righe del dataframe

# Che tipo di variabili ci sono nel nostro dataframe?
# Quali sono variabili categoriche e quali quelle qualitative?

str(studenti) # Struttura del dataframe!

# oppure si pu? utilizzare la funzione class() che d? informazioni
# su variabili specifiche del data frame

class(studenti)
class(studenti$Sesso)

# Posso costruire automaticamente una variabile categorica che assume diversi valori
studenti$Sesso <- as.factor( studenti$Sesso )
class(studenti$Sesso)

str(studenti)

# E' possibile chiedere i "livelli" della variabile categorica appena creata
levels(studenti$Sesso) # i livelli sono le categorie di variabili qualitative
# Il comando is.na() restituisce TRUE dove ci sono dati 
# non disponibili ( NA = Not Available)

is.na(studenti) # poco leggibile

# Per contare il numero di NA
sum( is.na( studenti ) )


### HELP 

# cosa fare quando ci si ricorda nome del comando ma non si ricordano
# il suo scopo oppure i suoi argomenti?
# help(NOMECOMANDO)
# ad esempio:

help(which)
# oppure
?which

# quando invece non ci si ricorda nome del comando si pu? utilizzare
# help.search("KEYWORD")

help.search("tabulate")

# restituisce un elenco di pacchetti::comandi nel cui help 
# ? contenuta la keyword specificata. A destra compare anche una 
# brevissima spiegazione del comando.


# FACCIAMO QUALCHE OPERAZIONE SUL DATA.FRAME 

# 1. Isoliamo le femmine
femmine = studenti[ which( studenti$Sesso == 'F' ) , ]
# quante sono? 
dim(femmine)

# potevamo sapere il loro numero anche senza creare il dataframe femmine? 
length( which( studenti$Sesso == 'F' ) )

# oppure possiamo calcolare una tabella di frequenza:
# il comando ? table

table(studenti$Sesso)
# N.B. funziona bene perch? stiamo guardando una variabile 
# categorica (factor) con "solo" 2 livelli
# vediamo cosa succede se facciamo la stessa cosa su una variabile quantitativa

table(studenti$Peso)
# non molto leggibile!!!

### SALVARE
# salviamo il dataframe con le femmine in un file di dati
# il comando ? write.table

write.table( femmine, file = 'femmine.txt' )

# se vogliamo controllare che sia stato salvato
dir()



# oppure ? possibile salvare i dati direttamente come oggetti di R, 
# in un file .RData in modo da poterlo caricare successivamente
# in R e partire esattamente da dove si era rimasti

save(femmine, file = 'femmine.RData')

# elimino femmine
rm(femmine)

# Per caricare i file .RData precedentemente salvati con save:
load('femmine.RData')

# ritroviamo lo stesso oggetto di prima! 

# N.B. se si vuole salvare l'intero ambiente di lavoro si usa
# il comando save.image() che genere sempre un file .RData
save.image('lab_1.RData')

# ATTENZIONE:
# la tabella o il file .RData verr? salvata nella directory di lavoro 
# che avete selezionato (se ne avete selezionata una), altrimenti in quella 
# di default (MAI USARE QUELLA DI DEFAULT!!)

# per salvare area di lavoro:
# selezionare la finestra della Console, cliccare il bottone 
# 'file' -> 'salva area di lavoro', ... (anche quando richiesto 
# a chiusura di R -> SCONSIGLIATO in questo caso)




### ANALISI DESCRITTIVA DI UN CARATTERE/VARIABILE QUALITATIVO
###   (DATI CATEGORICI)

# Le variabili qualitative non possono essere descritte numericamente; 
# E' possibile solamente trovare la tabella di distribuzione di frequenze 
# per le categorie della variabile, e tracciare grafici (diagrammi a barre 
# e a torta).
# Nei prossimi esercizi cercheremo di prendere confidenza 
# con questi strumenti.


##########################################
#############   ESERCIZIO 1  #############
##########################################

# Creo un vettore di realizzazioni di una variabile categorica

# La funzione factor converte l'argomento (vettore di numeri o caratteri)
# in una serie di realizzazioni di una variabile  categorica,
# i cui valori possibili sono riportati in Levels.

province <- c( "MI", "MI", "BG", "LO", "LO", "MB", "MB", "VA", "VA", "VA", "MI", "MB", "MI", 
              "VA", "MI", "BG", "BG", "CR", "MI", "MI", "SO", "CR", "MB", "MB" , "LO", "LO", 
              "MI", "BG", "MI", "MB", "LO", "LO", "LO", "MB", "MI", "MI", "MB", "MB" )
province

prov <- factor( province, levels = c( 'MI', 'LO', 'BG', 'CR', 'VA', 'SO', 'MB' ) )
## specificare i levels non ? necessario, ma permette di ordinare i levels come si vuole ( default ? 
## ordine alfabetico)
prov
levels(prov)

# come avrete notato, R non ? sensibile all'utilizzo di "" o ''
# scrivere quindi "MI" ? uguale a 'MI'

provi <- as.factor( province )
provi



provASSOLUTE <- table( prov ) # tabella delle frequenze assolute
provASSOLUTE
provRELATIVE <- table( prov ) / length( prov ) #tabella delle frequenze relative 
provRELATIVE

# oppure: 

provRELATIVE <- prop.table( provASSOLUTE )
provRELATIVE



##########################################
#############   ESERCIZIO 2  #############
##########################################

# Il vettore 'patenti' contiene l'indicazione della tipologia di patente che 30 individui intendono ottenere. 
# A seguito dell'esame alcuni di essi vengono promossi ('P' nel vettore esame) e altri bocciati.

patente <- c('B', 'A', 'B', 'D', 'A1', 'B', 'C', 'B', 'B', 'B', 'B','A1', 'A', 'A', 'B',
             'B', 'C', 'B', 'B', 'B', 'D', 'A1', 'B', 'A','B', 'A', 'C', 'D', 'B', 'B')

esame <- c('P', 'B', 'P', 'P', 'B', 'B', 'P', 'P', 'P', 'B', 'P', 'P', 'B', 'B', 'B', 
           'P', 'P', 'P', 'P', 'P', 'B', 'B', 'P', 'B', 'P', 'B', 'P', 'P', 'P', 'P')

# DOMANDE

# Creare le tabelle di frequenza assoluta e relativa sia per la tipologia di patente 
# che per l'esito dell'esame

# Cosa ? possibile commentare dal confronto tra le tipologie di patenti e l'esito dell'esame? 
# (es. esiste una tipologia di patente per cui nessun esaminato ha passato l'esame? 
# Quanti individui che fanno l'esame per la patente B vengono bocciati?)
# SUGGERIMENTO: ?table

# Costruiamo il data.frame dei dati: 

dati = data.frame(cbind(patente, esame))
# controlliamo la struttura dei dati con str

str(dati)

# per non fare confusione, mi tendo dati e cancello patente, esame: 

rm(patente, esame)


attach(dati)

# Per rispondere alla domanda: 

# tabelle delle frequenze assolute patente
table(patente)
# Tabelle delle frequenze relative patente
table(patente) /sum(table(patente))

# tabelle delle frequenze assolute esame
table(esame)
# tabelle delle frequenze relative esame
table(esame) /sum(table(esame))


# Esiste una interazione fra tipi di  patenti ed esito dell'esame? 
# (es. esiste un tipo  di patente per cui nessun esaminato ha passato l'esame? 


# Richiediamo la tabella CONGIUNTA a doppia entrata e quelle "condizionate":

# frequenze assolute
table(dati)

# frequenze relative 
table(dati)/sum(table(dati))


#### DOMANDE PIU' ostiche: 
# Quale percentuale di  individui che fanno l'esame per la patente D  ? bocciato?

table_given_patente = table(dati) / rowSums(table(dati))
table_given_patente
# Risp: 0.3333333 approx 33.3%

# Quale percentuale di individui che sono stati bocciati hanno sostentuo l'esame per la patente D?

table_given_exam = table(dati)  / colSums(table(dati))
table_given_exam
# Risp: 0.09090909 approc 9%


###########################
#### DIAGRAMMI A BARRE ####
###########################

# Plot del diagramma a barre: 

par(mfrow=c(1,2))
barplot(table(patente), col=rainbow(length(levels(patente))), main='frequenze assolute patenti')
barplot(table(patente) /sum(table(patente)), col=rainbow(length(levels(patente))), main='frequenze relative patenti')


par(mfrow=c(1,2))
barplot(table(esame), col=c('red', 'green'), main='frequenze assolute esami')
barplot(table(esame) /sum(table(esame)), col=c('red', 'green'), main='frequenze relative esami')




##########################################
#############   ESERCIZIO 3  #############
##########################################

# Analisi dei dati categorici
# 'Esposizione ai pesticidi'

pesticide <- c('O','O','J','O','J','O','F','O','F','O','N','F','J','J','F','J','O',
        'J','O','N','C','O','F','O','F','N','N','B','B','O','O','N','B','N','B',
        'C','F','J','M','O','O','F','O','O','J','J','J','O','O','B','M','M','O',
        'O','O','B','M','C','B','F')

# DOMANDE

# Creare le tabelle di frequenza assoluta e relativa


# Estrarre il sottocampione composto dai pesticidi di tipo 'O' e 'J' 

# Eliminare dal campione tutti i pesticidi di tipo 'N' e 'M' e creare le tabelle di frequenza
# assoluta e relativa del nuovo campione


