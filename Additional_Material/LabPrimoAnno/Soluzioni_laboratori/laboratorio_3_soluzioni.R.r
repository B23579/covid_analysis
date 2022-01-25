#########################################
############# LABORATORIO 3 #############
#########################################

##########################################
########### ESERCIZIO 1 - SOLUZIONE ######
##########################################

## analisi dei dati quantitativi contenuti 
## nel dataframe "iris" (già contenuto in R)

# 0) Visualizzare alcune informazioni (dimensioni, struttura, prime
#    osservazioni) del dataset "iris".
#    Creare poi un nuovo dataframe chiamato "iris2" contenente solo le
#    prime 4 colonne del dataframe "iris". Eseguire poi 
#    l'attach del nuovo dataframe.

# carico il db in memoria e lo chiamo iris
iris = iris

dim( iris )
head( iris )
str( iris )

iris2 = iris[ , 1:4 ]


# 1) Considerare solo le variabili "Petal.Length" e "Petal.Width": calcolare
#    covarianza campionaria e correlazione campionaria fra le due variabili
#    e commentare

cov( iris$Petal.Length, iris$Petal.Width )

cor( iris$Petal.Length, iris$Petal.Width )


# L'indice di correlazione indica una forte relazione tra
# le variabili considerate

# 2) Visualizzare, tramite opportuni grafici, le relazioni tra tutte
#    e quattro le variabili del dataset.


# Scatterplot
pairs( iris2 )

# Boxplot
boxplot( iris2, col = c( 2, 3, 4, 5 ) )




# 3) E' possibile studiare la differenza tra la lunghezza dei petali
# per le diverse specie di fiore? Commentare i risultati fornendo
# alcune statistiche descrittive e opportuni grafici di confronto

tapply( iris$Petal.Length, iris$Species, summary )
tapply( iris$Petal.Length, iris$Species, var )

# Boxplot di tutte le variabili stratificate per la specie
par ( mfrow = c( 2, 2 ) )

boxplot( iris$Sepal.Length ~ iris$Species, main = 'Sepal Lenght' )
boxplot( iris$Sepal.Width ~ iris$Species, main = 'Sepal Width' )
boxplot( iris$Petal.Length ~ iris$Species, main = 'Petal Lenght' )
boxplot( iris$Petal.Width ~ iris$Species, main = 'Petal Width' )

dev.off()


# 4) Quale specie risulta essere più grande nelle diverse misure del fiore?

# usiamo tapply per tutte le variabili del dataset intero iris 
# stratificando sulla specie e calcolando la media

tapply(iris$Sepal.Length, iris$Species, mean )
tapply(iris$Sepal.Width, iris$Species, mean )
tapply(iris$Petal.Length, iris$Species, mean )
tapply(iris$Petal.Width, iris$Species, mean )

# Potremmo anche utilizzare la mediana come indice dimensionale
# che è molto meno sensisibile della media a valori estremi
# Il procedimento è del tutto analogo a quanto fatto sopra

# Un modo più efficiente, ma tuttavia più complesso è il seguente
# si consiglia di consultare l'help di apply ( ?apply )
# questi comandi, incluso l'utilizzo delle function, non verrano
# richiesti all'esame
apply( iris2, 2, function(x) tapply(x, iris$Species, mean ) )



##########################################
########### ESERCIZIO 2 - SOLUZIONE ######
##########################################


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

sleep = read.table('sleep.txt', header = T )

# 1) Fornire i principali indici di dispersione e posizione per la variabile ore,
# sia per il campione totale e successivamente dopo avero stratificato i dati per tipo di farmaco

summary( sleep$ore )
var( sleep$ore )
sd( sleep$ore )

# stratifichiamo per tipo di farmaco

tapply( sleep$ore, sleep$farmaco, summary )
tapply( sleep$ore, sleep$farmaco, var )
tapply( sleep$ore, sleep$farmaco, sd )


# 2) Quale farmaco risulta più efficace nel garantire un maggior
# numero di ore di sonno?

boxplot( sleep$ore ~ sleep$farmaco )
# dagli indici calcolati al punto 1 e dal boxplot appena costruito, 
# si evidenzia che il farmaco B risulta più efficace per i pazienti

# 3) Identificare il paziente che dorme di meno? E quello che dorme di più?

sleep$ID[ sleep$ore == min( sleep$ore ) ]
sleep$ID[ sleep$ore == max( sleep$ore ) ]

# vediamo cosa succede se stratifico: non è detto che le cose
# rimangano uguali perchè non stavamo usando tutte le informazioni! 

sleep.A = sleep[ sleep$farmaco == 'A', ]
sleep.B = sleep[ sleep$farmaco == 'B', ]

sleep.A$ID[ sleep.A$ore == min( sleep.A$ore ) ]
sleep.B$ID[ sleep.B$ore == min( sleep.B$ore ) ]

# abbiamo capito che il paziente con il minor numero di ore di sonno
# assume il farmaco B

sleep.A$ID[ sleep.A$ore == max( sleep.A$ore ) ]
sleep.B$ID[ sleep.B$ore == max( sleep.B$ore ) ]
# in questo caso il paziente rimane lo stesso indipendentemente
# dal tipo di farmaco assunto


# ATTENZIONE: MODO PIU' FURBO E VELOCE
# si utilizzano i comandi which.min e which.max
# che fornisco l'elemento del vettore che soddisfa la condizione

which.min( sleep$ore ) # il 15esimo elemento di ore è il minimo
which.max( sleep$ore ) # il 17esimo elemento di ore è il masimo

# identifico i pazienti nel seguente modo: 
sleep$ID[ which.min( sleep$ore ) ]
sleep$ID[ which.max( sleep$ore ) ]

# provate a fare la stessa cosa con questi due comandi 
# tenendo conto dei farmaci


# 4) Fornire indicazioni su eventuali asimmetrie della distribuzione del numero 
# di ore di sonno in funzione del farmaco. Cosa si può notare dai grafici? 
# Notate qualche anomalia nelle misurazioni? Se sì, provate ad eliminare il dato
# e a ripetere l'analisi

boxplot( sleep$ore ~ sleep$farmaco, main = 'Boxplot ore di sonno', 
         ylab = 'Ore di sonno' )

# Si può notare che entrambi presentano una consistente asimmetria verso destra.
# Ciò si evidenzia dal fatto che la mediana risulta molto più spostata verso il primo quartile

tapply( sleep$ore, sleep$farmaco, mean ) > tapply( sleep$ore, sleep$farmaco, median )

# Abbiamo verificato che la media è maggiore della mediana per il farmaco A, ma non lo è
# per il farmaco B anche se dal boxplot non si direbbe!!! 
# Per il farmaco B, notiamo tuttavia la presenza di un outlier negativo molto distante. 
# Potrebbe essere un errore di misura dato che si allontana molto dalla distribuzione del resto
# della popolazione per cui, in questo caso, decidiamo di eliminarlo dai dati.

# N.B. la procedura di eliminazione degli outlier non deve essere fatta automaticamente.
# Ci deve essere una ragione metodologica valida a giustificare 
# l'eliminazione anche di un solo dato

# identifichiamo l'outlier ed eliminiamolo:

outlier = which( sleep$ore == min( sleep$ore ) )

sleep.clean = sleep[ -outlier, ]
dim(sleep)
dim(sleep.clean)

boxplot( sleep.clean$ore ~ sleep.clean$farmaco, main = 'Boxplot ore di sonno', 
         ylab = 'Ore di sonno' )

tapply( sleep.clean$ore, sleep.clean$farmaco, mean ) > tapply( sleep.clean$ore, sleep.clean$farmaco, median )

# abbiamo avuto una prova empirica di come la media sia un indice di posizione molto
# poco robusto poichè è estremamente sensibile alla presenza di valori devianti




