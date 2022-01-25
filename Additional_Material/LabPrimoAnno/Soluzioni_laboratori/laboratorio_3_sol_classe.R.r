###############################
#####    CORSO DI FSSB    #####
## per INGEGNERIA BIOMEDICA ###
###############################


############################################
######  ESERCIZI DA FARE IN CLASSE #########
#####    LABORATORIO 3 - SOLUZIONE  ########
############################################


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
dim(arance)
head(arance)
str(arance)

# 1) Trasformare le età in mesi considerandoli tutti di 30 giorni 
# approssimandoli all'unità. Utilizzare il comando round( mesi, 0 )
eta.mesi = round( arance$age / 30, 0 )
eta.mesi

# 2) Trasformare la circonferenza in diametro
# Suggerimento: R ha tante costanti memorizzate...
diametro = round( arance$circumference / pi, 2 )
diametro

# aggiungo al data.frame le variabili diametro e mesi
arance = data.frame(arance, eta.mesi, diametro)
head( arance )
names( arance )

# 3) Calcolare il massimo diametro raggiunto per ogni albero
# Assicurarsi che la variabile sulla quale vogliamo stratificare
# il campione sia categorica --> as.factor()

str( arance )

# come potete vedere, Tree è una variabile int
# la convertiamo e la sostituiamo direttamente
arance$Tree = as.factor( arance$Tree )
class( arance$Tree )
levels( arance$Tree )

# Calcolo il massimo del diametro in ogni albero:
tapply( arance$diametro, arance$Tree, max )

# 4) Qual è l'albero che ha il diametro più piccolo dopo 22 mesi?
minimo = min( arance$diametro[ arance$eta.mesi == 22 ] )
minimo
arance$Tree[ which( arance$diametro == minimo ) ]
# Il numero 3!

# 5) Quali sono gli alberi che presentano una maggiore crescita?
# Riportare i valori della crescita e mostrare il risultato graficamente
MAX = tapply( arance$diametro, arance$Tree, max ) 
MIN = tapply( arance$diametro, arance$Tree, min ) 
MAX - MIN

# L'albero che presenta una maggiore crescita in termini di differenza tra minimo 
# e massimo è il numero 4.

# Lo si può vedere anche dai boxplot:
boxplot( arance$diametro ~ arance$Tree, col = rainbow( length( levels( arance$Tree ) ) ),
         ylab = 'Diametro', xlab = 'Albero')

# Un metodo alternativo più complesso:
# Disegniamo l'andamento del diametro nel tempo
plot( unique( arance$eta.mesi ), arance$diametro[ which( arance$Tree == '1' ) ], 
      col = 1, type='l', ylim = c( 10, 80 ), lwd = 2, xlab = 'Eta [mesi]', ylab = 'Diametro',
      main = 'Andamento temporale del diametro degli albero')
lines( unique( arance$eta.mesi ), arance$diametro[ which( arance$Tree == '2' ) ], col = 2, lwd = 2 )
lines( unique( arance$eta.mesi ), arance$diametro[ which( arance$Tree == '3' ) ], col = 3, lwd = 2 )
lines( unique( arance$eta.mesi ), arance$diametro[ which( arance$Tree == '4' ) ], col = 4, lwd = 2 )
lines( unique( arance$eta.mesi ), arance$diametro[ which( arance$Tree == '5' ) ], col = 5, lwd = 2 )
legend('topleft', col = 1:5, lty = rep(1,5), lwd = rep(2,5),
       legend = c( 'Tree 1', 'Tree 2', 'Tree 3', 'Tree 4', 'Tree 5' ) )

# Disegniamo ora l'incremento:
crescite = tapply( arance$diametro, arance$Tree, diff )
crescite
plot( unique( arance$eta.mesi)[ -1 ], crescite[[ 1 ]], 
      type = 'l', lwd = 2, col = 1, ylim = c( 0, 20 ), main = 'Andamento della crescita',
      xlab = 'Eta [mesi]', ylab = 'Crescite')
lines( unique( arance$eta.mesi)[ -1 ], crescite[[ 2 ]], col = 2, lwd = 2 )
lines( unique( arance$eta.mesi)[ -1 ], crescite[[ 3 ]], col = 3, lwd = 2 )
lines( unique( arance$eta.mesi)[ -1 ], crescite[[ 4 ]], col = 4, lwd = 2 )
lines( unique( arance$eta.mesi)[ -1 ], crescite[[ 5 ]], col = 5, lwd = 2 )
legend('topright', col = 1:5, lty = rep(1,5), lwd = rep(2,5),
       legend = c( 'Tree 1', 'Tree 2', 'Tree 3', 'Tree 4', 'Tree 5' ) )



##############################
######## ESERCIZIO 2  ########
##############################

# 0) Caricare il dataset 'airquality.txt'
airquality = read.table( 'airquality.txt', header = T )
str( airquality )
dim(airquality)
# 1) Ci sono degli NA presenti? Se sì, quanti sono?

sum( is.na( airquality ) ) 
# Ci sono 44 dati mancanti in totale

# e se volessimo sapere velocemente quanti NA ci sono
# nelle diverse variabili?
apply( airquality, 2, function(x) sum( is.na( x ) ) )

# 2) Calcolare media e varianza per le prime 4 colonne
# Si utilizzi l'opzione "na.rm = TRUE" nei comandi (senza le "" )

mean( airquality$Ozone, na.rm = TRUE )
mean( airquality$Solar.R, na.rm = TRUE )
mean( airquality$Wind, na.rm = TRUE )
mean( airquality$Temp, na.rm = TRUE )

# oppure tutto insieme in un comando unico
apply( airquality[ , 1:4 ], 2, mean, na.rm = TRUE )

# Guardare help di apply ( ?apply ) 

# 3) Costruire un grafico di dispersione unico per le prime 4 
# colonne e calcolare covarianza e correlazione
# Si utilizzi l'opzione use = 'complete.obs' per questi due comandi per gestire gli NA

pairs( airquality[, 1:4 ] )
cov( airquality[, 1:4 ], use = 'complete.obs' )
cor( airquality[, 1:4 ], use = 'complete.obs' )

# Commentare il grafico e i risultati ottenuti
# Correlazione negativa di ozono e vento: l'ozono è minore se c'è più vento
# Correlazione positiva alta tra temperatura e ozono

# 4) Costruire i qqplot per le prime 4 variabili e
# commentare i risultati

par( mfrow = c( 2, 2 ) )
qqnorm( airquality[ , 1 ], main = 'Q-Q Plot per Ozone')
qqline( airquality[ , 1 ], col = 'violet', lwd = 2)
qqnorm( airquality[ , 2 ], main = 'Q-Q Plot per Solar.R')
qqline( airquality[ , 2 ], col = 'violet', lwd = 2)
qqnorm( airquality[ , 3 ], main = 'Q-Q Plot per Wind')
qqline( airquality[ , 3 ], col = 'violet', lwd = 2)
qqnorm( airquality[ , 4 ], main = 'Q-Q Plot per Temp')
qqline( airquality[ , 4 ], col = 'violet', lwd = 2)

# Nessuna delle variabili sembra seguire una distribuzione normale, in particolare
# le variabili Ozono e Radiazione Solare, per cui l'andamento si discosta molto dalla linea
# viola

