##################################################### TEST D'IPOTESI PER LA MEDIA SU UNO O 2 CAMPIONI ##

# Argomenti trattati :  - test per la media in ipotesi di normalita' :  verifica della prob di errore
# di primo tipo, di secondo tipo, e della potenza del test, tramite simulazione; - esempio di
# inferenza per la media in ipotesi di normalita' su un dataset reale, considerando uno o due
# campioni.

# esercizi di simulazione
numero_a_caso = 819260647
set.seed( numero_a_caso )

setwd('./Lab_0_kickoff/')

# Esercizio 1 ---------------------------------------------------------------------------------

# Consideriamo un test bilatero per la media mu di una popolazione gaussiana di varianza nota
# sigma^2. Sia il test :  H_0 :  mu = 50 vs H_1 :  mu != 50 La regione critica del test bilatero di
# livello alpha e'  R_alpha = {abs( media.camp - 50 ) / ( sigma / sqrt( n ) ) > z_( 1-alpha / 2 )}
# OBIETTIVO :
# Vogliamo ora verificare che l'errore di primo tipo venga commesso proprio con probabilita' alpha,
# come sappiamo dalla teoria per i test di livello alpha, tramite simulazione.

# Simuliamo 100.000 realizzazioni di un campione di 14 variabili aleatorie gaussiane con media mu =
# 50 e deviazione standard sigma = 2.5 nota.  Calcoliamo quindi la percentuale di realizzazioni
# campionarie che ci portano a rifiutare H_0

N = 1e+5
n = 14
sigma = 2.5
# media vera della popolazione da cui provengono i campioni
mu = 50
# media ipotizzata in H_0!
mu.0 = 50
# mi metto nella situazione in cui H_0 e' vera per verificare errore di primo tipo..
# livello teorico del test
alpha = 0.05

# vettore che conterra' il risultato del test ad ogni iterazione
esito = rep( 0, N )

for ( i in 1 : N ) {
  # ripeto il test N volte

  # ad ogni iterazione simulo i dati gaussiani su cui effettuare il test
  dati.sim = rnorm( n, mean = mu, sd = sigma )

  media.camp = mean( dati.sim )

  # calcolo la soglia della regione critica :  e' il quantile della Normale
  z.alpha = qnorm( 1 - alpha / 2 )

  # calcolo la statistica test
  Z.0 = abs( media.camp - mu.0 ) / ( sigma / sqrt( n ) )

  # effettuo il test :  esito = 1 se rifiuto, 0 se accetto
  esito[ i ] = ifelse( Z.0 > z.alpha, 1, 0)
}

# calcolo una stima della probabilita' di errore di primo tipo proporzione di volte in cui rifiuto
alpha.camp = mean( esito )
alpha.camp

# la stima di alpha e' molto vicina all'errore di primo tipo reale


#---------------------------------------------------------------------------------------------#
# EX :  provare cosa succede per un numero di tentativi N che va da 10 a 100000 con passo 100 # e
# graficare l'andamento della variabile apha.camp #
#---------------------------------------------------------------------------------------------#



# Consideriamo ora un test unilatero per la media mu di una popolazione gaussiana di varianza nota
# sigma^2. Sia il test :  H_0 :  mu <= 0 vs H_1 :  mu > 0 La regione critica del test unilatero di
# livello alpha e'  R_alpha = {media.camp > 0 + z_( 1-alpha )*sigma / sqrt( n )}

# OBIETTIVI :  1 ) Vogliamo valutare l'andamento dell'errore di II tipo beta, e della POTENZA del
# test, rispetto alla violazione dell'ipotesi nulla ( ovvero all'allontanarsi dal valore 0 della
# media vera della popolazione, mu ).  2 ) Vogliamo calcolare l'errore di II tipo teorico in
# corrispondenza di alcuni valori fissati di mu, e valutare quindi tramite simulazione che
# l'errore di II tipo venga commesso proprio con probabilita' beta, come sappiamo dalla teoria.


# OBIETTIVO 1 :  disegno di beta e della funzione potenza del test

# valori della media vera della popolazione :  e' un vettore di possibili violazioni di H_0, che
# vanno dalla violazione piu' blanda ( mu = 0.5 ) a quella piu' estrema ( mu = 5 )
mu = seq( 0.5, 5, by = 0.01 )

# devo stabilire le caratteristiche del campione che sto considerando
n = 30
sigma = 3
alpha = 0.01
# devo fissare il livello del test per trovare la potenza!


#--------------------------------------------------------#
# EX :  provare a vedere cosa cambia cambiando il livello #
#--------------------------------------------------------#

# media ipotizzata sotto H_0
mu_0 = 0

# errore di II tipo :  probabilita' di accettare H.0 quando e' falsa, ovvero quando la media mu ?
# effettivamente > 0

# quantile di ordine 1-alpha della normale std
z.alpha = qnorm( 1 - alpha )
beta = pnorm( z.alpha - mu / sigma * sqrt( n ) )
potenza = 1 - beta

# disegno l'andamento delle funzioni calcolate
dev.new()
plot( mu, beta, type = "l", lwd = 2, ylim = range( cbind( beta, potenza ) ),
      main = "Andamento di beta e potenza rispetto alla media vera",
      xlab = "media vera della popolazione", ylab = "" )
lines( mu, potenza, lwd = 2, col = "red" )
legend( 4, 0.7, legend = c( expression( beta ), "potenza" ), col = c( "black", "red" ),
        lwd = 2, cex = 0.85 )
abline( h = 0.5 )

# notiamo che piu' mu e' vicina a 0, piu' beta cresce e la potenza descresce, mentre allontanandosi
# da 0 ( e quindi dall'ipotesi nulla, secondo cui mu e'  negativa ) beta tende a 0 e la potenza ad 1.

# N.B. questo e' l'andamento TEORICO di beta e della potenza al variare di mu vediamo ora cosa
# succede empiricamente, simulando dei dati di media mu fissata ( scegliamo alcuni valori di mu ), e
# valutando il beta campionario



# OBIETTIVO 2 :  valutazione di beta e della funzione potenza del test in corrispondenza di alcuni
# valori di mu fissati

# valori selezionati della media vera della popolazione sono quelli in base ai quali simulero' i
# campioni di dati
mu.sel = c( 1, 1.5, 3 )

# valori teorici di beta e potenza in corrispondenza delle scelte fatte per mu
beta.sel = beta[ match( mu.sel, mu ) ]
beta.sel

potenza.sel = 1 - beta.sel
potenza.sel

# quante simulazioni?
N = 1000

esito = matrix( 0, N, length( mu.sel ) )

for ( i in 1 : N )
{
  # ad ogni iterazione simulo i dati gaussiani su cui effettuare il test ho diversi valori di mu da
  # cui simulare! altro ciclo..

  for ( j in 1 : length( mu.sel ) )
  {
    dati.sim = rnorm( n, mean = mu.sel[ j ], sd = sigma )

    media.camp = mean( dati.sim )

    # calcolo la statistica test :
    Z_0 = ( media.camp - mu_0 ) / sigma * sqrt( n )

    # effettuo il test :  esito = 1 se rifiuto, 0 se accetto
    esito[ i, j ] = ifelse( Z_0 > z.alpha, 1, 0 )
  }
}

# potenza empirica = proporzione di volte in cui ho effettivamente rifiutato
potenza.camp = mean( as.data.frame( esito ) )
potenza.camp
potenza.sel


beta.camp = 1 - potenza.camp
beta.camp
beta.sel

# c'e' un ottimo accordo tra valori teorici e valori derivanti dalla simulazione!  il che significa
# che il test sta effettivamente funzionando come ci aspettiamo in base alla teoria


# Esercizio 2 ---------------------------------------------------------------------------------

# Esempio guidato di analisi dei dati :  Test sulla media e sulla proporzione

# Carichiamo i dati contenuti nel file outcomes.txt [ fonte :  database clinico Lombardia ] Il
# database contiene 963 pazienti e l'osservazione di 4 variabili :  PRESSIONE :  pressione arteriosa
# sanguigna.  ST_RESOLUTION_70_60 :  riduzione dello slivellamento del tratto ECG a 1 ora dall'
# intervento ( angioplastica ) :  1 = si, 0 = no CREATININA_INGRESSO :  valori della creatinina in
# ingresso CREATININA_USCITA :  valori della creatinina in uscita

# Questi dati possono essere utilizzati per rispondere a diverse domande :  1 ) dato che i pazienti
# contenuti nel database sono infartati, e' ragionevole supporre che la pressione sanguigna di tale
# popolazione sia diversa da quella fisiologica ( 80 )?  2 ) le linee guida regionali per l'intervento
# di angioplastica indicano come soglia di 'accettabilita'' del protocollo che l'intervento produca
# una effettiva riduzione dello slivellamento ( a 1 ora ) almeno nel 70% dei casi. In base al
# campione a disposizione, e' possibile affermare che negli ospedali lombardi l'intervento viene
# effettuato con un protocollo accettabile?

# importazione del dataset
dati = read.table( "outcomes.txt", header = T )
dati

dim( dati )
# n e' il numero di pazienti ( dimensione del campione )
n = dim( dati )[ 1 ]
names( dati )

attach( dati )

# DOMANDA 1 :  TEST SULLA MEDIA

# per rispondere alla domanda dell'esercizio devo effettuare un test d'ipotesi :  eseguo un test
# sulla media vera mu della pressione sanguigna dei pazienti affetti da infarto. In base alla
# richiesta dell'esercizio vorremo verificare :  H_0 :  mu = mu.0 vs H_1 :  mu !=mu.0 dove la varianza
# della variabile che considero e' incognita.  La statistica test in questo caso e' dunque T.0 =
# abs( media.camp - mu.0 ) / s / sqrt( n ) dove mu.0 nel nostro caso e' 80, mentre la regione critica del
# test bilatero di livello alpha e'  R_alpha = {T.0 > t_( 1-alpha / 2,n-1 )}

# la variabile che mi interessa e' la pressione
n = sum( !is.na( PRESSIONE ) )
n
# non ci sono dati mancanti!

# primo passo :  dal momento che la varianza e' incognita devo verificare la normalita' dei dati
dev.new()
qqnorm( PRESSIONE, datax = T )
dati.ord = sort( PRESSIONE )
ranghi = 1 : n
F.emp = ( ranghi - 0.5 ) / n
z_j = qnorm( F.emp )
y_j = lm( z_j ~ dati.ord )$fitted.values
lines( dati.ord, y_j, col = "red", lwd = 2 )

# ci sono due outlier negativi INVEROSIMILI :  pressione sanguigna nulla, difficile essere vivi in
# tal caso..  rimuovo i due dati sospetti ( cambiando anche la dimensione campionaria )

PRESSIONE = PRESSIONE[ which( PRESSIONE != 0 ) ]
n = sum( !is.na( PRESSIONE ) )
n

# e rifaccio il qq-plot
dev.new()
qqnorm( PRESSIONE, datax = T )
dati.ord = sort( PRESSIONE )
ranghi = 1 : n
F.emp = ( ranghi - 0.5 ) / n
z_j = qnorm( F.emp )
y_j = lm( z_j ~ dati.ord )$fitted.values
lines( dati.ord, y_j, col = "red", lwd = 2 )
# variabile discreta ma molti dati e buon adattamento alla distribuzione Normale :  possiamo
# procedere!

# una volta verificata la normalita' posso procedere con il test :  fisso il livello
alpha = 0.01

# stima puntuale di media e deviazione standard
media.camp = mean( PRESSIONE )
devstd.camp = sd( PRESSIONE )

# quantile della corrispondente t-Student ( varianza incognita! )
t.alpha = qt( 1 - alpha / 2, n - 1 )

# calcolo dunque la statistica test T.0
T.0 = abs( media.camp - 80 ) / ( devstd.camp / sqrt( n ) )
T.0

# valore enorme!!  qual e' l'esito del test? la statistica test cade nella regione critica?
T.0 > t.alpha

# Al livello 1% ho evidenza per rifiutare H_0 ed affermare che la vera media della pressione
# sanguigna negli infartati e' diversa da 80.. visto per? il valore cos? elevato della statistica
# test, e per essere maggiormente precisi, calcoliamo il p-value del test bilatero a varianza
# incognita :  p-value = 2*P( t>T.0 ) con R si puo' fare in modo esatto!!!

pvalue = 2 * ( 1 - pt( T.0, n - 1 ) )
pvalue

# come si puo' osservare, il p-value e' 0, per cui l'evidenza per affermare che la media vera della
# pressione sia diversa da 80 e' molto forte

# N.B. esiste una funzione automatica di R che effettua il test t :
t.test( PRESSIONE, alternative = "two.sided", mu = 80, conf.level = 1 - alpha )
# utilizziamo la funzione per verificare di avere effettuato il test nel modo corretto..

# DOMANDA 2 :  TEST SULLA PROPORZIONE

# per rispondere alla domanda dell'esercizio devo effettuare un test d'ipotesi :  eseguo un test
# sulla proporzione vera p di casi in cui l'intervento avviene secondo protocollo ( slivellamento
# ridotto ) In particolare in base alla richiesta dell'esercizio vorremo verificare :  H_0 :  p = 0.7
# vs H_1 :  p > 0.7 ricordiamo che la statistica test in questo caso e'  Z.0 = ( p.camp - p.0 ) /
# sqrt( p.0*( 1 - p.0 ) / n ) dove p.0 nel nostro caso e' 0.7, mentre la regione critica del test
# unilatero di livello alpha e' R_alpha = {Z.0 > z_( 1-alpha )}

# fisso quindi
p.0 = 0.7
# e fisso il livello del test che voglio effettuare
alpha = 0.05
# ( poi per completezza calcolero' anche il p-value )

# variabile che voglio considerare :  ST_RESOLUTION_70_60 do un nome piu' semplice alla variabile
ST = ST_RESOLUTION_70_60
n = sum( ! is.na( ST ) )
# dimensione effettiva del dataset ( escludo i missing! )

# quantile che mi serve da limite della regione critica ( test unilatero!! )
z.alpha = qnorm( 1 - alpha )

# calcolo la stima puntuale della proporzione di casi in cui il trattamento ha successo :  conto
# quante volte vedo un successo ( S ) rispetto al totale

p.camp = sum( ST, na.rm = TRUE ) / n
p.camp
# la stima puntuale di p e' maggiore di 0.7, gia' questo e' un primo indizio a favore di H_1

# calcolo la statistica test :
Z.0 = ( p.camp - p.0 ) / sqrt( p.0 * ( 1 - p.0 ) / n )
Z.0

# qual e' l'esito del test? la statistica test cade nella regione critica?
Z.0 > z.alpha

# i dati forniscono quindi evidenza sufficiente per rifiutare l'ipotesi nulla, ed affermare che il
# protocollo negli ospedali lombardi ha succcesso almeno nel 70% dei casi, ed e' dunque accettabile

# la conclusione tratta e' forte? Quanto dipende dal livello scelto ( alpha )?  calcoliamo il
# p-value :

pvalue = 1 - pnorm( Z.0 )
pvalue
# il p-value e' molto basso, dunque abbiamo forte evidenza per affermare che H_1 e' vera


## N.B. come per il test t, esiste una funzione automatica di R che effettua il test per la
## proporzione :
counts = sum( ST, na.rm = TRUE )
prop.test( counts, n, p = 0.7, alternative = "greater", conf.level = 1 - alpha, correct = FALSE )
# utilizziamo la funzione per verificare di avere effettuato il test nel modo corretto..
# attenzione perch? R effettua una correzione per migliorare le prestazioni del test, mentre noi
# abbiamo effettuato un test classico, per cui dobbiamo mettere 'FALSE' all'argomento 'correct'
# per poter confrontare i risultati

detach( dati )





# Esercizio 3 ---------------------------------------------------------------------------------

# Esempio guidato di analisi dei dati.  Inferenza sulla media di una popolazione gaussiana

# Carichiamo i dati contenuti nel file tremperatura.txt :  sono gli stessi che avevo lasciato come
# compito nel laboratorio 1 per esercitarsi nell'analisi descrittiva ( 1 o 2 campioni ).  Il file
# contiene 130 osservazioni di 3 variabili :  Temperatura, Sesso e Freq_cardiaca [ fonte :  Mackowiak,
# P. A., Wasserman, S. S., and Levine, M. M.  ( 1992 ), 'A Critical Appraisal of 98.6 Degrees F, the
# Upper Limit of the Normal Body Temperature, and Other Legacies of Carl Reinhold August
# Wunderlich', Journal of the American Medical Association, 268, 1578-1580. ] la variabile
# 'Temperatura' si riferisce alla temperatura corporea ( gradi Fahrenheit ) la variabile 'Sesso' si
# riferisce al sesso del paziente ( U = uomo, D = donna ) la variabile 'Freq_cardiaca' si riferisce
# alla frequenza cardiaca ( battiti al minuto ) I dati provengono da un articolo pubblicato sul
# 'Journal of the American Medical Association' che studia se la vera temperatura media del corpo
# umano e' pari a 98.6 gradi Fahrenheit.

# Le due principali questioni a cui si vuole dare risposta nello studio da cui i dati provengono
# sono :  1 ) stabilire se la media reale della temperatura corporea della popolazione sia 98.6 gradi
# F 2 ) stabilire se ci sono differenze nella temperatura corporea dovute al sesso del soggetto, e
# in particolare se la temperatura corporea delle donne e' piu' alta di quella degli uomini

# importazione del dataset
dati = read.table( "temperatura.txt", header = T )
dati

dim( dati )
head(dati)
n = dim( dati )[ 1 ]  # n e' il numero di pazienti ( dimensione del campione )
names( dati )

attach( dati )

# ora possiamo utilizzare la variabile Temperatura, quella che ci interessa

# Domanda 1 ) :  Rispondiamo calcolando un intervallo di confidenza per la media, ed effettuando un
# test d'ipotesi sulla media della popolazione

# primo passo :  calcolo la stima puntuale della media della popolazione
media.camp = mean( Temperatura )
media.camp
# e' molto vicina alla media vera ipotizzata nello studio!

# valutiamo prima di procedere la Normalita' dei dati, dal momento che vorremo sia calcolare un
# intervallo di confidenza basato sulla distribuzione t di Student, sia effettuare un test sulla
# media in ipotesi di Normalita'

dev.new()
qqnorm( Temperatura, datax = T, pch = 16 )
temp.ord = sort( Temperatura )
ranghi = 1 : n
F.emp = ( ranghi - 0.5 ) / n
z_j = qnorm( F.emp )
y_j = lm( z_j ~ temp.ord )$fitted.values
lines( temp.ord, y_j, col = "red", lwd = 2 )
# buon adattamento dei dati alla distribuzione Normale :  possiamo procedere!

# INFERENZA SULLA MEDIA DI UNA POPOLAZIONE GAUSSIANA, A VARIANZA INCOGNITA

# calcolo un intervallo di confidenza per la media di livello 95%

alpha = 0.05
devstd.camp = sd( Temperatura )

t.alpha = qt( 1 - alpha / 2, n - 1 )
IC.alpha = c( media.camp - t.alpha * devstd.camp / sqrt( n ), media.camp + t.alpha * devstd.camp / sqrt( n ) )
IC.alpha

# N.B. il valore della media vera ipotizzato nello studio non e' contenuto nell'intervallo :  gi?
# questo mi sta dando informazioni sul test d'ipotesi che voglio fare.. quali?

# effettuo ora un test per verificare l'ipotesi H_0 :  mu = 98.6 F vs H_1 :  mu != 98.6 F La regione
# critica del test bilatero di livello alpha e'  R_alpha = {abs( media.camp - 98.6 ) / ( s / sqrt( n ) ) >
# t_( 1-alpha / 2 )( n-1 )}

# il quantile della t-Student l'abbiamo appena calcolato; calcolo dunque la statistica test T.0
T.0 = abs( media.camp - 98.6 ) / ( devstd.camp / sqrt( n ) )
T.0

# qual e' l'esito del test? la statistica test cade nella regione critica?
T.0 > t.alpha

# Al livello 5% ho evidenza per rifiutare H_0 ed affermare che la vera media della popolazione ?
# diversa da 98.6 F

# Per essere maggiormente precisi, calcoliamo il p-value del test test bilatero a varianza
# incognita :  p-value = 2*P( t>T.0 )

p = 2 * ( 1 - pt( T.0, n - 1 ) )
p

# come si puo' osservare, il p-value e' circa 0, per cui ho forte evidenza per affermare che la
# media vera sia diversa da 98.6

# INFERENZA SULLA DIFFERENZA TRA LE MEDIE DI DUE POPOLAZIONI

# Domanda 2 ) :  rispondiamo a questa domanda calcolando un intervallo di confidenza, ed effettuando
# un test d'ipotesi per la differenza tra le medie delle temperature corporee nelle due
# sottopopolazioni individuate dal sesso

# consideriamo innanzitutto i due campioni distinti per sesso
names( dati )

temp.m = Temperatura[ which( Sesso == "U" ) ]
temp.f = Temperatura[ which( Sesso == "D" ) ]
length( temp.m )
length( temp.f )
# ora ho due campioni di ampiezza dimezzata!
n = length( temp.m )


# primo passo :  calcolo la stima puntuale della temperatura media corporea maschile e femminile
media.m = mean( temp.m )
media.m
media.f = mean( temp.f )
media.f
# la temperatura nelle donne sembra mediamente piu' alta..

# tramite questi due valori posso calcolare subito la stima puntuale della differenza tra le medie
# delle temperature corporee maschile e femminile
diff.camp = media.f - media.m
diff.camp


# come prima, valutiamo prima di procedere la Normalita' dei dati dobbiamo farlo separatamente per
# i due gruppi!

dev.new()
par( mfrow = c( 2, 1 ) )

qqnorm( temp.m, datax = T, main = "QQ plot maschi", pch = 16 )
temp.ord = sort( temp.m )
ranghi = 1 : n
F.emp = ( ranghi - 0.5 ) / n
z_j = qnorm( F.emp )
y_j = lm( z_j ~ temp.ord )$fitted.values
lines( temp.ord, y_j, col = "red", lwd = 2 )

qqnorm( temp.f, datax = T, main = "QQ plot femmine", pch = 16 )
temp.ord = sort( temp.f )
ranghi = 1 : n
F.emp = ( ranghi - 0.5 ) / n
z_j = qnorm( F.emp )
y_j = lm( z_j ~ temp.ord )$fitted.values
lines( temp.ord, y_j, col = "red", lwd = 2 )

# buon adattamento dei dati alla distribuzione Normale :  possiamo procedere!

# calcolo un intervallo di confidenza per la differenza tra le medie di livello 95%

# IPOTESI :  le varianze teoriche ( incognite ) della temperatura nelle due sottopopolazioni sono
# uguali MA :  posso fare un test sul confronto tra le varianze nei due gruppi per verificare se
# questa ipotesi e' realistica per il nostro dataset

# stima puntuale della dev standard
sd( temp.m )
sd( temp.f )
# i due valori non sembrano cos? diversi ( stesso ordine di grandezza ) e niente mi porta a pensare
# che la varianza debba essere diversa nei due gruppi

# eseguiamo il test bilatero sulle varianze
f.0 = var( temp.m ) / var( temp.f )
alpha = 0.05
f.alpha1 = qf( alpha / 2, n - 1, n - 1 )
f.alpha2 = qf( 1 - alpha / 2, n - 1, n - 1 )

f.0 < f.alpha1 | f.0 > f.alpha2
# con un livello di significativit? del 5%, non ho evidenza per pensare che le due varianze siano
# diverse calcolo anche il p-value
p = 2 * min( pf( f.0, n - 1, n - 1 ), 1 - pf( f.0, n - 1, n - 1 ) )
p

# N.B. come per il test bilatero sulla varianza del singolo campione, visto che la distribuzione F
# e' asimmetrica per trovare il p-value devo prendere 2 volte il minimo tra la coda destra e la
# coda sinistra della distribuzione F in corrispondenza della statistica test F.0

# come per il test t e il test z, la stessa cosa si puo' fare in automatico con una funzione R :
var.test( temp.m, temp.f, alternative = "two.sided" )


# posso quindi procedere nell'ipotesi di varianza uguali!  calcolo la deviazione standard
# campionaria pooled, che tiene conto di entrambi i campioni
s.pooled = sqrt( ( ( n - 1 ) * sd( temp.m )^2 + ( n - 1 ) * sd( temp.f )^2 ) / ( 2 * n - 2 ) )
s.pooled

alpha = 0.05
t.alpha = qt( 1 - alpha / 2, 2 * n - 2 )

# realizzazione dell'IC per la differenza tra le medie
IC.alpha = c( diff.camp - t.alpha * s.pooled * sqrt( 2 / n ), diff.camp + t.alpha * s.pooled * sqrt( 2 / n ) )
IC.alpha

# non contiene lo 0! quindi esister? una differenza significativa nella temperatura corporea tra
# uomini e donne.. inoltre l'intervallo contiene dolo valori positivi, dunque la temperatura delle
# donne sembra effettivamente superiore a quella degli uomini..  verifichiamo con un test!

# effettuo ora un test per verificare l'ipotesi H_0 :  mu.f <= mu.m vs H_1 :  mu.f > mu.m La regione
# critica del test unilatero per due campioni di livello alpha e'  R_alpha = {( diff.camp -
# 0 ) / ( s.pooled*sqrt( 2 / n ) ) > t_( 1-alpha )( 2*n-2 )} dove diff.camp = media.camp.f - media.camp.m

# quantile della t-Student :
alpha = 0.05
t.alpha = qt( 1 - alpha, 2 * n - 2 )

# statistica test T.0 :
T.0 = diff.camp / ( s.pooled * sqrt( 2 / n ) )
T.0

# qual e' l'esito del test? la statistica test cade nella regione critica?
T.0 > t.alpha

# Al livello 5% ho evidenza per rifiutare H_0 ed affermare che esiste una differenza nella media
# della temperatura corporea nelle due sottopopolazioni.  Per essere maggiormente precisi,
# calcoliamo il p-value del test unilatero a varianza incognita :  p-value = P( t > T.0 )

pvalue = ( 1 - pt( T.0, 2 * n - 2 ) )
pvalue
# il p-value e' abbastanza basso, ma non bassissimo :  al livello 1% non avrei rifiutato H_0!


## N.B. posso usare la funzione t.test anche per fare un test di confronto tra due gruppi :
t.test( temp.f, temp.m, alternative = "greater", mu = 0, var.equal = TRUE, conf.level = 1 - alpha )
# utilizziamo la funzione per verificare di avere effettuato il test nel modo corretto..

detach( dati )




# Esercizio 4 ---------------------------------------------------------------------------------

# Esempio guidato di analisi dei dati :  Test per dati accoppiati

# Consideriamo ancora i dati contenuti nel file outcomes.txt e consideriamo le variabili
# CREATININA_INGRESSO :  valori della creatinina in ingresso ( pre-ricovero ) CREATININA_USCITA :
# valori della creatinina in uscita ( post-infarto )

# La misura della concentrazione di creatinina nel plasma e' un indicatore della funzione renale, e
# in particolare un suo aumento e' un possibile indice di danno renale; secondo una teoria non
# ancora accettata dalla comunit? scientifica internazionale, le disfunzioni ai reni sono una
# delle possibili complicanze dell'infarto.  Un professore del Policlinico di Milano sta
# conducendo uno studio sulle complicanze dell'infarto, e vuole dunque utilizzare il campione a
# disposizione per dimostrare la tesi che nei pazienti infartati si osservi un innalzamento nella
# concentrazione di creatinina nel plasma.

# importazione del dataset
dati = read.table( "outcomes.txt", header = T )
head(dati)

dim( dati )
n = dim( dati )[ 1 ]  # n e' il numero di pazienti ( dimensione del campione )
names( dati )

attach( dati )

# Per rispondere alla questione posta dall'esercizio devo provare che il livello di creatinina ?
# significativamente piu' alto al momento della dimissione rispetto al ricovero.  Dal momento che
# le misurazioni che ho a disposizione riguardano gli stessi pazienti, prima e dopo l'infarto, i
# due gruppi non sono indipendenti ma accoppiati; considero dunque le differenze.

# creo la nuova variabile differenza
DIFF = CREATININA_USCITA - CREATININA_INGRESSO
# in questo modo, per provare le complicanze dell'angioplastica, dovrei provare che la media delle
# differenze e' positiva

n = sum( !is.na( DIFF ) )
n


# per rispondere alla domanda dell'esercizio devo effettuare un test d'ipotesi :  eseguo un test per
# dati accoppiati sulla differenza media ( mu_d ) tra la creatinina post infarto e quella pre
# infarto.  In base alla richiesta dell'esercizio vorremo verificare :  H_0 :  mu_d <= 0 vs H_1 :  mu_d
# > 0 dove la varianza della variabile che considero e' incognita.  La statistica test in questo
# caso e' dunque T.0 = media.diff / ( s / sqrt( n ) ) mentre la regione critica del test unilatero di
# livello alpha e'  R_alpha = {T.0 > t_( 1-alpha,n-1 )}

# la variabile che mi interessa e' ora la differenza nei valori di creatinina devo verificare la
# normalita' delle differenze per poter procedere con il test..

dev.new()
qqnorm( DIFF, datax = T, main = "QQ creatinina" )
diff.ord = sort( DIFF )
ranghi = 1 : n
F.emp = ( ranghi - 0.5 ) / n
z_j = qnorm( F.emp )
y_j = lm( z_j ~ diff.ord )$fitted.values
lines( diff.ord, y_j, col = "red", lwd = 2 )
# i dati si adattano perfettamente alla distribuzione normale... posso procedere!

# stima puntuale della media e della varianza
media.diff = mean( DIFF )
media.diff  # la stima della media e' superiore a 0...
dev.stand.diff = sd( DIFF )
dev.stand.diff

# eseguo il test!  quantile della t-Student :
alpha = 0.01
t.alpha = qt( 1 - alpha, n - 1 )

# statistica test T.0 :
T.0 = media.diff / ( dev.stand.diff / sqrt( n ) )
T.0

# qual e' l'esito del test? la statistica test cade nella regione critica?
T.0 > t.alpha

# Al livello 1% ho evidenza per rifiutare H_0 ed affermare che esiste una differenza nella media
# della creatinina prima e dopo l'intervento.  Per essere maggiormente precisi, calcoliamo il
# p-value del test unilatero a varianza incognita per dati accoppiati :  p-value = P( t > T.0 )

pvalue = ( 1 - pt( T.0, n - 1 ) )
pvalue
# il p-value e' praticamente zero!


## N.B. posso usare la funzione t.test anche per fare un test per dati accoppiati :  ( basta impostare
## l'argomento 'paired' a 'TRUE' )
t.test( CREATININA_USCITA, CREATININA_INGRESSO, alternative = "greater", mu = 0, paired = TRUE, conf.level = 1 -
         alpha )
# utilizziamo la funzione per verificare di avere effettuato il test nel modo corretto..


detach( dati )
