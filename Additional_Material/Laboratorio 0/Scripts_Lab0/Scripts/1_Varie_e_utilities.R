#-------------------------------------------------------------------------------#
#
# Argomenti trattati:
# - Uso e rappresentazione delle pricipali Distribuzioni;
# - Indici di sintesi di una distribuzione  /  un campione
# - Test Statistici e Intervalli di Confidenza
# - Regressione Lineare semplice ( I )
#
#-------------------------------------------------------------------------------#


# Densita' di probabilita', Funzione di Ripartizione e inversa --------------------------------

dev.new()
layout( matrix( c( 1, 2, 3, 4, 5, 6, 7, 8, 9 ), 3, byrow = T ) )
# o in alternativa
par( mfrow = c( 3, 3 ) )

s = seq( -2, 2, by = 0.01 )

# Densita'
plot( s, dunif( s, 0, 1 ), main = "uniforme", type = "l", ylim = c( 0, 1 ) )
plot( s, dexp( s, 1 ), main = "esponenziale", type = "l", ylim = c( 0, 1 ) )
plot( s, dnorm( s, 0, 0.5 ), main = "normale", type = "l", ylim = c( 0, 1 ) )

# Funzioni di Ripartizione
plot( s, punif( s, 0, 1 ), main = "uniforme", type = "l", ylim = c( 0, 1 ) )
plot( s, pexp( s, 1 ), main = "esponenziale", type = "l", ylim = c( 0, 1 ) )
plot( s, pnorm( s, 0, 0.5 ), main = "normale", type = "l", ylim = c( 0, 1 ) )

# Inversa ( adesso fornisco i quantili, escludendo 0 e 1 altrim va a - / + infinito )
w = seq( 0.01 / 2, 1 - 0.01 / 2, by = 0.01 )
plot( w, qunif( w, 0, 1 ), main = "uniforme", type = "l" )
plot( w, qexp( w, 1 ), main = "esponenziale", type = "l" )
plot( w, qnorm( w, 0, 0.5 ), main = "normale", type = "l" )


# Somma di Esponenziali e' una Gamma

# Raffinamento
x1 = seq( 0, 10, 1 )
x2 = seq( 0, 10, 0.5 )
x3 = seq( 0, 10, 0.001 )
e1 = dexp( x1 )
e2 = dexp( x2 )
e3 = dexp( x3 )

par( mfrow = c( 1, 3 ) )
plot( x1, e1, type = "l", main = "passo 1" )
plot( x2, e2, type = "l", main = "passo 0.5" )
plot( x3, e3, type = "l", main = "passo 0.001" )

# Somma di esponenziali
exp1 = rexp( 1000, 2 )
exp2 = rexp( 1000, 2 )
exp3 = rexp( 1000, 2 )
gamma = exp1 + exp2 + exp3

dev.new()
hist( gamma, prob = T, ylim = c( 0, 0.6 ) )
grid = seq( 0, 6, 0.01 )
y = dgamma( grid, 3, 2 )
lines( grid, y, col = "blue" )

# Generazione di campioni casuali -------------------------------------------------------------

x = runif( n = 1000, min = 0, max = 1 )
y = rexp( n = 1000, rate = 1 )
z = rnorm( n = 1000, mean = 0, sd = 1 )

layout( matrix( c( 1, 2, 3, 4, 5, 6, 7, 8, 9 ), 3, byrow = T ) )

plot( x, main = "uniforme" )
plot( y, main = "esponenziale" )
plot( z, main = "normale" )

hist( x, main = "", col = "red", xlab = "x", prob = T )
lines( seq( -0.2, 1.2, length = 100 ), dunif( seq( -0.2, 1.2, length = 100 ) ),
       col = "blue", lty = 2, lwd = 2 )

hist( y, main = "", col = "red", xlab = "x", prob = T )
lines( seq( -1, 9, length = 100 ), dexp( seq( -1, 9, length = 100 ) ),
       col = "blue", lty = 2, lwd = 2 )

hist( z, main = "", col = "red", xlab = "x", prob = T )
lines( seq( -4, 4, length = 100 ), dnorm( seq( -4, 4, length = 100 ) ),
       col = "blue", lty = 2, lwd = 2 )

qqplot( qunif( ( 1:1000 / 1000 - 0.5 / 1000 ) ), x, col = "red",
        xlab = "quantile teorico", ylab = "quantile empirico", asp = 1 )
abline( 0, 1, col = "blue" )

qqplot( qexp( ( 1:1000 / 1000 - 0.5 / 1000 ) ), y, col = "red",
        xlab = "quantile teorico", ylab = "quantile empirico", asp = 1 )
abline( 0, 1, col = "blue" )

qqplot( qnorm( ( 1:1000 / 1000 - 0.5 / 1000 ) ), z, col = "red",
        xlab = "quantile teorico", ylab = "quantile empirico", asp = 1 )
abline( 0, 1, col = "blue" )

# Utilizzo del qqplot per vedere qualitativamente se # un campione e' estratto da una certa
# popolazione

dev.new()
layout( matrix( c( 1, 2, 3, 4, 5, 6, 7, 8, 9 ), 3, byrow = T ) )

# n = 1000
x = runif( n = 1000, min = 0, max = 1 )
y = rexp( n = 1000, rate = 1 )
z = rnorm( n = 1000, mean = 0, sd = 1 )

qqplot( qnorm( ( 1:1000 / 1000 - 1 / 2000 ) ), x, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1, main = "Unif( 0,1 )" )
qqplot( qnorm( ( 1:1000 / 1000 - 1 / 2000 ) ), y, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1, main = "Exp( 1 )" )
qqplot( qnorm( ( 1:1000 / 1000 - 1 / 2000 ) ), z, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1, main = "Norm( 0,1 )" )

# n = 100
x = runif( n = 100, min = 0, max = 1 )
y = rexp( n = 100, rate = 1 )
z = rnorm( n = 100, mean = 0, sd = 1 )

qqplot( qnorm( ( 1:100 / 100 - 1 / 200 ) ), x, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1 )
qqplot( qnorm( ( 1:100 / 100 - 1 / 200 ) ), y, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1 )
qqplot( qnorm( ( 1:100 / 100 - 1 / 200 ) ), z, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1 )

# n = 10
x = runif( n = 10, min = 0, max = 1 )
y = rexp( n = 10, rate = 1 )
z = rnorm( n = 10, mean = 0, sd = 1 )

qqplot( qnorm( ( 1:10 / 10 - 1 / 20 ) ), x, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1 )
qqplot( qnorm( ( 1:10 / 10 - 1 / 20 ) ), y, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1 )
qqplot( qnorm( ( 1:10 / 10 - 1 / 20 ) ), z, col = "red", xlab = "quantile teorico N( 0,1 )",
        ylab = "quantile empirico", asp = 1 )



# Test di normalita’ univariata ---------------------------------------------------------------

# Il test di Shapiro-Wilk e' un test per la verifica dell'hp di normalita' Venne introdotto nel
# 1965 da Samuel Shapiro e Martin Wilk.  La verifica della normalita' avviene confrontando due
# stimatori alternativi della varianza s2 uno stimatore non parametrico basato sulla combinazione
# lineare ottimale della statistica d'ordine di una variabile aleatoria normale al numeratore, e
# il consueto stimatore parametrico, ossia la varianza campionaria, al denominatore.

# La statistica W che se ne ricava pue' assumere valori da 0 a 1.  Qualora il valore della
# statistica W sia troppo piccolo, il test rifiuta l'ipotesi nulla che i valori campionari siano
# distribuiti come una variabile casuale normale.

# H0: i valori x_i provengono da una Normale H1: i valori x_i provengono da un'altra ditribuzione

x = rnorm( n = 1000, mean = 0, sd = 1 )
y = rnorm( n = 1000, mean = 2, sd = 5 )
z = rexp( n = 1000, 0.5 )

shapiro.test( x )
shapiro.test( y )
shapiro.test( z )
