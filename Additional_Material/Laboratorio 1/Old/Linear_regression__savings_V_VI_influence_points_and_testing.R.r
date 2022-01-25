
rm( list = ls() )

library( faraway )

# Leverages and residuals ---------------------------------------------------------------------

data( savings )

g = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings )

dev.new()
# Residui non standardizzati (o studentizzati)
plot( g$res, ylab = "Residuals", main = "Plot of residuals" )

sort( g$res )
sort( g$res ) [ c( 1, 50 ) ]


countries = row.names( savings )
identify( 1:50, g$res, countries ) # cliccare 2 volte sui punti a cui si vuole apporre il label

# "identify" è una funzione utile per l'individuazione da grafico dei punti influenti
# chiede in ingresso ascissa e ordinata del plot, insieme alle etichette dei dati

# Normalmente i residui vengono rappresentati rispetto ai corrispondenti valori di y, oppure rispetto
# ai singoli predittori. Questo serve anche per verificare le ipotesi della regressione lineare:
# omoschedasticità e gaussianità dei residui (vedi oltre nello script).
# La rappresentazione che ha in ascissa l'indice dell'osservazione non
# è molto informativa (a meno che non vogliamo studiare la distribuzione dei residui rispetto alla
# procedura di raccolta dei dati).

dev.new()
plot( savings$sr, g$residuals, pch = 16, col = 'black', xlab = 'sr', ylab = 'Residuals' )

dev.new()
par( mfrow = c( 2, 2 ))
plot( savings$pop15, g$residuals, pch = 16, col = 'black', xlab = 'Pop15', ylab = 'Residuals',
      main = 'Residuals vs. Pop15' )
plot( savings$pop15, g$residuals, pch = 16, col = 'black', xlab = 'Pop75', ylab = 'Residuals',
      main = 'Residuals vs. Pop 75' )
plot( savings$pop15, g$residuals, pch = 16, col = 'black', xlab = 'dpi', ylab = 'Residuals',
      main = 'Residuals vs. dpi' )
plot( savings$pop15, g$residuals, pch = 16, col = 'black', xlab = 'ddpi', ylab = 'Residuals',
      main = 'Residuals vs. ddpi')

# E' anche utile confrontare il tutto con la distribuzione originaria dei dati.
dev.new()
pairs( savings[ , c( 'sr', 'pop15', 'pop75', 'dpi', 'ddpi' ) ], pch = 16 )

# E con le informazioni di GOF (goodness of fit) del modello
summary( g )

### Leverages

X = model.matrix( g )
X
lev = hat( X )
lev
# oppure (meglio)
lev = hatvalues( g )
lev

# Ricordatevi: la traccia della diagonale della matrice "hat" (H) è pari al rango della matrice X, cioè,
# assumendo che le covariate non siano linearmente dipendenti (e che, chiaramente, p < n), p + 1.
# Questa è la dimensione del sottospazio vettoriale generato dalle combinazioni lineari delle colonne
# di X, e l'interpretazione geometrica della regressione lineare ordinaria (OLS) indica che H agisce
# su y (vettore delle osservaazioni) proiettandolo su tale sottospazio vettoriale (dando luogo alle
# \hat{y}).
sum(lev)

dev.new()
plot( savings$sr, lev, ylab = "Leverages", main = "Plot of Leverages", pch = 16, col = 'black' )
# rule of thumb: un punto è un leverage se hat( x )_i > 2 * ( p + 1 ) / n
abline( h = 2 * 5/50, lty = 2, col = 'red' )

watchout_points = lev[ which( lev > 2 * 5 / 50  ) ]
watchout_ids = seq_along( lev )[ which( lev > 2 * 5 / 50 ) ]

points( savings$sr[ watchout_ids ], watchout_points, col = 'red', pch = 16 )

sum( lev )      # verifica: sum_i hat( x )_i = p + 1

lev [ lev >  2 * 5 / 50 ]
sum(lev [ lev >  2 * 5 / 50 ])

# proviamo a fittare il modello senza questi
gl = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = ( lev < 0.2 ) )
summary( gl )
summary( g )

# guardiamo la variazione relativa percentuale dovuta a questi soli 4 punti
abs( ( g$coefficients - gl$coefficients ) / g$coefficients )

dev.new()
colors = rep( 'black', nrow( savings ) )
colors[ watchout_ids ] = c('red', 'blue', 'green', 'orange')

pairs( savings[ , c( 'sr', 'pop15', 'pop75', 'dpi', 'ddpi' ) ], pch = 16, col = colors,
       cex = 1 + 0.5 * as.numeric( colors != 'black' )    )


### Studentized Residuals

gs = summary( g )

gs$sigma

#  r_i = eps_hat_i / sqrt( S^2 * ( 1 - h_ii ) ), residui studentizzati
stud = g$residuals / ( gs$sigma * sqrt( 1 - lev ) )

dev.new()
par( mfrow = c( 1, 2 ) )
plot( savings$sr, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( savings$sr[watchout_ids], stud[watchout_ids], col = 'red', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )

plot( savings$sr, g$res, ylab = "Residuals", main = "Residuals", ylim = range( c( g$res, stud ) ), pch = 16 )
points( savings$sr[watchout_ids], g$res[watchout_ids], col = 'red', pch = 16 )


# Cook's Distances

# An influential point may either be an outlier or have large leverage, or both, but it will tend
# to have at least one of those properties.
# Cook's distance is a commonly used influence measure that combines these two properties.
# It can be expressed as:
#
#  C_i = ( r_i^2 / p * ( h_11 / ( 1 - h_ii ) ) )
#
#  dove
#
#  r_i = eps_hat_i / sqrt( S^2 * ( 1 - h_ii ) ), residui studentizzati
#
# Typically, points with Ci greater than 1 are classified as being influential.

Cdist = cooks.distance( g )
dev.new()
par( mfrow = c( 1, 2 ) )
plot( Cdist, pch = 16, xlab = 'Index', ylab = 'Cooks Distance', main = 'Cooks Distance' )
# Identifichiamo i leverage points
plot( lev, Cdist, pch = 16, xlab = 'Leverages', ylab = 'Cooks Distance', main = 'Cooks Distance vs. Leverages' )
points( lev[ watchout_ids ], Cdist[ watchout_ids ], col = colors[ watchout_ids ], pch = 16 )


library( car )
# Influence Plot
# Rappresenta i residui studentizzati contro i leverages, e li marca con un cerchio di dimensioni
# proporzionali alla distanza di Cook
dev.new()
influencePlot( g, id.method = "identify", main = "Influence Plot",
               sub = "Circle size is proportial to Cook's Distance" )

# Uno strumento automatico molto comodo è:
# Influence.measures produces a class "infl" object tabular display showing the DFBETAS for each
# model variable, DFFITS, covariance ratios, Cook's distances and the diagonal elements of the hat
# matrix. Cases which are influential with respect to any of these measures are marked with an
# asterisk."
influence.measures( g )

# Extra: Added Variable Plots

# Added variable plots (partial regression plots)
#
# These functions construct added-variable (also called partial-regression) plots for linear and
# generalized linear models.
#
# Dato un set di predittori e una risposta, l'AV plot permette di visualizzare l'effetto
# della parte di un predittore che non viene spiegata da tutti gli altri sulla parte di risposta
# che non viene spiegata da tutti gli altri predittori.
#
# Graficamente, immaginando di fissare l'attenzione su X1, si tratta di plottare il residuo di
# Y ~ X2 + X3 + ... + Xp contro i residui di X1 ~ X2 + X3 + ... + Xp.
#
# La regressione dei residui fornisce il coefficiente di regressione di X1 controllando tutte le altre
# variabili, cioè il suo effetto `netto` sulla risposta Y.
#
# Tali grafici possono anche essere usati per diagnosticare punti influenti e nonlinearità.

avPlots( g )
# Si confrontino tali grafici con
summary( g )



# Verifica delle ipotesi ----------------------------------------------------------------------

g = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings )

# Verifica dell'hp di omoschedasticità dei residui

# Plot dei residui ( epsilon.hat ) vs i valori fittati ( y.hat )
plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variabilità sembra sufficientemente uniforme

# Verifica dell'eventuale presenza di nonlinearità

# Plot dei residui ( epsilon.hat ) vs i singoli predittori ( x_i )
dev.new()
par( mfrow = c( 2, 2 ) )
plot( savings$pop15, g$res, xlab = "Population under 15", ylab = "Residuals",
      main = "Residuals vs pop15", pch = 16 )
plot( savings$pop75, g$res, xlab = "Population over 75", ylab = "Residuals",
      main = "Residuals vs pop75", pch = 16 )
plot( savings$dpi, g$res, xlab = "dpi", ylab = "Residuals", main = "Residuals vs dpi", pch = 16 )
plot( savings$ddpi, g$res, xlab = "ddpi", ylab = "Residuals", main = "Residuals vs ddpi", pch = 16 )


# Verifica dell'hp di Normalità
dev.new()
par( mfrow = c( 2, 2 ) )

# QQ plot
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )
# Andamento adeguatamente rettilineo, l'ipotesi è verificata

# La funzione `rstandard` calcola automaticamente i residui studentizzati,
# (mentre la funzione rstudent calcola i residui jackknife), attenzione perché i nomi sono
# contro-intuitivi!
qqnorm( rstudent( g ), ylab = "Studentized residuals", pch = 16 )
abline( 0, 1 )

# altri strumenti utili...
hist( g$res, 10, probability = TRUE, col = 'lavender', main = 'residuals'  )
boxplot( g$res, main = "Boxplot of savings residuals", pch = 16, col = 'lavender' )


# Shapiro-Wilk normality test
# p-val molto alto = > NON rifiuto H0: dati Gaussiani
shapiro.test( g$res )

# Esempi di violazione dell’ipotesi di omoschedasticità ---------------------------------------
par( mfrow = c( 3, 3 ) )

# Omoschedasticità
for( i in 1:9 )
  plot( 1:50, rnorm( 50 ), pch = 16 )

# Eteroschedasticità marcata (varianza funzione lineare di x)
for( i in 1:9 )
  plot( 1:50, ( 1:50 ) * rnorm( 50 ), pch = 16 )

# Eteroschedasticità blanda (varianza funzione sublineare di x)
for( i in 1:9 )
  plot( 1:50, sqrt( ( 1:50 ) ) * rnorm( 50 ), ylim = c( -50, 50 ), pch = 16 )

# Non linearità (varianza funzione nonlineare di x)
for( i in 1:9 )
  plot( 1:50, cos( ( 1:50 ) * pi/25 ) + rnorm( 50 ), pch = 16 )



# Esempi di violazione dell’ipotesi di normalità ----------------------------------------------

dev.new()
par( mfrow = c( 3, 3 ) )

# Normali
for( i in 1:9 )
{
  D = rnorm( 50 )

  qqnorm( D, pch = 16 )

  qqline( D, lty = 2, lwd = 2, col = 'red' )
}

dev.new()
par( mfrow = c(3,3) )
# Esponenziali
for( i in 1:9 )
{
  D = rexp( 50 )

  qqnorm( D, pch = 16 )

  qqline( D, lty = 2, lwd = 2, col = 'red' )
}

dev.new()
par( mfrow = c(3,3) )
# Log-normali
for( i in 1:9 )
{
  D = exp( rnorm( 50 ) )

  qqnorm( D, pch = 16 )

  qqline( D, lty = 2, lwd = 2, col = 'red' )
}

# Cauchy
for( i in 1:9 )
{
  D = rcauchy( 50 )

  qqnorm( D, pch = 16 )

  qqline( D, lty = 2, lwd = 2, col = 'red' )
}

dev.new()
par( mfrow = c(3,3) )
# Uniforme
for( i in 1:9 )
{
  D = runif( 50 )

  qqnorm( D, pch = 16 )

  qqline( D, lty = 2, lwd = 2, col = 'red' )
}

