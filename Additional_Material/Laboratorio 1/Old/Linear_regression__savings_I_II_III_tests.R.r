# LAb1 parte 1

library( faraway )

data( savings )
dim(savings)

# economic dataset on 50 different countries.
# These data are averages over 1960-1970 ( to remove business cycle or other short-term
#         fluctuations ).
#  - dpi is per-capita disposable income in U.S. dollars; ( reddito pro-capite in dollari, al
#         netto delle tasse )
#  - ddpi is the percent rate of change in per capita disposable income; ( potere d'acquisto -
#         indice economico aggregato, espresso in % )
#  - sr is aggregate personal saving divided by disposable income. ( risparmio personale diviso
#         per il reddito disponibile )
# The percentage population under 15 ( pop15 ) and over 75 ( pop75 ) are also recorded.
# The data come from Belsley, Kuh, and Welsch ( 1980 ).

# Overview delle prime righe
head(savings)


# Test di significatività di tutto il modello -------------------------------------------------

# Modello lineare completo

x11()
pairs( savings[ , c('sr','pop15','pop75','dpi','ddpi')], pch = 16)

g = lm( sr ~ pop15 + pop75 + dpi + ddpi, data = savings )
summary( g )
gs=summary(g)

names( g )

g$call
g$coefficients  #beta cappuccio
g$fitted.values

x=model.matrix(g)
y_hat_man = x %*% g$coefficients

g$residuals

g$rank  #p

help( vcov )
vcov( g )
summary( g )

# We can see directly the result of the test of whether any of the predictors have significance in the model.
# In other words, whether beta1, beta2, beta3 or beta4 is different from 0.
# Since the p-value of F-statistic is so small, this null hypothesis is rejected.

# in altre parole, dal test F vedo che ho evidenza per rifiutare l'hp nulla che sia equivalente il predire l'output
# semplicemente con la media anzichè usare dei predittori

# R2 non particolarmente elevato, R2adj peggio ancora

# Calcolo manuale del test F sulla significatività del modello:

# SStot = Sum ( yi-ybar )^2
sum( ( savings$sr-mean( savings$sr ) )^2 )
# SSres = Sum ( residuals^2 )
sum( g$res^2 )

( ( 983.63 - 650.71 )/4 )/( 650.706/45 ) # p = 5 ( p - 1 = r = 4 ),  n - p = 45

1 - pf( 5.7558, 4, 45 )  # p-value da una distribuz F di param opportuni




# Test di significatività del singolo predittore ----------------------------------------------

g2 = lm( sr ~ pop75 + dpi + ddpi, data = savings )
summary( g2 )

# Test sui singoli coefficienti

# a ) come F test su modelli annidati che si distinguono per un solo predittore
#               ( NON è il test F dell'output!!! )

sum( g2$residuals^2 )
sum( g$res^2 )

( ( 797.72-650.71 ) / 1 )/( 650.71 / 45 )

1 - pf( 10.167, 1, 45 )  # p-value test F

# b ) come test t sul singolo predittore
# We can relate this to the t-based test and p-value by
sqrt( 10.167 )

2 * ( 1-pt( 3.1886, 45 ) )



# Confronto tra modelli annidati --------------------------------------------------------------

anova( g2, g )

# il p-value è esattamente quello del test, perchè qui i 2 mod differiscono
# solo per il predittore in questione, ed è significativo in quanto il predittore rimosso
# lo era.


## EX ##

# Riprovare ad effettuare gli stessi test, in automatico e manualmente, per tutti i predittori del modello

# Effettuare un test sul predittore beta4 del primo modello ( relativo a ddpi ) per verificare
#		H0: beta4 = 0.35  vs  H1: beta4 > 0.35

g3 = lm( sr ~ pop15 + ddpi, data = savings )
summary(g3)


anova(g3, g)

