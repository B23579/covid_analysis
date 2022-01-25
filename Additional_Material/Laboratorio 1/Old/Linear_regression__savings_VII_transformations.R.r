
##############################
###   TRASFORMAZIONI   ###
##############################


# Box Cox Transformation ----------------------------------------------------------------------

library( faraway )
library( MASS )

data( gala )

help( gala )

# Description of the dataset:
# There are 30 Galapagos islands ( n ) and 7 variables ( r ) in the dataset.
# The relationship between the number of plant species and several geographic
# variables is of interest.
# M. P. Johnson and P. H. Raven ( 1973 ) "Species number and endemism: The Galapagos Archipelago revisited" Science, 179, 893-895

str( gala )
head( gala )

# - Species: the number of plant species found on the island
# - Endemics: the number of endemic species
# - Area: the area of the island ( km2 )
# - Elevation: the highest elevation of the island ( m )
# - Nearest: the distance from the nearest island ( km )
# - Scruz: the distance from Santa Cruz island ( km )
# - Adjacent: the area of the adjacent island ( square km )

g = lm( Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala )
summary( g )

dev.new()
plot( gala$Species, g$residuals, pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )

labels = rownames( gala )
identify( gala$Species, g$residuals, labels )

# Studentized Residuals

X = model.matrix( g )
lev = hat( X )

gs = summary( g )
gs$sigma
stud = g$residuals / ( gs$sigma * sqrt( 1 - lev ) )

dev.new()
plot( gala$Species, stud, pch = 16 )
abline( h = 0, col = 'blue', lty = 2, lwd = 2 )
abline( h = c(-2, 2), col = 'orange', lty = 2 )

dev.new()
plot( stud, pch = 16 )


# Diagnostica dei residui
dev.new()
par( mfrow = c( 1, 2 ) )
qqnorm( g$residuals, pch = 16, main = 'Residui' )
qqline( g$residuals, lwd = 2, lty = 2, col = 'red' )

shapiro.test( g$residuals )

qqnorm( stud, pch = 16, main = 'Residui studentizzati' )
qqline( stud, lwd = 2, lty = 2, col = 'red' )

shapiro.test( stud )


# Cerchiamo di applicare le trasformazioni di Box-Cox
#
# Bisogna trovare il valore di lambda tale che:
#
# ( y^lambda - 1 ) / lambda         per lambda != 0
#
# log( y )                          per lambda = 0
#
# sia il più possibile gaussiano.
# Ciò viene fatto trovando il lambda che massimizza la likelihood.

dev.new()
b1 = boxcox( g, plotit = T )
b2 = boxcox( g, lambda = seq( 0.0, 1.0, by = 0.05 ), plotit = T )

# We see that perhaps a cube-root transformation might be best here.
# A square root is also a possibility as this falls just within the
# confidence intervals.
# For sure, there is a strong need to transform.

g_trans = lm( I( Species^( 1/3 ) ) ~ Area + Elevation + Nearest + Scruz + Adjacent, gala )
summary( g_trans )
summary( g )

dev.new()
plot( g_trans$residuals, pch = 16 )
abline( h = 0, lwd = 2, col = 'red', lty = 2 )

dev.new()
plot( gala$Species^(1/3), rstandard( g_trans ), pch = 16, ylim = c(-2.5, 2.5) )
abline( h = 0, lwd = 2, col = 'red', lty = 2 )
abline( h = c(-2, 2), lwd = 1, col = 'orange', lty = 2 )

# Guardiamo il qqnorm
dev.new()
qqnorm( g_trans$residuals, pch = 16, main = 'Residui - trasformazione della risposta' )
qqline( as.numeric(g_trans$residuals), lwd = 1, col = 'red', lty = 2 )
shapiro.test( g_trans$residuals )


# Altro esempio -------------------------------------------------------------------------------

data( savings )

g = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings )
summary( g )

plot( g$residuals, pch = 16 )

qqnorm( g$residuals, pch = 16 )
qqline( g$residuals, lwd = 2, lty = 2, col = 'red' )

shapiro.test( g$residuals )

b1 = boxcox( g, plotit = T )
# considero solo valori di lambda "interpretabili"
b2 = boxcox( g, plotit = T, lambda = seq( 0.5, 1.5, by = 0.1 ) )

# The confidence interval for lambda runs from about 0.6 to about 1.4.
# We can see that there is no good reason to transform.


# Trasformazione dei predittori ---------------------------------------------------------------

# L'unica cosa che non ci convinceva troppo del modello precedente era l'R2 non troppo alto
# Vediamo se inserendo altri predittori ( trasformazioni di quelli già presenti ) le cose migliorano

# prendiamo i 2 significativi
summary( lm( sr ~ pop15, savings ) )  # R2 alto
summary( lm( sr ~ ddpi, savings ) )  # R2 basso => proviamo a incrementare

summary( lm( sr ~ ddpi + I( ddpi^2 ), savings ) )
# R2 maggiore, entrambi i predittori significativi
summary( lm( sr ~ ddpi + I( ddpi^2 ) + I( ddpi^3 ), savings ) )
# R2 simile al preced, overfitting rende inutili i predittori

g.new = lm( sr ~ pop15 + ddpi + I( ddpi^2 ), savings )
summary( g.new )

# risultato non particolarmente significativo, trattenimo modello precedente

