
####################################################
#  Regressione Lineare Semplice - PREDICTION   #
####################################################


# Si vuole stabilire se sussiste una relazione tra l'altezza di un gruppo di
# piante di pomodoro e il peso medio dei frutti raccolti ( in g )



# Import dei dati -----------------------------------------------------------------------------

peso   = c( 60, 65, 72, 74, 77, 81, 85, 90 )
altezza = c( 160, 162, 180, 175, 186, 172, 177, 184 )

# Model fitting -------------------------------------------------------------------------------

mod = lm( peso ~ altezza )
summary( mod )

# 1) informazioni sul modello utilizzato e sui residui
# 2) informazioni sul fitting del modello
# 3) informazioni su R2, sigma e ANOVA


# IC edi previsione per la media della risposta ------------------------------------------------

# definisce una griglia di valori all'interno dell'intervallo di definizione dei dati ( fuori la
# predizione potrebbe non essere affidabile )
grid = seq( min( altezza ), max( altezza ), 2 )

# calcolo valori predetti con relativi errori standard
# il secondo argomento DEVE essere un data frame, le cui colonne (variabili) devono avere lo stesso
# nome dei predittori del modello
y.pred = predict( mod, data.frame( altezza = grid ), se = T )

names( y.pred )

# sono i valori predetti ( y.hat_0 = x_0 * beta.hat ) in corrispondenza della griglia di nuove osservazioni
y.pred$fit
# standard error -> qui solo sqrt( x_0'X'Xx_0 ) perchè sono per la media
y.pred$se
# n - p = 8 - 2 = 6
y.pred$df

tc   = qt( 0.975, length( altezza ) - 2 )
y   = y.pred$fit
y.sup = y.pred$fit + tc * y.pred$se
y.inf = y.pred$fit - tc * y.pred$se

dev.new()
matplot( grid, cbind( y, y.inf, y.sup ), lty = c( 1, 2, 2 ), col = c( 1, 'blue', 'blue' ), type = "l",
         xlab = "altezza", ylab = "peso", main = 'IC per la media della risposta' )
points( altezza, peso, col = "black", pch = 16 )


# Intervallo di previsione per una nuova osservazione -----------------------------------------

y.pred2 = predict( mod, data.frame( altezza = grid ), interval = "prediction" )
# fornisce direttamente gli estremi inf e sup, che prima abbiamo costruito a mano (in un altro caso)
y.pred2

dev.new()
matplot( grid, y.pred2, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l",
         xlab = "altezza", ylab = "peso", main = 'IP per singole osservazioni' )
points( altezza, peso, col = "blue", pch = 16 )



# A confronto ---------------------------------------------------------------------------------

y.pred = predict( mod, data.frame( altezza = grid ), interval = "confidence" )

dev.new()
matplot( grid, y.pred2, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l", xlab = "altezza",
         ylab = "peso", main = "IC per la media e IP per singole osservazioni" )
lines( grid, y.pred [ , 2 ] , col = "blue", lty = 2, xlab = "altezza", ylab = "peso" )
lines( grid, y.pred [ , 3 ] , col = "blue", lty = 2, xlab = "altezza", ylab = "peso" )
points( altezza, peso, col = "black", pch = 16 )
