
##########################################################
#   Jackkinfe Residuals, Outliers & Cook's Distance   #
##########################################################

library( faraway )

# An outlier is a point that does not fit the current model. We need to be aware of such exceptions.
# An outlier test is useful because it enables us to distinguish between truly unusual points and
# residuals which are large but not exceptional.

# When an outlier is present, we exclude that point (say, the i-th point) and recompute the estimates
# to get beta.hat( i ) and sigma.hat^2( i ), where ( i ) denotes that the i-th case has been excluded.
# Hence
#       y.hat( i ) = X_i'beta.hat( i )
# If y.hat( i ) - y_i is large, then i is an outlier.

# La precedente procedura è chiamata Jackknife, o Leave-one-out, ed ha validità generale in statistica
# come metodo per determinare la sensitività di una stima rispetto ai particolari dati che l'hanno
# determinata.

# Per testare la presenza di outlier, a partire dalle quantità appena mostrate si definiscono i
# RESIDUI JACKKNIFE

#      t_i = epsilon.hat_i/( sigma.hat( i ) * sqrt( 1-h_ii ) )

# Since t_i is distributed according to a t with ( n-p-1 ) df, we can calculate a p-value to test
# whether point i-th is an outlier.
# However, we would probably want to test all cases so we must adjust the level of the test
# accordingly ( for example, using Bonferroni correction )

data( savings )

g = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings )


# Dall'help di `rstudent.lm` si legge che la funzione `rstudent` "gives the Studentized residuals
# respectively, i.e. residuals with unit variance, using a leave-one-out measure of the error
# variance respectively."
#
# Quindi `rstudent` fornisce i residui GIA' manipolati con la procedura jackknife.
#
jack = rstudent( g )

dev.new()
plot( jack, ylab = "jackknife Residuals", main = "Jackknife Residuals", pch = 16 )

jack [ abs( jack ) == max( abs( jack ) ) ]

# The largest residual of 2.85 is pretty big for a standard normal scale but is it an outlier?
# Compute the Bonferroni critical value:
qt( .05/( 50 * 2 ), 44 )

# Cosa si può concludere?

countries = rownames( savings )

# An influential point is one whose removal from the dataset would cause a large
#  change in the fit.
# An influential point may or may not be an outlier and may or may not have large
#  leverage but it will tend to have at least one of those two properties.

# Some measures of influence
#  1. Change in the coefficients:  beta.hat - beta.hat( i )
#  2. Change in the fit:  x'( beta.hat - beta.hat( i ) ) = y.hat - y.hat( i )
# These are hard to judge in the sense that the scale varies between datasets.

# A popular alternative are the Cook Statistics:

#  D_i = ( beta.hat - beta.hat( i ) )X'X( beta.hat - beta.hat( i ) )/( p * sigma.hat^2 )
#    = ( 1/p ) * r_i^2 * ( h_ii/( 1-h_ii ) )

cook = cooks.distance( g )

dev.new()
plot( cook, ylab = "Cooks distances", pch = 16 )
identify( 1:50, cook, countries )

# Proviamo a fittare il modello senza questi
gl = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = ( cook < max( cook ) ) )
summary( gl )

# e confrontiamo con il fit precedente

summary( g )

# The coefficient for ddpi changed by about 50%.
# We don't like our estimates to be so sensitive to the presence of just one country.

# Un'alternativa (DFBETA, cambiamento della pendenza ottenuto rimuovendo un'osservazione per volta)
ginf = lm.influence( g )
names( ginf )

dev.new()
plot( ginf$coef [ , 2 ] , ginf$coef [ , 3 ] , xlab = "Change in pop15 coef",
      ylab = "Change in pop75 coef", pch = 16 )

identify( ginf$coef [ , 2 ] , ginf$coef [ , 3 ] , countries )

# Il giappone emerge come osservazione sicuramente molto influente.
#
# Proviamo a rimuovelrlo:
gj = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = ( countries != "Japan" ) )

summary( gj )
