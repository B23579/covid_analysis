
# Probabilita di copertura IC -----------------------------------------------------------------

# Si generi un dataset di 100 elementi da una normale di media 4 e varianza 2
set.seed(1200)
dati.sim = rnorm(100,4,sqrt(2))

# Eseguiamo il test H_0: mu = 4 vs H_1: mu != 4
# Caso varianza nota
alpha = 0.05
n= length(dati.sim)
sigma = sqrt(2)

media = mean(dati.sim)
media

IC.noto = c( inf = media - sigma / sqrt(n) * qnorm ( 1 - alpha/2) , center = media , sup = media + sigma / sqrt(n) * qnorm ( 1 - alpha/2))
IC.noto

# Caso varianza incognita
alpha = 0.05
n= length(dati.sim)
sigma = sd(dati.sim)

media = mean(dati.sim)

IC.inc = c( inf = media - sigma / sqrt( n ) * qnorm( 1 - alpha / 2 ), center = media, sup = media  +  sigma / sqrt( n ) * qnorm( 1 - alpha / 2 ) )
IC.inc

# Confronto tra IC
rbind(IC.noto,IC.inc)
IC.noto[3]-IC.noto[1]
IC.inc[3]-IC.inc[1]

# L'IC costruito con la var nota (quantili della normale) e' piu' stretto di quello costruito
# con la var incognita (quantili della t)

# Valutiamo la probabilita' di copertura degli intervalli
N = 100  # Numero di intervalli
n = 1000  # Numero di campioni della Normale
alpha = 0.05  # Livello di confidenza

mat.IC.z = matrix(NA,N,3)
mat.IC.z
mat.IC.t = matrix(NA,N,3)
sigma = sqrt(2)
for (i in 1:N){
  sample = rnorm( n, 4, sqrt( 2 ) )
  mat.IC.z[ i, 1 ] = mean( sample ) - sigma / sqrt( n ) * qnorm( 1 - alpha / 2 )
  mat.IC.z[ i, 2 ] = mean( sample )
  mat.IC.z[ i, 3 ] = mean( sample )  +  sigma / sqrt( n ) * qnorm( 1 - alpha / 2 )
  mat.IC.t[ i, 1 ] = mean( sample ) - sd( sample ) / sqrt( n ) * qt( 1 - alpha / 2, n - 1 )
  mat.IC.t[ i, 2 ] = mean( sample )
  mat.IC.t[ i, 3 ] = mean( sample )  +  sd( sample ) / sqrt( n ) * qt( 1 - alpha / 2, n - 1 )
}


par( mfrow = c( 1, 2 ) )
plot( range( mat.IC.z ), c( 0.5, N  +  0.5 ), pch = "", xlab = "", ylab = "IC ( z )", main = "Probabilita' di copertura IC ( z )" )
for ( k in 1:N ) {
  lines( c( mat.IC.z[ k, 1 ], mat.IC.z[ k, 3 ] ), c( k, k ) )
  points( mat.IC.z[ k, 1 ], k, pch = 19 )  #95
  points( mat.IC.z[ k, 2 ], k, pch = 16, col = "green" )
  points( mat.IC.z[ k, 3 ], k, pch = 19 )
}
abline( v = 4, col = "red" )

# dev.new()
plot( range( mat.IC.t ), c( 0.5, N  +  0.5 ), pch = "", xlab = "", ylab = "IC ( t )", main = "Probabilita' di copertura IC ( t )" )
for ( k in 1:N ) {
  lines( c( mat.IC.t[ k, 1 ], mat.IC.t[ k, 3 ] ), c( k, k ) )
  points( mat.IC.t[ k, 1 ], k, pch = 19 )  #95
  points( mat.IC.t[ k, 2 ], k, pch = 16, col = "green" )
  points( mat.IC.t[ k, 3 ], k, pch = 19 )
}
abline( v = 4, col = "red" )


test.cop.z = NULL
test.cop.t = NULL
for ( i in 1:N ) {
  test.cop.z[ i ] = 4 < mat.IC.z[ i, 3 ] & 4 > mat.IC.z[ i, 1 ]
  test.cop.t[ i ] = 4 < mat.IC.t[ i, 3 ] & 4 > mat.IC.t[ i, 1 ]
}
cop.z = as.numeric( test.cop.z )
cop.t = as.numeric( test.cop.t )
sum( cop.z ) / N
sum( cop.t ) / N


# Lunghezza media
l.z = NULL
l.t = NULL
for ( i in 1:N ) {
  l.z[ i ] = mat.IC.z[ i, 3 ] - mat.IC.z[ i, 1 ]
  l.t[ i ] = mat.IC.t[ i, 3 ] - mat.IC.t[ i, 1 ]
}
mean( l.z )
mean( l.t )  # maggiore

