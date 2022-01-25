
library( faraway )



# Confidence Intervals ------------------------------------------------------------------------

data( savings )

# sintassi per fittare modello completo ( tutti i predittori )
g = lm( sr ~ ., savings )
summary( g )



# Costruzione manuale di IC per i singoli parametri -------------------------------------------

# Individual 95% confidence intervals for the regression parameters of pop75:
alpha = 0.05

# param di qt sono il livello ( 1-alpha/2 ) e i df ( 45 ovvero n-p )
t_alpha = qt( 1-alpha/2, 45 )
beta_hat_pop75 = g$coefficients[3]
se_beta_hat_pop75 = summary(g)[[4]][3,2]

IC_pop75 = c( beta_hat_pop75 - t_alpha * se_beta_hat_pop75 , beta_hat_pop75 + t_alpha * se_beta_hat_pop75)
IC_pop75

# beta.hat +/- t * se( beta )
##c( -1.69-2.01 * 1.08, -1.69 + 2.01 * 1.08 )
# NB: IC contiene lo 0! dunque, volendolo sfruttare per eseguire il test, NON ci fornisce evidenza
# per rifiutare H0: beta_pop75 = 0 ( infatti coefficiente non significativo )


# similarly for parameter of ddpi:
alpha = 0.05
t_alpha2 = qt( 1-alpha/2, 45 )
beta_hat_ddpi = g$coefficients[5]
se_beta_hat_ddpi = summary(g)[[4]][5,2]

IC_ddpi = c( beta_hat_ddpi - t_alpha * se_beta_hat_ddpi , beta_hat_ddpi + t_alpha * se_beta_hat_ddpi)
IC_ddpi
##c( 0.41-2.01 * 0.196, 0.41 + 2.01 * 0.196 )
# NB: qui l'IC utilizzato per eseguire il test mi porterebbe a rifiutare H0: beta_ddpi = 0, dato che
# 0 non rientra tra i valori "plausibili" che avrebbero reso probabile l'osservare il campione
# effettivamente osservato.

# Notice that this confidence interval is pretty wide in the sense that the upper limit is about 50
# times larger than the lower limit.
# This means that we are not really that confident about what the exact effect of growth on savings
# really is.

# Confidence intervals often have a duality with two-sided hypothesis tests.
# A 95% confidence interval contains all the null hypotheses that would not be rejected at the 5%
# level.
# Thus the interval for pop75 contains zero which indicates that the null hypothesis
#  H0 : beta_pop75 = 0 would not be rejected at the 5% level.

# IMPORTANT REMARK:
# We can see from the output above that the p-value is 12.5% - greater than 5% - confirming this point.
# In contrast, we see that the interval for ddpi does not contain zero and so the null hypothesis is
# rejected for this regression parameter.


# Costruzione di regioni di confidenza --------------------------------------------------------

# Joint 95% confidence region for parameters pop15 e pop75.

library( ellipse )  # load the library for drawing confidence ellipses

help( ellipse )

g

dev.new()
plot( ellipse( g, c( 2, 3 ) ), type = "l", xlim = c( -1, 0 ) )

# add the origin and the point of the estimates:
# vettore che stiamo testando nell'hp nulla
points( 0, 0 )
# le stime dei 2 param sono il centro dell'ellisse
points( g$coef [ 2 ] , g$coef [ 3 ] , pch = 18 )

# NOTA: la regione di confidenza non copre il valore del vettore beta proposto dall'hp nulla
#    = > RIFIUTO H0 secondo cui entrambi sono nulli = > ALMENO UNO dei 2 coeff non è uguale a zero

# Now we mark the one way confidence intervals on the plot for reference:
abline( v = c( -0.461-2.01 * 0.145, -0.461 + 2.01 * 0.145 ), lty = 2 )
abline( h = c( -1.69-2.01 * 1.08, -1.69 + 2.01 * 1.08 ), lty = 2 )


# COMMENTO:
# Le linee tratteggiate rappresentano i test "uno alla volta" eseguiti sui singoli coeff, di livello 95%
# Il fatto che l'ellisse non copra lo zero, ci porta a rifiutare l'hp nulla che la coppia di param
# sia nulla ( e che quindi le corrispondenti variabili siano NON significative )
# L'origine cade all'interno dell'IC si pop75 e non in quello di pop15, infatti il primo non abbiamo
# motivo per ritenere che sia significativam diverso da 0, mentre il secondo si

# Ci potrebbero essere casi in cui una regione porta a rifiutare e l'altra no
points( -0.22, 0.7, col = "red", lwd = 2 )
points( -0.71, 0, col = "blue", lwd = 2 )
# quindi preferire sempre la regione congiunta, ove possibile, perché sta tenendo conto della
# correlazione tra i coeff.

# correlazione tra v.a. pop15 e pop75
cor( savings$pop15, savings$pop75 )
