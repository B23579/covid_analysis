
##################################
###   VARIABLE SELECTION   ###
##################################


library( faraway )


# Stepwise procedure --------------------------------------------------------------------------

data( state )

# Data on the 50 states - the variables are population estimate as of July 1, 1975;
# per capita:
#   - income ( 1974 );
#   - illiteracy ( 1970, percent of population );
#   - life expectancy in years ( 1969-71 );
#   - murder and non-negligent manslaughter rate per 100, 000 population ( 1976 );
#   - percent high-school graduates ( 1970 );
#   - mean number of days with min temperature 32 degrees ( 1931-1960 ) in capital or large city;
#   - land area in square miles.

# We will take life expectancy as the response and the remaining variables as
# predictors - a fix is # necessary to remove spaces in some of the variable names.

statedata = data.frame( state.x77, row.names = state.abb, check.names = T )

head( statedata )

g = lm( Life.Exp ~ ., data = statedata )
summary( g )

# Which predictors should be included - can you tell from the p-values?
# Looking at the coefficients, can you see what operation would be helpful?

# Does the murder rate decrease life expectancy - that's obvious a priori, but how should these
# results be interpreted?




# Manual backward selection method ------------------------------------------------------------

# At each step we remove the predictor with the largest p-value and (ideally) stop when we have only
# predictors with p-values below 0.05

help('update')
help('update.formula')

# remove Area
g1 = update( g, . ~ . - Area )
summary( g1 )

# remove Illiteracy
g2 = update( g1, . ~ . - Illiteracy )
summary( g2 )

# Remove Income
g3 = update( g2, . ~ . - Income )
summary( g3 )

# remove Population
g4 = update( g3, . ~ . - Population )
summary( g4 )

# The final removal of variable Population is a close call. We may want to
# consider including this variable if interpretation is aided. Notice that the
# R2 for the full model of 0.736 is reduced only slightly to 0.713 in the final
# model.

# Thus the removal of four predictors causes only a minor reduction in fit.
X = statedata [ , -4 ]
cor( X )
# Note the high positive correlation between Murder and Illiteracy!
dev.new()
image( as.matrix( cor( X ) ), main = 'Correlation of X' )

names( X )

# Mind spurious correlations!
# http://www.tylervigen.com/spurious-correlations
# http://guessthecorrelation.com


# Automatic backward method  ------------------------------------------------------------------

# At each step we remove the predictor with the largest p-value over 0.05:


g1 = lm( Life.Exp ~ ., data = statedata )

help( step )

# AIC = - 2 log( L ) + 2 K

# We can specify either backward or forward (or even both of them)
step( g1, direction = "both" )

# Criterion based procedures ------------------------------------------------------------------

# 1. AIC & BIC
# 2. R2 adjusted
# 3. PRESS
# 4. Mallow's Cp

# Importing data (we are not reading them)
data( state )

help( state.x77 )
statedata = data.frame( state.x77, row.names = state.abb, check.names = T )



# 1. AIC & BIC

g = lm( Life.Exp ~ ., data = statedata )

AIC( g ) # -2 * log( likelihood ) + 2k
BIC( g ) # -2 * log( likelihood ) + log(n)k

g1 = step( g, direction = "backward" )
step( g )


# 2. adjusted R2 criterion

library( leaps )
help( leaps )


# solo matrice dei predittori senza colonna di 1
x = model.matrix( g ) [ , -1 ]
y = statedata$Life

adjr = leaps( x, y, method = "adjr2" )
adjr

help( maxadjr )
maxadjr( adjr, 5 )

# We see that the Population, Frost, HS graduation and Murder model has the largest
# R2adj

# The best three predictor model is in eighth place but the intervening models are
# not attractive since they use more predictors than the best model

# Other possibilities
Cp = leaps( x, y, method = "Cp" )
R2 = leaps( x, y, method = "r2" )

# IMPORTANT REMARK:
# Remember that Variable selection methods are sensitive to outliers and
# influential points.


# 3. PRESS criterion ###

library( qpcR )

help( PRESS )

mod = lm( Life.Exp ~ ., data = statedata )
attach( statedata )

PRESS( mod, verbose = T )

x = 1:10
y = rnorm( 10, x, 0.1 )
mod = lm( y ~ x )

press = PRESS( mod )
names( press )

plot( press$residuals )


# 4. Mallow's Cp statistic criterion

g = leaps(  y = statedata[ , 4 ], x = statedata[ , -  4 ], method = 'Cp' )
# g = leaps( x, y, method = "Cp" ) # Mallow's Statistics
Cpplot( g )

g$which[ which.min( g$Cp ) , ]

# The models are denoted by indices for the predictors.
# The competition is between
# the "456" model i.e. the Frost, HS graduation and Murder model and the model also
# including Population. Both models are on or below the Cpp line, indicating good fits.




