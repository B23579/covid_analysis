---
title: "Laboratorio con `R` - 1"
subtitle: "Metodi e Modelli per l'Inferenza Statistica - Ing. Matematica - a.a. 2018-19"
author: 
date: 17/05/2018
output:
pdf_document: yes
number_sections: yes
toc: no
urlcolor: blue
---

## -----------------------------------------------------------------------------------
##0. Required packages
## -----------------------------------------------------------------------------------

```{r load_packages, eval = TRUE, collapse = TRUE }
library( car )
library( ellipse )
library( faraway )
library( leaps )
library( qpcR )
```

  
## -----------------------------------------------------------------------------------
##1. Linear regression and tests for coefficients significance.
## -----------------------------------------------------------------------------------

__1.a__ Upload `faraway` library and the dataset `savings`, an economic dataset on 50 different countries.
These data are averages over 1960-1970 ( to remove business cycle or other short-term fluctuations ).
The recorded covariates are:

* __sr__ is aggregate personal saving divided by disposable income ( risparmio personale diviso per il reddito disponibile ).
* __pop15__ is the percentage population under 15.
* __pop75__ is the percentage population under 75.
* __dpi__ is per-capita disposable income in U.S. dollars ( reddito pro-capite in dollari, al netto delle tasse ).
* __ddpi__ is the rate [percentage] of change in per capita disposable income ( potere d'acquisto - indice economico aggregato, espresso in % ).

__solution__
```{r data_import, eval = TRUE, collapse = TRUE }
data( savings )

# Dimensioni
dim( savings )

# Overview delle prime righe
head( savings )
```

__1.b__ Visualize the data and try to fit a complete linear model, in which `sr` is the outcome of interest. Explore the output of the model. 

__solution__
For visualizing the data, we can plot the pairs.

```{r pairs, eval = TRUE, collapse = TRUE }

pairs( savings[ , c( 'sr', 'pop15', 'pop75', 'dpi', 'ddpi' ) ], pch = 16 )
```

Secondly, we can fit the complete linear model. 

```{r linearmodel, eval = TRUE, collapse = TRUE }
g = lm( sr ~ pop15 + pop75 + dpi + ddpi, data = savings )
#g = lm( sr ~ ., savings )
summary( g )

gs = summary( g )

names( g )

g$call
g$coefficients #beta_hat
g$fitted.values
X = model.matrix(g)
y_hat_man = X %*% g$coefficients #beta_hat

g$residuals

g$rank #p

#help( vcov )
vcov( g )
```

In order to measure the goodness of fit of the model, we have to look at $R^2$ and $R^2_{adj}$. Both of them are low in this case.

Through the F-statistic, we are investigating whether there is at least one covariate's parameter among $\beta_1$, $\beta_2$, $\beta_3$ and $\beta_4$ is different from 0.
Since the p-value of F-statistic is so small (0.0007904), the null hypothesis is rejected and there is at least one covariate's parameter that is different from 0.

__1.c__ Try to compute F-test, manually. 

__solution__

```{r Fmanual, eval = TRUE, collapse = TRUE }
# SStot = Sum ( yi-ybar )^2
SS_tot = sum( ( savings$sr-mean( savings$sr ) )^2 )

# SSres = Sum ( residuals^2 )
SS_res = sum( g$res^2 )

p = g$rank # p = 5 
n = dim(savings)[1] # n = 50

f_test = ( ( SS_tot - SS_res )/(p-1) )/( SS_res/(n-p) )

1 - pf( f_test, p - 1, n - p )

```


__1.d__ Test the significance of the parameter $\beta_1$ (the parameter related to pop_15), manually.


__solution__

We want to test:
$$
H_0: \beta_1 = 0 \qquad vs \qquad H_!: \beta_1 \neq 0
$$
There are several ways to execute this test:

* __t-test__

We compute the test, whose output is shown in the R summary.

```{r tsummarytest, eval = TRUE, collapse = TRUE }
X = model.matrix( g )

sigma2 = (summary( g )$sigma)^2
#manually
sigma2 = sum( ( savings$sr - g$fitted.values )^2 ) / ( n -p )

se_beta_1 = summary( g )$coef[ 2, 2 ]
#manually
se_beta_1 = sqrt( sigma2 * diag( solve( t( X ) %*% X ) )[2] )

T.0 = abs( ( g$coefficients[ 2 ] - 0 )/ se_beta_1 ) 

2*( 1-pt( T.0, n-p ) )
```

* __F-test on nested model__

You fit a nested model (the complete model without the covariate in which you are interested) then you compute the residuals of the 2 models and execute the F-test. 

__REMARK__ it is NOT the F-test that you find in the summary!

$$
F_0 = \frac{\frac{SS_{res}(\text{complete_model})-SS_{res}(\text{nested_model})}{df(\text{complete_model})- df(\text{nested_model})}}{\frac{SS_{res}(\text{complete_model})}{df(\text{complete_model})}} \sim F (df(\text{complete_model})- df(\text{nested_model}), df(\text{complete_model}))
$$

```{r lmnested, eval = TRUE, collapse = TRUE }
g2 = lm( sr ~ pop75 + dpi + ddpi, data = savings )
summary( g2 )
SS_res_2 = sum( g2$residuals^2 )

f_test_2 = ( ( SS_res_2 - SS_res ) / 1 )/( SS_res / (n-p) )

1 - pf( f_test_2, 1, n-p )  
```

* __t-test on the nested model__

You fit a nested model (the complete model without the covariate in which you are interested) then you compute the residuals of the 2 models and execute the t-test. 


```{r amano, eval = TRUE, collapse = TRUE }
2 * ( 1-pt( sqrt( f_test_2 ), n-p ) )
```

* __ANOVA between the two nested models__

```{r anova, eval = TRUE, collapse = TRUE }
anova( g2, g )
```

We notice that the outcome is the same in all the three methods. $\beta_1$ is signficant.

## Homework
__1.e__  Test the significance of all the regression parameters, separately. 

__1.f__ Test the regression parameter $\beta_4$ ( the one related to 'ddpi' ) for this test:
$$
		H_0: \beta_4 = 0.35 \qquad vs \qquad  H_1: \beta_4 > 0.35
$$

## -----------------------------------------------------------------------------------
## 2. Confidence Intervals and Regions
## -----------------------------------------------------------------------------------
### Confidence Intervals

__2.a__ 
Compute the $95\%$ confidence intervals for the regression parameter related to 'pop75'.

__solution__

The formula for the required confidence interval is:
$$
IC_{(1-\alpha)}(\beta_2) = [\hat{\beta}_2 \pm t_{1-\alpha/2}(n-p) \cdot se( \hat{\beta}_2 )],
$$
where $\alpha = 5\%$ and $df = n-p = 45$.

```{r testtparampop75, eval = TRUE, collapse = TRUE }
alpha = 0.05
t_alpha2 = qt( 1-alpha/2, n-p )
beta_hat_pop75 = g$coefficients[3]
se_beta_hat_pop75 = summary( g )[[4]][3,2]

IC_pop75 = c( beta_hat_pop75 - t_alpha2 * se_beta_hat_pop75, beta_hat_pop75 + t_alpha2 * se_beta_hat_pop75 )
IC_pop75
```

We observe that $IC_{(1-\alpha)}(\beta_2)$ includes $0$, so there is no evidence for rejectig $H_0: \beta_2 = 0$, at the $5\%$ level. Indeed, this parameter was not significant even in the previous section (p-value $12.5\%$).


__2.b__
Compute the $95\%$ confidence intervals for the regression parameter related to 'ddpi'.

__solution__
```{r testtparamddpi, eval = TRUE, collapse = TRUE }
alpha = 0.05
t_alpha2 = qt( 1-alpha/2, n-p )
beta_hat_ddpi = g$coefficients[5]
se_beta_hat_ddpi = summary( g )[[4]][5,2]

IC_ddpi = c( beta_hat_ddpi - t_alpha2 * se_beta_hat_ddpi, beta_hat_ddpi + t_alpha2 * se_beta_hat_ddpi )
IC_ddpi
```

In this case, we observe that $IC_{(1-\alpha)}(\beta_4)$ does NOT include $0$, so there is evidence for rejecting $H_0: \beta_4 = 0$, at the $5\%$ level. However, the lower bound of the $IC_{(1-\alpha)}(\beta_4)$ is really close to $0$. We can see from the output above that the p-value is $4.2\%$ - lower than $5\%$ - confirming this point.

Notice that this confidence interval is pretty wide in the sense that the upper limit is about 80 times larger than the lower limit.
This means that we are not really that confident about what the exact effect of growth on savings really is.

__REMARK__ Confidence intervals often have a duality with two-sided hypothesis tests.
A 95% confidence interval contains all the null hypotheses that would not be rejected at the 5% level.




### Confidence Regions

__2.c__
Build the joint $95\%$ confidence region for parameters 'pop15' e 'pop75'. And add the value of $(\beta_1,\beta_2)$ according to the null hypothesis.

__solution__
```{r conf_region, eval = TRUE, collapse = TRUE }
#help( ellipse )

plot( ellipse( g, c( 2, 3 ) ), type = "l", xlim = c( -1, 0 ) )

#add the origin and the point of the estimates:
#vettore che stiamo testando nell'hp nulla

points( 0, 0 )
points( g$coef[ 2 ] , g$coef[ 3 ] , pch = 18 )
```

The filled dot is the center of the ellipse and represents the estimates of the 2 parameters ($\hat{\beta_1}$, $\hat{\beta_2}$).
Now, we are interested in this test:

$$
		H_0: (\beta_1,\beta_2) = (0,0) \qquad vs \qquad  H_1: (\beta_1,\beta_2) \neq (0,0)
$$


We observe that the empty dot ($0$,$0$) is not included in the Confidence Region (which is now an ellipse), so we reject $H_0$ at $5\%$ level. In other words, we are saying that there is at least one parameter between $\beta_1$ and $\beta_2$ which is not equal to $0$. 

__REMARK__ It is important to stress that this Confidence Region is different from the one obtained by the cartesian product of the two Confidence Intervals, $IC_{(1-\alpha)}(\beta_1)$ X $IC_{(1-\alpha)}(\beta_2)$. The cartesian product of the two Confidence Intervals is represented by the four dashed lines.

```{r adding1dCR, eval = TRUE, collapse = TRUE }
beta_hat_pop15 = g$coefficients[2]
se_beta_hat_pop15 = summary( g )[[4]][2,2]

IC_pop15 = c( beta_hat_pop15 - t_alpha2 * se_beta_hat_pop15, beta_hat_pop15 + t_alpha2 * se_beta_hat_pop15 )
IC_pop15

plot( ellipse( g, c( 2, 3 ) ), type = "l", xlim = c( -1, 0 ) )

points( 0, 0 )
points( g$coef[ 2 ] , g$coef[ 3 ] , pch = 18 )

#new part
abline( v = c( IC_pop15[1], IC_pop15[2] ), lty = 2 )
abline( h = c( IC_pop75[1], IC_pop75[2] ), lty = 2 )
```

__REMARK__ The origin $(0,0)$ is included in the $IC_{(1-\alpha)}(\beta_2)$ and is NOT included in the $IC_{(1-\alpha)}(\beta_1)$, as expected from the previous point.



__REMARK__ It can happen that you could reject according to one Confidence Region and accept according to the other Confidence Region. So which region should we choose?  
We should alaways choose the joint Confidence Region (the elliptic one), because it is taking into account the correlation between the parameters.

```{r adding1dCRpoints, eval = TRUE, collapse = TRUE }
plot( ellipse( g, c( 2, 3 ) ), type = "l", xlim = c( -1, 0 ) )

points( 0, 0 )
points( g$coef[ 2 ] , g$coef[ 3 ] , pch = 18 )

abline( v = c( IC_pop15[1], IC_pop15[2] ), lty = 2 )
abline( h = c( IC_pop75[1], IC_pop75[2] ), lty = 2 )

#new part
points( -0.22, 0.7, col = "red", lwd = 2 )
points( -0.71, 0, col = "blue", lwd = 2 )
```

```{r corpop1575, eval = TRUE, collapse = TRUE }
cor( savings$pop15, savings$pop75 )
```



## -----------------------------------------------------------------------------------
## 3. Diagnostics: detecting influential points
## -------------------------------------------------------------------------------

The goal of diagnostics consists in detecting possible influential points in a sample. 
In general, an influential point is one whose removal from the dataset would cause a large change in the fit.
influential points are outliers and leverages. The definitions of outliers and leverages can overlap. A possible definition of outlier is 'a point that does not fit the chosen model'. On the other hand, a leverage is 'a point that significantly affects the estimates of the model'. It is immediate to see that often an outlier is also a leverage point.



Some measures of influential:

1. Change in the coefficients:  $\hat{\beta} - \hat{\beta}_i$ 
2. Change in the fit:  $x^T(\hat{\beta} - \hat{\beta}_i) = \hat{y} - \hat{y}_i$
These are hard to judge in the sense that the scale varies between datasets.


There are several approaches for identifying influential points in a sample, such as:

* __Leverages__
* __Standardized Residuals__
* __Studentized Residuals__  
* __Jacknife Residuals__
* __Cook's Distance__


### Leverages

__3.a__ Investigate possible leverages among data.
Leverages are defined as the diagonal elements of H matrix:
$$
H = X  (X^T X )^{-1} X^T 
$$


__solution__
```{r leverages, eval = TRUE, collapse = TRUE }

X = model.matrix( g )
X


lev = hat( X )
lev
# oppure 
lev = hatvalues( g )
lev

#manually
H = X %*% solve( t( X ) %*% X ) %*% t( X ) 
lev = diag( H )

sum(lev)
```


__REMARK__ The trace of the $H$ matrix (sum of the diagonal elements of a matrix) is equal to the rank of $X$ matrix, which is $p+1$, assuming that covariates are all linearly independent and $p<n$.
This is the size of the vectorial subspace generated by the linear combinations of the columns of $X$. The geometric interpretation of the linear regression (OLS) states that $H$ acts on $\textbf{y}$ (vector of outcomes) by projecting it on the former subspace. The final output is $\hat{\textbf{y}}$.

__Rule of thumb:__ a point is a leverage if:
$$
\hat{h}_{ii} > 2 \cdot \frac{p}{ n}
$$

```{r leveragesplot, eval = TRUE, collapse = TRUE }

plot( g$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", pch = 16, col = 'black' )

abline( h = 2 * p/n, lty = 2, col = 'red' )

watchout_points_lev = lev[ which( lev > 2 * p/n  ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]

points( g$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

sum( lev )      # verifica: sum_i hat( x )_i = r + 1

lev [ lev >  2 * 5 / 50 ]
sum( lev [ lev >  2 * 5 / 50 ] )
```

__3.b__
Fit the model without leverages.

__solution__
```{r modelnoleverages, eval = TRUE, collapse = TRUE }
gl = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = ( lev < 0.2 ) )
summary( gl )
#summary( g )
```

Moreover, investigate the relative variation of $\hat{\beta}$ due to these influential points.

```{r variationleverages, eval = TRUE, collapse = TRUE }
abs( ( g$coefficients - gl$coefficients ) / g$coefficients )
```

The leverages affect the estimate heavily (there is a variation of $22\%$ at least).

```{r identifyingleveragesinpairs, eval = TRUE, collapse = TRUE }
colors = rep( 'black', nrow( savings ) )
colors[ watchout_ids_lev ] = c('red', 'blue', 'green', 'orange')

pairs( savings[ , c( 'sr', 'pop15', 'pop75', 'dpi', 'ddpi' ) ], pch = 16, col = colors, cex = 1 + 0.5 * as.numeric( colors != 'black' )    )
```


### Standardized Residuals

__3.c__ Plot the residuals of the complete model.

__solution__
```{r residualsbis, eval = TRUE, collapse = TRUE }

# Residui non standardizzati (nè studentizzati)

plot( g$res, ylab = "Residuals", main = "Plot of residuals" )

sort( g$res )
sort( g$res ) [ c( 1, 50 ) ]

countries = row.names( savings )
identify( 1:50, g$res, countries ) # cliccare 2 volte sui punti a cui si vuole apporre il label
```

`identify` is a useful function for detecting influent points. 
In input, you should call the x and y axes of the plot and the labels of data.

Usually, the residuals are represented wrt y-values or the single predictors.

This is useful also for testing the model hypotheses: omoschedasticity and normality of residuals (flash forward).  

The representation with the index of the observation as x-axis is not that useful (except if we are interested in investigating the distribution of the residuals wrt the procedure used for data collection).


__3.d__ Plot the __Standardized Residuals__ of the complete model.

__Rule of thumb__ 
Given that standardized residuals are defined as:
$$
r_i^{std} = \frac{y_i-\hat{y}_i}{\hat{S}}
$$
influential points satisfy the following inequality:
$$
|r_i^{std}|>2
$$

__solution__
```{r residuals, eval = TRUE, collapse = TRUE }
gs = summary(g)
res_std = g$res/gs$sigma
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd

# Residui standardizzati (non studentizzati)

plot( g$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( g$fitted.values[watchout_ids_rstd], res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( g$fitted.values[watchout_ids_lev], res_std[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'), c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )

sort( g$res/gs$sigma )
sort( g$res/gs$sigma ) [ c( 1, 50 ) ]

countries = row.names( savings )
identify( 1:50, g$res/gs$sigma, countries ) # cliccare 2 volte sui punti a cui si vuole apporre il label
```
 

### Studentized Residuals

__3.e__ Compute the Studentized Residuals, highlighting the influential points.

__solution__

Studentized residuals, $r_i$, are computed as:
$$
 r_i = \frac{\hat{\varepsilon}_i}{\hat{S} \cdot \sqrt{ ( 1 - h_{ii} ) }} \sim t(n-p)
$$

Since $r_i$ is distributed according to a Student-$t$ with $( n-p )$ df, we can calculate a p-value to test whether point $i-th$ is an outlier. However, we would probably want to test all cases so we must adjust the level of the test accordingly ( for example, using Bonferroni correction ).

`rstandard` gives jackknife residuals automaically.

```{r standardizeres, eval = TRUE, collapse = TRUE }
gs = summary( g )

gs$sigma

#manually
stud = g$residuals / ( gs$sigma * sqrt( 1 - lev ) )

#automatically
stud = rstandard( g )

watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
watchout_stud


plot( g$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( g$fitted.values[watchout_ids_stud], stud[watchout_ids_stud], col = 'pink', pch = 16 )
points( g$fitted.values[watchout_ids_rstd], stud[watchout_ids_rstd], col = 'red', pch = 16 )
points( g$fitted.values[watchout_ids_lev], stud[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink','red','orange'), c('Studentized Residual', 'Standardized Residuals', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )
```

We do not see pink dots, because Studentized residuals and Standardized residuals identify the same influential points.


### Jackknife residuals

The idea behind Jackknife residuals consists in seeing the impact of a single realization on the model.  In order to do so, we exclude one observation at a time and we estimate model parameters without that specific observation.  

For example, if we exclude the $i-th$ observation, then we compute:
$$
       \hat{y}_{(-i)} = X_{(-i)}^T \cdot \hat{\beta}_{(-i)}
$$

The former procedure is called *Jackknife*, or *Leave-one-out*, and has a general validity in Statistics as a method for defining the sensitibility of an estimate with respect to specific data that determined it. The formula for computing the Jackknife residuals is:

$$
r_{(-i)} = r_{i} \cdot \sqrt{\frac{(n-p-1)}{(n-p-r_{i}^2)}}
$$
in which $r_i$ is the studentized residuals.

`rstudent` gives jackknife residuals automatically.

```{r jackknifebis, eval = TRUE, collapse = TRUE }
jack = rstudent( g )

jack_man = stud * sqrt( ( n - p - 1 )/( n - p - stud^2 ) )

plot( jack, ylab = "jackknife Residuals", main = "Jackknife Residuals", pch = 16 )
abline( h = c( -2, 2 ), col = 'orange' )

jack [ abs( jack ) > 2 ]
```
The highest jackknife residual is $2.853558$, related to Zambia. Two can be considered as outlier: Chile and Zambia.

Graphical explanation of leave-one-out idea.
We fit n-model through `lm.influence` function. We use all the observations except the i-th observation (*leave-one-out*) for fitting the i-th model. The output of the function is the diffence between the estimates in the model fitted with $n-1$ obesrvations and the model fitted with $n$ observations.

```{r cookstatinflu, eval = TRUE, collapse = TRUE }
ginf = lm.influence( g )
names( ginf )

#we leave Australia out
g_aus = lm(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = savings, subset = ( countries != "Australia" ) )
g$coef - g_aus$coef
ginf$coefficients[ 1, ]

countries = rownames( savings )

plot( ginf$coef [ , 2 ] , ginf$coef [ , 3 ] , xlab = "Change in pop15 coef",
      ylab = "Change in pop75 coef", pch = 16 )
#identify( ginf$coef [ , 2 ] , ginf$coef [ , 3 ] , countries )
```

Japan is an influential point. 



### Cook's distance

Cook's distance is a commonly used influential measure that combines the two characteristics of an influential point. It can be expressed as:
$$
  C_i =  r_i^2 / p \cdot \Big[ \frac{h_{ii}}{ 1 - h_{ii} } \Big] 
$$

in which $r_i$ are the studentized residuals.

__Rule of thumb__
A point is defined influential if:
$$
C_i > \frac{4}{n-p}
$$

```{r cookdist, eval = TRUE, collapse = TRUE }
Cdist = cooks.distance( g )

watchout_ids_Cdist = which( Cdist > 4/(n-p) ) 
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist


par( mfrow = c( 1, 3 ) )
plot( g$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( g$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], col = 'green', pch = 16 )
plot( g$fitted.values, stud, pch = 16, xlab = 'Fitted values', ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( g$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], col = 'pink', pch = 16 )
plot( g$fitted.values, lev, pch = 16, xlab = 'Fitted values', ylab = 'Leverages', main = 'Leverages' )
points( g$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ], col = 'orange', pch = 16 )
```


__3.f__ Fit the model without influential points wrt Cook's distance and compare the outcome to the former model (on the complete dataset).

__solution__
```{r cookstatfitred, eval = TRUE, collapse = TRUE }
#id_to_keep = (1:n)[ - watchout_ids_Cdist ]
id_to_keep = !( 1:n %in% watchout_ids_Cdist )

gl = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings[ id_to_keep, ]  ) 

abs( ( gl$coef - g$coef )/g$coef )
summary( g )
```

The coefficient for dpi changed by about 64%.

__Influential Plot__

The influential plot represents the studentized residuals vs leverages, and highlights them with a circle which is proportional to Cook's distance.
 

```{r influentialplot, eval = TRUE, collapse = TRUE }
influencePlot( g, id.method = "identify", main = "influential Plot",
               sub = "Circle size is proportial to Cook's Distance" )
```

__Influential measures__

influential.measures produces a class "infl" object tabular display showing several diagnostics measures (such as $h_{ii}$ and Cook's distance).
Those cases which are influential with respect to any of these measures are marked with an asterisk.

```{r influencemeasures, eval = TRUE, collapse = TRUE }
influence.measures( g )
```
 
There are other indices for detecting influential points, such as DFBETAs and DFFITs.

`https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html`




## -----------------------------------------------------------------------------------
## 4. HYPOTHESES TESTING
## --------------------------------------------------------------------------------


### Omoschedasticity 

__4.a__ Plot residuals ( $\hat{\varepsilon}$ ) vs fitted  values ( $\hat{y}$ ).

```{r omosched, eval = TRUE, collapse = TRUE }

plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variabilità sembra sufficientemente uniforme
```

### Nonlinearity 

__4.b__
Plot residuals ( $\hat{\varepsilon}$ ) vs predictors ( $x_i$ ).


```{r nonlinear, eval = TRUE, collapse = TRUE }

par( mfrow = c( 2, 2 ) )
plot( savings$pop15, g$res, xlab = "Population under 15", ylab = "Residuals",
      main = "Residuals vs pop15", pch = 16 )
plot( savings$pop75, g$res, xlab = "Population over 75", ylab = "Residuals",
      main = "Residuals vs pop75", pch = 16 )
plot( savings$dpi, g$res, xlab = "dpi", ylab = "Residuals", main = "Residuals vs dpi", pch = 16 )
plot( savings$ddpi, g$res, xlab = "ddpi", ylab = "Residuals", main = "Residuals vs ddpi", pch = 16 )
```

### Normality

```{r normal, eval = TRUE, collapse = TRUE }


# QQ plot
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )
# Andamento adeguatamente rettilineo, l'ipotesi è verificata

# Shapiro-Wilk normality test
# p-val molto alto = > NON rifiuto H0: dati Gaussiani
shapiro.test( g$res )
```

__REMARK__

*rstandard* function automatically computes the studentized residuals.

*rstudent* function automatically computes the jackknife residuals. 
 
```{r normalbis, eval = TRUE, collapse = TRUE }
qqnorm( rstudent( g ), ylab = "Jackknife residuals", pch = 16 )
abline( 0, 1 )

# altri strumenti utili...
hist( g$res, 10, probability = TRUE, col = 'lavender', main = 'residuals'  )
boxplot( g$res, main = "Boxplot of savings residuals", pch = 16, col = 'lavender' )
```

### Nonlinearity/Collinearity

__Added Variable Plots (partial regression plots)__

These functions build added-variable (also called partial-regression) plots for linear and generalized linear models.

Given a set of predictors and an outcome, the function *avPlots*  allows to visualize the effect of a specific predictor that is not explained by the others. 

Let's suppouse to focus on $X_1$, the function plots the residuals of:
$$
 Y \sim X_2 + X_3 + ... + X_p,
$$
vs the residuals of:
$$
X_1 \sim X_2 + X_3 + ... + X_p.
$$
The regression of residuals gives the regression coefficient $X_1$, by considering all the other variables, this means its net effect on the outcome $Y$.

This plot can be also used for the diagnosis influential points and nonlinearity.

```{r partialregressionplot, eval = TRUE, collapse = TRUE }

avPlots( g )
# Si confrontino tali grafici con
summary( g )
```

__VIF__
Variance Inflation Factor is another index of collinearity. 
$$
Var( \beta_j ) = \frac{S^2}{ ( n-1 ) \cdot S_j^2 } \cdot \frac{1}{ 1-R_j^2 }
$$

```{r vif, eval = TRUE, collapse = TRUE }
vif( g )
```

In this case, __pop75__ and __pop15__ show the highest collinearity. 

### Violation of omoschedasticity

```{r violataomosched, eval = TRUE, collapse = TRUE }

par( mfrow = c( 3, 3 ) )

# Omoschedasticità
for( i in 1:9 )
  plot( 1:50, rnorm( 50 ), pch = 16 )

# Eteroschedasticità marcata (varianza funzione lineare di x)
for( i in 1:9 )
  plot( 1:50, ( 1:50 ) * rnorm( 50 ), pch = 16 )

# Eteroschedasticità blanda (varianza funzione sublineare di x)
for( i in 1:9 )
  plot( 1:50, sqrt( ( 1:50 ) ) * rnorm( 50 ), ylim = c( -20, 20 ), pch = 16 )

# Non linearità (varianza funzione nonlineare di x)
for( i in 1:9 )
  plot( 1:50, cos( ( 1:50 ) * pi/25 ) + rnorm( 50 ), pch = 16 )
```


### Violation of normality

```{r violatanorm, eval = TRUE, collapse = TRUE }

par( mfrow = c( 3, 3 ) )

# Normali
for( i in 1:9 )
{
  D = rnorm( 50 )

  qqnorm( D, pch = 16 )

  qqline( D, lty = 2, lwd = 2, col = 'red' )
}


# Esponenziali
for( i in 1:9 )
{
  D = rexp( 50 )

  qqnorm( D, pch = 16 )

  qqline( D, lty = 2, lwd = 2, col = 'red' )
}


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


par( mfrow = c(3,3) )
# Uniforme
for( i in 1:9 )
{
  D = runif( 50 )

  qqnorm( D, pch = 16 )

  qqline( D, lty = 2, lwd = 2, col = 'red' )
}
```

## -----------------------------------------------------------------------------------
## 5. Transformation: Box-Cox
## -----------------------------------------------------------------------------------

In this section we would like to answer the following question: what should we do when there is a clear violation of hypotheses?
The answer consists in investigating variable transformations (transformation of the outcome).

__Warning__ Transforming a variable can lead to a more difficult interpretation of the model.

An algorithm that helps us in variable transformation is the *Box-Cox* algorithm.
It detects the best $\lambda$ among a family of transformations ($\frac{y^{\lambda}-1}{\lambda}$, if $\lambda\neq0$, otherwise $log(y)$) in oder to gain the Normality/Omoschedasticity for __positive data__. 

Here we report an example (multiple linear regression). We consider a dataset with 4 columns: freq, length, sex, age. Then we apply a linear regression model in which 'freq' is the outcome and the others are the predictors. Finally, we test the Normality hypothesis of the model.

```{r boxcoxsimplinreg, eval = TRUE, collapse = TRUE }
#import data
dati = read.table( 'delfini.txt', header = T )

mod = lm( freq ~ length + sex + age, data = dati )
summary( mod )

#summary(mod)
mod_res <- ( mod$residuals - mean( mod$residuals ) )/sd( mod$residuals )
plot( mod$fitted, mod_res, xlab = 'Fitted values',  ylab = 'Standardized residuals'  )


qqnorm( mod$residuals )
qqline( mod$residuals, col = 'blue' )
abline( 0, 1, col = 'red' )

qqnorm( mod_res )
abline( 0, 1, col = 'red' )

shapiro.test( mod_res )

```

Good fit of the model: $R^2 = 75.75\%$ and the predictor is significant $p-value < 2.2e-16$.
However, there is a clear evidence of nonnormality (we can see it both with the qqplot and the Shapiro-Wilk test). So, we apply the Box-Cox transformation.
*Remark* we can apply the Box-Cox transformation, because *freq* is positive.

```{r boxcoxtransf, eval = TRUE, collapse = TRUE }
b = boxCox( mod )
names(b)
#y likelihood evaluation
#x lambda evaluated
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda
```

We can see that the best transformation is the one related to the *maximum* of the curve.
The estimates are obtained through *Maximum Likelihood* method. According to this method, the best $\lambda$ is very close to 0.
Despite of that, we would prefer the most interpretable transformation among the allowed ones: $\lambda = 0$. So the chosen transformation is the logarithm of the outcome variable.

Finally, we test the new model and we investigate the standardized residuals.

```{r boxcoxnewmodel, eval = TRUE, collapse = TRUE }

dati$freq2 = log( dati$freq )

mod1 = lm( freq2 ~ length + sex + age, data = dati )
summary( mod1 )

plot( mod1$fitted.values, mod1$residuals )

qqnorm( ( mod1$residuals - mean( mod1$residuals ) )/sd( mod1$residuals ) )
abline( 0, 1, col = 'red' )

shapiro.test( mod1$residuals )

```

We can conclude that the Box-Cox transformation helped us in improving the model.


## -----------------------------------------------------------------------------------
## 6. Variable Selection: stepwise procedure
## -----------------------------------------------------------------------------------

__6.a__
Load `state` dataset, in which data about the 50 states of USA are collected. The variables are population estimate as of July 1, 1975, per capita:

* __income__ ( 1974 );
* __illiteracy__ ( 1970, percent of population );
* __life expectancy__ in years ( 1969-71 );
* __murder__ and non-negligent manslaughter rate per 100, 000 population ( 1976 );
* __percent high-school graduates__  ( 1970 );
* __mean number of days with min temperature 32 degrees__( 1931-1960 );
* __in capital or large city__;
* __land area__ (in square miles).

We will take life expectancy as the response and the remaining variables as
predictors.

```{r stepwiseproc, eval = TRUE, collapse = TRUE }
data( state )
statedata = data.frame( state.x77, row.names = state.abb, check.names = T )

head( statedata )
```

__6.b__
Fit and investigate the complete linear model on this data.

Which predictors should be included - can you tell from the p-values?
Looking at the coefficients, can you see what operation would be helpful?

Does the murder rate decrease life expectancy - that's obvious a priori, but how should these results be interpreted?

```{r glmstata, eval = TRUE, collapse = TRUE }
g = lm( Life.Exp ~ ., data = statedata )
summary( g )
```

__Manual backward selection method__ 

At each step we remove the predictor with the largest p-value and (ideally) stop when we have only predictors with p-values below 0.05

```{r backselmanual, eval = TRUE, collapse = TRUE }
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
```

The final removal of variable *Population* is a close call. We may want to
 consider including this variable if interpretation is aided. Notice that the
 $R^2$ for the full model of 0.736 is reduced only slightly to 0.713 in the final
 model.

Thus the removal of four predictors causes only a minor reduction in fit.

```{r correlcov, eval = TRUE, collapse = TRUE }
X = statedata [ , -4 ] #not considering the response variable
cor( X )
# Note the high positive correlation between Murder and Illiteracy!

heatmap( cor( X ), Rowv = NA, Colv = NA, symm = TRUE, keep.dendro = F)
#image( as.matrix( cor( X ) ), main = 'Correlation of X' )


pairs( X )
```

Mind spurious correlations!
`http://www.tylervigen.com/spurious-correlations`
`http://guessthecorrelation.com`


__Automatic backward selection method__

At each step we remove the predictor with the largest p-value over 0.05:


```{r backselautomatic, eval = TRUE, collapse = TRUE }
g = lm( Life.Exp ~ ., data = statedata )

help( step )

# We can specify either backward or forward (or even both of them)
step( g, direction = "both" ) #it goes backward
#AIC criterion is the default

AIC( g1 )
AIC( g2 )
AIC( g3 )
AIC( g4 )
```

According to an automatic selection on AIC, the best model is $g3$: Population + Murder + HS.Grad + Frost.

__Criterion based procedures__ 

1. AIC & BIC;
2. $R^2$ adjusted;
3. Mallow's Cp;
4. PRESS.[shown in class, not in lab]

__1. AIC & BIC__
$$
AIC = -2 \cdot \text{log( likelihood )} + 2k
$$
where $k$ is the number of parameter in the model.
$$
BIC = -2 \cdot \text{log( likelihood )} + k \cdot \text{log(n)}
$$
```{r backselautomaticAIC, eval = TRUE, collapse = TRUE }
g = lm( Life.Exp ~ ., data = statedata )

AIC( g ) 
BIC( g )

g_AIC_back = step( g, direction = "backward" ) #k=2 is the default for pure AIC
g_BIC_back = step( g, direction = "backward", k = log(n) )

BIC(g1)
BIC(g2)
BIC(g3)
BIC(g4)
```
The best model according to BIC is still $g3$.


__2. $R^2$ adjusted__
```{r backselautomaticR2, eval = TRUE, collapse = TRUE }
help( leaps )

# solo matrice dei predittori senza colonna di 1
x = model.matrix( g ) [ , -1 ]
y = statedata$Life

adjr = leaps( x, y, method = "adjr2" )
adjr

bestmodel_adjr2_ind = which.max( adjr$adjr2 )
g$coef[ which( adjr$which[ bestmodel_adjr2_ind, ] ) + 1 ]

help( maxadjr )
maxadjr( adjr, 5 )
```

We see that also in this case $g3$, the model with Population + Murder + HS graduation + Frost, is the best one, since it has the largest $R^2_{adj}$ ($71.26\%$).

```{r backselautomaticpr2, eval = TRUE, collapse = TRUE }
# Other possibilities
R2 = leaps( x, y, method = "r2" )

bestmodel_R2_ind = which.max( R2$r2 )
R2$which[ bestmodel_R2_ind, ] 
```

According to $R^2$, the best model is the complete one.

__REMARK__
Remember that Variable selection methods are sensitive to influential points.

__3. Mallow's Cp__

__Rule of thumb__
The best model according to Cp, is the one that leads to $Cp$ close to $p$. if we are uncertain, we should choose the simplest model. 

```{r backselautomaticCp, eval = TRUE, collapse = TRUE }
g_Cp_model = leaps(  y = statedata[ , 4 ], x = statedata[ , -  4 ], method = 'Cp' )
# g = leaps( x, y, method = "Cp" ) # Mallow's Statistics
Cpplot( g_Cp_model )

g_Cp_coef = which( g_Cp_model$which[ which.min( g_Cp_model$Cp ) , ] )
g$coefficients[ g_Cp_coef + 1 ]

#il modello migliore è 456, cioè g4 (Cp ottimo ~ p).
```

The models are denoted by indices for the predictors.
The best model according to $Cp$ is the "456" model i.e. the Murder, HS graduation and Frost model.

## -----------------------------------------------------------------------------------
## 7. Prediction
## -----------------------------------------------------------------------------------

We want to establish the relation between the height of tomatoes plants and the average weight of the picked tomatoes [g].

The data are as follows:

```{r data, eval = TRUE, collapse = TRUE }
peso   = c( 60, 65, 72, 74, 77, 81, 85, 90 )
altezza = c( 160, 162, 180, 175, 186, 172, 177, 184 )
```

__7.a__ 
Fit the complete model and analyze it.

```{r datapom, eval = TRUE, collapse = TRUE }
mod = lm( peso ~ altezza )
summary( mod )
```

__7.b__ 
Compute the __Confidence Interval__ for the prediction of the *average outcome*.

__solution__ 

We define a grid of values (in the range of the data, in order to have reliable prediction).

We compute the predicted values:
$$
\hat{y}_{new} = x_{new}\hat{\beta}
$$
and the related standard errors:
$$
se(\mathbb{E}[y_{new}]) = \hat{S} \cdot \sqrt{x_{new}^T (X^TX)^{-1} x_{new}}
$$


__REMARK__ The second call of `predict`, $x_{new}$, must be a data.frame with the columns with the same names of the original predictors in the model.


```{r confinterpred, eval = TRUE, collapse = TRUE }
grid = seq( min( altezza ), max( altezza ), 2 )

#automatically
y.pred = predict( mod, data.frame( altezza = grid ), interval = "confidence", se = T )

names( y.pred )

y.pred$fit[ ,1 ] # predicted values $\hat{y}_{new}$.
y.pred$fit[ ,2 ] # LB confidence interval for $y_{new}$.
y.pred$fit[ ,3 ] # UB confidence interval for $y_{new}$.


# manually
ndata = cbind( rep( 1, length( grid ) ), grid )
y.pred_fit = ndata %*% mod$coefficients 
y.pred_fit

#standard error
y.pred$se
#manually
y.pred_se = rep( 0, 14 )
X = model.matrix( mod )
for( i in 1:14 )
{
 y.pred_se[ i ] = summary( mod )$sigma * sqrt( t( ndata[i,] ) %*% solve( t(X) %*% X ) %*% ndata[i,] ) 
}
y.pred_se

# n - p = 8 - 2 = 6
y.pred$df

tc    = qt( 0.975, length( altezza ) - 2 )
y     = y.pred$fit[ ,1 ]
y.sup = y.pred$fit[ ,1 ] + tc * y.pred$se
y.inf = y.pred$fit[ ,1 ] - tc * y.pred$se

IC = cbind( y, y.inf, y.sup )

IC
y.pred$fit

matplot( grid, cbind( y, y.inf, y.sup ), lty = c( 1, 2, 2 ), col = c( 1, 'blue', 'blue' ), type = "l", xlab = "altezza", ylab = "peso", main = 'IC per la media della risposta' )
points( altezza, peso, col = "black", pch = 16 )
```

__7.c__ 

Compute the *Prediction Interval* for the one new observation. In this case the standard errors are:
$$
se(y_{new}) = \hat{S} \cdot \sqrt{1+x_{new}^T (X^TX)^{-1} x_{new}}
$$

```{r predinter, eval = TRUE, collapse = TRUE }
y.pred2 = predict( mod, data.frame( altezza = grid ), interval = "prediction", se = T )
# fornisce direttamente gli estremi inf e sup, che prima abbiamo costruito a mano (in un altro caso)

y.pred2$fit[ ,1 ] # predicted values $\hat{y}_{new}$.
y.pred2$fit[ ,2 ] # LB prediction interval for $y_{new}$.
y.pred2$fit[ ,3 ] # UB prediction interval for $y_{new}$.


#manually
ndata = cbind( rep( 1, length( grid ) ), grid )
y.pred_fit = ndata %*% mod$coefficients 
y.pred_fit

# standard error
y.pred2$se.fit
#manually
y.pred2_se = rep( 0, 14 )

for( i in 1:14 )
{
 y.pred2_se[ i ] = summary( mod )$sigma * sqrt(  1 + t( ndata[i,] ) %*% solve( t(X) %*% X ) %*% ndata[i,] ) 
}
y.pred2_se

#In this case y.pred2_se != y.pred2$se.fit

tc    = qt( 0.975, length( altezza ) - 2 )
y     = y.pred2$fit[,1]
y.sup = y.pred2$fit[,1] + tc * y.pred2_se
y.inf = y.pred2$fit[,1] - tc * y.pred2_se

IP = cbind( y, y.inf, y.sup )
y.pred2$fit

matplot( grid, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l",
         xlab = "altezza", ylab = "peso", main = 'IP per singole osservazioni' )
points( altezza, peso, col = "blue", pch = 16 )
```


__7.d__

Compare the Intervals obtained at __7.b__ and __7.c__.

```{r predintcompar, eval = TRUE, collapse = TRUE }

matplot( grid, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l", xlab = "altezza", ylab = "peso", main = "IC per la media e IP per singole osservazioni" )
lines( grid, y.pred$fit[ , 2 ] , col = "blue", lty = 2, xlab = "altezza", ylab = "peso" )
lines( grid, y.pred$fit[ , 3 ] , col = "blue", lty = 2, xlab = "altezza", ylab = "peso" )
points( altezza, peso, col = "black", pch = 16 )
```

According to theory, the prediction interval is broader that the confidence interval (see the standard errors) and all the points are inside the prediction interval, while only few of them are inside the confidence interval.












