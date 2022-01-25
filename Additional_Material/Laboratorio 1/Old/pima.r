
############################################################
#                                                          #
#     Esempio 2 per Regressione Lineare Multipla: PIMA     #
#                                                          #
############################################################


# Study conducted by the National Institute of Diabetes and Digestive and Kidney Diseases 
# on 768 adult female Pima Indians living near Phoenix.

pima <- read.table("pima-data.txt", header=T) # read the data into R

pima[1:10,] # take a look

# The variables represents:
#     - pregnant: the number times pregnant
#     - glucose: the plasma glucose concentration at 2 hours in an oral glucose tolerance test
#     - blood: the diastolic blood pressure (mmHg)
#     - triceps: the triceps skin fold thickness (mm)
#     - insulin: the 2-hour serum insulin (mu U/ml)
#     - bmi: the body mass index (weight in kg/(height in m2))
#     - pedigree: the diabetes pedigree function
#     - age: the age (years)
#     - test: whether the patient showed signs of diabetes (0=negative, 1=positive)

# At this stage, we are looking for anything unusual or unexpected, say indication of a data-entry error, or anything that show inconsistency with the pre-knowledge about the data. Let's first calculate some numerical summaries.

summary(pima)  

# Take a close look at the minimum and maximum values of each variable.

# It is weird that blood pressure equals zero (also check variables glucose, triceps, insulin, bmi). 
# Let's check their sorted values to find out how many 0's in the variable blood.

sort(pima$blood)  

# It seems likely that the zero has been used as a missing value code. In a real investigation, one would likely be able to question what really happened and if missing, whether there exists a systematic missing mechanism.
# R use "NA" as the missing value code. Let's set all zero values of the variables to NA.
pima$blood[pima$blood == 0] <- NA # set zero values in the variable blood to "NA", where "==" means "equal" in R
pima$glucose[pima$glucose == 0] <- NA # set zero values in the variable glucose to "NA"
pima$triceps[pima$triceps == 0] <- NA # set zero values in the variable triceps to "NA"
pima$insulin[pima$insulin == 0] <- NA # set zero values in the variable insulin to "NA"
pima$bmi[pima$bmi == 0] <- NA # set zero values in the variable bmi to "NA"

# The variable test is a qualitative variable, whose numerical coding is meaningless. 
# In R, a qualitative variable should be assigned as a "factor" so that R can handle it in an appropriate way.

pima$test <- factor(pima$test) # assign the variable test as a factor in R

summary(pima$test) # take a look

# It is even better to use descriptive labels:
  
levels(pima$test) # check how variable test is coded now
levels(pima$test) <- c("negative", "positive") # assign descriptive labels to variable test
levels(pima$test) # check how variable test is coded now

# Now, let's take a look of the summary of the dataset again.
summary(pima) # take a look



# Now we can do some plots to examine the distribution of variables. Use the variable blood as an example.
hist(pima$blood) # draw histogram of variable blood

# From the plot,
#    We see a bell-shaped distribution for the blood pressures centered around 70.
#    Notice that histogram plot may obscure some features of the data because its construction requires some inputs specified by the user, such as the spacing on the horizontal axis.
#    For this reason, a smoothed version of the histogram might be preferred.
plot(density(pima$blood, na.rm=TRUE))  # the function "density" computes kernel density estimates, "na.rm=True" option removes missing values.

# Now, note a couple of bi-variate plots.
par(mfrow = c(1, 2))
plot(pedigree ~ blood, pima)  # the command draws a scatter plot because the variable blood is a quantitative variable
plot(pedigree ~ test, pima)  # it draws a side-by-side box plot because the variable test is a qualitative variable
par(mfrow = c(1,1))


## Fit a linear model:

m1<-lm(pima$pregnant~pima$glucose)

summary(m1) 
# Modello molto significativo (p-val test di Fisher molto basso), ma spiega poca % della variabilità complessiva (R2 bassissimo))
# Effetto della concentraz del glucosio proporzionale all'output

# You can try other linear models
m2<-lm(pima$pregnant~pima$glucose+pima$blood)
m3<-lm(pima$pregnant~pima$glucose+pima$blood+pima$triceps+pima$insulin+pima$bmi)

summary(m3)

