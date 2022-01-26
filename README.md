# covid_analysis

***
## Goal of the project 

Consider your dataset starting from 1st October 2020 to 1st February 2021 (basically, the second wave) and prepare your presentation by considering the following points:

Perform some explanatory analysis for your data, especially by use of graphical tools.

Describe the quality of the data and discuss whether and how a plausible statistical model could be posed.

Build a model for your response variable Y. To this aim you can adopt any among the regression techniques covered during the course. Comment the estimates from the best model obtained.

By building your model, evaluate the inclusion of some covariates and their effect on the response variable. Some possible covariates could regard: the regional colors (yellow, orange, red), the partial lockdown regime, some region-specific laws and rules, etc.

Check the model fit by using the proper tools, such as residuals plots.

Provide 10-15 days-forward predictions and check their accuracy (say, for the period February 2nd to February 15, 2021).

Compare alternative models in terms of predictive information criteria and comment.

And remember: all the models are wrong, but some are useful. Feel free to use any model (possibly one from those covered in the course).

## Description of the data 

The dataset is for the Covid-19 spreading outbreak from the official website of [Protezione Civile](https://github.com/pcm-dpc/COVID-19), by using the following command:

```
read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
```

***


