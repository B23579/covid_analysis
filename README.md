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

The dataset is for the Covid-19 spreading outbreak from the official website of [Protezione Civile](https://github.com/pcm-dpc/COVID-19), it was download by using the following command:

```
read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
```

The dataset contains the following variables:

`data` Date of notification

`stato` Country of reference

`codice_regione` Code of the Region (ISTAT 2019)

`denominazione_regione` Name of the Region

`lat` Latitude

`long` Longitude

`ricoverati_con_sintomi` Hospitalised patients with symptoms

`terapia_intensiva` Intensive Care

`ingressi_terapia_intensiva` Daily admissions to intensive care

`totale_ospedalizzati` Total hospitalised patients

`isolamento_domiciliare` Home confinement

`totale_positivi` Total amount of current positive cases (Hospitalised patients + Home confinement)

`variazione_totale_positivi` New amount of current positive cases (totale_positivi current day - totale_positivi previous day)

`nuovi_positivi` New amount of current positive cases (totale_casi current day - totale_casi previous day)

`dimessi_guariti` Recovered

`deceduti` Death (cumulated values)

`totale_casi` Total amount of positive cases

`tamponi` Tests performed

`casi_testati` Total number of people tested

***


