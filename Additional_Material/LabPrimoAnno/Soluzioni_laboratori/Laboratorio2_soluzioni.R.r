##########################################
########### ESERCIZIO 1 ##################
##########################################
# Esercizio 7 pag.35 del Ross, 2014.

# Considera il livello di colesterolo nel sangue dei primi 
# 100 studenti presenti in 'appendiceA.txt' (il dataset usato finora).
# 1. Suddividere gli studenti in maschi e femmine, costruire una tabella con le 
#    frequenze relative 
# 2. Costruire in un unico grafico l'istogramma del colesterolo per i maschi e per le
#    femmine e analogamente per il boxplot
# 3. Cosa possiamo affermare sul rapporto tra sesso dello studente 
#    e livello di colesterolo?

# SOLUZIONE

studenti_orig = read.table(file = 'appendiceA.txt', header=T)
studenti = studenti_orig[1:100,]
head(studenti)

femmine = studenti[which(studenti$Sesso == 'F'),]
maschi = studenti[which(studenti$Sesso == 'M'),]

f_rel = table(studenti$Sesso)/length(studenti$Sesso)
f_rel


par ( mfrow = c( 2, 1 ) )

hist( femmine$Colesterolo, prob = TRUE,
      main = 'Istogramma del colesterolo delle femmine', xlab = 'Colesterolo',
      ylab = 'Densita', col = 'pink', xlim = range( studenti$Colesterolo ), 
      breaks = seq( min( studenti$Colesterolo ), max( studenti$Colesterolo ), length = 10 ) )
abline(v=median(femmine$Colesterolo), col = 'red')
abline(v=mean(femmine$Colesterolo), col = 'green')

hist( maschi$Colesterolo, prob = TRUE,
      main = 'Istogramma del colesterolo dei maschi', xlab = 'Colesterolo',
      ylab = 'Densita', col = 'lightblue', xlim = range( studenti$Colesterolo ), 
      breaks = seq( min( studenti$Colesterolo ), max( studenti$Colesterolo ), length = 10 ) )
abline(v=median(maschi$Colesterolo), col = 'red')
abline(v=mean(maschi$Colesterolo), col = 'green')

windows()
boxplot(studenti$Colesterolo ~ studenti$Sesso, col = c('pink','lightblue'),
        ylab = 'Colesterolo', main = 'Boxplot del Colesterolo m/f')

## osservazioni: 
# 1. La distribuziojne del colesterolo delle femmine non presenta particolari 
# asimmetrie (media e mediana sono simili), non si evidenzia presenza di outliers
# 2. La distribuzione del colesterolo dei maschi risulta, invece, più asimmetrica
# (in particolare, asimmetria verso destra, media > mediana), non sin evidenzia la
# presenza di outliers
# 3. Dal copnfronto tra le due distribuzione, si vede che in media, il colesterolo 
# delle femmine è più alto di quello dei maschi.


rm(list=ls())
graphics.off()


##########################################
#########  ESERCIZO 2  ###################
##########################################
## Consideriamo adesso i dati contenuti 
## nel file "studenti.txt"

# Nota bene: prima operazione da fare: selezionare il file 'studenti.txt'
# e salvarlo nella directory di lavoro.

# 0) importare in R il dataset "studenti.txt" e salvarlo in un dataframe

# 1) Calcolare gli indici di posizione per la variabile "Taglio":
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.9

# 2) Calcolare gli indici di dispersione visti per la variabile "Taglio":
#    varianza della popolazione, deviazione standard, range
#    e range interquartile

# 3) Costruire un istogramma che illustri le frequenze relative 
#    della variabile "Taglio".
#    Ricavare le seguenti informazioni contenute nella funzione 
#    che ha generato l'istogramma:
#    punti centrali, frequenza relativa e assoluta di ogni classe.
#    Che considerazioni si possono trarre dall'istogramma?

# 4) Costruire un boxplot con le osservazioni della variabile "Taglio".
#    Che considerazioni si possono trarre dal boxplot?

# 5) Calcolare gli indici di posizione per la variabile "Taglio"
#    nei due sottocampioni individuati dal genere:
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.9

# 6) Calcolare gli indici di dispersione per la variabile "Taglio"
#    nei due sottocampioni individuati dal genere:
#    varianza della popolazione, deviazione standard, 
#    range e range interquartile

# 7) Costruire gli istogrammi che illustrino le frequenze relative
#    della variabile "Taglio" nei due sottocampioni individuati dal genere.
#    Che considerazioni si possono fare confrontando i due istogrammi?

# 8) Costruire i boxplot con le osservazioni della variabile "Taglio"
#    per i due sottocampioni individuati dal genere.
#    Che considerazioni si possono fare confrontando i due boxplot?

# SOLUZIONE

# 0) importare in R il dataset "studenti.txt" e salvarlo in un dataframe
studenti = read.table(file = 'studenti.txt', header=T)
head(studenti)
dim(studenti)

# 1) Calcolare gli indici di posizione per la variabile "Taglio":
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.9

taglio = studenti$Taglio
mean(taglio)
median(taglio)
min(taglio)
max(taglio)
quantile(taglio, probs = 0.25)
quantile(taglio, probs = 0.5)
quantile(taglio, probs = 0.75)

# oppure
summary(taglio)

quantile(taglio, probs = 0.9)


# 2) Calcolare gli indici di dispersione visti per la variabile "Taglio":
#    varianza della popolazione, deviazione standard, range
#    e range interquartile

var(taglio)
sd(taglio)
diff(range(taglio))
IQR(taglio)

# 3) Costruire un istogramma che illustri le frequenze relative 
#    della variabile "Taglio".
#    Ricavare le seguenti informazioni contenute nella funzione 
#    che ha generato l'istogramma:
#    punti centrali, frequenza relativa e assoluta di ogni classe.
#    Che considerazioni si possono trarre dall'istogramma?

hist(taglio, prob = TRUE, col = 'yellow')
## l'area di ogni rettangolo corrisponde alla frequenza relativa della classe

info = hist(taglio,  plot=FALSE)
info

punti_centrali = info$mids
punti_centrali

f_ass = info$counts
f_ass

tot_osservazioni = sum(f_ass)
f_rel = f_ass/tot_osservazioni
f_rel

# oppure

density = info$density

f_rel2 = density*(diff(info$breaks))
f_rel2


## osservazioni: nelle prime due classi si concentra la maggior parte delle osservazioni
# (circa 83%) con a seguire una lunga coda a destra

# 4) Costruire un boxplot con le osservazioni della variabile "Taglio".
#    Che considerazioni si possono trarre dal boxplot?

boxplot(taglio, main ='Boxplot di Taglio', ylab = 'Taglio', col = 'yellow')

## osservazioni: ci sono parecchi outliers e per questo la distribuzione
# risulta marcatamente asimmetrica

# 5) Calcolare gli indici di posizione per la variabile "Taglio"
#    nei due sottocampioni individuati dal genere:
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.9


tapply(studenti$Taglio, studenti$Sesso, mean)
tapply(studenti$Taglio, studenti$Sesso, max)
tapply(studenti$Taglio, studenti$Sesso, min)
tapply(studenti$Taglio, studenti$Sesso, median)
tapply(studenti$Taglio, studenti$Sesso, quantile, prob = 0.25)
tapply(studenti$Taglio, studenti$Sesso, quantile, prob = 0.75)

# riassumendo..
tapply(studenti$Taglio, studenti$Sesso, summary)

tapply(studenti$Taglio, studenti$Sesso, quantile, prob = 0.9)

# 6) Calcolare gli indici di dispersione per la variabile "Taglio"
#    nei due sottocampioni individuati dal genere:
#    varianza della popolazione, deviazione standard, 
#    range e range interquartile

tapply(studenti$Taglio, studenti$Sesso, var)
tapply(studenti$Taglio, studenti$Sesso, sd)
diff(tapply(studenti$Taglio, studenti$Sesso, range)$F)
diff(tapply(studenti$Taglio, studenti$Sesso, range)$M)
tapply(studenti$Taglio, studenti$Sesso, IQR)


# 7) Costruire gli istogrammi che illustrino le frequenze relative
#    della variabile "Taglio" nei due sottocampioni individuati dal genere.
#    Che considerazioni si possono fare confrontando i due istogrammi?

par ( mfrow = c( 2, 1 ) )

hist( studenti$Taglio[which(studenti$Sesso=='M')], prob = TRUE,
      main = 'Istogramma del Taglio dei maschi', xlab = 'Taglio',
      ylab = 'Densita', col = 'lightblue', xlim = range( studenti$Taglio ), 
      ylim = c(0,0.05),
      breaks = seq( min( studenti$Taglio), max( studenti$Taglio), length = 10 ) )
abline(v = median(studenti$Taglio[which(studenti$Sesso=='M')]), col ='red')
abline(v = mean(studenti$Taglio[which(studenti$Sesso=='M')]), col ='green')

hist( studenti$Taglio[which(studenti$Sesso=='F')], prob = TRUE,
      main = 'Istogramma del Taglio delle femmine', xlab = 'Taglio',
      ylab = 'Densita', col = 'pink', xlim = range( studenti$Taglio ), 
      ylim = c(0,0.05),
      breaks = seq( min( studenti$Taglio), max( studenti$Taglio), length = 10 ) )
abline(v = median(studenti$Taglio[which(studenti$Sesso=='F')]), col ='red')
abline(v = mean(studenti$Taglio[which(studenti$Sesso=='F')]), col ='green')

## osservazioni: il Taglio dei maschi è molto più basso e quasi totalmente concentrato
## nelle prime due classi, mentre i dati di taglio delle femmine sono più dispersi. 
## Inoltre, la media delle femmine è più alta.


# 8) Costruire i boxplot con le osservazioni della variabile "Taglio"
#    per i due sottocampioni individuati dal genere.
#    Che considerazioni si possono fare confrontando i due boxplot?
windows()
boxplot(studenti$Taglio ~ studenti$Sesso, col = c('pink','lightblue'),
        main = 'Bopxplot del Taglio f/m')

## osservazioni:le femmine presentano moltim più dati outliers e di nuovo si osserva
## che in media le femmine hanno misure di Taglio maggiori

