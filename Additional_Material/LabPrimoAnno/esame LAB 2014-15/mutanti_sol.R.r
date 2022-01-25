# Matricole Pari: usare il dataset mutanti_pari.txt
# Matricole Dispari: usare il dataset mutanti_dispari.txt

##########################
##### MATRICOLE PARI ####
##########################


# Nel dataset mutanti sono raccolte le informazioni riguardanti la popolazione del villaggio di Thamos che è stato recentemente invaso dall’esercito di Slymers formato da mutanti antropomorfi. 

# Il CISM (Centro Italiano Studi sui Mutanti) ha condotto un’indagine sulla popolazione di Thamos per analizzare le differenze tra i Mutanti e i Non Mutanti, in particolare ha raccolto dati sulla lunghezza degli arti (cm), sulla lunghezza dei capelli (cm) e sulla forza fisica (midichlorian) di 100 abitanti del villaggio. Queste misurazioni sono raccolte nel dataset mutanti, contenente braccio, capelli, forza e tipo:  (‘NM’ per gli individui Non Mutanti e ‘M’ per i Mutanti)

# Il CISM pensa che i Mutanti possano essere affetti da Sansonite e chiede il vostro aiuto per confermare o smentire questa ipotesi.

# Per trarre una conclusione adeguata rispondete alle seguenti domande riportando i principali comandi utilizzati e le adeguate conclusioni statistiche.

	# 0.	Importare Dataset

mutanti <- read.table('mutanti_pari.txt', header=TRUE)
head(mutanti)	
str(mutanti)
	
	# 1.	Calcolare gli indici di posizione della variabile Forza 

summary(mutanti$forza)

	# 2.	Rappresentare l’istogramma della Forza con classi (intervalli) di uguale lunghezza (50) nell’intervallo (0,600) (attenzione: 0 e 600 non sono il minimo e il massimo della variabile). Indicare le 5 classi con la maggiore frequenza assoluta e riportare il valore di tale frequenza. Qual è la classe modale? 
	
istogramma <- hist(mutanti$forza, prob=TRUE, breaks=seq(0,600,length=13))	

istogramma$counts
quinta_classe_freq_ass <- sort(istogramma$counts, decreasing=TRUE)[5]
cinque_classi <- which(istogramma$counts>=quinta_classe_freq_ass)
istogramma$mids[cinque_classi]

which.max(istogramma$counts)
istogramma$mids[which.max(istogramma$counts)]

	# 3.	Riportare la tabella delle frequenze relative della variabile categorica Mutante
f_ass <- table(mutanti$tipo)
f_rel <- prop.table(f_ass)
f_rel
	# 4.	Proporre una rappresentazione grafica della distribuzione della variabile Forza distinguendo in base alla categoria Tipo. Cosa si può concludere?
	
boxplot(mutanti$forza ~ mutanti$tipo)

# oppure

hist(mutanti$forza[which(mutanti$tipo=='M')], xlim=range(mutanti$forza), col="blue")	
hist(mutanti$forza[which(mutanti$tipo=='NM')], xlim=range(mutanti$forza), add=TRUE, col="red")
	
	# 5.	Calcolare la correlazione tra la Forza e la Lunghezza dei capelli per il dataset completo.
cor(mutanti$forza, mutanti$capelli)

	# 6.	Calcolare la correlazione tra la Forza e la Lunghezza dei capelli distinguendo tra Mutanti e Non mutanti. Commentare i risultati.

mut <- mutanti[which(mutanti$tipo=='M'),]
non_mut <- mutanti[which(mutanti$tipo=='NM'),]

cor(mut$forza, mut$capelli)
cor(non_mut$forza, non_mut$capelli)	
	
	# 7.	Analizzare la normalità dei dati sulla lunghezza del Braccio.
	
hist(mutanti$braccio, prob=TRUE)
lines(sort(mutanti$braccio), dnorm(sort(mutanti$braccio), mean(mutanti$braccio), sd(mutanti$braccio)))

qqnorm(mutanti$braccio)
qqline(mutanti$braccio)

shapiro.test(mutanti$braccio) # ok normali


	# 8.	Costruire un modello di regressione completo per spiegare il legame lineare tra la Forza e tutte le altre variabili (Braccio, Lunghezza dei capelli e la variabile categorica tipo=M or NM).  Effettuare l’opportuna selezione dei predittori influenti. Commentare la significatività  del modello finale e darne un’interpretazione alla luce dell’ipotesi del CISM.
	
forza <- mutanti$forza
capelli <- mutanti$capelli
braccio <- mutanti$braccio
tipo <- as.numeric(mutanti$tipo)-1 # 0=mutante, 1=non_mutanta
	
mod <- lm(forza ~ capelli + braccio + tipo + capelli:tipo + braccio:tipo)
summary(mod)

mod_2 <- lm(forza ~ capelli + tipo + capelli:tipo + braccio:tipo)
summary(mod_2)

mod_3 <- lm(forza ~ capelli + tipo + capelli:tipo)
summary(mod_3)

mod_def <- mod_3
mod_def$coefficients

int_mut <- mod_def$coefficients[1]
int_non_mut <- mod_def$coefficients[1] + mod_def$coefficients[3]
int_mut
int_non_mut

c_ang_mut <- mod_def$coefficients[2]
c_ang_non_mut <- mod_def$coefficients[2]+mod_def$coefficients[4] 
c_ang_mut
c_ang_non_mut	
	
	# 9.	Analizzare i residui (standardizzati) del modello finale calcolato al punto 9.
	
res_std <- mod_def$res/summary(mod_def)$sigma	

plot(mod_def$fitted.values, res_std)	
abline(h=0)
abline(h=2)
abline(h=-2)

hist(res_std, prob=TRUE)
lines(sort(res_std), dnorm(sort(res_std), 0, 1))

qqnorm(res_std)
qqline(res_std)

shapiro.test(res_std)

	# 10.	Prevedere sia puntualmente che con un intervallo di  livello 90% il nuovo valore della Forza di un individuo Mutante con le seguenti caratteristiche: Lunghezza braccio = 45 cm e Lunghezza capelli = 35 cm

nuovo_dato <- data.frame(tipo=0, braccio=45, capelli=35)
predict(mod_def, newdata=nuovo_dato, interval='prediction', level=0.9)