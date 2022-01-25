###############################
#####    CORSO DI FSSB    #####
## per INGEGNERIA BIOMEDICA ###
###############################


############################################
###  ESERCIZI LABORATORIO 1 - SOLUZIONE  ###
############################################

## ESERCIZIO 3

# Analisi dei dati categorici
# 'Esposizione ai pesticidi'

pesticide <- c('O','O','J','O','J','O','F','O','F','O','N','F','J','J','F','J','O',
               'J','O','N','C','O','F','O','F','N','N','B','B','O','O','N','B','N','B',
               'C','F','J','M','O','O','F','O','O','J','J','J','O','O','B','M','M','O',
               'O','O','B','M','C','B','F')

# DOMANDE

# Creare le tabelle di frequenza assoluta e relativa di pesticide

# Visualizzare mediante opportuni grafici il dataset

# Estrarre il sottocampione composto dai pesticidi di tipo 'O' e 'J' 

# Eliminare dal campione tutti i pesticidi di tipo 'N' e 'M' e creare le tabelle di frequenza
# assoluta e relativa del nuovo campione

# SOLUZIONE

# Creare le tabelle di frequenza assoluta e relativa

pesticide <- as.factor(pesticide)
freq_ass <- table(pesticide)
freq_ass

freq_rel <- freq_ass/length(pesticide)
freq_rel

# Visualizzare mediante opportuni grafici il dataset

barplot(freq_ass, col=rainbow(length(levels(pesticide))), main='pesticidi')

barplot(prop.table(freq_ass), col=rainbow(length(levels(pesticide))), main='pesticidi')

# nel primo grafico (senza prop.table) l'altezza di ogni rettangolo è pari alla 
# frequenza assoluta; introducendo, invece, prop.table si visualizzano le frequenze
# relative.

pie(freq_ass, col=rainbow(length(levels(pesticide))), main='pesticidi')

# Estrarre il sottocampione composto dai pesticidi di tipo 'O' e 'J' 

gruppo_OJ = pesticide[which(pesticide == "O" | pesticide == "J")]
gruppo_OJ


# Eliminare dal campione tutti i pesticidi di tipo 'N' e 'M' e creare le tabelle di frequenza
# assoluta e relativa del nuovo campione

gruppo_noMN = pesticide[-which(pesticide == "N" | pesticide == "M")]
gruppo_noMN


table(gruppo_noMN)
table(gruppo_noMN)/length(gruppo_noMN)

## oppure

prop.table(table(gruppo_noMN))

