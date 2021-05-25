library(xtable)

#### Einlesen und kürzen #####
# Umbenennen des eingelesenen Datensatzes
Jonas <- ds

#Erhebungszeitrum
time <- Jonas$STARTED[Jonas$MAXPAGE == 10]
min(time)
max(time)
rm(time)

#Alles raus außer Demografisches, aber nur vollständige
data.Jonas.Stichprobe <- Jonas[,-c(1:5, 15:32, 35:50, 52:55)]
data.Jonas.Stichprobe <- data.Jonas.Stichprobe[data.Jonas.Stichprobe$MAXPAGE == 10,]


data.Nadine.Stichprobe <- Nadine[,-c(1,2)]
data.Nadine.Stichprobe <- data.Nadine.Stichprobe[complete.cases(data.Nadine$Nadine.CK18),]


##### Ausgabe #####
#Geschlecht
table(data.Jonas.Stichprobe[,1])
#Abitur
table(data.Jonas.Stichprobe[,2])
#Abiturfach
table(data.Jonas.Stichprobe[,4])
table(data.Jonas.Stichprobe[,3])

#letzte Chemiestunde
table(data.Jonas.Stichprobe[,5])[1:5]

#Noch ein Studium
table(data.Jonas.Stichprobe[,6])
table(data.Jonas.Stichprobe[,7])


tab <- table(data.Jonas.Stichprobe[,7])
tab <- tab[which(tab != 0)]
list <- rownames(tab)
list[4] <- "B.Sc. Biochemie"
rownames(tab) <- list
rm(list)

print(xtable(tab),
      floating=FALSE,
      latex.environments=NULL,
      booktabs = T,
      file="/Users/jonastrautner/Documents/Studium/Zula/Arbeit/Tabellen/Stichprobe_Studiengang.tex")


table(data.Jonas.Stichprobe[,8])

table(subset(data.Jonas.Stichprobe$D001, data.Jonas.Stichprobe$D007 == "B.Sc. Chemie"))

