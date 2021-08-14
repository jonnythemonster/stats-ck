### Packages ####
library(psych) #für alpha()
library(car) #für leveneTest()
library(lawstat) #für levene.test()
library(effectsize) #für cohens_d
library(dplyr)
library(rstatix) # für Wilcoxon Effekgröße
library(coin)
library(ggsignif)
library(tikzDevice)
library(xtable)
### Daten einlesen ####
## Import-Dateien durchlaufen lassen und jeweils ds in anderen Datensatz überschreiben
Jonas <- ds
rm(ds)
rm(as.data.frame.avector)
rm("[.avector")

Nadine <- ds
rm(ds)
rm(as.data.frame.avector)
rm("[.avector")

Nadine.R <- ds
rm(ds)
rm(as.data.frame.avector)
rm("[.avector")


#### Datenaufbereitung #####
# Jonas 
FS <- rep(1,61)
SG <- rep(NA, 61)

#neuer Datensatz mit relevanten Informationen für Auswertung
data.Jonas <- data.frame(Jonas$D001, Jonas$D002, Jonas$D003, Jonas$D004, Jonas$D005, Jonas$D006, 
                         Jonas$D006_02, Jonas$D007, Jonas$D007_06,
                         Jonas$CK01, Jonas$CK02, Jonas$CK03, Jonas$CK04, Jonas$CK05, Jonas$CK06, 
                         Jonas$CK07, Jonas$CK08, Jonas$CK09, Jonas$CK10, Jonas$CK11, Jonas$CK12, 
                         Jonas$CK13, Jonas$CK14, Jonas$CK15, Jonas$CK16, Jonas$CK17, Jonas$CK18, FS, SG)

#Kodierung des Studiengangs
data.Jonas$SG[which(data.Jonas$Jonas.D007=="B.Sc. Chemie")] <- 1
data.Jonas$SG[which(data.Jonas$Jonas.D007=="Sonstiges:")] <- 2
data.Jonas$SG[which(data.Jonas$Jonas.D007=="Lehramt Gymnasium")] <- 3
data.Jonas$SG[which(data.Jonas$Jonas.D007=="Lehramt Realschule")] <- 4
data.Jonas$SG[which(data.Jonas$Jonas.D007=="Lehramt Hauptschule")] <- 5
data.Jonas$SG[which(data.Jonas$Jonas.D007=="Lehramt Grundschule")] <- 6

data.J <- data.Jonas[,-c(1:9)]
names(data.J) <- c("CK01", "CK02", "CK03", "CK04", "CK05", "CK06", 
                   "CK07", "CK08", "CK09", "CK10", "CK11", "CK12", 
                   "CK13", "CK14", "CK15", "CK16", "CK17", "CK18", "FS", "SG")

#Übertragung der ID auf Zeilennamen
row.names(data.J) <- data.Jonas$Jonas.SERIAL

# Nadien Rgbg
SG <- rep(NA, 58)
FS <- rep(NA, 58)
data.Nadine <- data.frame(Nadine$SD06, Nadine$SD05,
                          Nadine$CK01, Nadine$CK02, Nadine$CK03, Nadine$CK04, Nadine$CK05, Nadine$CK06, 
                          Nadine$CK07, Nadine$CK08, Nadine$CK09, Nadine$CK10, Nadine$CK11, Nadine$CK12, 
                          Nadine$CK13, Nadine$CK14, Nadine$CK15, Nadine$CK16, Nadine$CK17, Nadine$CK18, FS, SG)

data.Nadine$FS <- data.Nadine$Nadine.SD06
data.Nadine$SG[which(data.Nadine$Nadine.SD05=="Gymnasium")] <- 3
data.Nadine$SG[which(data.Nadine$Nadine.SD05=="Realschule")] <- 4
data.Nadine$SG[which(data.Nadine$Nadine.SD05=="Mittelschule")] <- 5
data.Nadine$SG[which(data.Nadine$Nadine.SD05=="Grundschule")] <- 6

data.N <- data.Nadine[,-c(1:2)]
names(data.N) <- c("CK01", "CK02", "CK03", "CK04", "CK05", "CK06", 
                   "CK07", "CK08", "CK09", "CK10", "CK11", "CK12", 
                   "CK13", "CK14", "CK15", "CK16", "CK17", "CK18", "FS", "SG")

# Nadine Rest
Nadine.R <- Nadine.R[order(Nadine.R$QUESTNNR),]
data <- data.frame(Nadine.R$SD06, Nadine.R$SD05, 
                   Nadine.R$CK01, Nadine.R$CK02, Nadine.R$CK03, Nadine.R$CK04, Nadine.R$CK05, Nadine.R$CK06, 
                   Nadine.R$CK07, Nadine.R$CK08, Nadine.R$CK09, Nadine.R$CK10, Nadine.R$CK11, Nadine.R$CK12, 
                   Nadine.R$CK13, Nadine.R$CK14, Nadine.R$CK15, Nadine.R$CK16, Nadine.R$CK17, Nadine.R$CK18)
data$code <- paste(Nadine.R$SD01_01, Nadine.R$SD01_02, Nadine.R$SD01_03, sep="")

dup <- which(duplicated(data$code)==T)

pos1 <- c()
pos2 <- c()
for(i in dup){
  pos1[i] <- which(data$code==data$code[i]) ### Warnings sind ok
  pos2[i] <- i
}
pos1 <- pos1[!is.na(pos1)]
pos2 <- pos2[!is.na(pos2)]

data[pos1,c(3:11)] <- data[pos2,3:11]
data <- data[pos1,]

SG <- rep(NA, 32)
FS <- rep(NA, 32)
data1 <- data.frame(data$Nadine.R.SD06, data$Nadine.R.SD05,
                    data$Nadine.R.CK01, data$Nadine.R.CK02, 
                    data$Nadine.R.CK03, data$Nadine.R.CK04, 
                    data$Nadine.R.CK05, data$Nadine.R.CK06,
                    data$Nadine.R.CK07, data$Nadine.R.CK08, 
                    data$Nadine.R.CK09, data$Nadine.R.CK10, 
                    data$Nadine.R.CK11, data$Nadine.R.CK12,
                    data$Nadine.R.CK13, data$Nadine.R.CK14, 
                    data$Nadine.R.CK15, data$Nadine.R.CK16, 
                    data$Nadine.R.CK17, data$Nadine.R.CK18, FS, SG)

data1$FS <- data$Nadine.R.SD06
data1$SG[which(data$Nadine.R.SD05=="Gymnasium")] <- 3
data1$SG[which(data$Nadine.R.SD05=="Realschule")] <- 4
data1$SG[which(data$Nadine.R.SD05=="Mittelschule")] <- 5
data1$SG[which(data$Nadine.R.SD05=="Grundschule")] <- 6
data1$SG[which(data$Nadine.R.SD05=="Berufsschule")] <- 7
data1$SG[which(data$Nadine.R.SD05=="Sonstiges:")] <- 8
data1$SG[which(is.na(data$Nadine.R.SD05))] <- -99
data1$FS[which(is.na(data$Nadine.R.SD06))] <- -99

data.NR <- data1[,-c(1:2)]
names(data.NR) <- c("CK01", "CK02", "CK03", "CK04", "CK05", "CK06", 
                   "CK07", "CK08", "CK09", "CK10", "CK11", "CK12", 
                   "CK13", "CK14", "CK15", "CK16", "CK17", "CK18", "FS", "SG")

#Entfernen aller nicht benötigten Daten
rm(dup, pos1, pos2, i, FS, SG, data1)

# Zusammenführen der Daten
data.gesamt <- rbind(data.J, data.N, data.NR)
data.gesamt <- data.gesamt[complete.cases(data.gesamt),]

##### Umkodierung ######
data.gesamt$R_CK01 <- NA
data.gesamt$R_CK02 <- NA
data.gesamt$R_CK03 <- NA
data.gesamt$R_CK04 <- NA
data.gesamt$R_CK05 <- NA
data.gesamt$R_CK06 <- NA
data.gesamt$R_CK07 <- NA
data.gesamt$R_CK08 <- NA
data.gesamt$R_CK09 <- NA
data.gesamt$R_CK10 <- NA
data.gesamt$R_CK11 <- NA
data.gesamt$R_CK12 <- NA
data.gesamt$R_CK13 <- NA
data.gesamt$R_CK14 <- NA
data.gesamt$R_CK15 <- NA
data.gesamt$R_CK16 <- NA
data.gesamt$R_CK17 <- NA
data.gesamt$R_CK18 <- NA

data.gesamt$R_CK01[which(data.gesamt$CK01==levels(data.gesamt$CK01)[4])] <- 1
data.gesamt$R_CK01[which(data.gesamt$CK01!=levels(data.gesamt$CK01)[4])] <- 0

data.gesamt$R_CK02[which(data.gesamt$CK02==levels(data.gesamt$CK02)[4])] <- 1
data.gesamt$R_CK02[which(data.gesamt$CK02!=levels(data.gesamt$CK02)[4])] <- 0

data.gesamt$R_CK03[which(data.gesamt$CK03==levels(data.gesamt$CK03)[4])] <- 1
data.gesamt$R_CK03[which(data.gesamt$CK03!=levels(data.gesamt$CK03)[4])] <- 0

data.gesamt$R_CK04[which(data.gesamt$CK04==levels(data.gesamt$CK04)[4])] <- 1
data.gesamt$R_CK04[which(data.gesamt$CK04!=levels(data.gesamt$CK04)[4])] <- 0

data.gesamt$R_CK05[which(data.gesamt$CK05==levels(data.gesamt$CK05)[8] | data.gesamt$CK05==levels(data.gesamt$CK05)[1] |data.gesamt$CK05==levels(data.gesamt$CK05)[2] | data.gesamt$CK05==levels(data.gesamt$CK05)[3])] <- 1
data.gesamt$R_CK05[which(data.gesamt$CK05!=levels(data.gesamt$CK05)[8] | data.gesamt$CK05==levels(data.gesamt$CK05)[1] |data.gesamt$CK05==levels(data.gesamt$CK05)[2] | data.gesamt$CK05==levels(data.gesamt$CK05)[3])] <- 0

data.gesamt$R_CK06[which(data.gesamt$CK06==levels(data.gesamt$CK06)[1])] <- 1
data.gesamt$R_CK06[which(data.gesamt$CK06!=levels(data.gesamt$CK06)[1])] <- 0

data.gesamt$R_CK07[which(data.gesamt$CK07==levels(data.gesamt$CK07)[2])] <- 1
data.gesamt$R_CK07[which(data.gesamt$CK07!=levels(data.gesamt$CK07)[2])] <- 0

data.gesamt$R_CK08[which(data.gesamt$CK08==levels(data.gesamt$CK08)[3])] <- 1
data.gesamt$R_CK08[which(data.gesamt$CK08!=levels(data.gesamt$CK08)[3])] <- 0

data.gesamt$R_CK09[which(data.gesamt$CK09==levels(data.gesamt$CK09)[1])] <- 1
data.gesamt$R_CK09[which(data.gesamt$CK09!=levels(data.gesamt$CK09)[1])] <- 0

data.gesamt$R_CK10[which(data.gesamt$CK10==levels(data.gesamt$CK10)[3])] <- 1
data.gesamt$R_CK10[which(data.gesamt$CK10!=levels(data.gesamt$CK10)[3])] <- 0

data.gesamt$R_CK11[which(data.gesamt$CK11==levels(data.gesamt$CK11)[2])] <- 1
data.gesamt$R_CK11[which(data.gesamt$CK11!=levels(data.gesamt$CK11)[2])] <- 0

data.gesamt$R_CK12[which(data.gesamt$CK12==levels(data.gesamt$CK12)[3])] <- 1
data.gesamt$R_CK12[which(data.gesamt$CK12!=levels(data.gesamt$CK12)[3])] <- 0

data.gesamt$R_CK13[which(data.gesamt$CK13==levels(data.gesamt$CK13)[3])] <- 1
data.gesamt$R_CK13[which(data.gesamt$CK13!=levels(data.gesamt$CK13)[3])] <- 0

data.gesamt$R_CK14[which(data.gesamt$CK14==levels(data.gesamt$CK14)[1])] <- 1
data.gesamt$R_CK14[which(data.gesamt$CK14!=levels(data.gesamt$CK14)[1])] <- 0

data.gesamt$R_CK15[which(data.gesamt$CK15==levels(data.gesamt$CK15)[3])] <- 1
data.gesamt$R_CK15[which(data.gesamt$CK15!=levels(data.gesamt$CK15)[3])] <- 0

data.gesamt$R_CK16[which(data.gesamt$CK16==levels(data.gesamt$CK16)[2])] <- 1
data.gesamt$R_CK16[which(data.gesamt$CK16!=levels(data.gesamt$CK16)[2])] <- 0

data.gesamt$R_CK17[which(data.gesamt$CK17==levels(data.gesamt$CK17)[2])] <- 1
data.gesamt$R_CK17[which(data.gesamt$CK17!=levels(data.gesamt$CK17)[2])] <- 0

data.gesamt$R_CK18[which(data.gesamt$CK18==levels(data.gesamt$CK18)[1])] <- 1
data.gesamt$R_CK18[which(data.gesamt$CK18!=levels(data.gesamt$CK18)[1])] <- 0



#### Trennschärfen #####
data.recode <- data.gesamt[,21:38]
iac <- alpha(data.recode)

#Trennschärfen über gesammten Fragebogen
trennschärfe <- iac$item.stats$r.drop
#Namen der Items
names(data.recode)
#Übersicht über alle Items
tr.data <- data.frame(names(data.recode), trennschärfe)

#Trennung in Atombau und Chemische Reaktion
data.cr <- data.recode[,1:9]
data.a <- data.recode[,10:18]

#Berechnung separater Trennschärfen
iac.cr <- alpha(data.cr)
iac.a <- alpha(data.a)
t.cr <- iac.cr$item.stats$r.drop
t.a <- iac.a$item.stats$r.drop

#Zusammenführen der einzelnen Testteile
t <- c(t.cr, t.a)

#Übersicht berechnet nach Themebereichen
tr.data.getrennt <- data.frame(names(data.recode), t)
tr.data.getrennt$names.data.recode.[which(tr.data.getrennt$t <.15)]
# -> "CK01" "CK02" "CK05" "CK06" "CK15" entfernen

### Itemschwierigkeit ###
itemsch <- proportions(sapply(data.gesamt[21:38], table),2)
itemsch_cr <- proportions(sapply(data.gesamt[21:29], table),2)
itemsch_a <- proportions(sapply(data.gesamt[30:38], table),2)
which(itemsch[2,] < 0.2)
# CK05 entfernen

namen.cr <- c("CK01", "CK02", "CK03", "CK04", "CK05", "CK06", "CK07", "CK08", "CK09")
namen.a <- c("CK10", "CK11", "CK12", "CK13", "CK14", "CK15", "CK16", "CK17", "CK18")
tr.latex <- data.frame(namen.cr, t.cr, itemsch_cr[2,], namen.a, t.a, itemsch_a[2,])
colnames(tr.latex) <- c("Items CR", "Trennschärfe", "Schwierigkeit", "Items A", "Trennschärfe", "Schwierigkeit")
tbl <- xtable(tr.latex, align = "ll|l|l|l|l|l", caption = "Trennschärfen getrennt nach Themenbereichen")
print.xtable(tbl,
             booktabs = TRUE,
             include.rownames=FALSE,
             caption.placement="top",
             latex.environments="raggedright",
             file="/Users/jonastrautner/Documents/Studium/Zula/Arbeit/Tabellen/Trennscharfe.tex")




### Testfassung ###
data.test.gesamt <- data.recode[-c(1,2,5,6,15,19)]


##### Reliabilität #####
#Cronbachs alpha für gesamten
alpha <- alpha(data.test.gesamt)
#Gesamtwert für Cronbachs Alpha
cronbachs <- alpha[["total"]][["raw_alpha"]]



##### Studierendenvergleich #####
data.test.sg <- data.gesamt[c("FS","SG", "R_CK03", "R_CK04", "R_CK06", "R_CK07", 
                              "R_CK08", "R_CK09", "R_CK10", "R_CK11", "R_CK12",
                              "R_CK13", "R_CK14", "R_CK15", "R_CK16", "R_CK17", 
                              "R_CK18")]

#Entfernen von Nadines Daten
data.test.sg <- data.test.sg[which(data.test.sg$FS==1),]
data.test.sg <- data.test.sg[which(data.test.sg$SG==1 | data.v1$SG==3),]
#Tennen nach Studiengängen
data.test.gym <- data.test.sg[which(data.test.sg$SG==3),]
data.test.bsc <- data.test.sg[which(data.test.sg$SG==1),]

#Entfernen von SG und FG
#data.test.sg <- data.test.sg[-c(1,2)]
#data.test.gym <- data.test.gym[-c(1,2)]
#data.test.bsc <- data.test.bsc[-c(1,2)]

data.test.sg$score <- rowSums(data.test.sg[3:17])
data.test.gym$score <- rowSums(data.test.gym[3:17])
data.test.bsc$score <- rowSums(data.test.bsc[3:17])

#data.vergleich <- data.frame(data.recode$score, data.gesamt$FS, data.gesamt$SG)
#names(data.vergleich) <- c("score", "FS", "SG")

#data.v1 <- data.vergleich[which(data.vergleich$FS==1),]
#data.v2 <- data.vergleich[which(data.vergleich$FS==2),]

#table(data.vergleich$SG)

#data.sg <- data.v1[which(data.v1$SG==1 | data.v1$SG==3),]

# Varianzhomogenität -> gegeben
lev <- levene.test(data.test.sg$score, data.test.sg$SG)
p_lev <- lev[["non.bootstrap.p.value"]]
rm(lev)

#Ausreiser in Lehramt? -> nein
boxplot(data.test.gym$score)
boxplot(data.test.bsc$score)

summary(data.test.bsc$score)
sd(data.test.bsc$score)
summary(data.test.gym$score)
sd(data.test.gym$score)

#Bestimmung Stichprobengröße
length(data.test.gym$score)
length(data.test.bsc$score)

#Test auf Normalverteilung
shapiro.test(data.test.gym$score)
# -> Normalverteilung
shapiro.test(data.test.bsc$score)
# -> Normalverteilung

#t-Test: aber keine Intervallskalierung gegeben
t_test <-  t.test(data.test.sg$score~data.test.sg$SG, var.equal = T)
p_SG_V_tT <- t_test[["p.value"]]
rm(t_test)
#Effekstärke
cohens_d(data.test.bsc$score, data.test.gym$score)

#Wilcoxon-Test: non-parametrisch
wil <- wilcox.test(data.test.sg$score~data.test.sg$SG, paired=FALSE, 
                   exact = TRUE, conf.level = .95, conf.int = TRUE)
p_SG_V_MWU <- wil[["p.value"]]
rm(wil)
#Effekstärke auch für Wilcoxon
wil_d <- wilcox_effsize(data.test.sg, score~SG)
p_wil_d <- wil_d[4]


#Boxplot
plot <- data.frame(
  Score = c(data.test.bsc$score, data.test.gym$score),
  Studiengang = c(rep("B. Sc.", length(data.test.bsc$score)), rep("Lehramt", length(data.test.gym$score)))
)

#unten weil, sonst kein alpha -> psych wird maskiert
library(ggplot2) 

# here we open a tex file for output, and set the plots dimensions
tikzDevice::tikz(file = "/Users/jonastrautner/Documents/Studium/Zula/Arbeit/Abbildungen/SB_boxplot.tex", width = 5, height = 3)

ggplot(data = plot, aes(x=Studiengang, y=Score, fill=Studiengang))+
  geom_boxplot()+ 
  theme_minimal() +
  geom_jitter(shape=16, position=position_jitter(0.1), alpha = .3)+
  scale_fill_manual(values=c("steelblue", "indianred"))+
  ylim(-0.1,20)+
  geom_signif(
    y_position = 18, xmin = 1, xmax = 2, 
    annotation = "*", tip_length = 0.0
  )+
  ylab("Score")

# closing the graphics device saves the file we opened with tikzDevice::tikz
dev.off()

#### Speichern R-Session ####
save.image("~/Documents/Studium/Zula/CK-Test R/workspace.RData")
