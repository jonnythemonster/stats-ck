### Packages ####
library(psych) #für alpha()
library(car) #für leveneTest()
library(lawstat) #für levene.test()
library(effectsize) #für cohens_d
library(dplyr)
library(coin)
library(ggsignif)
library(tikzDevice)
library(xtable)


### Daten einlesen ####
load("data_Jonas.Rdat")
load("data_Nadine.Rdat")
load("data_NR.Rdat")



#### Datenaufbereitung #####
# Jonas 
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

# Nadien Rgbg
data.Nadine$FS <- data.Nadine$Nadine.SD06
data.Nadine$SG[which(data.Nadine$Nadine.SD05=="Gymnasium")] <- 3
data.Nadine$SG[which(data.Nadine$Nadine.SD05=="Realschule")] <- 4
data.Nadine$SG[which(data.Nadine$Nadine.SD05=="Mittelschule")] <- 5
data.Nadine$SG[which(data.Nadine$Nadine.SD05=="Grundschule")] <- 6

data.N <- data.Nadine[,-c(1:2)]
names(data.N) <- c("CK01", "CK02", "CK03", "CK04", "CK05", "CK06", 
                   "CK07", "CK08", "CK09", "CK10", "CK11", "CK12", 
                   "CK13", "CK14", "CK15", "CK16", "CK17", "CK18", "FS", "SG")

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



#### Itemschwierigkeit ####
itemsch <- proportions(sapply(data.gesamt[21:38], table),2)
itemsch_cr <- proportions(sapply(data.gesamt[21:29], table),2)
itemsch_a <- proportions(sapply(data.gesamt[30:38], table),2)
which(itemsch[2,] < 0.2)
# CK05 entfernen



#### Testfassung ####
data.test.gesamt <- data.recode[-c(1,2,5,6,15,19)]



##### Reliabilität #####
#Cronbachs alpha für gesamten
alpha(data.test.gesamt)



##### Studierendenvergleich #####
data.test.sg <- data.gesamt[c("FS","SG", "R_CK03", "R_CK04", "R_CK06", "R_CK07", 
                              "R_CK08", "R_CK09", "R_CK10", "R_CK11", "R_CK12",
                              "R_CK13", "R_CK14", "R_CK15", "R_CK16", "R_CK17", 
                              "R_CK18")]

#Entfernen von Nadines Daten
data.test.sg <- data.test.sg[which(data.test.sg$FS==1),]
data.test.sg <- data.test.sg[which(data.test.sg$SG==1 | data.test.sg$SG==3),]
#Tennen nach Studiengängen
data.test.gym <- data.test.sg[which(data.test.sg$SG==3),]
data.test.bsc <- data.test.sg[which(data.test.sg$SG==1),]

data.test.sg$score <- rowSums(data.test.sg[3:17])
data.test.gym$score <- rowSums(data.test.gym[3:17])
data.test.bsc$score <- rowSums(data.test.bsc[3:17])

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
wilcox.test(data.test.sg$score~data.test.sg$SG, paired=FALSE, 
                   exact = TRUE, conf.level = .95, conf.int = TRUE)

#Effekstärke auch für Wilcoxon
library(rstatix) # für Wilcoxon Effekgröße
wilcox_effsize(data.test.sg, score~SG)


#Boxplot
plot <- data.frame(
  Score = c(data.test.bsc$score, data.test.gym$score),
  Studiengang = c(rep("B. Sc.", length(data.test.bsc$score)), rep("Lehramt", length(data.test.gym$score)))
)

#unten weil, sonst kein alpha -> psych wird maskiert
library(ggplot2) 

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