## Ucitavanje podataka

Krediti <- read.table("putanja_do_german.txt", quote="\"", comment.char="")
View(Krediti)

## Imenovanje stupaca

colnames(Krediti)[1] <- "Status tekuæeg raèuna"
colnames(Krediti)[2] <- "Trajanje u mjesecima"
colnames(Krediti)[3] <- "Kreditna povijest"
colnames(Krediti)[4] <- "Svrha kredita"
colnames(Krediti)[5] <- "Iznos kredita"
colnames(Krediti)[6] <- "Štednja"
colnames(Krediti)[7] <- "Trajanje zaposlenja"
colnames(Krediti)[8] <- "Stopa otplate primanja"
colnames(Krediti)[9] <- "Braèni status i Spol"
colnames(Krediti)[10] <- "Dužnici/Jamci"
colnames(Krediti)[11] <- "Trenutno prebivalište od"
colnames(Krediti)[12] <- "Imovina"
colnames(Krediti)[13] <- "Starost"
colnames(Krediti)[14] <- "Obroèni planovi"
colnames(Krediti)[15] <- "Stanovanje"
colnames(Krediti)[16] <- "Broj kredita u banci"
colnames(Krediti)[17] <- "Posao"
colnames(Krediti)[18] <- "Broj jamaca"
colnames(Krediti)[19] <- "Telefon"
colnames(Krediti)[20] <- "Strani radnik"
colnames(Krediti)[21] <- "Matrica troškova"

## Pridruzivanje skupa podataka uz pomoc naredbe attach

attach(Krediti)


## Pretvaranje varijable u faktor

`Status tekuæeg raèuna` <- as.factor(`Status tekuæeg raèuna`)
`Kreditna povijest` <- as.factor(`Kreditna povijest`)
`Svrha kredita` <- as.factor(`Svrha kredita`)
Štednja <- as.factor(Štednja)
`Trajanje zaposlenja` <- as.factor(`Trajanje zaposlenja`)
`Braèni status i Spol` <-as.factor(`Braèni status i Spol`)
`Dužnici/Jamci` <- as.factor(`Dužnici/Jamci`)
Imovina <- as.factor(Imovina)
`Obroèni planovi` <- as.factor(`Obroèni planovi`)
Stanovanje <- as.factor(Stanovanje)
Posao <- as.factor(Posao)
Telefon <- as.factor(Telefon)
`Strani radnik` <- as.factor(`Strani radnik`)
`Matrica troškova` <- as.factor(`Matrica troškova`)


## Zadatak a)

graf1 <- table(`Status tekuæeg raèuna`)
barplot(graf1, col="coral2", main = "Status tekuæeg raèuna", ylab = "Frekvencija", xlab = "Status tekuæeg raèuna")

graf2 <- hist(`Trajanje u mjesecima`, col = "cornflowerblue", main = "Histogram\nTrajanje u mjesecima", ylab = "Frekvencija")

graf3 <- table(`Kreditna povijest`)
pie(graf3, main = "Kreditna povijest")

graf4 <- table(`Svrha kredita`)
barplot(graf4, col= "orange", main = "Svrha kredita", ylab = "Frekvencija", xlab = "Svrha kredita")

graf5 <- hist(`Iznos kredita`, col = "brown", main = "Histogram\nIznos kredita", ylab = "Frekvencija")

graf6 <- table(Štednja)
barplot(graf6, col = "#3F612D", main = "Štednja", ylab = "Frekvencija", xlab = "Štednja")

graf7 <- table(`Trajanje zaposlenja`)
barplot(graf7, col = "#7952B3", main = "Trajanje zaposlenja", ylab = "Frekvencija", xlab = "Trajanje zaposlenja")

graf8 = boxplot(`Stopa otplate primanja`, main = "Boxplot\nStopa rata u % raspoloživog dohotka")

graf9 = table(`Braèni status i Spol`)
pie(graf9, main = "Braèni status i Spol")

graf10 = table(`Dužnici/Jamci`)
pie(graf10, main = "Dužnici / Jamci")

graf11 <- boxplot(`Trenutno prebivalište od`, main = "Boxplot\nTrenutno prebivalište od")

graf12 <- table(Imovina)
barplot(graf12, horiz = "TRUE", main = "Imovina", ylab = "Frekvencija", xlab = "Imovina", col = "#01937C")

graf13 <- hist(Starost, col = "#F54748", main = "Histogram\nStarost", ylab = "Frekvencija")

graf14 <- table(`Obroèni planovi`)
barplot(graf14, col = "#1EAE98", main = "Obroèni planovi", ylab = "Frekvencija", xlab = "Obroèni planovi")

graf15 <- table(Stanovanje)
barplot(graf15, col = "#C67ACE", horiz = "TRUE", main = "Stanovanje", ylab = "Frekvencija", xlab = "Stanovanje")

graf16 <- hist(`Broj kredita u banci`, main = "Histogram\nBroj kredita u banci", ylab = "Frekvencija", col = "#558776")

graf17 <- table(Posao)
barplot(graf17, col = "#FFB037", main = "Posao", ylab = "Frekvencija", xlab = "Posao")

graf18 <- hist(`Broj jamaca`, breaks = 3, main = "Histogram\nBroj jamaca", ylab = "Frekvencija", col = "#BF1363")

graf19 <- table(Telefon)
barplot(graf19, col = "#301B3F", horiz = TRUE, main = "Telefon", ylab = "Frekvencija", xlab = "Telefon")

graf20 <- table(`Strani radnik`)
pie(graf20, main = "Strani radnik")

graf21 <- table(`Matrica troškova`)
barplot(graf21, col = "#00AF91", main = "Matrica troškova", ylab = "Frekvencija", xlab = "Matrica troškova")

##Zadatak b)

##install.packages("report")

library("report")
library(corrplot)

numericke <- Krediti[ ,c(2, 5, 8, 11, 13, 16, 18)]
round(cor(numericke), digits = 4)


corrplot(cor(numericke),
         method = "color",
         addCoef.col="grey",
         type = "upper"
)



test1 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$`Iznos kredita`)
plot(numericke$`Trajanje u mjesecima`, numericke$`Iznos kredita`, xlab = "Trajanje u mjesecima", ylab = "Iznos kredita")
test1
report(test1)

test2 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$`Stopa otplate primanja`)
plot(numericke$`Trajanje u mjesecima`, numericke$`Stopa otplate primanja`, xlab = "Trajanje u mjesecima", ylab = "Stopa otplate prema primanjima")
test2
report(test2)

test3 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$`Trenutno prebivalište od`)
plot(numericke$`Trajanje u mjesecima`, numericke$`Trenutno prebivalište od`, xlab = "Trajanje u mjesecima", ylab = "Trajanje trenutnog prebivališta")
test3
report(test3)

test4 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$Starost)
plot(numericke$`Trajanje u mjesecima`, numericke$Starost, xlab = "Trajanje u mjesecima", ylab = "Starost")
test4
report(test4)

test5 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$`Broj kredita u banci`)
plot(numericke$`Trajanje u mjesecima`, numericke$`Broj kredita u banci`, xlab = "Trajanje u mjesecima", ylab = "Broj kredita u banci")
test5
report(test5)

test6 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$`Broj jamaca`)
plot(numericke$`Trajanje u mjesecima`, numericke$`Broj jamaca`, xlab = "Trajanje u mjesecima", ylab = "Broj jamaca")
test6
report(test6)

test7 <- cor.test(numericke$`Iznos kredita`, numericke$`Stopa otplate primanja`)
plot(numericke$`Iznos kredita`, numericke$`Stopa otplate primanja`, xlab = "Iznos kredita", ylab = "Stopa otplate prema primanjima")
test7
report(test7)

test8 <- cor.test(numericke$`Iznos kredita`, numericke$`Trenutno prebivalište od`)
plot(numericke$`Iznos kredita`, numericke$`Trenutno prebivalište od`, xlab = "Iznos kredita", ylab = "Trajanje trenutnog prebivališta")
test8
report(test8)

test9 <- cor.test(numericke$`Iznos kredita`, numericke$Starost)
plot(numericke$`Iznos kredita`, numericke$Starost, xlab = "Iznos kredita", ylab = "Starost dužnosnika")
test9
report(test9)

test10 <- cor.test(numericke$`Iznos kredita`, numericke$`Broj kredita u banci`)
plot(numericke$`Iznos kredita`, numericke$`Broj kredita u banci`, xlab = "Iznos kredita", ylab = "Broj kredita u banci")
test10
report(test10)

test11 <- cor.test(numericke$`Iznos kredita`, numericke$`Broj jamaca`)
plot(numericke$`Iznos kredita`, numericke$`Broj jamaca`, xlab = "Iznos kredita", ylab = "Broj jamaca")
test11
report(test11)

test12 <- cor.test(numericke$`Stopa otplate primanja`, numericke$`Trenutno prebivalište od`)
plot(numericke$`Stopa otplate primanja`, numericke$`Trenutno prebivalište od`, xlab = "Stopa otplate prema primanjima", ylab = "Trajanje trenutnog prebivališta")
test12
report(test12)

test13 <- cor.test(numericke$`Stopa otplate primanja`, numericke$Starost)
plot(numericke$`Stopa otplate primanja`, numericke$Starost, xlab = "Stopa otplate prema primanjima", ylab = "Starost dužnosnika")
test13
report(test13)

test14 <- cor.test(numericke$`Stopa otplate primanja`, numericke$`Broj kredita u banci`)
plot(numericke$`Stopa otplate primanja`, numericke$`Broj kredita u banci`, xlab = "Stopa otplate prema primanjima", ylab = "Broj kredita u banci")
test14
report(test14)

test15 <- cor.test(numericke$`Stopa otplate primanja`, numericke$`Broj jamaca`)
plot(numericke$`Stopa otplate primanja`, numericke$`Broj jamaca`, xlab = "Stopa otplate prema primanjima", ylab = "Broj jamaca")
test15
report(test15)

test16 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$Starost)
plot(numericke$`Trajanje u mjesecima`, numericke$Starost, xlab = "Trajanje trenutnog prebivališta", ylab = "Starost dužnosnika")
test16
report(test16)

test17 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$`Broj kredita u banci`)
plot(numericke$`Trajanje u mjesecima`, numericke$`Broj kredita u banci`, xlab = "Trajanje trenutnog prebivališta", ylab = "Broj kredita u banci")
test17
report(test17)

test18 <- cor.test(numericke$`Trajanje u mjesecima`, numericke$`Broj jamaca`)
plot(numericke$`Trajanje u mjesecima`, numericke$`Broj jamaca`, xlab = "Trajanje trenutnog prebivališta", ylab = "Broj jamaca")
test18
report(test18)

test19 <- cor.test(numericke$Starost, numericke$`Broj kredita u banci`)
plot(numericke$Starost, numericke$`Broj kredita u banci`, xlab = "Starost dužnosnika", ylab = "Broj kredita u banci")
test19
report(test19)

test20 <- cor.test(numericke$Starost, numericke$`Broj jamaca`)
plot(numericke$Starost, numericke$`Broj jamaca`, xlab = "Starost dužnosnika", ylab = "Broj jamaca")
test20
report(test20)

test21 <- cor.test(numericke$`Broj kredita u banci`, numericke$`Broj jamaca`)
plot(numericke$Starost, numericke$`Broj kredita u banci`, xlab = "Broj kredita u banci", ylab = "Broj jamaca")
test21
report(test21)


##Zadatak c)

## Instaliraj prvo RTools: https://www.youtube.com/watch?v=njZBf34Akgo
## Postupak kako instalirati potrebne biblioteke za zadatak c): http://www.sthda.com/english/wiki/normality-test-in-r
## install.packages("ellipsis")
## install.packages("ggpubr", dependencies = TRUE)
## library("ggpubr")

shapiro.test(numericke$`Trajanje u mjesecima`)
ggdensity(numericke$`Trajanje u mjesecima`,ylab = "Gustoæa", xlab = "Trajanje u mjesecima", main = "Gustoæa za\nTrajanje u mjesecima")
ggqqplot(numericke$`Trajanje u mjesecima`, main = "Quantile-Quantile Plot")

shapiro.test(numericke$`Iznos kredita`)
ggdensity(numericke$`Iznos kredita`, ylab = "Gustoæa", xlab = "Iznos kredita", main = "Gustoæa za\nIznos kredita")
ggqqplot(numericke$`Iznos kredita`, main = "Quantile-Quantile Plot")

shapiro.test(numericke$`Stopa otplate primanja`)
ggdensity(numericke$`Stopa otplate primanja`, ylab = "Gustoæa", xlab = "Stopa rata u % raspoloživog dohotka", main = "Gustoæa za\nStopu rata u % raspoloživog dohotka")
ggqqplot(numericke$`Stopa otplate primanja`, main = "Quantile-Quantile Plot")

shapiro.test(numericke$`Trenutno prebivalište od`)
ggdensity(numericke$`Trenutno prebivalište od`, ylab = "Gustoæa", xlab = "Trenutno prebivalište od", main = "Gustoæa za\nTrenutno prebivalište od")
ggqqplot(numericke$`Trenutno prebivalište od`, main = "Quantile-Quantile Plot")

shapiro.test(numericke$Starost)
ggdensity(numericke$Starost, ylab = "Gustoæa", xlab = "Starost", main = "Gustoæa za\nStarost")
ggqqplot(numericke$Starost, main = "Quantile-Quantile Plot")

shapiro.test(numericke$`Broj kredita u banci`)
ggdensity(numericke$`Broj kredita u banci`, ylab = "Gustoæa", xlab = "Broj kredita u banci", main = "Gustoæa za\nBroj kredita u banci")
ggqqplot(numericke$`Broj kredita u banci`, main = "Quantile-Quantile Plot")

shapiro.test(numericke$`Broj jamaca`)
ggdensity(numericke$`Broj jamaca`, ylab = "Gustoæa", xlab = "Broj jamaca", main = "Gustoæa za\nBroj Jamaca")
ggqqplot(numericke$`Broj jamaca`, main = "Quantile-Quantile Plot")


##Zadatak d)


## Provjera uvjeta za provedbu jednofaktorske analize

##Priprema podskupova podataka

SvrhaKreditaA40 <- subset(Krediti, subset = `Svrha kredita`=="A40")
SvrhaKreditaA41 <- subset(Krediti, subset = `Svrha kredita`=="A41")
SvrhaKreditaA42 <- subset(Krediti, subset = `Svrha kredita`=="A42")
SvrhaKreditaA43 <- subset(Krediti, subset = `Svrha kredita`=="A43")
SvrhaKreditaA44 <- subset(Krediti, subset = `Svrha kredita`=="A44")
SvrhaKreditaA45 <- subset(Krediti, subset = `Svrha kredita`=="A45")
SvrhaKreditaA46 <- subset(Krediti, subset = `Svrha kredita`=="A46")
SvrhaKreditaA47 <- subset(Krediti, subset = `Svrha kredita`=="A47")
SvrhaKreditaA48 <- subset(Krediti, subset = `Svrha kredita`=="A48")
SvrhaKreditaA49 <- subset(Krediti, subset = `Svrha kredita`=="A49")
SvrhaKreditaA410 <- subset(Krediti, subset = `Svrha kredita`=="A410")

PosaoA171 <- subset(Krediti, subset = Posao=="A171")
PosaoA172 <- subset(Krediti, subset = Posao=="A172")
PosaoA173 <- subset(Krediti, subset = Posao=="A173")
PosaoA174 <- subset(Krediti, subset = Posao=="A174")

## Testiranje normalnosti razdiobe za svaki od podskupova

shapiro.test(SvrhaKreditaA40$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA41$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA42$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA43$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA44$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA45$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA46$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA47$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA48$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA49$`Trajanje u mjesecima`)
shapiro.test(SvrhaKreditaA410$`Trajanje u mjesecima`)


shapiro.test(PosaoA171$`Trajanje u mjesecima`)
shapiro.test(PosaoA172$`Trajanje u mjesecima`)
shapiro.test(PosaoA173$`Trajanje u mjesecima`)
shapiro.test(PosaoA174$`Trajanje u mjesecima`)


shapiro.test(SvrhaKreditaA40$`Iznos kredita`)
shapiro.test(SvrhaKreditaA41$`Iznos kredita`)
shapiro.test(SvrhaKreditaA42$`Iznos kredita`)
shapiro.test(SvrhaKreditaA43$`Iznos kredita`)
shapiro.test(SvrhaKreditaA44$`Iznos kredita`)
shapiro.test(SvrhaKreditaA45$`Iznos kredita`)
shapiro.test(SvrhaKreditaA46$`Iznos kredita`)
shapiro.test(SvrhaKreditaA47$`Iznos kredita`)
shapiro.test(SvrhaKreditaA48$`Iznos kredita`)
shapiro.test(SvrhaKreditaA49$`Iznos kredita`)
shapiro.test(SvrhaKreditaA410$`Iznos kredita`)


shapiro.test(PosaoA171$`Iznos kredita`)
shapiro.test(PosaoA172$`Iznos kredita`)
shapiro.test(PosaoA173$`Iznos kredita`)
shapiro.test(PosaoA174$`Iznos kredita`)


##  Izraèun varijance varijable "Trajanje u mjesecima" po modalitetima varijable "Svrha kredita" i "Posao"

tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Svrha kredita`, var)
tapply(Krediti$`Trajanje u mjesecima`, Krediti$Posao, var)

##  Izraèun varijance varijable "Iznos kredita" po modalitetima varijable "Svrha kredita" i "Posao"

tapply(Krediti$`Iznos kredita`, Krediti$`Svrha kredita`, var)
tapply(Krediti$`Iznos kredita`, Krediti$Posao, var)

## Bartlett-ov test

bartlett.test(`Trajanje u mjesecima` ~ `Svrha kredita`, data = Krediti)
bartlett.test(`Trajanje u mjesecima` ~ Posao, data = Krediti)

bartlett.test(`Iznos kredita` ~ `Svrha kredita`, data = Krediti)
bartlett.test(`Iznos kredita` ~ Posao, data = Krediti)

## Levenov test

library(car)
leveneTest(Krediti$`Trajanje u mjesecima`, Krediti$`Svrha kredita`, center = mean)
leveneTest(Krediti$`Trajanje u mjesecima`, Krediti$Posao, center = mean)

leveneTest(Krediti$`Iznos kredita`, Krediti$`Svrha kredita`, center = mean)
leveneTest(Krediti$`Iznos kredita`, Krediti$Posao, center = mean)


## Kruskal-Wallisov test

kruskal.test(`Trajanje u mjesecima` ~ `Svrha kredita`, data = Krediti)
kruskal.test(`Trajanje u mjesecima` ~ Posao, data = Krediti)

kruskal.test(`Iznos kredita` ~ `Svrha kredita`, data = Krediti)
kruskal.test(`Iznos kredita` ~ Posao, data = Krediti)


## Zadatak e)

shapiro.test(SvrhaKreditaA40$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA41$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA42$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA43$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA44$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA45$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA46$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA47$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA48$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA49$`Stopa otplate primanja`)
shapiro.test(SvrhaKreditaA410$`Stopa otplate primanja`)


bartlett.test(`Stopa otplate primanja` ~ `Svrha kredita`, data = Krediti)

## Rjesenje Bartlettovog testa: Prihvaæa se H0 hipoteza koja govori da su varijance ovih grupa meðusobno jedanke 

tapply(Krediti$`Stopa otplate primanja`, Krediti$`Svrha kredita`, var)

## Provedba analize varijance

boxplot(`Stopa otplate primanja` ~ `Svrha kredita`, data = Krediti, xlab = "Svrha kredita", ylab = "Stopa rata u % raspoloživog dohotka")


SvrhaKredita <- as.factor(`Svrha kredita`)

AnovaModel.1 <- aov(`Stopa otplate primanja` ~ SvrhaKredita, data = Krediti)
summary(AnovaModel.1)

## Zakljuèak: Provedbnom Anova testa dobili smo da je p = 0,0646, odnosno (p > 0,05) pa prema tome zakljuèujemo 
## da ne postoje signifikantne razlike meðu ovim grupama

## Prosjeène vrijednosti po klasama 
tapply(Krediti$`Stopa otplate primanja`, Krediti$`Svrha kredita`, mean)

## Standardne devijacije po klasama 
tapply(Krediti$`Stopa otplate primanja`, Krediti$`Svrha kredita`, sd)

library(multcomp)
TukeyHSD(AnovaModel.1)
.Pairs <- glht(AnovaModel.1, linfct = mcp(SvrhaKredita = "Tukey"))
.Pairs
confint(.Pairs)
cld(.Pairs)
plot(confint(.Pairs))

## Ne postoji signifikantna razlika meðu grupama


## Zadatak f)

TrajanjeZaposlenjaA71 <- subset(Krediti, subset = `Trajanje zaposlenja`=="A71")
TrajanjeZaposlenjaA72 <- subset(Krediti, subset = `Trajanje zaposlenja`=="A72")
TrajanjeZaposlenjaA73 <- subset(Krediti, subset = `Trajanje zaposlenja`=="A73")
TrajanjeZaposlenjaA74 <- subset(Krediti, subset = `Trajanje zaposlenja`=="A74")
TrajanjeZaposlenjaA75 <- subset(Krediti, subset = `Trajanje zaposlenja`=="A75")

BracniStatusISpol91 <- subset(Krediti, subset = `Braèni status i Spol` =="A91")
BracniStatusISpol92 <- subset(Krediti, subset = `Braèni status i Spol` =="A92")
BracniStatusISpol93 <- subset(Krediti, subset = `Braèni status i Spol` =="A93")
BracniStatusISpol94 <- subset(Krediti, subset = `Braèni status i Spol` =="A94")
BracniStatusISpol95 <- subset(Krediti, subset = `Braèni status i Spol` =="A95")

TB1 <- 01 ## A71 A91
TB2 <- 02 ## A71 A92
TB3 <- 03 ## A71 A93
TB4 <- 04 ## A71 A94
TB5 <- 05 ## A71 A95

TB6 <- 06 ## A72 A91
TB7 <- 07 ## A72 A92
TB8 <- 08 ## A72 A93
TB9 <- 09 ## A72 A94
TB10 <- 10 ## A72 A95

TB11 <- 11 ## A73 A91
TB12 <- 12 ## A73 A92
TB13 <- 13 ## A73 A93
TB14 <- 14 ## A73 A94
TB15 <- 15 ## A73 A95

TB16 <- 16 ## A74 A91
TB17 <- 17 ## A74 A92
TB18 <- 18 ## A74 A93
TB19 <- 19 ## A74 A94
TB20 <- 20 ## A74 A95

TB21 <- 21 ## A75 A91
TB22 <- 22 ## A75 A92
TB23 <- 23 ## A75 A93
TB24 <- 24 ## A75 A94
TB25 <- 25 ## A75 A95


Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A71" & Krediti$`Braeni status i Spol` == "A91"] <- TB1
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A71" & Krediti$`Braeni status i Spol` == "A92"] <- TB2
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A71" & Krediti$`Braeni status i Spol` == "A93"] <- TB3
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A71" & Krediti$`Braeni status i Spol` == "A94"] <- TB4
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A71" & Krediti$`Braeni status i Spol` == "A95"] <- TB5

Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A72" & Krediti$`Braeni status i Spol` == "A91"] <- TB6
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A72" & Krediti$`Braeni status i Spol` == "A92"] <- TB7
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A72" & Krediti$`Braeni status i Spol` == "A93"] <- TB8
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A72" & Krediti$`Braeni status i Spol` == "A94"] <- TB9
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A72" & Krediti$`Braeni status i Spol` == "A95"] <- TB10

Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A73" & Krediti$`Braeni status i Spol` == "A91"] <- TB11
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A73" & Krediti$`Braeni status i Spol` == "A92"] <- TB12
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A73" & Krediti$`Braeni status i Spol` == "A93"] <- TB13
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A73" & Krediti$`Braeni status i Spol` == "A94"] <- TB14
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A73" & Krediti$`Braeni status i Spol` == "A95"] <- TB15

Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A74" & Krediti$`Braeni status i Spol` == "A91"] <- TB16
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A74" & Krediti$`Braeni status i Spol` == "A92"] <- TB17
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A74" & Krediti$`Braeni status i Spol` == "A93"] <- TB18
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A74" & Krediti$`Braeni status i Spol` == "A94"] <- TB19
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A74" & Krediti$`Braeni status i Spol` == "A95"] <- TB20

Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A75" & Krediti$`Braeni status i Spol` == "A91"] <- TB21
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A75" & Krediti$`Braeni status i Spol` == "A92"] <- TB22
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A75" & Krediti$`Braeni status i Spol` == "A93"] <- TB23
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A75" & Krediti$`Braeni status i Spol` == "A94"] <- TB24
Krediti$TZaposBStatus[Krediti$`Trajanje zaposlenja` == "A75" & Krediti$`Braeni status i Spol` == "A95"] <- TB25


shapiro.test(TrajanjeZaposlenjaA71$`Trajanje u mjesecima`)
shapiro.test(TrajanjeZaposlenjaA72$`Trajanje u mjesecima`)
shapiro.test(TrajanjeZaposlenjaA73$`Trajanje u mjesecima`)
shapiro.test(TrajanjeZaposlenjaA74$`Trajanje u mjesecima`)
shapiro.test(TrajanjeZaposlenjaA75$`Trajanje u mjesecima`)

shapiro.test(TrajanjeZaposlenjaA71$`Iznos kredita`)
shapiro.test(TrajanjeZaposlenjaA72$`Iznos kredita`)
shapiro.test(TrajanjeZaposlenjaA73$`Iznos kredita`)
shapiro.test(TrajanjeZaposlenjaA74$`Iznos kredita`)
shapiro.test(TrajanjeZaposlenjaA75$`Iznos kredita`)

shapiro.test(BracniStatusISpol91$`Trajanje u mjesecima`)
shapiro.test(BracniStatusISpol92$`Trajanje u mjesecima`)
shapiro.test(BracniStatusISpol93$`Trajanje u mjesecima`)
shapiro.test(BracniStatusISpol94$`Trajanje u mjesecima`)
shapiro.test(BracniStatusISpol95$`Trajanje u mjesecima`)

shapiro.test(BracniStatusISpol91$`Iznos kredita`)
shapiro.test(BracniStatusISpol92$`Iznos kredita`)
shapiro.test(BracniStatusISpol93$`Iznos kredita`)
shapiro.test(BracniStatusISpol94$`Iznos kredita`)
shapiro.test(BracniStatusISpol95$`Iznos kredita`)

##  Izraèun varijance varijable "Trajanje u mjesecima" po modalitetima varijable "Svrha kredita" i "Posao"

tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Trajanje zaposlenja`, var)
tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Braèni status i Spol`, var)

##  Izraèun varijance varijable "Iznos kredita" po modalitetima varijable "Svrha kredita" i "Posao"

tapply(Krediti$`Iznos kredita`, Krediti$`Trajanje zaposlenja`, var)
tapply(Krediti$`Iznos kredita`, Krediti$`Braèni status i Spol`, var)

## Bartlett-ov test

bartlett.test(`Trajanje u mjesecima` ~ `Trajanje zaposlenja`, data = Krediti)
bartlett.test(`Trajanje u mjesecima` ~ `Braèni status i Spol`, data = Krediti)

bartlett.test(`Iznos kredita` ~ `Trajanje zaposlenja`, data = Krediti)
bartlett.test(`Iznos kredita` ~ `Braèni status i Spol`, data = Krediti)

## Levenov test

library(car)
leveneTest(Krediti$`Trajanje u mjesecima`, Krediti$`Trajanje zaposlenja`, center = mean)
leveneTest(Krediti$`Trajanje u mjesecima`, Krediti$`Braèni status i Spol`, center = mean)

leveneTest(Krediti$`Iznos kredita`, Krediti$`Trajanje zaposlenja`, center = mean)
leveneTest(Krediti$`Iznos kredita`, Krediti$`Braèni status i Spol`, center = mean)

## ANOVA
install.packages("multcomp")
library(multcomp)

install.packages("RcmdrMisc")
library(RcmdrMisc)

AnovaModel1 <- (lm(`Trajanje u mjesecima` ~ `Trajanje zaposlenja`*`Braèni status i Spol`, data = Krediti))
Anova(AnovaModel1)

tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Trajanje zaposlenja`, mean)
tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Trajanje zaposlenja`, sd)
tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Trajanje zaposlenja`, function(x) sum(!is.na(x)))

tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Braèni status i Spol`, mean)
tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Braèni status i Spol`, sd)
tapply(Krediti$`Trajanje u mjesecima`, Krediti$`Braèni status i Spol`, function(x) sum(!is.na(x)))

plotMeans(Krediti$`Trajanje u mjesecima`, Krediti$`Trajanje zaposlenja`, error.bars = "conf.int")
plotMeans(Krediti$`Trajanje u mjesecima`, Krediti$`Braèni status i Spol`, error.bars = "conf.int")

AnovaModel2 <- (lm(`Iznos kredita` ~ `Trajanje zaposlenja`*`Braèni status i Spol`, data = Krediti))
Anova(AnovaModel2)

tapply(Krediti$`Iznos kredita`, Krediti$`Trajanje zaposlenja`, mean)
tapply(Krediti$`Iznos kredita`, Krediti$`Trajanje zaposlenja`, sd)
tapply(Krediti$`Iznos kredita`, Krediti$`Trajanje zaposlenja`, function(x) sum(!is.na(x)))

tapply(Krediti$`Iznos kredita`, Krediti$`Braèni status i Spol`, mean)
tapply(Krediti$`Iznos kredita`, Krediti$`Braèni status i Spol`, sd)
tapply(Krediti$`Iznos kredita`, Krediti$`Braèni status i Spol`, function(x) sum(!is.na(x)))

plotMeans(Krediti$`Iznos kredita`, Krediti$`Trajanje zaposlenja`, error.bars = "conf.int")
plotMeans(Krediti$`Iznos kredita`, Krediti$`Braèni status i Spol`, error.bars = "conf.int")

## post hoc test
## Pretvaranje u jednofaktorsku ANOVU

Krediti$TZaposBStatus <- as.factor(Krediti$TZaposBStatus)

tapply(Krediti$`Trajanje u mjesecima`, Krediti$TZaposBStatus, var)
bartlett.test(`Trajanje u mjesecima` ~ TZaposBStatus, data = Krediti)
leveneTest(Krediti$`Trajanje u mjesecima`, Krediti$TZaposBStatus, center=mean)

bartlett.test(`Iznos kredita` ~ TZaposBStatusTukeyHSD(AnovaModel.1), data = Krediti)
leveneTest(Krediti$`Iznos kredita`, Krediti$TZaposBStatus, center=mean)

AnovaModel5 <- aov(`Trajanje u mjesecima` ~ TZaposBStatus, data = Krediti)
summary(AnovaModel5)

TukeyHSD(AnovaModel5)
.Pairs2 <- glht(AnovaModel5, linfct = mcp(TZaposBStatus = "Tukey"))
summary(.Pairs2)
confint(.Pairs2)
cld(.Pairs2)


AnovaModel6 <- aov(`Iznos kredita` ~ TZaposBStatus, data = Krediti)
summary(AnovaModel6)

TukeyHSD(AnovaModel6)
.Pairs3 <- glht(AnovaModel6, linfct = mcp(TZaposBStatus = "Tukey"))
summary(.Pairs3)
confint(.Pairs3)
cld(.Pairs3)

## Zadatak g)

Tab <- table(`Svrha kredita`, Posao)
Tab
barplot(Tab, beside = T, legend = T)

CTest <- chisq.test(Tab, correct = TRUE)
CTest

## Odbacujemo H0 hipotezu i prihvaæamo H1 hipotezu koja govori da su "Svrha kredita"" i "Braèni status i Spol" meðusobno ovisni.


## Zadatak h)

Tab2 <- table(`Svrha kredita`, `Braèni status i Spol`)
Tab2
barplot(Tab2, beside = T, legend = T)


CTest2 <- chisq.test(Tab2, correct = TRUE)
CTest2

## Kod signifikantnosti od 1% ostajemo kod H0 hipoteze koja govori da "Svrha kredita"" i "Braèni status i Spol" nisu meðusobno ovisni.
## Kod signifikantnosti od 5% odbacujemo H0 hipotezu i prihvaæamo H1 hipotezu koja govori da su "Svrha kredita"" i "Braèni status i Spol" meðusobno ovisni.


## Zadatak i)

library("olsrr")
head(Krediti, 4)
model <- lm(`Iznos kredita` ~ `Status tekuaeg raeuna` + `Trajanje u mjesecima` +`Stopa otplate primanja`  +Starost+`Broj kredita u banci`+`Broj jamaca`+`Braeni status i Spol`+`Trenutno prebivaliste od`,  data = Krediti)
summary(model)
summary(model)$coefficient
plot(Krediti$`Iznos kredita`, resid(model))
abline(0,0)
olsrr::ols_regress(model)

#nakon izbora
model2 <- lm(`Iznos kredita` ~ `Status tekuaeg raeuna` + `Trajanje u mjesecima` +`Stopa otplate primanja`  +Starost,  data = Krediti)
summary(model2)
summary(model2)$coefficient
plot(Krediti$`Iznos kredita`, resid(model2))
abline(0,0)
