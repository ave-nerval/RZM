# MANJKAJOCE VREDNOSTI IN FAKTORSKA ANALIZA

## ----setup, include=FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(echo = F, warning = F)
library(psych)
library(MASS)
library(Hmisc)
library(rgl)
library(blockmodeling)
library(mclust)
library(sn)
library(multiUS)
library(mice)
library(kableExtra)
library(foreign)
library(VIM)


## --------------------------------------------------------------------------------------------
podatki <- read.spss("ZA7546_v1-0-0.sav", 
                     use.value.labels = FALSE, 
                     to.data.frame = TRUE, 
                     use.missings = TRUE)


## ---- eval=T---------------------------------------------------------------------------------
spr <- c("v212", "v213", "v214", "v215", "v216", "v217", "v218", "v219", "v220")
podatki2 <- podatki[,spr]

sprSLO <- c("sosedi", "regija", "drzava", "Evropa", "svet", "starejsi", "nezaposleni", "priseljenci", "bolni")
colnames(podatki2) <- sprSLO

# rekodiranje


## ---- eval=T---------------------------------------------------------------------------------
for (i in 1:length(sprSLO)) {
  # rekodiranje
  podatki2[, sprSLO[i]] <- Recode(podatki2[, sprSLO[i]], "
                                                       '5' = 1;
                                                       '4' = 2;
                                                       '3' = 3;
                                                       '2' = 4;
                                                       '1' = 5
                                                       ") }

druge_spr <- c("v225", "v226", "v243_GR")

podatki_d <- podatki[,druge_spr]
colnames(podatki_d) <- c("spol", "starost", "izobrazba")
podatki_d$starost <- 2017 - podatki_d$starost

podatkiVsi <- cbind(podatki2, podatki_d)


## --------------------------------------------------------------------------------------------
#sum(rowMeans(is.na(podatkiVsi))==1)



## ---- fig.cap="Delez manjkajocih vrednosti (%) pri posameznih spremenljivkah"----------------
barplot(round(colMeans(is.na(podatkiVsi))*100, 1), las=2, ylab="Delež manjkajočih vrednosti (%)", cex.names=0.8)


## ----fig.cap="Prikaz manjkajočih vrednosti (rdeča)"------------------------------------------
par(mfrow=c(2,2))
matrixplot(podatkiVsi)
matrixplot(podatkiVsi, sortby = "spol")
matrixplot(podatkiVsi, sortby = "starost")
matrixplot(podatkiVsi, sortby = "izobrazba")


## --------------------------------------------------------------------------------------------
podatkiVsi <- podatkiVsi[rowSums(is.na(podatkiVsi)) != ncol(podatkiVsi), ]


## ----fig.cap="Korelacija med pojavom manjkajočih vrednosti pri spremenljivkah. Izračunan je Pearsonov koeficient korelacije, vrednost koeficienta je pomnožena z 10."----
is.miss <- is.na(podatkiVsi)
manjkajoce <- cor(is.miss)
rownames(manjkajoce) <- colnames(manjkajoce) <- colnames(podatkiVsi)
manjkajoce[is.na(manjkajoce)] <- 0
plot.mat(manjkajoce)


## --------------------------------------------------------------------------------------------
# STAROST
starost_pval <- NULL
spol_pval <- NULL
izobrazba_pval <- NULL

for (i in 1:length(sprSLO)) {
  podatkiVsi[,i] 
  starost_pval[i] <- t.test(podatkiVsi$starost ~ is.na(podatkiVsi[,i]))$p.value
  spol_pval[i] <- chisq.test(table(is.na(podatkiVsi[i]), podatkiVsi$spol))$p.value
  izobrazba_pval[i] <- chisq.test(table(is.na(podatkiVsi[i]), podatkiVsi$izobrazba))$p.value

  
  }

Tabela2 <- cbind(sprSLO, round(starost_pval,4), round(spol_pval,4), round(izobrazba_pval,4))
colnames(Tabela2) <- c("Spremenljivka", "Starost (t-test)", "Spol (Chi2 test)", "Izobrazba (Chi2 test)")
kable(Tabela2,caption = "Tabela 1: Testiranje povezanosti manjkajočih vrednosti (vrednosti p)") %>%
  kable_styling("striped",full_width = F, font_size=10)%>%
  column_spec(1, bold = T, color = "black", background = "yellow")


## ----fig.cap="Pearsonove korelacije med vsebinskimi spremenljivkami, pomnožene z 10"---------
com <- complete.cases(podatkiVsi[, sprSLO])
dataCOMPLETE <- podatkiVsi[com, sprSLO]
n <- nrow(dataCOMPLETE)
R <- cor(dataCOMPLETE)
vrstni.red <- 1:9
plot.mat(R[vrstni.red, vrstni.red], main = "")
#cortest.bartlett(R = R, n = n)



## ----fig.cap="Anti-image matrika za vsebinske spremenljivke. Izven diagonale so parcialni korelacijski koeficienti pomnozeni z -1, na diagonali so vrednosti kaiser-Meyer-Olkin-ove mere (KMO). Vse vrednosti so pomnožene s 100."----
AI <- antiImage(dataCOMPLETE)$AIR
plot.mat(AI)
#antiImage(dataCOMPLETE)$KMO


## ----fig.cap="Scree diagram in paralelna analiza za faktorsko analizo. Uporabili smo metodo glavnih osi."----
fa.parallel(x = R, n.obs = n, fa = "fa", fm = "pa", n.iter = 100, main="") 



## ----fig.cap="Pattern utezi"-----------------------------------------------------------------
FA.obliminCOMPLETE <- fa(r = dataCOMPLETE, nfactors = 3, n.obs = n, rotate = "oblimin", scores = TRUE, fm = "pa", max.iter = 1000)
plot.mat(FA.obliminCOMPLETE$loadings[,], main = "")
uteziCOMPLETE <- FA.obliminCOMPLETE$loadings[,]


## --------------------------------------------------------------------------------------------
numSprem <- 9
dataMEANimp <- podatkiVsi[,sprSLO]
for (i in 1:numSprem){
  dataMEANimp[is.na(dataMEANimp[,i]), i] <- mean(dataMEANimp[,i], na.rm = TRUE)
}
#round(colMeans(is.na(dataMEANimp))*100, 1)
R_meanImp <- cor(dataMEANimp)
vrstni.red <- 1:9
#plot.mat(R_meanImp[vrstni.red, vrstni.red], main = "")
AI <- antiImage(dataMEANimp)$AIR
#plot.mat(AI)
#antiImage(dataMEANimp)$KMO
#fa.parallel(x = R, n.obs = n, fa = "fa", fm = "pa", n.iter = 100, main="") 
FA.oblimin_MEANimp <- fa(r = dataMEANimp, nfactors = 3, n.obs = n, rotate = "oblimin", scores = TRUE, fm = "pa", max.iter = 1000)
#plot.mat(FA.oblimin_MEANimp$loadings[,], main = "")

uteziMEAN <- FA.oblimin_MEANimp$loadings[,]



## --------------------------------------------------------------------------------------------
dataKNNimp <- podatkiVsi
dataKNNimp <- KNNimp(data = podatki2, scale = TRUE)
#round(colMeans(is.na(dataKNNimp))*100, 1)
dataKNNimp <- dataKNNimp[,sprSLO]


R_KNNimp <- cor(dataKNNimp)
vrstni.red <- 1:9
#plot.mat(R_KNNimp[vrstni.red, vrstni.red], main = "")
AI <- antiImage(dataKNNimp)$AIR
#plot.mat(AI)
#antiImage(dataKNNimp)$KMO
#fa.parallel(x = R, n.obs = n, fa = "fa", fm = "pa", n.iter = 100, main="") 
FA.oblimin_KNNimp <- fa(r = dataKNNimp, nfactors = 3, n.obs = n, rotate = "oblimin", scores = TRUE, fm = "pa", max.iter = 1000)
#plot.mat(FA.oblimin_KNNimp$loadings[,], main = "")

uteziKNN <- FA.oblimin_KNNimp$loadings[,]



## ---- results='hide'-------------------------------------------------------------------------
dataMICEimp <-  mice(data = podatkiVsi, m=10, maxit=100)
utezi <- matrix(NA, nrow=9,ncol=1)
for (i in 1:10){
  data <- complete(dataMICEimp,i)[,1:9]
  FA.oblimin_MICEimp <- fa(r = data, nfactors = 3, n.obs = n, rotate = "oblimin", scores = TRUE, fm = "pa", max.iter = 1000)
  utezi1 <- FA.oblimin_MICEimp$loadings
  utezi <- cbind(utezi,utezi1)
}
utezi <- utezi[,-1]

PA2 <- rowMeans(utezi[,seq(1,30,3)])
PA1 <- rowMeans(utezi[,seq(2,30,3)])
PA3 <- rowMeans(utezi[,seq(3,30,3)])
uteziMICE <- cbind(PA2, PA1, PA3)


## --------------------------------------------------------------------------------------------
utezi <- cbind(round(uteziCOMPLETE,4), round(uteziMEAN,4), round(uteziKNN,4), round(uteziMICE,4))

kable(utezi, caption = "Tabela 2: Ocenjene pattern utezi za faktorski model glede na metodo obravnave manjkajocih vrednosti") %>%
  column_spec(1, bold = T, color = "black", background = "yellow") %>%
  add_header_above(c(" ", "Complete" = 3, "MEAN imp" = 3, "KNN" = 3, "MICE" = 3)) %>%
  kable_styling(latex_options = c("repeat_header"), font_size=6)


