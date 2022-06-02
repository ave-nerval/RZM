### PRIMERJAVA LDL IN QDL    ###
### Eva Lavrencic            ###
### Racunsko zahtevne metode ###
################################

library(MASS)
library(ggplot2)
library(parallel)
library(doParallel)
library(doRNG)
library(reshape2)
library(see)
library(effectsize)

#load("workspace.RData")


# PARAMETRI
st_spr <- 6
razmerje_var <- c(1,2,3,5) #Stevilo spremenljivk
korelacija <- seq(from = 0, to = 0.8, by = 0.2) #Korelacija med spremenljivkami
n_vzorec <- c(10, 20, 30, 50, 100) #Velikost skupin

m <- 10 #stevilo simulacij

# ZASNOVA: kombinacije parametrov x stevilo simulacij
zasnova <- expand.grid(korelacija, n_vzorec, razmerje_var)
zasnova <- do.call(rbind, replicate(m, zasnova, simplify=FALSE))
colnames(zasnova) <- c("korelacija", "velikost_skupin", "razmerje_var")

# PRIPRAVA ZA PARALELNO RACUNANJE
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)


# SIMULACIJA
rez <- foreach(i = 1:nrow(zasnova), .combine = "rbind", .packages = c("MASS", "blockmodeling", "car", "fpc", "DescTools")) %dorng% {

  var1 <- 1; var2 <- zasnova[i, "razmerje_var"]; var3 <- zasnova[i, "razmerje_var"]^2
  Sigma1 <- matrix(zasnova[i, "korelacija"], nrow = st_spr, ncol = st_spr)
  Sigma2 <- matrix(zasnova[i, "korelacija"]*var2, nrow = st_spr, ncol = st_spr)
  Sigma3 <- matrix(zasnova[i, "korelacija"]*var3, nrow = st_spr, ncol = st_spr)
  
  diag(Sigma1) <- 1 #var-kov matrika
  diag(Sigma2) <- zasnova[i, "razmerje_var"] #var-kov matrika
  diag(Sigma3) <- zasnova[i, "razmerje_var"]^2
  
  
  mu1 <- rep(0, times = st_spr) #povprecje za vsako skupino
  mu2 <- rep(1, times = st_spr)
  mu3 <- rep(2, times = st_spr)
  
  n <- zasnova[i, "velikost_skupin"] #velikost skupin

  # GENERIRANJE PODATKOV: ZA OCENO MODELO
  Skupina1 <- mvrnorm(n = n, Sigma = Sigma1, mu = mu1)
  Skupina2 <- mvrnorm(n = n, Sigma = Sigma2, mu = mu2)
  Skupina3 <- mvrnorm(n = n, Sigma = Sigma3, mu = mu3)
  
  vseSkupine <- rbind(Skupina1, Skupina2, Skupina3)
  skupinaModel <- rep(c(0,1,2), times = c(n,n,n)) #oznaka skupine
  dataModel <- cbind(vseSkupine, skupinaModel)

  # TESTNI PODATKI
  
  Skupina1 <- mvrnorm(n = 500, Sigma = Sigma1, mu = mu1)
  Skupina2 <- mvrnorm(n = 500, Sigma = Sigma2, mu = mu2)
  Skupina3 <- mvrnorm(n = 500, Sigma = Sigma3, mu = mu3)
  
  vseSkupine <- rbind(Skupina1, Skupina2, Skupina3)
  skupinaTest <- rep(c(0,1,2), times = c(500,500,500)) #oznaka skupine
  dataTest <- cbind(vseSkupine, skupinaTest)
  
  # MODEL
  #LDA
  LDAmodel <- lda(x = dataModel[,1:st_spr], grouping = dataModel[,(st_spr+1)])
  LDAprobs <- predict(LDAmodel, newdata = dataTest[,1:st_spr])$post[,2]
  LDAclass <- predict(LDAmodel, newdata = dataTest[,1:st_spr])$class
  #QDA
  QDAmodel <- qda(x = dataModel[,1:st_spr], grouping = dataModel[,(st_spr+1)])
  QDAprobs <- predict(QDAmodel, newdata = dataTest[,1:st_spr])$post[,2]
  QDAclass <- predict(QDAmodel, newdata = dataTest[,1:st_spr])$class
  
  # TOCNOST NAPOVEDI
  
  LDArez <- mean(LDAclass==skupinaTest)
  QDArez <- mean(QDAclass==skupinaTest)

  
  #shranimo rezultate
  cbind(zasnova[i,], "LDA" = LDArez, "QDA" = QDArez)
}

rez2 <- rez

# zakljucimo paralelno racunanje
stopCluster(cl)

# GRAFICNI PRIKAZ IN ANALIZA PODATKOV
MeltedData <- melt(as.data.frame(rez), 
                   id.vars = c("korelacija", "velikost_skupin", "razmerje_var"), 
                   measure.vars = c("LDA", "QDA"))

ggplot(data = MeltedData, mapping = aes(x = korelacija, y = value, group=variable, col=variable)) +
  facet_grid(velikost_skupin ~ razmerje_var) +
  stat_summary(fun = mean, geom="line")




# STATISTICNI TESTI: KATERI DEJAVNIKI IMAJO VPLIV
rez2$korelacija <- as.factor(rez$korelacija)
rez2$velikost_skupin <- as.factor(rez$n)
rez2$razmerje_var <- as.factor(rez$razm_var)

modelLDA <- aov(LDA ~ korelacija*n*razm_var, data = rez2)
summary(modelLDA)
plot(eta_squared(modelLDA))

modelQDA <- aov(QDA ~ korelacija*n*razm_var, data = rez2)
summary(modelQDA)
plot(eta_squared(modelQDA))

# Vpliv dejavnikov na razliko
modelRazlika <- aov((LDA - QDA) ~ korelacija*n*razm_var, data = rez2)
summary(modelRazlika)
plot(eta_squared(modelRazlika))












