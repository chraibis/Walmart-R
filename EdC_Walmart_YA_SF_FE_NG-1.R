library(dummy)

# Chargement des donnees
train <- read.csv("train.csv")
features <- read.csv("features.csv")



#### Fonctions ####

#fonction de decalage de dates
decaler <- function(x,k){
  n <- length(x)
  y <- rep(0,n)
  if(k<0){
    y[1:(n+k)] <- x[(-k+1):n]
  }
  else{
    y[(k+1):n] <- x[1:(n-k)]
  }
  return(y)
}

#fonction de mesure d'erreur
wmae <- function(y,yhat,y_weights){
  res <- sum(((abs(y-yhat))*y_weights))/sum(y_weights)
  res
}
w <- ifelse(deptData$IsHoliday.x == 0, 5, 1)

#fonction de validation croisee
validation_croisee <- function(machines,f,g,machines.names){
  n <- nrow(machines)
  erreur <- 0
  
  for (i in f:(n-g-1)){
    res <- COBRA(train.design = df[1:i,-1],
                 train.responses = df[1:i,1],
                 test = df[(n-g):n,],
                 machines = machines,
                 machines.names = machines.names)
    Prediction <- res$predict
    erreur <- erreur + wmae(df$Weekly_Sales[(i+1):(i+g)], Prediction, w[(i+1):(i+g)])
  }
  return(erreur / (n-f-g-1))
}




#### Preparation des donnees pour un magasin et un departement ####

#on travaille sur le magazin 45, departement 1
store <- 45
dep <- 1

#on construit deptdata, qui contient toutes les donnees sur le magazin, departement donne
trainStore <- train[train$Store == store, ]
featStore <- features[features$Store == store, ]
storeData <- merge(trainStore, featStore, by = 'Date')
deptData <- storeData[storeData$Dept == dep, ]
plot(deptData$Weekly_Sales, type = 'l')

#IsHoliday sera une variable bineaire indiquant si on est sur un jour ferie ou pas
IsHoliday <- deptData$IsHoliday.x
IsHoliday[which(IsHoliday==TRUE)] <- 1
IsHoliday[which(IsHoliday==FALSE)] <- 0

#definition de dates "cles" correspondant a des pics de ventes
d_Super_Bowl <- c('2010-02-12', '2011-02-11', '2012-02-10', '2013-02-08')
d_Labor_Day <- c('2010-09-10', '2011-09-09', '2012-09-07', '2013-09-06')
d_Thanksgiving <- c('2010-11-26', '2011-11-25', '2012-11-23', '2013-11-29')
d_Christmas <- c('2010-12-31', '2011-12-30', '2012-12-28', '2013-12-27')
d_Easter <- c('2010-04-02', '2011-04-22', '2012-04-06')
#on construit des valeurs binaires de la meme maniere que pour IsHoliday
Easter <- ifelse(as.character(deptData$Date) %in% d_Easter, 1, 0)
Super_Bowl <- ifelse(as.character(deptData$Date) %in% d_Super_Bowl, 1, 0)
Labor_Day <- ifelse(as.character(deptData$Date) %in% d_Labor_Day, 1, 0)
Thanksgiving <- ifelse(as.character(deptData$Date) %in% d_Thanksgiving, 1, 0)
Christmas <- ifelse(as.character(deptData$Date) %in% d_Christmas, 1, 0)

#on construit aussi des variables bineaires indiquant le mois sur lequel on se trouve
Month <- months(as.Date(deptData$Date, format = '%Y-%m-%d'))
dummyMonth <- dummy(data.frame(Month))

#construction du dataframe contenant toutes les donnees construites precedemment
#et de Weekly_Sales, valeur a apprendre puis predire
df <- data.frame(Weekly_Sales = deptData$Weekly_Sales,
                 Month = as.factor(Month),
                 Janvier = dummyMonth$Month_January,
                 Fevrier = dummyMonth$Month_February,
                 Mars = dummyMonth$Month_March,
                 Avril = dummyMonth$Month_April,
                 Mai = dummyMonth$Month_May,
                 Juin = dummyMonth$Month_June,
                 Juillet = dummyMonth$Month_July,
                 Aout = dummyMonth$Month_August,
                 Septembre = dummyMonth$Month_September,
                 Octobre = dummyMonth$Month_October,
                 Novembre = dummyMonth$Month_November,
                 Decembre = dummyMonth$Month_December,
                 Ete = as.factor(as.numeric(dummyMonth$Month_May) + as.numeric(dummyMonth$Month_June) + as.numeric(dummyMonth$Month_July) + as.numeric(dummyMonth$Month_August) + as.numeric(dummyMonth$Month_September)),
                 Hiver = as.factor( as.numeric(dummyMonth$Month_November) + as.numeric(dummyMonth$Month_January) + as.numeric(dummyMonth$Month_March)),
                 Easter = Easter,
                 Super_Bowl = Super_Bowl,
                 Labor_Day = Labor_Day,
                 Thanksgiving = Thanksgiving,
                 Christmas = Christmas,
                 IsHoliday = IsHoliday,
                 Temperature = deptData$Temperature
)
md <- as.data.frame( apply(deptData[,9:13],2, function(x) ifelse(is.na(x), 0, 1)) )
colnames(md) <- paste('MD', c(1:5), sep='')
df <- cbind(df,md)

#Decalage des dates "cles" pour qu'ils correspondent aux pics de ventes
df$Super_Bowl <- decaler(df$Super_Bowl,1) 
df$Christmas <- decaler(df$Christmas,-1) + decaler(df$Christmas,-2)
df$Thanksgiving <- decaler(df$Thanksgiving,-3) + decaler(df$Thanksgiving,-4)
df$Easter <- decaler(df$Easter,0)
#Affichage des pics
plot(deptData$Weekly_Sales, type = 'l')
index <- which(df$Super_Bowl == 1)
points(index, df$Weekly_Sales[index], col = 6, pch = 19)
index <- which(df$Christmas == 1)
points(index, df$Weekly_Sales[index], col = 2, pch = 19)
index <- which(df$Thanksgiving == 1)
points(index, df$Weekly_Sales[index], col = 3, pch = 19)
index <- which(df$Easter == 1)
points(index, df$Weekly_Sales[index], col = 4, pch = 19)

#Construction d'un echantillon d'apprentissage et de test
n <- nrow(df)
Traindf <- df[1:104,]
Testdf <- df[105:n,]
Trainw <- w[1:104]
Testw <- w[105:n]



#### Methode 1 : Regression ####

#Regression sur les donnees des evenements/dates cles
modLm <- lm(Weekly_Sales ~ Easter + Christmas + Super_Bowl + Thanksgiving , Traindf)
#apprentissage
summary(modLm)
plot(Traindf$Weekly_Sales, type = 'l')
lines(modLm$fitted.values, col = '2')
wmae(Traindf$Weekly_Sales,modLm$fitted.values,Trainw)
#prediction sur donnees test
PmodLm <- predict(modLm, Testdf)
plot(Testdf$Weekly_Sales, type = 'l')
lines(PmodLm, col = 'red')
wmae(Testdf$Weekly_Sales,PmodLm,Testw)

#Regression sur 9 variables: les evenements et les mois ou la variabilite est elevee
modLm <- lm (Weekly_Sales ~ Easter + Christmas + Super_Bowl + Thanksgiving + Mai + Juin + Juillet + Aout + Septembre , Traindf)
#apprentissage
summary(modLm)
plot(Traindf$Weekly_Sales, type = 'l')
lines(modLm$fitted.values, col = '2')
wmae(Traindf$Weekly_Sales,modLm$fitted.values,Trainw)
#prediction
PmodLm <- predict(modLm, Testdf)
plot(Testdf$Weekly_Sales, type = 'l', main="prediction sur donnees test")
lines(PmodLm, col = '2')
wmae(Testdf$Weekly_Sales,PmodLm,Testw)

#Regression sur 6 variables: au lieu de prendre chaque mois, on definit 2 periodes, ete et hiver
#correspondant aux periodes ou se trouvent les mois interessants
modLm <- lm(Weekly_Sales ~ Easter + Christmas + Super_Bowl + Thanksgiving + Ete + Hiver, Traindf)
#apprentissage
summary(modLm)
plot(Traindf$Weekly_Sales, type = 'l')
lines(modLm$fitted.values, col = '2')
wmae(Traindf$Weekly_Sales,modLm$fitted.values,Trainw)
#prediction
PmodLm <- predict(modLm, Testdf)
plot(Testdf$Weekly_Sales, type = 'l', main="prediction sur donnees test")
lines(PmodLm, col = '2')
wmae(Testdf$Weekly_Sales,PmodLm,Testw)

#analysons les residus de la regression : bruit blanc gaussien?
res_student <- ts(rstudent(modLm), start = c(2010,2,5), frequency = 52)
plot(res_student, type='o', xlab="semaines", ylab="ventes hebdomadaires", main="residus estimes studentises")
abline(0, 0, col="red", lwd=2)
op <- par(mfrow = c(1,2))
res_student <- as.vector(res_student)
ro <- acf(res_student , lag=25, ylim = c(-1,1), main = expression("ACF "), xlab="Lag (en semaines)", lwd=2)
alpha <- pacf(res_student , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en semaines)", lwd=2)
par(op)
qqnorm(res_student)
qqline(res_student)



#### Methode 2 : Foret Aleatoires ####

library(randomForest)
set.seed(2374)

#apprentissage
modRf <- randomForest(Weekly_Sales ~ ., data = Traindf)
#prediction
PmodRf <- predict(modRf, Testdf)
plot(Testdf$Weekly_Sales, type = 'l', main="prediction sur donnees test")
lines(PmodRf, col = '2')
wmae(Testdf$Weekly_Sales, PmodRf, Testw)

#analysons les residus de randomForest : bruit blanc gaussien?
res <- ts(PmodRf - Testdf$Weekly_Sales)
plot(res, type='o', xlab="semaines", ylab="ventes hebdomadaires", main="residus de prediction")
abline(0, 0, col="red", lwd=2)
op <- par(mfrow = c(1,2))
res <- as.vector(res)
ro <- acf(res, lag=25, ylim = c(-1,1), main = expression("ACF "), xlab="Lag (en semaines)", lwd=2)
alpha <- pacf(res, lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en semaines)", lwd=2)
par(op)



#### Methode 3 : xgBoost ####

library(xgboost)

#pretraitement des donnees
Donnees <- Traindf
for (j in 1:length(Donnees[1,]))
{
  Donnees[,j] <- as.numeric(Donnees[,j])
}
for (j in 1:length(Testdf[1,]))
{
  Testdf[,j] <- as.numeric(Testdf[,j])
}
Testdf <- as.matrix(Testdf)
modXGB <- xgboost(data = as.matrix(Donnees[,-1]), label = Traindf$Weekly_Sales,eta = 0.025, gamma = 0.05, nfold = 5, nrounds = 250,max.depth =12, objective = "reg:linear", eval_metric ="rmse")

#prediction
PmodXGB <- predict(modXGB,Testdf[,-1])
erreur <-wmae(df$Weekly_Sales[105:n],PmodXGB,w[105:n])
plot(ts(df$Weekly_Sales,start = 105, end = 143), type = 'l',
     main = "modèle prédit avec Gradient Boosting",
     ylab = "weekly sales")
lines(ts(PmodXGB,start=105), col = 'red')
#trace des régions de confiance et de prédiction
lines(ts(PmodXGB,start=105), col = 'red')
plot(ts(df$Weekly_Sales,start = 105, end = 143), type = 'l', main = "région de confiance", ylab ="weekly sales")
IC_confiance <- predict(modXGB,Testdf[,-1], interval = "confidence", level = 0.95 )
matlines(1:143, IC_confiance, lty=c(1,2,2),col=c("red","blue","blue"))
IC_prediction <- predict(modXGB,Testdf[,-1], interval = "prediction", level = 0.95)
plot(ts(df$Weekly_Sales,start = 105, end = 143), type = 'l', main = "région de prédicion",xlab = "temps(semaines)" , ylab = "ventes hebdomadaires")
matlines(1:143, IC_prediction, lty=c(1,2,2),col=c("blue","red","red"))

#trace de feature diagram
library(ggplot2)
library(Ckmeans.1d.dp)
library(DiagrammeR)
names <- dimnames(Donnees)[[2]]
importance_matrix <- xgb.importance(names, model = modXGB)
xgb.plot.importance(importance_matrix[1:21,])
xgb.plot.tree(feature_names = names, model = modXGB, n_first_tree = 2)

# on a effectué un deuxieme modele juste avec les variables explicatives donnees par le featuring
Donnees_ameliorees <- cbind(Donnees[,1],Donnees[,14], Donnees[,15],Donnees[,16],Donnees[,17],Donnees[,19],Donnees[,20])
Test_ameliorees <- cbind(Testdf[,1],Testdf[,14], Testdf[,15],Testdf[,16],Testdf[,17],Testdf[,19],Testdf[,20])
modXGB_ameliore <- xgboost(data = as.matrix(Donnees_ameliorees[,-1]), label = Donnees_ameliorees[,1],eta = 0.025, gamma = 0.05, nfold = 5, nrounds = 250,max.depth =12, objective = "reg:linear", eval_metric ="rmse")
PmodXGB2 <- predict(modXGB_ameliore,Test_ameliorees[,-1])
erreur <-wmae(df$Weekly_Sales[105:n],PmodXGB2,w[105:n])

## Tracé du modèle prédit
plot(ts(df$Weekly_Sales,start = 105, end = 143), type = 'l',
     main = "Gradient Boosting juste avec les variables significatives ",
     ylab = "weekly sales")
lines(ts(PmodXGB2,start=105), col = 'red')

#analysons les residus de la regression : bruit blanc gaussien?
res <- ts(PmodXGB - Testdf$Weekly_Sales)
plot(res, type='o', xlab="semaines", ylab="ventes hebdomadaires",
     main="residus de prediction")
abline(0, 0, col="red", lwd=2)
op <- par(mfrow = c(1,2))
res <- as.vector(res)
ro <- acf(res, lag=25, ylim = c(-1,1), main = expression("ACF"),
          xlab="Lag (en semaines)", lwd=2)
alpha <- pacf(res, lag=25, ylim = c(-1,1), main = expression("PACF"),
              xlab="Lag (en semaines)", lwd=2)
par(op)



#### methode 4 : agregation de modeles ####

library(COBRA)
ntrain <- 104
train <- df[1:ntrain,]
test <- df[-(1:ntrain),]

validation_croisee <- function(machines,f,g,machines.names){
  n <- nrow(machines)
  erreur <- 0
  
  for (i in f:(n-g-1)){
    res <- COBRA(train.design = df[1:i,-1],
                 train.responses = df[1:i,1],
                 test = df[(n-g):n,],
                 machines = machines,
                 machines.names = machines.names)
    Prediction <- res$predict
    erreur <- erreur + wmae(df$Weekly_Sales[(i+1):(i+g)], Prediction, w[(i+1):(i+g)])
  }
  return(erreur / (n-f-g-1))
}

#### Analyse de la fiabilite des modeles ####

#cross-validation pour la regression a 6 variables
f <- 80
g <- 30
erreur <- 0
for (i in f:(n-g-1)){
  modLm <- lm(Weekly_Sales ~ Easter + Christmas + Super_Bowl + Thanksgiving +  Ete + Hiver, df[1:i,])
  PmodLm <- predict(modLm,df[(n-g):n,])
  erreur <- erreur + wmae(df$Weekly_Sales[1:i], PmodLm, w[1:i])
}
erreur <- erreur / (n-f)

#cross-validation pour le Random Forest
f <- 80
g <- 30
erreur <- 0
for (i in f:(n-g-1)){
  modRf <- randomForest(Weekly_Sales ~., data = df[1:i,], ntree = 20)
  PmodRf <- predict(modRf,df[(n-g):n,])
  erreur <- erreur + wmae(df$Weekly_Sales[1:i], PmodRf, w[1:i])
}
erreur <- erreur / (n-f)

# Cross-validation pour xgBoost
f <- 80
g <- nrow(Testdf)
erreur <- 0
for (i in f:(n-g-1)){
  modXGB <- xgboost(data = as.matrix(Donnees[1:i,-1]), label = Traindf[1:i,1],eta = 0.025, gamma = 0.1, nfold = 5, nrounds = 250,max.depth =12, objective = "reg:linear", eval_metric ="rmse")
  PmodXGB <- predict(modXGB,Testdf[,-1])
  erreur <- erreur + wmae(df$Weekly_Sales[1:i], PmodXGB, w[1:i])
}
erreur <- erreur / (n-f-g-1)

