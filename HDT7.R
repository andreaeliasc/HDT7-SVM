library(e1071)
library(caret)
library(rJava)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(dplyr)
library(tidyr)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(cluster)#Para calcular la silueta
library(e1071)#para cmeans
library(cluster)#Para calcular la silueta
library(mclust) #mixtures of gaussians
library(fpc)#para hacer el plotcluster
library(NbClust)#Para determinar el número de clusters optimo
library(factoextra)#Para hacer graficos bonitos de clustering
library(e1071)
library(caret)
library(corrplot) # install.packages("corrplot")


porcentaje<-0.7
datos<-read.csv("train.csv", stringsAsFactors = FALSE)
set.seed(123)


datos$grupo <- ifelse(datos$SalePrice<178000, "3", 
                      ifelse(datos$SalePrice<301000, "2",
                             ifelse(datos$SalePrice<756000,"1",NA)))


datos$grupo <- as.factor(datos$grupo)


scatter.smooth(datos$LotFrontage, datos$SalePrice)
scatter.smooth(datos$LotArea, datos$SalePrice)
scatter.smooth(datos$GrLivArea, datos$SalePrice)
scatter.smooth(datos$YearBuilt, datos$SalePrice)
scatter.smooth(datos$BsmtUnfSF, datos$SalePrice)
scatter.smooth(datos$TotalBsmtSF, datos$SalePrice)
scatter.smooth(datos$X1stFlrSF, datos$SalePrice)
scatter.smooth(datos$GarageYrBlt, datos$SalePrice)
scatter.smooth(datos$GarageArea, datos$SalePrice)
scatter.smooth(datos$YearRemodAdd, datos$SalePrice)
scatter.smooth(datos$TotRmsAbvGrd, datos$SalePrice)
scatter.smooth(datos$MoSold, datos$SalePrice)
scatter.smooth(datos$OverallQual, datos$SalePrice)


datos <- datos[,c("LotFrontage","LotArea","GrLivArea","GarageArea","YearRemodAdd","SalePrice" ,"grupo")]

datos <- na.omit(datos)

head(datos, 10)


porcentaje<-0.7
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

head(train)

head(test)

modelo.nn2 <- nnet(grupo~.,data = train[,c(1:5,7)], size=6, rang=0.0000001,
                   decay=5e-4, maxit=500) 
modelo.nn2


prediccion2 <- as.data.frame(predict(modelo.nn2, newdata = test[,1:5]))
columnaMasAlta<-apply(prediccion2, 1, function(x) colnames(prediccion2)[which.max(x)])
columnaMasAlta
test$prediccion2<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción
head(test, 30)


cfm<-confusionMatrix(as.factor(test$prediccion2),test$grupo)
cfm
