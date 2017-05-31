install.packages("caret")
install.packages("nnet")
library(caret)
library(nnet)

datos_mora <- read.table("mora_toyset.csv", header = T,sep = ",", row.names = 1)
str(datos_mora)
set.seed(123)
inTrain <- createDataPartition(y=datos_mora$Yield, p=0.7, list=F)
training <- datos_mora[inTrain,]
testing <- datos_mora[-inTrain,]
#normalizacion
norm_training <- preProcess(training, method = "range")
training_norm <- predict(norm_training,training)
testing_norm <- predict(norm_training,testing)
#optimizacion de parametros
ctrl <- expand.grid(size = c(2,4,6), decay = c(0.1,0.5,0.8))
#entrenamiento
model <- train(Yield~., data = training_norm, method="nnet", tuneGrid = ctrl, trControl = trainControl(method = "cv", number = 10), lineOut = T)
model
#desempeño
pred_val <- predict(model, testing_norm)
postResample(pred_val, testing_norm$Yield)
plot(pred_val,testing_norm$Yield,col="red", pch=19)
abline(0,1,lty=2)
varImp <- varImp(model)
plot(varImp)
