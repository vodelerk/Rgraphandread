#Taller Caret con datos de iris

#Librerias
library(caret)
library(e1071)

#Datos
data(iris)

#Featureplot
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top          
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:3], 
            y = iris[, 4],
            plot = "scatter",
            layout = c(3, 1))

featurePlot(x=iris[,1:4], 
            y=iris[,5], 
            plot="density", 
            scales=list(x=list(relation="free"),
                        y=list(relation="free")), 
            auto.key=list(columns=3))

featurePlot(x=iris[,1:4], 
            y=iris[,5], 
            plot="box",
            scales=list(x=list(relation="free"), y=list(relation="free")),
            auto.key=list(columns=3))


#Particion de los datos
#Comprender set seed.
sample(LETTERS, 5) 
sample(LETTERS, 5) 
set.seed(42)
sample(LETTERS, 5)
set.seed(42)
sample(LETTERS, 5) 


set.seed(3456) 
trainIndex <- createDataPartition(iris$Species,
                                  p = .8, list = FALSE, times = 1)

trainIndex <- createDataPartition(iris$Species,
                                  p = .8, list = FALSE, times = 2)

trainIndex2 <- createDataPartition(iris$Sepal.Length,
                                   p = .8, list = FALSE, times = 1)

head(trainIndex)

irisTrain <- iris[ trainIndex,]
irisTest <- iris[-trainIndex,]

#Entrenar el modelo (Ambas opciones son validas)
model_knn1 <- train(irisTrain[, 1:4], irisTrain[, 5], method='knn')

model_knn1 <- train(Species~.,irisTrain, method='knn')

#Entrenar el modelo con parametro de preprocesamiento
cctrl <- trainControl(method="boot",number=10)
model_knn2A <- train(irisTrain[, 1:4], irisTrain[, 5], method='knn',trControl=cctrl, preProcess=c("center", "scale"))

#Entrenar el modelo con parametro de preprocesamiento
cctrl <- trainControl(method="repeatedcv", repeats=10)
model_knn2B <- train(irisTrain[, 1:4], irisTrain[, 5], method='knn',trControl=cctrl, preProcess=c("center", "scale"))

# Entrenar el modelo utilizando el parametro de preprocesamiento
model_knn3 <- train(irisTrain[, 1:4], irisTrain[, 5], method='knn', preProcess=c("center", "scale"))

#Entrenar modelo con tunelength igual a 10
model_knn4 <- train(Species~.,irisTrain, method='knn', tuneLength=10)

#Entrenar modelo con tuneGrid 
model_knn5 <- train(Species~.,irisTrain, method='knn', tuneGrid=expand.grid(k=seq(1,20, by=2)))


#Entrenar el modelo cambiando la metrica
cctrl <- trainControl(method="boot",repeats = 3,number=10)
model_knn6 <- train(irisTrain[, 1:4], irisTrain[, 5], method='knn', metric="Kappa",tuneLength=10)


#Besttune y final model del modelo_knn6

model_knn6$bestTune

model_knn6$finalModel

# Predicciones
predictions<-predict(object=model_knn1,irisTest[,1:4])
head(predictions)
predictionsprob<-predict(object=model_knn1,irisTest[,1:4], type="prob")


# Matriz de confusion
confusionMatrix(predictions,irisTest[,5])

#Cambiar el parametro mode en la matriz de confusion
confusionMatrix(predictions,irisTest[,5],mode="everything")
confusionMatrix(predictions,irisTest[,5],mode="sens_spec")
confusionMatrix(predictions,irisTest[,5],mode="prec_recall")


