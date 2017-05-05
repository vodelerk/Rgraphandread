###########################################################################
# Taller de KNN: ClasificaciOn del dataset Default, del package ISLR
# Autor: Javier Diaz, PhD
# Universidad ICESI
###########################################################################

####################################################
# 1. AnAlisis exploratorio (bAsico)

#install.packages("caret")
#install.packages("e1071")
#install.packages("ISLR")
#install.packages("ggplot2")
library(ggplot2)
library(caret)
library(e1071)
library(ISLR)

# Cargar y explorar el dataset de defaults de tarjetas de crEdito
# QuE ven de particular en los datos (Niveles, Rangos de las variables): .............

# El dataset estA balanceado con respecto a la clase "default"? ..........
# El baseline de la predicciOn mAs bAsica de seleccionar la clase mayoritaria 
# me darIa un accuracy de: .............

summary(Default) 
str(Default)
head(Default)
ggplot(data=Default, aes(x=balance, y=income, color=default)) + geom_point(size=2, alpha=.5)
featurePlot(x=Default[,c("balance", "income", "student")], y=Default$default, plot="pairs")


####################################################
# 2. Aprender modelo y utilizarlo para predecir

# Entrenar un modelo de KNN con la base de datos
# Por defecto el mEtodo train utiliza mEtodos de resampleo para determinar el 
# valor Optimo de los parAmetros del algoritmo (mEtodo) utilizado para el aprendizaje
modelo <- train(default~., data = Default, method = "knn")
modelo
modelo$bestTune # el K Optimo encontrado con la configuraciOn utilizada (la que viene por defecto)
modelo$finalModel #explica el tipo de modelo aprendido
plot(modelo) # evoluci???n de la m???trica en funcion del K

# La mEtrica de evaluaciOn usada por defecto por Caret es: .........
# El valor de K escogido por Caret es de: .........

# Se realizan predicciones con el mEtodo predict
predicciones <- predict(modelo, newdata=Default) #type "raw" por defecto
predicciones

prediccionesProb <- predict(modelo, newdata=Default, type="prob")
prediccionesProb

# Analizar una nueva instancia
nuevaInstancia <- data.frame(student="No", balance=1500, income=7000)
predict(modelo, newdata = nuevaInstancia)

#Interpretar las mEtricas dadas por la matriz de confusiOn: ........
confusionMatrix(predicciones, Default$default)

# QuE tan bien nos fue con nuestro modelo con respecto al baseline? ........
# Podemos decir con una significancia estadIstica del 95% que nuestro modelo es
#   mejor que el baseline? .......

####################################################
# 3. Aleatoriedad en los protocolos de evaluaciOn

# Repitamos lo que acabamos de hacer sin cambiar nada y comparemos las mEtricas de evaluaciOn
modelo <- train(default~., data = Default, method = "knn")
modelo
plot(modelo)

# Son diferentes. Esto se debe a que los protocolos de evaluaciOn utilizados son aleatorios,
# y los tirajes aleatorios cambian cada vez que se realizan.
# Explique donde estA la aleatoriedad del procedimiento utilizado: .................

# Para poder garantizar la reproducibilidad del proceso, se establece un mismo valor
# de la "semilla" del generador aleatorio. Un numEro con respecto al quE el generador 
# siempre va a dar la misma secuencia de nUmeros "pseudo-aleatorios"
set.seed(12345) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
modelo <- train(default~., data = Default, method = "knn")
modelo

set.seed(12345) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
modelo <- train(default~., data = Default, method = "knn")
modelo

predicciones <- predict(modelo, newdata=Default) #type "raw" por defecto
confusionMatrix(predicciones, Default$default)

####################################################
# 4. ???QuE tan bien nos fue con la mEtrica de evaluaciOn?

# El valor del accuracy obtenido es de: ...........

# Teniendo en cuenta que el baseline es muy elevado, 
# la soluciOn que tenemos por ahora apenas si llega a sobrepasar levemente ese nivel. 
# Como las clases estAn muy desbalanceadas, es mejor utilizar Kappa que Accuracy.
# El valor del kappa es: ..........
# El valor del kappa baseline es: .......... (Piensenlo con respecto a la definiciOn del Kappa)

# Entrenar un modelo de KNN con la base de datos, utilizando Kappa para escoger el mejor K
set.seed(12345) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
modelo <- train(default~., data = Default, method = "knn", metric="Kappa")
modelo
plot(modelo)

#No siempre se tiene el mismo resultado de K con diferentes m???tricas

####################################################
# 5. Tuning de la bUsqueda de los parAmetros del algoritmo
#    De pronto los 3 intentos por defecto con valores distintos de k son muy pocos

# Entrenar un modelo de KNN con la base de datos, ensayar con 10 configuraciones de K diferentes
set.seed(12345) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
modelo <- train(default~., data = Default, method = "knn", metric="Kappa", tuneLength=10)
modelo
plot(modelo)

predicciones <- predict(modelo, newdata=Default) #type "raw" por defecto
confusionMatrix(predicciones, Default$default)

####################################################
# 6. EstandarizaciOn de los datos

# AcordEmonos de lo diferentes que eran los Ordenes de magnitud de las variables predictivas
# Vamos a modificar los datos a ver si podemos mejorar un poco la situaciOn
set.seed(12345) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
modeloStd <- train(default~., data = Default, method = "knn", metric="Kappa", tuneLength=10,
                   preProcess = c("center","scale"))
modeloStd
plot(modeloStd)

# Por lo menos sobrepasamos la tasa del accuracy del baseline, obteniendo un kappa un poco mas decente
predicciones <- predict(modeloStd, newdata=Default) #type "raw" por defecto
confusionMatrix(predicciones, Default$default)


####################################################
# 7. UtilizaciOn de CV con repeticiones para identificar el mejor valor de K

# Vamos a blindarnos con respecto a la escogencia del K, utilizando
# un protocolo de cross validation con 5 folds, repetido 5 veces.
# Tambi???n, como ya tenemos una idea m???s clara mAs o menos clara de cuAles son los valores de K
# Optimos, vamos a definir especificamente los valores que queremos que se evaluen.
# Esto tambiEn nos permite estar mas seguros de la estimaciOn de la mEtrica
# de evaluaciOn utilizada (kappa)
set.seed(12345) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
trControlRepCv <- trainControl(method="repeatedcv", number=5, repeats=5)
grid <- data.frame(k=c(1,2,3,4,5,6,7,8,9,10))
modeloStdRepCv <- train(default~., data = Default, method = "knn", metric="Kappa",
                        tuneGrid=grid,
                        preProcess = c("center","scale"),
                        trControl=trControlRepCv)
modeloStdRepCv
plot(modeloStdRepCv)

#El K ideal se encuentra entre ....... y ........

predicciones <- predict(modeloStdRepCv, newdata=Default) #type "raw" por defecto
confusionMatrix(predicciones, Default$default)

# Esta estimaciOn del kappa y accuracy es mAs confiable
