

####################################################
# 1. Analisis exploratorio (basico)

#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)

#Datos
data(GermanCredit)

#Estadisticas descriptivas
summary(GermanCredit)
str(GermanCredit)

####################################################
# 2. Separar el dataset en training y test set usando caret

set.seed(1234)
Train <- createDataPartition(GermanCredit$Class, p=0.8, list=FALSE)
training <- GermanCredit[Train, ]
testing <- GermanCredit[-Train, ]

####################################################
# 3. Entrenar un modelo por regresi�n lineal y predecir CAMBIARLO POR CARET LM

training$Class2<-as.numeric(training$Class)
training$Class2[training$Class2==2]<-0

#Empleando caret: caret nos indica que estamos empleando el modelo incorrecto dado que tenemos un problema de clasificacion
modelo_mpl<-train(Class2~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training, method = "lm", metric = "RMSE")
modelo_mpl<-train(Class~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training, method = "lm", metric = "RMSE")

#A continuaci�n se estima el modelo por regresion lineal pero utilizando la funcion lm
modelo_mpl<-lm(Class2~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training)
summary(modelo_mpl) 

# Se realizan predicciones con el metodo predict, aplica el modelo entrenado a los datos de evaluacion
predicciones <- predict(object=modelo_mpl,testing[,-10]) #type "raw" por defecto

#�Que observa?
summary(predicciones)

#Convertir las predicciones en formato numerico (van en un rango de -0.1144 a 0.7639) a una variable categorica, default es utilizar como corte el valor de 0.5
predicciones_mpl<-ifelse(predicciones>0.5,"Good","Bad")

# Analizar una nueva instancia
nuevaInstancia <- data.frame(Duration=50,CreditHistory.NoCredit.AllPaid=1,Purpose.UsedCar=1,CheckingAccountStatus.none=0,Housing.Own=0)
predict(modelo_mpl, newdata = nuevaInstancia)

# Calcular la matriz confusi�n: 
confusionMatrix(data=predicciones_mpl, testing$Class)
#Accurancy: ......
#Kappa: ......
#�Considera que este es un buen modelo?

#Sensitivity:....
#Specificity:....
#�Que le indica la especificidad y la sensibilidad?

####################################################
# 4. Entrenar un modelo por regresi�n logistica y predecir

mod_logit <- train(Class ~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training, method="glm", family="binomial")


# Se realizan predicciones con el metodo predict
predicciones <- predict(mod_logit, testing[,-10]) #type "raw" por defecto
head(predicciones,10)
prediccionesProb <- predict(mod_logit, testing[,-10], type="prob")
head(prediccionesProb,10)


####################################################
# 5. Analizar una nueva instancia
nuevaInstancia <- data.frame(Duration=50,CreditHistory.NoCredit.AllPaid=1,Purpose.UsedCar=1,CheckingAccountStatus.none=0,Housing.Own=0)
predict(mod_logit, newdata = nuevaInstancia)

####################################################
# 6. Interpretar coeficientes: odds ratios

exp(coef(mod_logit$finalModel))

         
# Duration 0.9636512: 
#Interpretacion Duration: Por cada dia adicional que dure el credito respecto al promedio de 21 d�as, 
#la probabilidad de que un cliente sea catalogado como un buen deudor se incrementa en 1.98 puntos.

# CheckingAccountStatus.none 5.6035617  : una persona sin status en la cuenta corriente tiene una 
#probabilidad 5.6 veces mayor de ser catalogado como un cliente bueno que uno que s� tiene.


#Interprete los otros:
# Housing.Own  1.7777031 
# CreditHistory.NoCredit.AllPaid   0.2855018   
# Purpose.UsedCar 3.5411033 

####################################################
# 7. Calcular la matriz confusi�n
confusionMatrix(data=predicciones, testing$Class)

#Accurancy: ......
#Kappa: ......
#�Considera que este es un buen modelo?
#�Es mejor el modelo por regresion lineal o el modelo  por regresion logistica?

#Sensitivity:....
#Specificity:....
#�Que le indica la especificidad y la sensibilidad?










mod_tree <- train(Class ~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training, method="knn")
mod_tree <- train(Class ~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training, method="nb")
mod_tree <- train(Class ~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training, method="rpart")
mod_tree <- train(Class ~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training, method="rf")


# Se realizan predicciones con el metodo predict
predicciones <- predict(mod_tree, testing[,-10]) #type "raw" por defecto
confusionMatrix(data=predicciones, testing$Class)
