###########################################################
###########################################################
###########################################################
#                   Diplomado
#     Anal???tica y Grandes Vol???menes de Datos
#               Universidad Icesi
###########################################################
#          Estad???stica para la anal???tica                 
###########################################################
#           Julio C???sar Alonso
#           Profesor Titular
#           jcalonso@icesi.edu.co
###########################################################
###########################################################
###########################################################
###########################################################
#     Ejercicio ANOVA y dendrogram

# Objetivo: estimar un modelo para las ventas en funci???n de EDUCAC, EXPLAB,VSUPER y EDAD




######################
# Primer paso        #
# lectura de datos   #
######################
datos<-read.csv(file.choose(), sep=",")  # leemos los datos  EjRegmulti.csv


########################################
#            Segundo paso              #
# estimaci???n del modelo empleando MCO  #
#######################################
head(datos)
class(datos)

class(ventas)
str(datos)

datos$VSUPER <- as.factor(datos$VSUPER)
# forma 1
R1 <- lm(VENTAS ~ EDUCAC + EXPLAB + VSUPER + EDAD, data= datos)

# forma 2
formula<-VENTAS ~ EDUCAC + EXPLAB + VSUPER + EDAD
R1 <- lm(formula, data= datos)
# forma 3

R1 <- lm(VENTAS ~ EDUCAC + EXPLAB + VSUPER + EDAD, datos )

R1
summary(R1)
print(R1)
attributes(R1)
layout(matrix(c(1,2,3,4),2,2)) 
plot(R1)
anova(R1)   # Tabla ANOVA
coef(R1)    # coeficientes
confint(R1, level = 0.95) # IC para coef
predict(R1)
predict(R1, interval="confidence")   # media
predict(R1, interval="prediction")   # realizaci???n


r1<-summary(R1)
attributes(r1)
plot(r1$residuals)
r1
# parece que la edad no es importante, podemos eliminar esta variable


R2 <- lm(VENTAS ~ EDUCAC + EXPLAB + VSUPER, datos )  
summary(R2)

