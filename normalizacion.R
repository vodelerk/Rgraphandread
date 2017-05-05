##########################################################################
# Taller de normalizaciOn y transformaciOn logaritmica - visualizaciones
# Datos basados en el dataset Wage, del paquete ISLR
##########################################################################

#librerIas necesarias
require(ggplot2)
require(scales)

#Cargar los datos en el dataframe 'salarios'
salarios <- read.csv(file = "02 - Acme.csv")
head(salarios)

# A primera vista, quE pueden decir de las niveles de medida y de las escalas de los atributos?
# ............

# Vamos ahora a visualizar los datos numEricos

#salario x edad, ejes con la misma escala
# AsI se ven los datos desde el punto de vista de los algoritmos que tienen que analizar distancias
qplot(y=salarios$edad, x=salarios$monto, ylab="edad", xlab="salario") + 
  scale_x_continuous(labels=comma) +
  geom_point(color='darkblue') +
  coord_equal(ratio=1)

# Interpreten el grAfico anterior: .............


#salario x edad, ejes con diferentes escalas, pero ajustadas
# Si los ejes estuvieran con magnitudes similares, no habrIa problemas de escala
qplot(y=salarios$edad, x=salarios$monto, ylab="edad", xlab="salario") + 
  scale_x_continuous(labels=comma) +
  geom_point(color='darkblue')

#####################

#salario x edad, datos normalizados
# Estos datos ya estAn transformados con respecto a los problemas de centralizaciOn y escala
# El problema que vemos ahora es que los salarios no estan simEtricamente distribuidos.
# Hay una gran proporciOn de empleados con salarios bajos, y pocos con salarios altos
qplot(y=scale(salarios$edad), x=scale(salarios$monto), ylab="edad", xlab="salario") + 
  scale_x_continuous(labels=comma) +
  geom_point(color='darkblue')

#####################

#densidad de los salarios sin transformar
# Tenemos una distribuciOn de los salarios 'estirada' hacia la derecha
# Algunos algoritmos basados en suposiciones estadIsticas (i.e. distribuciOn normal), dependen
# de la forma de la distribuciOn de los datos, teniendo que ser simEtrica.
qplot(monto, data=salarios, geom="density", fill=empresa, alpha = I(.5),
      ylab="densidad", xlab="salario", main="DistribuciOn de los salarios") +
  scale_x_continuous(labels=comma) +
  theme(legend.position='none')

#####################

#Vamos a aplicar los dos tipos de transformaciones para 'arreglar' la forma
#de la distribuciOn de los datos

#densidad de los salarios despues de transformaciOn logarItmica
qplot(log(monto), data=salarios, geom="density", fill=empresa, alpha = I(.5),
      ylab="densidad", xlab="log(salario)", main="DistribuciOn de log(salario)") +
  scale_x_continuous(labels=comma) +
  theme(legend.position='none')

#densidad de los salarios despues de transformaciOn de raiz
qplot(monto^0.3, data=salarios, geom="density", fill=empresa, alpha = I(.5),
      ylab="densidad", xlab="salario^0.3", main="DistribuciOn de salario^0.3") +
  scale_x_continuous(labels=comma) +
  theme(legend.position='none')