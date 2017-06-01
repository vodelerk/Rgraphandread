####################################################
# PRIMERA PARTE: Clustering jerárquico
####################################################

####################################################
# 1. Análisis exploratorio
library(ggplot2)
library(caret)

proteinas <- read.table("02 - protein.txt", header = TRUE, sep = "\t")
str(proteinas)
summary(proteinas) 

# De qué se trata el dataset?
# Qué opinan de los valores de los montos comprados
# Qué hay que hacer?

modelo_estandar<- preProcess(proteinas[,-1], method = c("center", "scale"))
proteinas2 <- predict(modelo_estandar, proteinas[,-1])
rownames(proteinas2) <- proteinas$Country
summary(proteinas2)
proteinas2

####################################################
# 2. Clusterización jerárquica

#matriz de distancias entre países
distancias <- dist(proteinas2[,-1], method = "euclidean")
distancias

#Vamos a crear tres plots de los dendrogramas en una sola fila para compararlos
par(mfrow=c(1,3))

modelo_single <- hclust(distancias, method="single")
modelo_single
plot(modelo_single, labels=rownames(proteinas2), main="single", hang=-1)

modelo_complete <- hclust(distancias, method="complete")
plot(modelo_complete, labels=rownames(proteinas2), main="complete", hang=-1)

modelo_average <- hclust(distancias, method="average")
plot(modelo_average, labels=rownames(proteinas2), main="average", hang=-1)

#ahora vamos a analizar los metodos centroid y ward con complete
plot(modelo_complete, labels=rownames(proteinas2), main="complete", hang=-1)

modelo_centroid <- hclust(distancias, method="centroid")
plot(modelo_centroid, labels=rownames(proteinas2), main="centroid", hang=-1)

modelo_ward <- hclust(distancias, method="ward.D2")
plot(modelo_ward, labels=rownames(proteinas2), main="ward", hang=-1)

par(mfrow=c(1,1))

####################################################
# 3. Punto de corte, selección del K
hcClusters <- cutree(modelo_ward, k=5)
plot(modelo_ward, labels=rownames(proteinas2), main="ward", hang=-1)
rect.hclust(modelo_ward, k=5)
proteinas2$hcClusters <- as.factor(hcClusters)
proteinas2
names(proteinas2)

####################################################
# 4. Interpretemos y visualicemos los países en plots de 2 dimensiones

# Veamos los consumos de carne, pescado y frutas/verduras de los países de cada cluster
for(i in 1:5) {
  print(paste("cluster", i))
  print(proteinas[hcClusters==i, c("Country", "RedMeat", "Fish", "Fr.Veg")])
}

# El cluster 1 tiene a países pobres, que comen pocas cantidades, con dietas poco saludables (poco pescado y frutas y verduras)
# El cluster 2 tiene a los países de Europa central que comen bastante carne
# El cluster 3 tiene a los que comen mucho pescado, y casi nada de verduras/frutas
# El cluster 4 tiene países bastante diferentes. Por un lado Italia y Grecia, por otro USSR y Hungría
# El cluster 5 tiene a los dos países de la península ibérica

ggplot(data=proteinas2, aes(x=Fish, y=RedMeat)) + geom_point(size=2, alpha=.5, colour=hcClusters) +
  geom_text(aes(label=rownames(proteinas2), hjust=-0.1), colour=hcClusters)

ggplot(data=proteinas2, aes(x=Fish, y=Fr.Veg)) + geom_point(size=2, alpha=.5, colour=hcClusters) +
  geom_text(aes(label=rownames(proteinas2), hjust=-0.1), colour=hcClusters)

ggplot(data=proteinas2, aes(x=RedMeat, y=Fr.Veg)) + geom_point(size=2, alpha=.5, colour=hcClusters) +
  geom_text(aes(label=rownames(proteinas2), hjust=-0.1), colour=hcClusters)

####################################################
# 5. Usando ggplot, se ve mas bonito
#install.packages("ggdendro")
library(ggdendro)
dendrograma <- dendro_data(modelo_ward, type="rectangle")

# Vamos a modificar un poco la informaciOn utilizada por GGPLOT para pintar los arboles
dendrograma[["labels"]] 

# Queremos agregarle el cluster, creamos un nuevo data frame con el label y el cluster
clusters.df <- data.frame(label=rownames(proteinas2), cluster=factor(hcClusters))
clusters.df

# Hacemos un merge entre los datos del dendrograma y este dataframe 
dendrograma[["labels"]] <- merge(dendrograma[["labels"]], clusters.df, by="label")
dendrograma[["labels"]] 

# Pintamos el dendrograma
ggplot() + 
  geom_segment(data=segment(dendrograma), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=label(dendrograma), aes(x, y, label=label, hjust=0, color=cluster), 
            size=3) +
  coord_flip() +  #lo hacemos vertical
  scale_y_reverse(expand=c(0.2, 0)) + #volteamos el eje y, y le damos mas espacio al texto
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank()
  )

####################################################
# SEGUNDA PARTE: COMPONENTES PRINCIPALES
####################################################

ggplot(data=proteinas2, aes(x=Fish, y=RedMeat)) + geom_point(size=2, alpha=.5, colour=hcClusters) +
  geom_text(aes(label=rownames(proteinas2), hjust=-0.1), colour=hcClusters)

####################################################
# 1. Componentes principales
# Dificil de poder hacerse una idea de los clusters con plots de 2 dimensiones, cuando hay 9 atributos
# usamos PCA

pcomp <- prcomp(proteinas2[,-10])
pcomp

#porcentaje de informacion
varianzasPC <- pcomp$sdev^2
porcentajeInfoPC <- varianzasPC / sum(varianzasPC)
porcentajeInfoPC

sum(porcentajeInfoPC[1:2]) #En los 2 primeros PCs
#62.7%
sum(porcentajeInfoPC[1:3]) #en los 3 primeros PCs
#75.2%


####################################################
# 2. Interpretación y visualización de ejes 

# intentemos entender los PCs
biplot(pcomp, c(1,2))
# el PC1 tiene en el lado positivo las nueces y cereales, y en el negativo las carnes rojas, blancas, huevos y leche
# el PC2 sigue la misma dirección del pescado y las frutas y verduras
biplot(pcomp, c(1,3))
# el PC3 sigue el sentido de las nueces y cereales, y el sentido inverso de todas las proteinas animales

# podríamos obtener los datos ya proyectados directamente 
head(pcomp$x)[,1:3]

# Pero tambi?n podemos utilizar el objeto como un modelo de "predicciOn" para nuevos datos
# proyectamos los datos en los dos primeros componentes principales
proteinas_prcomp <- predict(pcomp, proteinas2)[,1:3]
head(proteinas_prcomp)

proteinas_prcomp <-as.data.frame(proteinas_prcomp)
proteinas_prcomp$hcClusters <- proteinas2$hcClusters

# visualizamos los datos segUn los 2 primeros PCs
ggplot(data=proteinas_prcomp, aes(x=PC1, y=PC2)) +
  geom_point(size=2, alpha=.5, colour=proteinas_prcomp$hcClusters) +
  geom_text(aes(label=rownames(proteinas2), hjust=0.5,vjust=0), colour=proteinas_prcomp$hcClusters)

# el cluster verde y el rojo parecen bien sobrelapados, veamos con el 3er PC
ggplot(data=proteinas_prcomp, aes(x=PC1, y=PC3)) +
  geom_point(size=2, alpha=.5, colour=proteinas_prcomp$hcClusters) +
  geom_text(aes(label=rownames(proteinas2), hjust=0.5,vjust=0), colour=proteinas_prcomp$hcClusters)

# El cluster 1 (negro) tiene a paises pobres, que comen pocas cantidades, con dietas poco saludables (poco pescado y frutas y verduras)
#   --> Comen nueves y cereales 
# El cluster 2 (rojo) tiene a los paises de europa central que comen bastante carne
#   --> Pocas nueces y cerales
# El cluster 3 (verde) tiene a los que comen mucho pescado, y casi nada de verduras/frutas
#   --> Pocas nueces y cerales
# El cluster 4 (azul) tiene paises bastante diferentes. Por un lado Italia y Grecia, por otro USSR y Hungria
# El cluster 5 (cyan) tiene a los dos paises de la penInsula ibErica
#   --> muchas frutas y verduras

####################################################
# TERCERA PARTE: EVALUACION DE CLUSTERING
####################################################


####################################################
# 1. EvaluaciOn de los clusters por bootstrap

#install.packages("fpc")
library(fpc)
set.seed(3333)

#Creamos 300 particionamientos (valor por defecto) diferentes 
modelo_bootstrap <- clusterboot(proteinas2[,-10], clustermethod = hclustCBI, method="ward.D2", k=5, B=300)
clusters_boot<- modelo_bootstrap$result$partition

# los clusters encontrados son los mismos que antes?
for(i in 1:5) {
  print(paste("cluster", i))
  print(proteinas[clusters_boot==i, c("Country", "RedMeat", "Fish", "Fr.Veg")])
}

proteinas_prcomp$hcClustersBoot <- clusters_boot

table(proteinas_prcomp$hcClusters, proteinas_prcomp$hcClustersBoot)
# NO SON LOS MISMOS!!!! Habia overfitting en nuestra visiOn inicial

# visualizamos los datos segUn los 2 primeros PCs
ggplot(data=proteinas_prcomp, aes(x=PC1, y=PC2)) +
  geom_point(size=2, alpha=.5, colour=proteinas_prcomp$hcClustersBoot) +
  geom_text(aes(label=rownames(proteinas2), hjust=0.5,vjust=0), colour=proteinas_prcomp$hcClustersBoot)

modelo_bootstrap$bootmean #vector de las estabilidades de cada cluster
# El clusters 4(azul) es muy estable
# los clusters 1(negro), 2(rojo) y 5(cyan) son estables
# el cluster 3(verde) indica algo de estructura, aunque no muy estable

# Cuantas veces se disolvieron los clusters de las 300 corridas?
modelo_bootstrap$bootbrd

####################################################
# 2. Determinación del K a partir del WSS (within sum of squares)

# Vamos a analizar la variaci?n de la suma de las diferencias cuadradas entre los valores y sus medias
# a medida que se cambia el punto de corte segUn el nUmero de clusters K especificado

# Para K=1:
#----------
# Suma de las varianzas de cada columna del dataset. Recordemos var = (sum(dif cuadradas)) / (n-1)
vars <- apply(proteinas2[-10],2,var)
sumvars <- sum(vars)
#no queremos la varianza sino la suma de las diferencias cuadradas, multiplicamos por (n-1)
wss <- (nrow(proteinas2[-10]) - 1)*sumvars

# Para K>1, calculamos las diferencias cuadradas en un ciclo
#-----------------------------------------------------------
maxK <- 15
for (k in 2:maxK) { 
  #k=2
  wssK <- 0 #Aqui va a quedar el total de WSS para el k actual
  clusters <- cutree(modelo_ward, k=k)
  proteinas2$clusters <- clusters
  #plot(modelo_ward, labels=rownames(proteinas2), main="ward", hang=-1)
  #rect.hclust(modelo_ward, k=k)
  
  for(i in 1:k) { #recorrido de los clusters
    #i=2
    clusterData <- subset(proteinas2[,-c(10,11)], clusters==i)
    centroide <- apply(clusterData, 2, FUN=mean)
    wssK <- wssK + sum(apply(clusterData, 1, FUN=function(fila) {sum((fila-centroide)^2)}))
  }
  #Ya acabamos este ciclo, guardamos el wss del K en el vector de WSS
  wss[k] <- wssK
}

plot(1:maxK, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")  

# Aunque un codo muy bien definido no es visible, podemos intuir que a partir de k=4 o 5,
# la reducción de la varianza no es muy significativa

####################################################
# 3. Determinación del K a partir de silhouette
library(cluster)
plot(silhouette(cutree(modelo_ward,2), distancias ))
plot(silhouette(cutree(modelo_ward,3), distancias ))
plot(silhouette(cutree(modelo_ward,4), distancias ))
plot(silhouette(cutree(modelo_ward,5), distancias ))
plot(silhouette(cutree(modelo_ward,6), distancias ))
plot(silhouette(cutree(modelo_ward,7), distancias ))