


db <- rbind(sb20121, sb20122)

library(rJava)
library(xlsx)
library(readxl)

quibdo <- read_excel("Sedes_quibdo.xlsx", col_names = TRUE)
colnames(quibdo) <- c("Secretaria","C.departamento", "N.departamento", "C.municipio", "N.municipio", "C.Establecimiento", "N.Establecimiento","C.sede", "N.sede", "Zona", "Direccion","Telefono", "Estado.sede", "Niveles", "Modelos", "grados")

#eliminar los campos que no son de interes

quibdo <- quibdo[,-c(1:3, 6:7, 11:12, 14:16)]
head(quibdo)

levels(quibdo$Estado.sede)

quibdo <- quibdo[quibdo$Estado.sede!="CIERRE DEFINITIVO" & quibdo$Estado.sede!="CIERRE TEMPORAL",]

quibdo <- droplevels(quibdo)

table(quibdo$Estado.sede)

quibdo$Estado.sede <- NULL

dim(sb2012)

quibdo_estudiantes <- merge(quibdo, sb2012, by.x = "C.sede", by.y = "COLE_COD_DANE_INSTITUCION", all.x = FALSE, all.y = FALSE)

dim(quibdo_estudiantes)

summary(quibdo_estudiantes)



table(quibdo_estudiantes$Zona)
prop.table(table(quibdo_estudiantes$Zona))*100

library(plyr)


quibdo_sedes<-ddply(quibdo_estudiantes, c("C.sede"), summarise,
                    N    = length(PUNT_MATEMATICAS),
                    meanmath = round(mean(PUNT_MATEMATICAS, na.rm = T),2), 
                    meanleng = round(mean(PUNT_LENGUAJE, na.rm = T),2),
                    meaningles= round(mean(PUNT_INGLES, na.rm = T),2),
                    sdmath = round(sd(PUNT_MATEMATICAS, na.rm = T),2), 
                    sdleng = round(sd(PUNT_LENGUAJE, na.rm = T),2),
                    sdingles= round(sd(PUNT_INGLES, na.rm = T),2),
                    estu_edad=round(mean(ESTU_EDAD, na.rm = T),2))

View(quibdo_sedes)


summarize(quibdo_sedes)

hist(quibdo_sedes$meanmath, main = "Histograma promedio matematicas", col.main = "green", font.main=2, xlab = "Promedio Matematicas", ylab = "Frecuencia")


boxplot(quibdo_sedes$estu_edad, main ='caja de bigotes edad', ylab = 'Edad')

#barplot(quibdo_sedes$N, main ="grafico de barras num estudiantes", )

outlierKD(quibdo_sedes,estu_edad)

qnt <- quantile(quibdo_sedes$estu_edad, probs = c(.25, .75),na.rm = T)
H <- 1.5 * IQR(quibdo_sedes$estu_edad, na.rm = T)
quibdo_sedes$atipicosedad <- ifelse (quibdo_sedes$estu_edad > qnt[2] + H | quibdo_sedes$estu_edad < qnt[1] - H,1,0)


cor(quibdo_sedes)


tabla1 <-table(quibdo_estudiantes$COLE_NATURALEZA, quibdo_estudiantes$COLE_CARACTER)
addmargins(tabla1)
prop.table(tabla1,1)*100
addmargins(tabla1)


top10lenguaje <- quibdo_sedes[ order(-quibdo_sedes[,4]),]
top10lenguaje <- top10lenguaje[c(1:10),]
top10lenguaje <- droplevels(top10lenguaje)
top10lenguaje[,"N.sede"]