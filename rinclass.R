bancos <- read.csv("bank-full-clase.csv", header=T, sep=";")
bancos[c(1:10),c(1:3)]
dim(bancos)

names(bancos)

names(bancos)[1] <- "age"
names(bancos)[14] <- "Number.child"

str(bancos)

sapply(bancos,class)

table(sapply(bancos,class))
summary(as.character(bancos$age))


cols<- c(1,6,10)
bancos[,cols]<-apply(bancos[,cols],2,function(x) as.numeric(x))

nacimiento <- "1987-01-21"
nacimiento <- as.Date(nacimiento,format = "%Y-%m-%d")

Sys.Date()


Sys.Date() -nacimiento

#(Sys.date(), )

library(lubridate)

bancos$fecha <- paste(bancos$day,bancos$month,bancos$year,sep = "-")
bancos$fecha <- as.Date(bancos$fecha, format = "%d-%b-%Y")
bancos$fecha <- as.Date(paste(bancos$day,bancos$month,bancos$year,sep = "-"))

bancos<-bancos[,-21]
bancos$fecha


bancos$dia <- day(bancos$fecha)
bancos&mes <- month(bancos$fecha)
bancos$anio <- year(bancos$fecha)

duplicated(bancos)
table(duplicated(bancos))
table(duplicated(bancos$age,bancos$Number.child))
table(bancos$age)
as.data.frame(table(bancos$age))
bancos2 <- bancos[!duplicated(bancos$age,bancos$Number.child),]
duplicated(bancos$age)


is.na(bancos)
table(is.na(bancos))
bancos$fecha<-NULL

colSums(is.na(bancos))
table(rowSums(is.na(bancos)))

levels(bancos$job)
table(bancos$job)
table(is.na())

chocolate <- c(1,0,0,1,1,1,1,0,0,"NA",0,0)
is.factor(chocolate)
is.numeric(chocolate)
chocolate.f <- factor(chocolate, labels=c("No","Si","NA"))
is.na(chocolate.f)
levels(chocolate.f)
levels(chocolate.f)!="NA"
levels(chocolate.f)[levels(chocolate.f)!="NA"]

#chocolate.f2 <- factor(chocolate.f,levels(chocolate.f))[levels(chocolate.f)!="NA"])

bancos.svp <- bancos[complete.cases(bancos),]

data(iris)
iris$Sepal.Length[1:10] <- NA
iris$Sepal.Width[40:50] <- NA
iris$Petal.Width[70:90] <- NA

summary(iris)

iris$Petal.Width[is.na((iris$Petal.Width))]<-mean(iris$Petal.Width,na.rm = TRUE)
summary(iris$Petal.Width)


iris$Sepal.Width[is.na(iris$Petal.Width) & iris$Species =="Setosa"] <-mean(iris$Petal.Width[])

table(bancos$marital)
levels(bancos$marital)
install.packages("stringr")
library(stringr)
bancos$marital <- as.factor(str_trim(bancos$marital))
levels(bancos$marital)

table(bancos$housing)
bancos$housing <- tolower(bancos$housing)

table(bancos$child, bancos$Number.child, dnn = c("Has children?","Number of child"))

