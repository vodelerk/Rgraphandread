x<- c(1,2,10,20,10)
x[x==10]
table(x)

y <- matrix(1:20, nrow = 5, ncol=4)


diabetes <- c("Type1","Type2","Type1","Type1")
diabetes <- factor(diabetes)

PatientStatus<-factor(c("Poor","Improved","Excellent"), ordered = TRUE)

x<-factor(c("yes","yes","no","yes","no"))
x


x<-factor(c("yes","yes","no","yes","no"), levels=c("yes","no"))

patientID <- c(1,2,3,4)
age <- c(25,34,28,52)
diabetes <- c("Type2","Type1","Type1","Type1")
status <- c("Poor","Improved","Excellent","Poor")
patientData <- data.frame(patientID,age,diabetes,status)

x<-c(1,2,NA,10,3)
is.na(x)
is.nan(x)

newpatientdata <- edit(patientData)

dfusedcars <-read.table("usedcars.csv")
dfusedcars


otravar <- read.csv("usedcars.csv")
otravar

estudiantes <- read.csv("studentgrades.csv", colClasses = c("numeric","character","character","numeric","numeric"))
estudiantes
str(estudiantes)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv?accessType=DOWNLOAD%22" 
download.file(fileUrl,destfile = "uscommunities.csv")


name <- c("Alex","lilly","Mark","oliver")
age <- c(25,31,23,52)
height <- c(177,163,190,179)
weight <- c(57,69,83,75)
sex <- as.factor(c("F","F","M","M"))
df <- data.frame(row.names=name,age,height,weight,sex)


levels(df$sex) <- c("M","F")


head(sb20122,5)
colnames(sb20121)

