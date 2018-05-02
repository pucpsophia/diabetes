install.packages("psych")
install.packages("pastecs")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("descr")
install.packages("car")
install.packages("MASS")
install.packages("caret")
install.packages("glmnet")
install.packages("BLR")
install.packages("lars")
install.packages("corrplot")

library(corrplot)
library(lars)
library(BLR)
library(glmnet)
library(caret)
library(MASS)
library(car)
library(stats) 
library(descr)
library(corrplot)
library(ggplot2)
library(dplyr)
library(psych)
library(pastecs)

setwd(dir = "/Users/gvalderrama/Documents/Studio/diabetes")
#setwd(dir = "/Users/gregory/Documents/pucp/diabetes")
#setwd(dir = "f://diabetes")

data <- read.csv(file="data.csv", header=TRUE, sep=",", 
                 colClasses = c("character"))

data$EDAD <- as.numeric(data$EDAD)
data$TIEMPODIAG <- as.numeric(data$TIEMPODIAG)
data$PESO <- as.numeric(data$PESO)
data$TALLA <- as.numeric(data$TALLA)
data$IMC <- as.numeric(data$IMC)
data$HEMOGLOBINA <- as.numeric(data$HEMOGLOBINA)
data$GLICOSILADA <- as.numeric(data$GLICOSILADA)
data$TRIGLICERIDOS <- as.numeric(data$TRIGLICERIDOS)
data$COLESTEROL <- as.numeric(data$COLESTEROL)
data$HDL <- as.numeric(data$HDL)
data$ALBUMINA <- as.numeric(data$ALBUMINA)
data$BUN <- as.numeric(data$BUN)
data$UREA <- as.numeric(data$UREA)
data$CREATININA <- as.numeric(data$CREATININA)
data$TFGCKD <- as.numeric(data$TFGCKD)
data$TFGMDRD <- as.numeric(data$TFGMDRD)

dim(data)
str(data)
names(data)
head(data)
summary(data)

#TFG = EDAD , SEXO, CRERATENINA
# filter(data, SEXO == "F")
data_selected <- select(data, -(EDAD), -(PESO) , - (TALLA), -(ALIAS), -(SEXO), -(ECIVIL), -(TRATAMIENTO), -(HTA), -(PIEDIABETICO), -(TFGMDRD), -(CREATININA))
describe(data_selected)
names(data_selected)
colnames(data_selected) <- c("TIE", "IMC", "HEMO", "GLI", "TRI", "COL", "HDL", "ALB","BUN", "URE",  "TFG" )


#transformar datos entre 0 y 1 
max_data <- as.vector(apply(data_selected, 2, max))
min_data <- as.array(apply(data_selected, 2, min))

data_scaled <- as.data.frame(scale(data_selected,center = min_data, scale = max_data - min_data))
boxplot(data_scaled)
#correlaciones
cor(data_scaled)
# las columnas con mayor corelacion lineal 
hist(data_scaled$TFG)
hist(data_scaled$BUN)
hist(data_scaled$GLI)
hist(data_scaled$HEMO)
hist(data_scaled$IMC)

plot(data_scaled$URE , data_scaled$TFG)
plot(data_scaled$BUN , data_scaled$TFG)
plot(data_scaled$GLI , data_scaled$TFG)
plot(data_scaled$HEMO , data_scaled$TFG)
plot(data_scaled$IMC , data_scaled$TFG)

lm_URE_TFG = lm(  data_scaled$TFG ~ data_scaled$URE , data = data_scaled)
summary(lm_URE_TFG)
plot(data_scaled$URE , data_scaled$TFG)
abline(lm_URE_TFG)

par(mfrow=c(2,2))
plot(lm_URE_TFG)

par(mfrow=c(1,1))
plot(lm_URE_TFG,which=5)



lm_BUN_TFG = lm(  data_scaled$TFG ~ data_scaled$BUN , data = data_scaled)
summary(lm_BUN_TFG)
plot(data_scaled$BUN , data_scaled$TFG, cex = 1.3,lwd = 2)
abline(lm_BUN_TFG,lwd=3)



names(lm_URE_TFG$coefficients)
stem(lm_URE_TFG$residuals)
abline(a=0,b=0)
stem(Residual)


distinct( select (data, ECIVIL))
distinct( select (data, TRATAMIENTO))
distinct( select (data, PIEDIABETICO))

summarise(data_a, tiempodm_mean = mean(TIEMPODM))

summarise(group_by(data_a, PIEDIABETICO), tipo = n_distinct(PIEDIABETICO), pie = n())

plot(data_a$SEXO)

sample_n(data_a, 10)


sex <- factor(data_a$SEXO, levels=c("F", "M"), labels=c("Femenino", "Masculino"))

head(data)

final_data = data.frame(ALIAS = as.character(data$ALIAS), 
                        EDAD =as.numeric(data$EDAD),
                        SEXO = factor(data$SEXO, levels=c("F", "M"), labels=c("Femenino", "Masculino")),
                        ECIVIL = factor(data$ECIVIL, levels=c("SOLTERO", "CASADO", "VIUDO"), labels=c("Soltero", "Casado", "Viudo")),
                        TIEMPODM = as.numeric(data$TIEMPODM),
                        PESO = as.numeric(data$PESO),
                        TALLA = as.numeric(data$TALLA)
)

head(final_data)

boxplot(final_data$EDAD, main = "Edad", ylab= "years" )
quantile(final_data$EDAD, p=c(.05, .25, .5, .75, .95))

hist(final_data$EDAD, main = "Edad", ylab= "pacientes", xlab = "edad" )

counts <- table(final_data$ECIVIL)
barplot(counts, main="Edad Pacientes", 
        xlab="Number of Gears")

dotchart(final_data$EDAD)


#correlation 


d <-  select(final_data, EDAD , TIEMPODM)

corrplot(cor(d))

plot(final_data$EDAD, final_data$TIEMPODM, xlab="Edad", ylab="DM")


x_tab <- CrossTable(final_data$SEXO, final_data$ECIVIL,
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)


boxplot(TALLA ~ SEXO, data = final_data)

normalize_fun <-  function(x){
  y <- ( x - min(x) ) / (  max(x) - min(x))
  return (y)
}

normalize_fun(final_data$TIEMPODM)

qqnorm(final_data$TIEMPODM)


attach(final_data)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
# plot(EDAD, TIEMPODM, type="b", lty=3, lwd=3, pch=15, cex=2)
plot(EDAD, TIEMPODM, cex=2)
abline(lm(EDAD~TIEMPODM))
title("Regression of EDAD  on TIEMPODM ")
hist(final_data$TIEMPODM, main = "TIEMPO DM", xlab = "DM")
boxplot(final_data$TIEMPODM)
par(opar)
detach(final_data)


hist(final_data$TIEMPODM, main = "TIEMPO DM", xlab = "DM")
plot(density(final_data$TIEMPODM))


boxplot(EDAD ~ TIEMPODM, data=final_data,
        main="Car Mileage Data",
        xlab="TIEMPODM",
        ylab="EDAD")



describe (final_data)

# filter  slice  filter(data, sexo == F)
# slice slice(data, 1:20)
# arrange ordernar 
# select select (data, nfila = fila)
# distinct
# mutate  nuevas variables
# transmute nuevo data frame
# summarise 
# sample_n sample_frac




