install.packages("psych")
install.packages("pastecs")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("descr")


library(descr)
library(corrplot)
library(ggplot2)
library(dplyr)
library(psych)
library(pastecs)

# setwd(dir = "/Users/gvalderrama/Documents/Studio/diabetes")
setwd(dir = "/Users/gregory/Documents/pucp/diabetes")
#setwd(dir = "f://diabetes")

data <- read.csv(file="data.csv", header=TRUE, sep=",", 
                 colClasses = c("character"))

head(data)
dim(data)
describe(data)
stat.desc(data)


filter(data, SEXO == "F")
data_a <- select(data, -(TFG))
head(data_a)

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




