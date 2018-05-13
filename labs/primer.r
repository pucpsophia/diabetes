install.packages("lmtest")
install.packages("gvlma")

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
library(dplyr)
library(ggpubr)
library(GGally)
library(lmtest)
library(gvlma)


setwd(dir = "f://diabetes")

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
data_selected_extend <- select(data, -(EDAD), -(PESO) , - (TALLA), -(ALIAS), -(SEXO), -(TRATAMIENTO), -(HTA), -(PIEDIABETICO), -(TFGMDRD), -(CREATININA))
describe(data_selected)
names(data_selected)
colnames(data_selected) <- c("TIE", "IMC", "HEMO", "GLI", "TRI", "COL", "HDL", "ALB","BUN", "URE",  "TFG" )


boxplot(TFGCKD ~ ECIVIL , data_selected_extend)


lm1 = lm( TFG ~ BUN , data = data_selected)
lm2 = lm( TFG ~ BUN + URE  , data = data_selected)

anova(lm1, lm2)

# < 0.5 me quedo con el modelo con mas variables 


lmcompleto <- lm(TFG ~ ., data = data_selected)
lmNulo <- lm(TFG ~ 1, data = data_selected)
summary(lmNulo)

# step backward 
lmback <- step(lmcompleto, direction= "backward")
summary(lmback)

lmfor <- step(lmNulo,scope =list(lower= lmNulo, upper=lmcompleto), direction = "forward")
summary(lmfor)

lmwise <- step(lmNulo,scope =list(upper=lmcompleto), direction = "both")
summary(lmwise)



# Bondad de ajuste
# revizar r cuadro proporcion de variabilidad que puedo explicar
# redisual debe ser pequenio en relacion con 


shapiro.test(fitted(lmwise))

# In statistics, a collection of random variables is heteroscedastic (or heteroskedastic;[a] from Ancient Greek hetero "different" and skedasis "dispersion") if there are sub-populations that have different variabilities from others. Here "variability" could be quantified by the variance or any other measure of statistical dispersion. Thus heteroscedasticity is the absence of homoscedasticity.




bptest(formula = TFG ~ BUN + COL + URE + HEMO, data = data_selected, varformula = ~ fitted.values(lmwise), studentize = FALSE)




gvlma(lm1) 
# Validacion del modelo

