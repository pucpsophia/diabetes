# dependencies 

install.packages("psych")
install.packages("pastecs")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("descr")
install.packages("car")

# for mac os 
# install.packages("data.table", dependencies=TRUE)
# update.packages() 
install.packages("MASS")
install.packages("caret")
install.packages("glmnet")
install.packages("BLR")
install.packages("lars")
install.packages("corrplot")
install.packages("ggpubr")
install.packages("GGally")
install.packages("BCA")
install.packages("backports")
install.packages("gvlma")
install.packages("lmtest")

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
library(MASS)

# Load data , please set the folder 

#setwd(dir = "f://diabetes")

setwd(dir = "/Users/gregory/Documents/pucp/diabetes")
data <- read.csv(file="data.csv", header=TRUE, sep=",", colClasses = c("character"))

names(data)
head(data)

# categorical columns 
data$SEXO  <- as.factor(data$SEXO)
data$ECIVIL  <- as.factor(data$ECIVIL)
data$TRATAMIENTO  <- as.factor(data$TRATAMIENTO)
data$PIEDIABETICO  <- as.factor(data$PIEDIABETICO)
data$HTA  <- as.factor(data$HTA)

# numeric columns 

data$EDAD <- as.numeric(data$EDAD)
data$TIEMPODIAG <- as.numeric(data$TIEMPODIAG)
data$PESO <- as.numeric(data$PESO)
data$TALLA <- as.numeric(data$TALLA)
data$IMC <- as.numeric(data$IMC)  # indice de masa corporal
data$HEMOGLOBINA <- as.numeric(data$HEMOGLOBINA) # indicador para medir la anemia
data$GLICOSILADA <- as.numeric(data$GLICOSILADA) # indicador para medir la diabetes
data$TRIGLICERIDOS <- as.numeric(data$TRIGLICERIDOS) # 
data$COLESTEROL <- as.numeric(data$COLESTEROL) #
data$HDL <- as.numeric(data$HDL) # 
data$ALBUMINA <- as.numeric(data$ALBUMINA) #
data$BUN <- as.numeric(data$BUN) #
data$UREA <- as.numeric(data$UREA) #
data$CREATININA <- as.numeric(data$CREATININA) # 
data$TFGCKD <- as.numeric(data$TFGCKD) # tasa de filtracion glomerular 
data$TFGMDRD <- as.numeric(data$TFGMDRD)


dim(data) # 30 row , 22 columns
str(data)

# select only numeric columns for initial analisis 

data_selected <- select(data, -EDAD,-PESO,-TALLA,-ALIAS,-SEXO,-ECIVIL,-TRATAMIENTO,-HTA,-PIEDIABETICO,-TFGMDRD,-CREATININA)

names(data_selected)
# rename columns to generate a nice graphs 

colnames(data_selected) <- c("TIE", "IMC", "HEMO", "GLI", "TRI", "COL", "HDL", "ALB","BUN", "URE",  "TFG" )


describe(data_selected) # summary(data_selected)

# scale to have a nice graph
data_scaled <- as.data.frame(scale(data_selected))

# box plot the data distribution 

boxplot(data_scaled)

# deteccion outliers 
boxplot.stats(data_selected$TIE)

# generate pie chart to categorical variables 

par(mfrow=c(2,3)) 

casados = length(which(data$ECIVIL == "CASADO"))
solteros = length(which(data$ECIVIL == "SOLTERO"))
slices <- c(casados, solteros, viudos)
lbls <- c(
  paste("Casados", "\n" , casados, sep=""), 
  paste("Solteros", "\n" , solteros, sep="") 
)
pie(slices, labels = lbls, col =  c("#7FDBFF", "#B10DC9"))

insulina = length(which(data$TRATAMIENTO == "INSULINA"))
metformina = length(which(data$TRATAMIENTO == "ADO"))
ninguna = length(which(data$TRATAMIENTO == "NINGUNO"))

slices <- c(insulina, metformina, ninguna)
lbls <- c(
  paste("Insulina", "\n" , insulina, sep=""),
  paste("ADO", "\n" , metformina, sep=""),
  paste("Ninguna", "\n" , ninguna, sep=""))
pie(slices, labels = lbls, col =  c("#7FDBFF", "#01FF70", "#3D9970"))


masculinos = length(which(data$SEXO == "M"))
femeninos = length(which(data$SEXO == "F"))
slices <- c(masculinos, femeninos)
lbls <- c( paste("Hombre", "\n" , masculinos, sep=""), paste("Mujer", "\n" , femeninos, sep=""))
pie(slices, labels = lbls, col = c("#7FDBFF", "#B10DC9"))

hta_si = length(which(data$HTA == "SI"))
hta_no = length(which(data$HTA == "NO"))

slices <- c(hta_si, hta_no)
lbls <- c(
  paste("HTA SI", "\n" , hta_si, sep=""),
  paste("HTA NO", "\n" , hta_no, sep=""))
pie(slices, labels = lbls, col =  c("#7FDBFF", "#01FF70"))

isquemico = length(which(data$PIEDIABETICO == "ISQUEMICO"))
neuropatico = length(which(data$PIEDIABETICO == "NEUROPATICO"))

slices <- c(isquemico, neuropatico)
lbls <- c(
  paste("Isquemico", "\n" , isquemico, sep=""),
  paste("Neuropatico", "\n" , neuropatico, sep=""))
pie(slices, labels = lbls, col =  c("#7FDBFF", "#01FF70"))


par(mfrow=c(1,1)) 

# histogram views to numeric variables 

par(mfrow=c(2,3)) 

hist(data$EDAD, col="#39CCCC", main="", xlab="Edad en años", ylab="Frecuencia")
hist(data$TIEMPODIAG, col="#39CCCC", main="", xlab="Años diagnostico", ylab="Frecuencia")
hist(data$PESO, col="#39CCCC", main="", xlab="Peso en kilogramos", ylab="Frecuencia")
hist(data$TALLA, col="#39CCCC", main="", xlab="Talla en metros", ylab="Frecuencia")
hist(data$HEMOGLOBINA, col="#39CCCC", main="", xlab="Hemoglobina (g/dL)", ylab="Frecuencia")
hist(data$GLICOSILADA, col="#39CCCC", main="", xlab="Hemoglobina Glicosilada (%)", ylab="Frecuencia")
hist(data$TRIGLICERIDOS, col="#39CCCC", main="", xlab="Trigliseridos (mg/dL) Glicosilada (%)", ylab="Frecuencia")
hist(data$COLESTEROL, col="#39CCCC", main="", xlab="Colesterol (mg/dL)", ylab="Frecuencia")
hist(data$HDL, col="#39CCCC", main="", xlab="HDL (mg/dL)", ylab="Frecuencia")
hist(data$ALBUMINA, col="#39CCCC", main="", xlab="Albumina (g/dL)", ylab="Frecuencia")
hist(data$BUN, col="#39CCCC", main="", xlab="BUN (mg/dL)", ylab="Frecuencia")
hist(data$UREA, col="#39CCCC", main="", xlab="UREA (mg/dL)", ylab="Frecuencia")
hist(data$CREATININA, col="#39CCCC", main="", xlab="Creatinina (mg/dL)", ylab="Frecuencia")

par(mfrow=c(1,1)) 

"
La colinealidad indica si en el modelo alguna variable independiente es combinación lineal de otras.
http://www.hrc.es/bioest/Reglin_15.html
http://analisisydecision.es/tag/vif/
https://www.statmethods.net/stats/rdiagnostics.html
"
ggpairs(data_selected[,-11] ) 

# remove URE varibale because has colineality with BUN

data_non_colineal <- select(data_selected, -URE )


# analize correlation between independ variables and depend variables 

ggpairs(data_non_colineal) # cor(data_scaled)

# BUN, HEMO, GLI

#  ========================== simple lineal regrasion tfg - bun

ggplot(data_non_colineal, aes( x = BUN, y = TFG)) + geom_point(shape=1) + geom_smooth(method=lm)
lm_tfg_bun = lm(TFG ~ BUN, data = data_non_colineal)
summary(lm_tfg_bun) 

par(mfrow=c(2,2)) 
plot(lm_tfg_bun)
par(mfrow=c(1,1)) 

# normal test of residuals for less than 30 elements ; p-value > 0.05 es normal

shapiro.test(lm_tfg_bun$residuals) # < 0.5 this is not a normal distribution

# distribution of studentized residuals
residuos <- studres(lm_tfg_bun) 
hist(residuos, freq=FALSE, main="")
xfit<-seq(min(residuos),max(residuos),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Breusch-Pagan test. p-value > 0.05 homosedasticity 
ncvTest(lm_tfg_bun)  

#  ========================================================================


#  ========================== simple lineal regrasion tfg - HEMO

ggplot(data_non_colineal, aes( x = HEMO, y = TFG)) + geom_point(shape=1) + geom_smooth(method=lm)
lm_tfg_hemo = lm( TFG ~ HEMO, data = data_non_colineal)
summary(lm_tfg_hemo) # p value superior a 0.5 y R Square bajo descartamos esta regresion simple

# normal test of residuals for less than 30 elements
shapiro.test(lm_tfg_hemo$residuals) # ; p-value > 0.05 es normal
# Breusch-Pagan test. p-value > 0.05 homosedasticity 
ncvTest(lm_tfg_bun)  

# distribution of studentized residuals
residuos <- studres(lm_tfg_bun) 
hist(residuos, freq=FALSE, main="")
xfit<-seq(min(residuos),max(residuos),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


#  ========================================================================

#  ========================== simple lineal regrasion TFG - GLI

ggplot(data_non_colineal, aes( x = GLI, y = TFG)) + geom_point(shape=1) + geom_smooth(method=lm)
lm_tfg_gli = lm(TFG~GLI, data = data_non_colineal)
summary(lm_tfg_gli) # p value superior a 0.5 y R Square bajo descartamos esta regresion simple


# normal test of residuals for less than 30 elements
shapiro.test(lm_tfg_gli$residuals) #  ; p-value > 0.05 es normal
# Breusch-Pagan test. p-value > 0.05 homosedasticity 
ncvTest(lm_tfg_gli)  

#  ========================================================================


#  ========================== multi lineal  regrasion TFG  


# step wise utilization

lm_full  <- lm(TFG ~ ., data = data_non_colineal)
lm_null  <- lm(TFG ~ 1, data = data_non_colineal)


# step backward 
lm_backward <- step(lm_full, direction= "backward")
summary(lm_backward) # HEMO, COL, HDL, BUN ,  R2 = 0.838


lm_forward <- step(lm_null,scope =list(lower= lm_null, upper=lm_full), direction = "forward")
summary(lm_forward) # BUN COL HEMO HDL, R2  = 0.838


lm_both <- step(lm_null,scope =list(upper=lm_full), direction = "both")
summary(lm_both) # BUN COL HEMO HDL, R2 = 0.838



par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(lm_both)
par(mfrow=c(1,1)) # Change back to 1 x 1


# we must reduce the number of variabnles in the model to guarantee the best mean of the independ variable againts depend variable.

lm_final = lm(TFG ~ BUN, data = data_non_colineal)
summary(lm_final) # R2 0.726


# step wise recommendation

lm_final_full = lm(TFG ~ BUN + COL + HEMO + HDL, data = data_non_colineal)
summary(lm_final_full) # R2 0.838

# model without HDL
lm_final_full_n_hdl = lm(TFG~BUN + COL + HEMO, data = data_non_colineal)
summary(lm_final_full_n_hdl) # R2 0.8303

# evaluate if there is a significant change
anova(lm_final_full, lm_final_full_n_hdl) # significancia menor a 0.5 por lo que si hay diferencia 

# model without HEMO
lm_final_full_n_HEMO = lm(TFG ~ BUN + COL, data = data_non_colineal)
summary(lm_final_full_n_HEMO) # R2 0.82
anova(lm_final_full_n_hdl, lm_final_full_n_HEMO) 

# pruebas de bondad
#http://data.library.virginia.edu/diagnostic-plots/
#https://www.statmethods.net/stats/rdiagnostics.html


lm_tfg_model = lm(TFG ~ BUN + COL, data = data_non_colineal)
summary(lm_tfg_model)
qqPlot(lm_tfg_model, main="") #qq plot for studentized resid

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(lm_tfg_model)
par(mfrow=c(1,1)) # Change back to 1 x 1


# normal test of residuals for less than 30 elements
shapiro.test(lm_tfg_model$residuals) #  ; p-value > 0.05 es normal
# Breusch-Pagan test. p-value > 0.05 homosedasticity 
ncvTest(lm_tfg_model)  


# Influential Observations added variable plots 
# av.Plots(lm_tfg_model) Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data_non_colineal)-length(lm_tfg_model$coefficients)-2)) 
plot(lm_tfg_model, which=4, cook.levels=cutoff)

# Influence Plot 
# Data points with large residuals (outliers) and/or high leverage may distort the outcome and accuracy of a regression. Cook's distance measures the effect of deleting a given observation. Points with a large Cook's distance are considered to merit closer examination in the analysis.

influencePlot(lm_tfg_model,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals 
# qq plot for studentized resid
qqPlot(lm_tfg_model, main="Lineal Model QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(lm_tfg_model) 
hist(sresid, freq=FALSE, main="Distribucion de los residuos")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(lm_tfg_model)
bptest(lm_tfg_model)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(lm_tfg_model)

# Evaluate Collinearity
vif(lm_tfg_model) # variance inflation factors 
sqrt(vif(lm_tfg_model)) > 2 # problem?

# summary test
library(gvlma)
gvmodel <- gvlma(lm_tfg_model) 
summary(gvmodel)

summary(lm_tfg_model)$r.squared

# Now we will add the  categorical variables
names(data)
str(data)

# best model numeric variables
lm_tfg__categorical = lm(TFGCKD ~ BUN + COLESTEROL, data = data)


# evaluate tratamiento
lm_tfg__tratamiento = lm(TFGCKD ~ BUN + COLESTEROL + TRATAMIENTO  , data = data)

anova(lm_tfg__categorical, lm_tfg__tratamiento) # p-value > 0.05


lm_tfg__categorical = lm(TFGCKD ~ BUN + COLESTEROL  , data = data)
lm_tfg__categorical_sexo = lm(TFGCKD ~ BUN + COLESTEROL + SEXO  , data = data)
anova(lm_tfg__categorical, lm_tfg__categorical_sexo)


lm_tfg__categorical = lm(TFGCKD ~ BUN + COLESTEROL  , data = data)
lm_tfg__categorical_pie = lm(TFGCKD ~ BUN + COLESTEROL + PIEDIABETICO  , data = data)
anova(lm_tfg__categorical, lm_tfg__categorical_pie)

# PIE Diabetico is a categorical variable that increment the significance
summary(lm_tfg__categorical_pie)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(lm_tfg__categorical_pie)
par(mfrow=c(1,1)) # Change back to 1 x 1

# test normalidad
shapiro.test(lm_tfg__categorical_pie$residuals)
# test homocedasticidad 
ncvTest(lm_tfg__categorical_pie)

gvmodel <- gvlma(lm_tfg__categorical_pie) 
summary(gvmodel)

coefficients(lm_tfg__categorical_pie) # model coefficients
confint(lm_tfg__categorical_pie, level=0.95)
# test anova 
anova(lm_tfg__categorical_pie)
# covariance matrix
vcov(lm_tfg__categorical_pie)

install.packages("relaimpo")
library(relaimpo)
calc.relimp(lm_tfg__categorical_pie,type=c("lmg","last","first","pratt"),
            rela=TRUE)
boot <- boot.relimp(lm_tfg__categorical_pie, b = 1000, type = c("lmg", 
                                            "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result