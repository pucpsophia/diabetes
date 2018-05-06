install.packages("survey")
library(survey)
data(api)
head(apipop)
dim(apipop)
names(apipop)
# define context
attach(apipop)
# estratificacion
table(stype)
# apply cross 
by(enroll, stype, mean, na.rm=T)
by(api00, stype, mean)

# estratifciado

table(stype)
#muestra
set.seed(12345)
#indice colegios elementales 
index_e = c(sample(which(stype=="E"), 100))
index_h = c(sample(which(stype=="H"), 50))
index_m = c(sample(which(stype=="M"), 50))

index = c(index_e, index_h, index_m )

sample1 = apipop[index,]
head(sample1)

desenoMAE <- svydesign(id=~1, strata =~stype, fpc = c(rep(4421,100), rep(755, 50), rep(1018, 50) ), data = sample1)
svymean(~api00, desenoMAE, na.rm=TRUE, deff = TRUE)
# deff efecto disenio 1.15 lo que quiere decir que MASS es mejor en este caso  
svytotal(~enroll, desenoMAE, na.rm=TRUE, deff = TRUE)
# efecto disenio , entre menor a uno mejor 

# disenio mass
N =  dim(apipop)[1]
indexmass <-  sample(N, 200)
sample_mass <-  apipop[indexmass, ]
disenoMASS <- svydesign(id=~1, fpc = rep(N, 200), data =  sample_mass )
svymean(~api00, disenoMASS)
svytotal(~enroll, disenoMASS, na.rm=TRUE)


install.packages("sampling")
library(sampling)
names(apipop)
#obtener muestras 
strata(apipop, c("stype"), size = c(100, 50, 50), method = "srswor")
