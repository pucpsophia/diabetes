
#######################################
### Mínimos cuadrados generalizados ###
#######################################

data(longley) 
g <- lm(Employed ~ GNP + Population, data=longley)
summary(g,cor=T)
res <-g$residuals
plot(seq(1:16),res, pch=16)
abline(h=0,lty=2)
cor(g$res[-1],g$res[-16])
g$res
g$res[-1]
g$res[-16]

V<-diag(16)
#V<-(1/(1-0.3104092^2))*0.3104092^abs(row(V)-col(V))
V<-0.3104092^abs(row(V)-col(V))
X<-model.matrix(g)
V.inv<-solve(V)
beta<-solve(t(X)%*%V.inv%*%X)%*%t(X)%*%V.inv%*%longley$Empl
beta

res<-longley$Empl-X%*%beta
sig<-sum(res^2)/g$df
sqrt(diag(solve(t(X)%*%V.inv%*%X))*sig)
cor(res[-1],res[-16])

V<-diag(16)
V<-(1/(1-cor(res[-1],res[-16])^2))*cor(res[-1],res[-16])^abs(row(V)-col(V))
X<-model.matrix(g)
V.inv<-solve(V)
beta<-solve(t(X)%*%V.inv%*%X)%*%t(X)%*%V.inv%*%longley$Empl
beta

res<-longley$Empl-X%*%beta
sig<-sum(res^2)/g$df
sqrt(diag(solve(t(X)%*%V.inv%*%X))*sig)
cor(res[-1],res[-16])

library(nlme)
g<-gls(Employed~GNP+Population,correlation=corAR1(form=~Year),data=longley)
summary(g)
intervals(g)

####################################
### Mínimos cuadrados ponderados ###
####################################

GastoX <- c(3, 3,	3, 5,	5, 5,	9, 9,	9, 9,	9, 12, 12,	12,	12,	12,	12,	15,	15,	15,	15,	15,	15,	17,	17,	17,	19,	19,	19,	19)
VendaY <- c(81, 73,	72,	91,	99,	97,	127, 114,	116, 123,	131, 141,	151, 147,	131, 145,	147,	179, 166,	181, 178,	185, 156,	176, 189,	192, 203, 193, 219, 214)
cbind(GastoX,VendaY)

ajuste = lm(VendaY ~ GastoX)
summary(ajuste)
plot(VendaY ~ GastoX, xlab="Gasto", ylab="Ventas")
abline(ajuste)

windows()
par(mfrow=c(1,2))
plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Residuos")
abline(h=0)
plot(GastoX,residuals(ajuste),xlab="Gasto",ylab="Residuos")
abline(h=0)

windows()
par(mfrow=c(1,2))
hist(residuals(ajuste), xlab="Residuos",ylab="Frequencia",main="")
qqnorm(residuals(ajuste), ylab="Residuos",main="")
qqline(residuals(ajuste))

gastovenda <- cbind(GastoX,VendaY)
v <- tapply(gastovenda[,2],as.factor(gastovenda[,1]),var)
gasto <- unique(GastoX, fromLast = TRUE)
plot(gasto,v,xlab="Gasto",ylab="Varianza (Venta|Gasto)" )

ajuste_ponderado <- lm(VendaY ~ GastoX, weights = 1/GastoX)
summary(ajuste_ponderado)
anova(ajuste_ponderado)

windows()
par(mfrow=c(1,2))
plot(fitted(ajuste_ponderado),residuals(ajuste_ponderado)*sqrt(1/GastoX),xlab="Valores Ajustados",ylab="Residuos")
abline(h=0)
plot(GastoX,residuals(ajuste_ponderado)*sqrt(1/GastoX),xlab="Gasto",ylab="Residuos")
abline(h=0)
