#Universidad Nacional Autonoma de Mexico
#FES Acatlan
#proyecto de series de tiempo
#semestre 2021-II

#Automoviles ecologicos vendidos en Mexico 

install.packages("tseries")
install.packages("lmtest")
install.packages("stats4")
install.packages("strucchange")
install.packages("FinTS")

library(readxl)
library(tseries)
library(lmtest)
library(stats4)
library(strucchange)
library(FinTS)



file.choose()
ruta_excel<-"C:\\Users\\VICENTE\\Documents\\Proyecto Series de tiempo\\Analisis-de-Datos-Automóviles-Ecológicos.xlsx"
Datos_Autos<-read_excel(ruta_excel,sheet="serie")

#Creamos el objeto de series de tiempo
tsA<-ts(Datos_Autos$Autos,start = c(2016,1), frequency = 12)
tsA

plot.ts(tsA, col = "blue", xlab="Fecha",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico",sub="2016-2020")

#Podemos ver, que nuestra serie no es homocedastica, de igual manera presenta una tendencia
#A simple vista no es facil ver si tiene variacion estacional 

dev.off()
G1 <- par(mfrow = c(1, 3))
plot.ts(tsA,xlab="Fecha",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico",sub="Enero de 2016-Enero de 2021")
acf(tsA, lag.max = 36, main="ACF Autos", xlab="Rezago")
acf(tsA, type = "partial", lag.max = 36, main="PACF Feminicidos", xlab="Rezago")

#procedemos a realizar un transformacion para que sea homocedastica.
#Usando Logaritmo

ln_tsA<-log(tsA)
ln_tsA
dev.off()
plot.ts(ln_tsA,xlab="Fecha",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico (ln)",sub="Enero de 2016-Enero de 2021")

dev.off()  
G2 <- par(mfrow = c(1,3))
plot.ts(ln_tsA,xlab="Fecha",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico (ln)",sub="Enero de 2016-Enero de 2021")
acf(ln_tsA, lag.max = 36, main="ACF Feminicidos", xlab="Rezago")
acf(ln_tsA, type = "partial", lag.max = 36, main="PACF Feminicidos", xlab="Rezago")


#logaritmo con primera diferencia , para quitar la tendencia 
lnd_tsA<-diff(log(tsA),1)
plot.ts(lnd_tsA,xlab="Fecha",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico (lnd)",sub="Enero de 2016-Enero de 2021")
#a ojo de buen cubero ya se ve estacionaria :)

#Prueba 

#veamos las tres graficas juntas 
par(mfrow = c(1,3))
plot.ts(tsA, col = "blue", xlab="Fecha",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico",sub="Enero de 2016-Enero de 2021")
plot.ts(ln_tsA,xlab="Fecha",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico (ln)",sub="Enero de 2016-Enero de 2021")
plot.ts(lnd_tsA,xlab="Fecha",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico (lnd)",sub="Enero de 2016-Enero de 2021")


#Vemos que si eliminamos la tendencia por lo que no es necesario recurrir a otra. 
dev.off()
G3 <- par(mfrow = c(1, 3))
plot.ts(lnd_tsA,xlab="Fecha",col="blue",ylab="Autos",main="Automoviles Ecologicos vendidos en Mexico (lnd)",sub="Enero de 2016-Enero de 2021")
acf(lnd_tsA, col="orange", lag.max = 48, main="ACF Autos", xlab="Rezago")
acf(lnd_tsA,col="purple",  type = "partial", lag.max = 48, main="PACF Autos", xlab="Rezago")

###Proponemos los modelos para nuestra serie###
#Para empezar a proponer los modelos, podemos ver en nuestra acf, que no hay un ciclo que se vea a simple vita
#Por lo que podemos pensar en que nuestros modelos serán Arimas y no tendremos que recurrir 
# a un sarima.
#Propusimos varios arimas y nos quedamos con los 3 que más nos interesaron por 
# lo que se veía en los coeficientes resultantes del test
# Nuestro alpha es de 0.05

# 1er modelo significativo
ARIMA_4_1_2 <- arima(log(tsA), order = c(4,1,2))
ARIMA_4_1_2 
coeftest(ARIMA_4_1_2)

#2do modelo significativo
ARIMA_2_1_2 <- arima(log(tsA), order = c(2,1,2))
ARIMA_2_1_2
coeftest(ARIMA_2_1_2)

#3er modelo significativo
ARIMA_2_1_0 <- arima(log(tsA), order = c(2,1,0))
ARIMA_2_1_0 
coeftest(ARIMA_2_1_0)



#Propusimos estos SARIMA porque queríamos ver lo que planteamos en nuestra investigación. 
#Sin embargo, no fueron descartados desde que se le aplicaron el test de coeficientes 
#ya que tambien representan sus valores significativos. 
#Propusimos varios sarimas y nos quedamos con los 3 que mas nos interesaron

#######proponemos modelos sarimas###

#primer modelo significativo
SARIMA_1_1_0_12_0_0 <- arima(log(tsA), order = c(1,1,0), seasonal = list(order = c(1,0,0), period = 12))
SARIMA_1_1_0_12_0_0
coeftest(SARIMA_1_1_0_12_0_0)

#segundo modelo significativo
SARIMA_0_1_1_12_0_0 <- arima(log(tsA), order = c(0,1,1), seasonal = list(order = c(1,0,0), period = 12))
SARIMA_0_1_1_12_0_0
coeftest(SARIMA_0_1_1_12_0_0)


#Tercer modelo significativo
SARIMA_2_1_0_0_0_12 <- arima(log(tsA), order = c(2,1,0), seasonal = list(order = c(0,0,1), period = 12))
SARIMA_2_1_0_0_0_12
coeftest(SARIMA_2_1_0_0_0_12)




################### MODELOS #####################################
# En cada uno de los modelos graficamos las ACF's y PACF's, ya que son indicadores un 
# tanto analaticos pues nos daban un idea del comportamiento del modelo 
#Modelo 1 ARIMA
dev.off()
Gf1<- par(mfrow = c(1, 2))
acf(ARIMA_4_1_2$residuals, lag.max = 48, main="ACF Autos modelo4", xlab="Rezago")
acf(ARIMA_4_1_2$residuals, type = "partial", lag.max = 48, main="PACF Autos Resi f1", xlab="Rezago")
#Modelo 2 ARIMA
dev.off()
Gf2<- par(mfrow = c(1, 2))
acf(ARIMA_2_1_2$residuals, lag.max = 48, main="ACF Autos modelo4", xlab="Rezago")
acf(ARIMA_2_1_2$residuals, type = "partial", lag.max = 48, main="PACF Autos Resi f2", xlab="Rezago")
#Modelo 3 ARIMA
dev.off()
Gf3<- par(mfrow = c(1, 2))
acf(ARIMA_2_1_0$residuals, lag.max = 48, main="ACF Autos modelo4", xlab="Rezago")
acf(ARIMA_2_1_0$residuals, type = "partial", lag.max = 48, main="PACF Autos Resi f3", xlab="Rezago")



#modelo1
#SARIMA_1_1_0_12_0_0
dev.off()
Gf4<- par(mfrow = c(1, 2))
acf(SARIMA_1_1_0_12_0_0$residuals, lag.max = 48, main="ACF Autos modelo1", xlab="Rezago")
acf(SARIMA_1_1_0_12_0_0$residuals, type = "partial", lag.max = 48, main="PACF Autos Resi f1", xlab="Rezago")

#modelo2
#SARIMA_0_1_1_12_0_0
dev.off()
Gf5<- par(mfrow = c(1, 2))
acf(SARIMA_0_1_1_12_0_0$residuals, lag.max = 48, main="ACF Autos modelo2", xlab="Rezago")
acf(SARIMA_0_1_1_12_0_0$residuals, type = "partial", lag.max = 48, main="PACF Autos Resi f2", xlab="Rezago")

#modelo3
#SARIMA_2_1_0_0_0_12
dev.off()
Gf6<- par(mfrow = c(1, 2))
acf(SARIMA_2_1_0_0_0_12$residuals, lag.max = 48, main="ACF Autos modelo4", xlab="Rezago")
acf(SARIMA_2_1_0_0_0_12$residuals, type = "partial", lag.max = 48, main="PACF Autos Resi f3", xlab="Rezago")

##### ##Criterios de seleccion####
#ya que tenemos un cantida poca de datos(62) unicamente usaremos Akoike

AIC(ARIMA_4_1_2)#37
AIC(ARIMA_2_1_2)#38
AIC(ARIMA_2_1_0)#41
AIC(SARIMA_1_1_0_12_0_0)#39
AIC(SARIMA_0_1_1_12_0_0)#37
AIC(SARIMA_2_1_0_0_0_12)#32

# segun la prueba de Akoike resulta que el mejor es el modelo SARIMA_2_1_0_0_0_12
#para BIC tambien nos confirma que el mejor modelo es SARIMA_2_1_0_0_0_12

coeftest(ARIMA_4_1_2) #pronostico 2330.80
coeftest(SARIMA_2_1_0_0_0_12) #pronostico 2768

##Validacion de supuestos
##### los residuos #####
resid <- SARIMA_2_1_0_0_0_12$residuals
resid

dev.off()
plot.ts(resid)
hist(resid, main = "Histograma de los Residuales",freq = F, xlab= "Residuales", ylab = "Densidad", col = "purple")
lines(density(resid),col = "red")

dev.off()
RP <- par(mfrow = c(1, 3))
plot.ts(resid, main="Residuales", col= "purple")
acf(resid, lag.max = 100, col= "blue", main="ACF Residuales")
acf(resid, type = "partial", col = "pink", main="PACF Residuales", lag.max = 100)

#####Los supuestos de los modelos####
####ADF TEST intenta probar estacionaridad H0 es de raiz unitaria que es un proceso no estacionario
adf_pv_resid<-NULL
for(i in 1:20){adf_pv_resid[i]<-adf.test(resid, k=i)$p.value}
adf_pv_resid
#para el primer resago es estacionario 

##Ljnug-Box Test hipotesis nula que es ruido blanco
lb_pv_resid<-NULL
for(i in 1:40){lb_pv_resid[i]<-as.numeric(Box.test(resid, type = 'Ljung-Box', lag = i)$p.value)}
lb_pv_resid

##### Pronostico #####
fore<-predict(SARIMA_2_1_0_0_0_12, n.ahead = 12)
fore
dev.off()
ts.plot(tsA, exp(fore$pred),col="blue", xlim=c(2018,2022), ylim=c(800,4000), main="Autos ecologicos vendidos en Mexico - Pronostico")
U <- exp(fore$pred)+exp(fore$se) 
L <- exp(fore$pred)-exp(fore$se)
xx <- c(time(U), rev(time(U))) 
yy <- c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha =.2))
U
L
par(mfrow = c(1, 2))
plot(U)
plot(L)

#pronostico para febrero 2768-3136 

