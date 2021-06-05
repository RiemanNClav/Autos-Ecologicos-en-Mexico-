
########Analis descriptivo de los datos##########
file.choose()
Base2 = read.csv("C:\\Users\\VICENTE\\Downloads\\Base2.csv")
Base3= read.csv("C:\\Users\\VICENTE\\Downloads\\Base3.csv")

Base3 = Base3[2:7,]
Base3_t = (t(Base3))
Base3_t = data.frame(Base3_t)
Base2 = Base2[2:7,]
colnames(Base3_t) = c("2016", "2017", "2018", "2019", "2020", "2021")
Base3_t = Base3_t[2:13,]

Base2_t = (t(Base2))
Base2_t = data.frame(Base2_t)
Base2_t = Base2_t[2:34,]
colnames(Base2_t) = c("2016","2017", "2018", "2019", "2020", "2021")

#ventas mensuales por año ----------------------------------------------
par(mfrow = c(1,3))
plot(Base3_t$"2016", xlab = "Meses del Año", 
     ylab = "Automóviles Ecológicos", 
     main = "Ventas (2016)", xlim = c(1,12), pch=1, 
     cex=1, col = "blue", lwd = 4, ylim = c(1,3500))

text(Base3_t$"2016", labels = c("EN", "FE","MZO", "AB", "MY", "JUN", 
                               "JUL", "AG", "SE", "OC", "NO", "DIC"), 
     pos = c(1,3))

plot(Base3_t$"2017", xlab = "Meses del Año", 
     ylab = "Automóviles Ecológicos", 
     main = "Ventas (2017)", xlim = c(1,12), pch=1, 
     cex=1, col = "black", lwd = 4, ylim = c(1,3500))
text(Base3_t$"2017", labels = c("EN", "FE","MZ", "AB", "MY", "JUN", 
                               "JUL", "AG", "SE", "OC", "NO", "DIC"), 
     pos = c(1,3))

plot(Base3_t$"2018", xlab = "Meses del Año", 
     ylab = "Automóviles Ecológicos", 
     main = "Ventas (2018)", xlim = c(1,12), pch=1, 
     cex=1, col = "blue", lwd = 4, ylim = c(1,3500))
text(Base3_t$"2018", labels = c("EN", "FE","MZ", "AB", "MY", "JUN", 
                               "JUL", "AG", "SE", "OC", "NO", "DIC"), 
     pos = c(1,3))

par(mfrow = c(1,3))
plot(Base3_t$"2019", xlab = "Meses del Año", 
     ylab = "Automóviles Ecológicos", 
     main = "Ventas (2019)", xlim = c(1,12), pch=1, 
     cex=1, col = "black", lwd = 4, ylim = c(1,3500))
text(Base3_t$"2019", labels = c("EN", "FE","MZ", "AB", "MY", "JUN", 
                               "JUL", "AG", "SE", "OC", "NO", "DIC"), 
     pos = c(1,3))

plot(Base3_t$"2020", xlab = "Meses del Año", 
     ylab = "Automoviles Ecológicos", 
     main = "Ventas (2020)", xlim = c(1,12), pch=1, 
     cex=1, col = "blue", lwd = 4, ylim = c(1,3500))
text(Base3_t$"2020", labels = c("EN", "FE","MZ", "AB", "MY", "JUN", 
                               "JUL", "AG", "SE", "OC", "NO", "DIC"), 
     pos = c(1,3))

plot(Base3_t$"2021", xlab = "Meses del Año", 
     ylab = "Automóviles Ecológicos", 
     main = "Ventas (2021)", xlim = c(1,12), pch=1, 
     cex=1, col = "black", lwd = 4, ylim = c(1,3500))
text(Base3_t$"2021", labels = c("EN", "FE","MZ", "AB", "MY", "JUN", 
                               "JUL", "AG", "SE", "OC", "NO", "DIC"), 
     pos = c(1,3))








#VENTAS TOTALES POR AÑO -------------------------------------------------

Suma1 = sum(Base2_t$"2016")
Suma2 = sum(Base2_t$"2017")
Suma3 = sum(Base2_t$"2018")
Suma4 = sum(Base2_t$"2019")
Suma5 = sum(Base2_t$"2020")
vector = c(rep("2016",Suma1),rep("2017",Suma2),
           rep("2018",Suma3),rep("2019",Suma4),rep("2020",Suma5))
vector = as.factor(vector)
plot(vector, main = "Ventas Totales", col = c("blue","black"), 
     xlab = "Años", ylab = "Automóviles Ecológicos")

#Esatidisticos de cada variable -------------------------------
par(mfrow = c(1,2))
BOXPLOT = boxplot(as.numeric(Base3_t$"2016"), 
                  col = c("blue", "black"), notch = T,
                  main = "Año 2016", 
                  ylab = "Automóviles Ecológicos")
abline(h=median(as.numeric(Base3_t$"2016")), lty=6, col = "black")
abline(h=mean(as.numeric(Base3_t$"2016")), lty=6, col = "red")


BOXPLOT1 = boxplot(as.numeric(Base3_t$"2017"), 
                  col = "blue", notch = T,
                  main = "Año 2017", 
                  ylab = "Automóviles Ecológicos")
abline(h=median(as.numeric(Base3_t$"2017")), lty=6, col = "black")
abline(h=mean(as.numeric(Base3_t$"2017")), lty=6, col = "red")

str(BOXPLOT)
str(BOXPLOT1)

par(mfrow = c(1,2))
BOXPLOT3 = boxplot(as.numeric(Base3_t$"2018"), 
                   col = "blue", notch = T,
                   main = "Año 2018", 
                   ylab = "Automóviles Ecológicos")
abline(h=median(as.numeric(Base3_t$"2018")), lty=6, col = "black")
abline(h=mean(as.numeric(Base3_t$"2018")), lty=6, col = "red")

BOXPLOT4 = boxplot(as.numeric(Base3_t$"2019"), 
                   col = "blue", notch = T,
                   main = "Año 2019", 
                   ylab = "Automóviles Ecológicos")
abline(h=median(as.numeric(Base3_t$"2019")), lty=6, col = "black")
abline(h=mean(as.numeric(Base3_t$"2019")), lty=6, col = "red")

BOXPLOT5 = boxplot(as.numeric(Base3_t$"2020"), 
                   col = "blue", notch = T,
                   main = "Año 2020", 
                   ylab = "Automóviles Ecológicos")
abline(h=median(as.numeric(Base3_t$"2020")), lty=6, col = "black")
abline(h=mean(as.numeric(Base3_t$"2020")), lty=6, col = "red")
str(BOXPLOT3)
str(BOXPLOT4)
str(BOXPLOT5)