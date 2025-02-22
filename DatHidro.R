inp <- read.csv("FDC.csv", na.strings="")
# Gr�fico de caudales.
plot(inp[,2],
     main= "Volumen diario del caudal de los R�os Estrella y Banano desde 1973 hasta 1983",
     type = "l", col="pink", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/d�a")
lines(inp[,3],
      col= "lightblue")

# Resumen de los datos. 
summary(inp[,2:3])

# Histograma del Rio Estrella. 
hist(inp[,2],  main = "Volumen caudal del R�o Estrella desde 1973 hasta 1983", col="pink",  xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/d�a")

# Histograma del rio Banano.
hist(inp[,3],  main = "Volumen diario del caudal del R�o Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/d�a")

names(inp) <- c("Fechas", "Estrella", "Banano")
attach(inp)
Tempdate <- strptime(inp[,1], format = "%d/%m/%Y")
MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN=sum)
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN=sum)


# Caudal anual para los R�os Estrella y Banano.
plot(MAQ_Banano, ylim= c(100, 3000),  xlim= c(1, 10), main = "Volumen anual del caudal de los R�os Estrella y Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")
lines(MAQ_Estrella, col="pink")

# C�lculo del r�gimen mensual del caudal para el R�o Estrella y Banano.
MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN=sum)
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN=sum)

# Gr�fico acumulados mensuales
plot(MMQ_Estrella, main = "Regimen mensual del caudal del R�o Estrella desde 1973 hasta 1983", col="pink", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")

plot(MMQ_Banano, main = "Regimen mensual del caudal del R�o Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")
# An�lisis de correlaci�n con el m�todo de spearman 
corinp <- cor(inp[,2:3], method = "spearman")
plot(Estrella, Banano, main = "An�lisis de correlaci�n entre los caudales del R�o Banano y el R�o Estrella desde 1973 hasta 1983", col= "purple")

# Modelo para predecir el caudal del r�o banano (independiente) con base en el Estrella (dependiente).

inp.lm <- lm(Estrella ~ Banano, data=inp)
summary(inp.lm)
plot(inp.lm)
