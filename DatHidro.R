inp <- read.csv("FDC.csv", na.strings="")
# Gráfico de caudales.
plot(inp[,2],
     main= "Volumen diario del caudal de los Ríos Estrella y Banano desde 1973 hasta 1983",
     type = "l", col="pink", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/día")
lines(inp[,3],
      col= "lightblue")

# Resumen de los datos. 
summary(inp[,2:3])

# Histograma del Rio Estrella. 
hist(inp[,2],  main = "Volumen caudal del Río Estrella desde 1973 hasta 1983", col="pink",  xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/día")

# Histograma del rio Banano.
hist(inp[,3],  main = "Volumen diario del caudal del Río Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/día")

names(inp) <- c("Fechas", "Estrella", "Banano")
attach(inp)
Tempdate <- strptime(inp[,1], format = "%d/%m/%Y")
MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN=sum)
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN=sum)


# Caudal anual para los Ríos Estrella y Banano.
plot(MAQ_Banano, ylim= c(100, 3000),  xlim= c(1, 10), main = "Volumen anual del caudal de los Ríos Estrella y Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")
lines(MAQ_Estrella, col="pink")

# Cálculo del régimen mensual del caudal para el Río Estrella y Banano.
MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN=sum)
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN=sum)

# Gráfico acumulados mensuales
plot(MMQ_Estrella, main = "Regimen mensual del caudal del Río Estrella desde 1973 hasta 1983", col="pink", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")

plot(MMQ_Banano, main = "Regimen mensual del caudal del Río Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")
# Análisis de correlación con el método de spearman 
corinp <- cor(inp[,2:3], method = "spearman")
plot(Estrella, Banano, main = "Análisis de correlación entre los caudales del Río Banano y el Río Estrella desde 1973 hasta 1983", col= "purple")

# Modelo para predecir el caudal del río banano (independiente) con base en el Estrella (dependiente).

inp.lm <- lm(Estrella ~ Banano, data=inp)
summary(inp.lm)
plot(inp.lm)
