# Tarea-02
Exploración de datos
---
title: "Datos de Hidrología"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Tarea 2 Procesamiento de Datos
### Exploración de Datos
#### Emily Bolaños Carvajal

El objetivo de la siquiente tarea es la creación de gráficos de los caudales recolectados durante el periodo de 1973 hasta 1983 del Río Estrella y Río Banano en Costa Rica, a partir de material .csv, para su posterior observación y análisis. Lo anterior se hara por medio del código en R que se visibiliza a continuación.

```{r datos}
inp <- read.csv("FDC.csv", na.strings="")
# Gráfico de caudales.
plot(inp[,2],
     main= "Volumen diario del caudal de los Ríos Estrella y Banano desde 1973 hasta 1983",
     type = "l", col="pink", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/día")
lines(inp[,3],
      col= "lightblue")
```  

El el gráfico anterior se muestran los volumenes anuales durante todos los años de los que se suministraron datos, en celeste se encuentran los datos del Río Banano y en Rosado los del Río Estrella; en la información se observa como el primero tiene un volumen de agua notoriamente mayor en su caudal. 

#### Resumen de los datos. 
Al comparar los datos en una tabla se obtienen los siguientes valores para cada uno de los ríos.
```{r summary}
summary(inp[,2:3])
```

Además, en base a la información obtenida de la graficación de los siquientes historiogramas, se observa que los datos, se aprecia que los valores de agua en el caudal del Río Estrella suelen ser bajos, el Río Banano, comparte la situación.

##### Histograma del Rio Estrella. 

```{r histo}
hist(inp[,2],  main = "Volumen caudal del Río Estrella desde 1973 hasta 1983", col="pink",  xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/día")
```  

##### Histograma del rio Banano.

```{r hist}
hist(inp[,3],  main = "Volumen diario del caudal del Río Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm/día")
```  


Para el trabajo con los datos, se observa una forma de facilitar la programación por medio de la simplificación de nombres, adición de la fecha y de formato tomando en cuenta siempre la importancia de el uso que se le da a las mayúsculas y minúsculas. 

Lo siguiente será utilizado para darle un formato a los datos del archivo y poder crear los MAQ.
```{r names}
names(inp) <- c("Fechas", "Estrella", "Banano")
attach(inp)
```  

```{r Tempdate}
Tempdate <- strptime(inp[,1], format = "%d/%m/%Y")
```  

```{r MAQ}
MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN=sum)
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN=sum)
``` 


### Caudal anual para los Ríos Estrella y Banano.
En base al los siguientes gráfico es posible observar los puntos de mayor caudal para cada río, siendo en años cercanosm Además es fácil observar como los caudales para el Río Banano son generalmente muchos mayores que los del Río Estrella.

```{r anual}
plot(MAQ_Banano, ylim= c(100, 3000),  xlim= c(1, 10), main = "Volumen anual del caudal de los Ríos Estrella y Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")
lines(MAQ_Estrella, col="pink")
```  


### Cálculo del régimen mensual del caudal para el Río Estrella y Banano.
Este código a continuación es similar al MAQ, simplemente se cambia el formato de año "%Y" a mes "%m". 

En el gráfico se observan por aparte los puntos de mayor y menor caudal, los más altos serán noviembre (Río Banano) y mayo (Río Estrella), el más bajo es julio para ambos casos. 
```{r MMQ}
MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN=sum)
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN=sum)
```  

### Gráfico acumulados mensuales
En ambos gráficos podemos observar en el valor X, en fechas, los 12 meses del año, a eso es lo que se le considera como mensual, un promedio de cada mes en base a los 10 años originales de los datos.

```{r mensualest}
plot(MMQ_Estrella, main = "Regimen mensual del caudal del Río Estrella desde 1973 hasta 1983", col="pink", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")
```  

```{r mensualbana}
plot(MMQ_Banano, main = "Regimen mensual del caudal del Río Banano desde 1973 hasta 1983", col="lightblue", xlab = "Fechas", 
     ylab = "Volumen del Caudal en mm")
```  

### Análisis de correlación con el método de spearman 
En el siguiente gráfico de correlación se observa a simple vista que los datos, auqnue sí están correlacionado por su cercanía, no poseen una correlación fuerte, pues hay datos sumamente bajos y altos.
```{r corinp}
corinp <- cor(inp[,2:3], method = "spearman")
plot(Estrella, Banano, main = "Análisis de correlación entre los caudales del Río Banano y el Río Estrella desde 1973 hasta 1983", col= "purple")
```  
 

### Modelo para predecir el caudal del río banano (independiente) con base en el Estrella (dependiente).
En esta última parte del código se pueden apreciar los cuartiles,el coeficiente de correlación y cuatro (4) gráficos complejo.
```{r inp.lm}
inp.lm <- lm(Estrella ~ Banano, data=inp)
summary(inp.lm)
plot(inp.lm)
```  
En base a la información anterior se determina y demuestra la baja correlación entre ambos ríos y se suponen posibles causas de esta, como la geología del terreno y la cobertura.
Finalmente se puede asegurar que el Río Banano, suponiendo que no esté contaminado, es una fuente mayor del recurso hídrico para la zona caribeña.
