# Entrega final módulo 5.

### Pablo Lorente Domínguez.

#### Introducción.

Para la realización de este ejercicio se ha elegido un dataset con los datos de 3 supermercados de Birmania donde nos centraremos principalmente en averiguar en qué se basan las puntuaciones de satisfacción de los clientes.

#### Contexto

El crecimiento de los supermercados en la mayoría de las ciudades pobladas es cada vez mayor y la competencia en el mercado también es alta. El conjunto de datos es un histórico de ventas de una empresa de supermercados que ha registrado datos en 3 sucursales diferentes (A, B, C) durante 3 meses. Los métodos de análisis de datos predictivos son fáciles de aplicar con este conjunto de datos.

```{r}
library(tidyverse)
library(dplyr)
library(corrplot)
library(caret)
```

```{r}
df <- read.csv("supermarket_sales - Sheet1.csv", stringsAsFactors = T)
ncol(df)
nrow(df)
head(df, 10)
```

Información del dataset.

1. **Invoice id**: Número de identificación de factura de comprobante de venta generado por computadora
2. **Branch**: Sucursal del supercentro (hay 3 sucursales disponibles identificadas por A, B y C).
3. **City**: Ubicación de los supercentros
4. **Customer type**: Tipo de clientes, registro por Socios para clientes con tarjeta de socio y Normal para sin tarjeta de socio.
5. **Gender**: Género del cliente
6. **Product line**: Grupos generales de categorización de artículos - Accesorios electrónicos, Accesorios de moda, Alimentos y bebidas, Salud y belleza, Hogar y estilo de vida, Deportes y viajes
7. **Unit price**: Precio de cada producto en $
8. **Quantity**: Número de productos comprados por cliente
9. **Tax.5.**: 5% de tasa de impuestos por compra del cliente
10. **Total**: Precio total con impuestos incluidos
11. **Date**: Fecha de compra (Registro disponible desde enero de 2019 hasta marzo de 2019)
12. **Time**: Hora de compra (10 a. m. a 9 p. m. ) 
13. **Payment**: Pago utilizado por el cliente para la compra (hay 3 métodos disponibles: efectivo, tarjeta de crédito y billetera electrónica)
14. **COGS**: costo de los bienes vendidos
15. **Gross margin percentage**: Porcentaje de margen
16. **Gross income**: Ingreso bruto
17. **Rating**: Calificación de estratificación del cliente en su experiencia de compra general (en una escala de 1 a 10)

De aquí vemos que hay 17 variables y un total de 1000 registros. La sucursal A se situa en Yangon, la B en Mandalay y la C en Naypyitaw, aunque en principio esto no es relevante eliminaremos ciudad ya que podemos identificarlas por sucursal A, B o C.

```{r}
df$City <- NULL
summary(df)
str(df)
```

Puedo ver que algunas de las variables son categóricas, como Invoice.ID, Branch, Customer.type, Gender, Product.line, Date, Time y Payment.

Tambien se observa que en gross.margin.percentage el valor es el mismo en todos los campos, debido a que este es el margen de beneficio aplicado a la venta de todos los productos.

Inspeccionamos en busca de posibles valores nulos.


```{r}
sapply(df, function(x) sum(is.na(x)))
```

Vemos que no hay valores nulos.

```{r}
for (columna in 1:ncol(df)){
  if (class(df[,columna]) == "factor"){
    # Por defecto se mostrará un gráfico de barras.
    plot(df[,columna], 
         col = topo.colors(length(levels(df[,columna]))),
         las = 1,
         main = paste("Diagrama de barras de: ", colnames(df[columna])))
  } else {
    # Para las variables numéricas, histograma.
    hist(df[, columna],
         border = "blue", 
         col = "tomato", 
         las = 1, 
         main = paste("Histograma de: ", colnames(df[columna])),
         xlab  = colnames(df[columna]))
  }
}
```


Algunas observaciones sobre las gráficas individuales:

* **Branch**: La sucursal con mayor número de clientes ha sido la A y la que menos la C, aunque no se observa una gran diferencia.

* **Customer.type**: Hay prácticamente la misma distribución entre los dos tipos de clientes recogidos en los datos (con membresía o sin membresía).

* **Gender**: La distribución vuelve a ser similar en el género de los clientes.

* **Tax.5., Total, gross.income y cogs**: Siguen exactamente la misma distribución (1-1) tal como se esperaba.

* **gross.margin.percentage**: Es una constante como ya habíamos comentado.

* **Rating**: Comprende valores entre 4 y 10 teniendo su mayor pico en 4.

```{r}
plot(df$Branch, df$Rating, 
     col = heat.colors(length(levels(df$Branch))),
     main = "Distribución de satisfacción por sucursal",
     las = 1)
```

Los clientes de la sucursal B están un poco menos satisfechos que los de la sucursal A y C, habiendo en las tres sucursales una mediana cercana al 7.

```{r}
dfprod<-aggregate(Rating~Product.line,df,mean);dfprod
ggplot(df,aes(x=Product.line,y=Rating,fill=Product.line))+geom_boxplot(trim=F)

```

El valor promedio de mayor satisfacción del cliente es de la categoría de productos de alimentos y bebidas (alimentos y bebidas), como se observa en el gráfico anterior, con una puntuación de 7,11. La categoría de productos de salud y belleza (salud y estilo de vida) tiene el valor medio más bajo, con una puntuación de 6,84.

Vamos a ver qué satisfacción hay entre clientes con membresía y sin ella.

```{r}
dfct<-aggregate(Rating~Customer.type,df,mean);dfct
ggplot(df,aes(x=Customer.type,y=Rating,fill=Customer.type))+geom_boxplot(trim=F)
```

Sus medias están muy cercanas teniendo los miembros una media de 7 y los no miembros una media de 6.94.


```{r}
dfgen<-aggregate(Rating~Gender,df,mean);dfgen
ggplot(df,aes(x=Gender,y=Rating,fill=Gender))+geom_boxplot(trim=F)
```

El valor de calificación mediana de los consumidores femeninos es ligeramente mayor que el valor de calificación mediana de los consumidores masculinos. Las calificaciones IQR de las consumidoras también son más altas que las de los hombres. Sin embargo, la calificación promedio de las mujeres es 6.96, que es más baja que la calificación promedio de los hombres de 6.98.

Observemos también la proporción de genero con respecto a la membresía y sus votaciones medias.

```{r}
ggplot(data=df, aes(x = `Customer.type`)) +
geom_bar(aes(fill = Gender), stat = "count",position=position_dodge()) +
scale_fill_manual(values = c("#e5989b", "#0a9396")) +
theme_bw() +
geom_text(aes(label = ..count.., fill = Gender), vjust =-0.5, col = "black",
          stat = "count", position = position_dodge(0.9))
ggplot(df,aes(x=Customer.type,y=Rating,fill=Gender))+geom_boxplot(trim=F)
dfgc<-aggregate(Rating~Gender + Customer.type ,df,mean);dfgc
```

Hay una mayor parte de miembros femeninos y de no miembros masculinos pero no nos esclarece demasiado con respecto a sus satisfacciones, se sigue manteniendo sus medias entre 6.94 y 7. La mediana de clientes no miembros femeninos es ligeramente superior a 7, pero no dista demasiado de las demas.

Echamos un breve vistazo a las correlaciones entre variables numéricas.

```{r}
dfnum <- select_if(df, is.numeric)
dfnum <- na.omit(dfnum)
corrplot(cor(dfnum), method = "number",
           type = "lower",
           tl.cex = 0.8,
           tl.srt = 40,
           tl.col = "black")
```

Gross.income, Total, Tax.5. y COGS tienen una relación 1-1 como ya hemos mencionado. Procedemos a ver qué ocurre con gross.margin.percentage.

```{r}
summary(dfnum)
pairs(dfnum)
round(cor(dfnum),2)
```

La variable gross.margin.percentage al ser una constante mete bastante ruido al modelo, por lo que procederemos a eliminarla.

```{r}
dfnum$gross.margin.percentage <- NULL
summary(dfnum)
pairs(dfnum)
round(cor(dfnum),2)

```

Al haber todavía variables con una alta correlación procederemos a recalcular las correlaciones en cada iteración.

```{r}
findCorrelation(cor(dfnum), cutoff = 0.9, names = T, verbose = T, exact = T)
```

Vamos a eliminar las variables Total, cogs y Tax.5. debido a sus altas correlaciones y volvemos a recomputar.

```{r}
dfnum$Total <- NULL
dfnum$Tax.5. <- NULL
dfnum$cogs <-  NULL

cor(dfnum)
corrplot::corrplot(cor(dfnum), method = "number", type = "lower",
           tl.cex = 0.8,
           tl.srt = 40,
           tl.col = "black")
```

El valor de correlación entre la variable Unit.price-Quantity, Unit.price-Rating y Quantity-Rating, como se ve en el gráfico de correlación, es muy pequeño, lo que indica que las tres variables numéricas tienen una relación muy baja. Por otro lado los valores de Unit.price-gross.income y Quantity-gross.income guardan una correlación positiva notable. A mayor precio o mayor cantidad, mayor es el ingreso bruto.

Vamos a calcular la normalidad de las variables numéricas que tenemos.

Recordemos que:

H0 <- p-value >  0.05 # Variable proviene de una distribucion normal.

H1 <- p-value <= 0.05 # No es normal.

```{r}
dfnum %>% dplyr::select(Unit.price, Quantity, gross.income, Rating)
sapply(sample_n(dfnum, 1000), function(x) round(shapiro.test(x)$p.value,15))

```


El test de Shapiro indica que los datos no se distribuyen normalmente, ya que el valor p es menor que 0,05. Por este motivo debemos abstenernos por el momento de utilizar ANOVA.

Normalizamos las variables para poder trabajar con ellas.

```{r}
df$Unit.price <- scale(df$Unit.price)
df$Quantity <- scale(df$Quantity)
df$gross.income <- scale(df$gross.income)
df$Rating <- scale(df$Rating)

```

Procedemos a anular los campos que no nos van a servir para el modelo de regresión.

```{r}
df_regre=df
df_regre$Date<-NULL
df_regre$Time<-NULL
df_regre$Invoice.ID<-NULL

```

Hacemos el modelo de regresión con todas las variables.

```{r}
regre_1 <- lm(Rating ~ ., data = df_regre)
summary(regre_1)
```

El R cuadrado es muy bajo (0.01135), lo cual solo explicaría un 1% de los casos y el p-valor es demasiado alto (0.6615), lo que indica que la relación entre la satisfacción y el resto de variables no es significativa.

Realizamos un step para ver qué variables pueden ser útiles para realizar un modelo de regresión.

```{r}
step(object =regre_1, direction = "backward", trace = 1)
```

El mejor modelo ha sido el que tiene AIC más pequeño, es decir, el último de la salida de la función step.

```{r}
Modelo_final <-lm(Rating ~ Branch + Unit.price + Quantity + Tax.5., data = df_regre)
summary(Modelo_final)
```


Aun con este modelo el R cuadrado sigue siendo muy pequeño, menor al 1%, y el p-valor demasiado grande (0.1434), lo que no hace que las variables sean significativas.

Hacemos un análisis de asunción del modelo.

```{r}
par(mfrow=c(2,2))
plot(Modelo_final)
```

Podemos observar que en 3 de las 4 gráficas los datos están muy desvirtuados del ajuste formando nubes de puntos. En la gráfica de Normal Q-Q debería de estar lo más ajustado a la línea punteado posible pero los extremos de la cola están fuera, lo que indica que no están distribuidos de manera normal. Esto nos confirma que el mejor modelo que hemos podido elegir no es válido para nuestro estudio.

#### CONCLUSIONES:

El parámetro que nos interesaba analizar que era la satisfacción, no parece teber relación con ninguna de las variables del dataset lo que nos llevaría a pensar que harían falta más datos para averiguar en qué se pueden basar las puntuaciones.


