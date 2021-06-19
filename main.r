## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: MÉTODOS DE REMUESTREO Y VALIDACIÓN DE MODELOS: VALIDACIÓN CRUZADA Y BOOTSTRAP [Parte 1]
## 
##  4. Fuentes:
##     https://rpubs.com/rdelgado/405322
##     https://tereom.github.io/est-computacional-2018/bootstrap-en-r.html"


## ------------------------------------------------------------------------------------
# Guardamos todos los OUTPUTS:
sink("OUTPUTS.txt")

# Cargamos el paquete ISLR:
#install.packages("ISLR")
library(ISLR)

# Exportamos la base de datos para una visualización de ellos
write.csv(Weekly, file = "Weekly.csv", row.names = F)

# Creamos una semilla
set.seed(1)

# Índices aleatorios para el set de entrenamiento 
indices.train <- sample(x = nrow(Weekly), size = 0.5*(nrow(Weekly)), 
                        replace = FALSE)

print("indices.train")
print(indices.train)

# Subgrupos de entrenamiento y test
datos.entrenamiento <- Weekly[indices.train, ]

print("datos.entrenamiento")
print(datos.entrenamiento)

datos.test <- Weekly[-indices.train, ]

print("datos.test")
print(datos.test)


## ------------------------------------------------------------------------------------
# Cargamos la libreria por el operador "pipe" %>%
library(dplyr)

# Creamos una tabla de de frecuencias de "Direction", despues pasamos esas tablas a porcentaje y después la redondeamosa a 3 cifras
print("verificar que la distribución de la variable respuesta `Direction` se distribuye aproximadamente de manera similar entre el set de entrenamiento y test")

print(prop.table(table(datos.entrenamiento$Direction)) %>% round(digits = 3))

# Resultado:
"En este caso, las proporciones son similares en ambos casos."

#ajustamos el modelo logístico con los datos asignados a entrenamiento:
modelo.logistico <- glm(Direction ~ Lag2, data = Weekly, family = "binomial", 
                        subset = indices.train)

print("ajustamos el modelo logístico con los datos asignados a entrenamiento:")
print(summary(modelo.logistico))


# Codificación de la variable respuesta para el modelo
attach(Weekly)
print("# Codificación de la variable respuesta para el modelo")
print(contrasts(Direction))


## ------------------------------------------------------------------------------------
# Cálculo de la probabilidad predicha por el modelo con los datos de test
prob.modelo <- predict(object = modelo.logistico, newdata = datos.test, 
                       type = "response")

# Vector de caracteres “Down”
pred.modelo <- rep("Down", length(prob.modelo))

# Sustitución de “Down” por “Up” si la probabilidad a posteriori > 0,5
pred.modelo[prob.modelo > 0.5] <- "Up"


# Matriz de confusión
print("# Matriz de confusión")
print(table(pred.modelo, datos.test$Direction))


# Test error rate
print("# Test error rate")
print(mean(pred.modelo != datos.test$Direction))

# Resultado:
"La estimación del test error rate del modelo mediante validación simple es del 46,78%, por lo que el modelo acierta con sus predicciones en solo un 1 – 0,4678 = 53,2% de los casos."


## ------------------------------------------------------------------------------------
# Ahora se muestra el mismo proceso llevado a cabo anteriormente, repitiéndolo 100 veces (en cada iteración los datos se van a repartir de manera distinta en entrenamiento y test).


# Vector donde se almacenarán los 100 test error estimados
cv.error <- rep(NA, 100)

for (i in 1:100){
  # importante la creación de nuevos índices para cada iteración, de lo contrario, el test error obtenido siempre sería el mismo
  indices.train <- sample(x = nrow(Weekly), size = 0.5*(nrow(Weekly)), 
                        replace = FALSE)
  datos.entrenamiento <- Weekly[indices.train, ]
  datos.test <- Weekly[-indices.train, ]
  modelo.logistico <- glm(Direction ~ Lag2, data = Weekly, family = "binomial", 
                          subset = indices.train)
  prob.modelo <- predict(object = modelo.logistico, newdata = datos.test, 
                         type = "response")
  pred.modelo <- rep("Down", length(prob.modelo))
  pred.modelo[prob.modelo > 0.5] <- "Up"
  cv.error[i] <- mean(pred.modelo != datos.test$Direction)
}

print("Estadisticos del Vector donde se almacenarán los 100 test error estimados")
print(summary(cv.error))


## ------------------------------------------------------------------------------------
# Cargamos las librerias:
library(ggplot2)
library(gridExtra)

# Ahora Graficaremos para ver los resultados del test de error:

png(filename = "test_error.png")

# Creamos el boxplot:
boxplot <- ggplot(data = data.frame(cv.error = cv.error),
                  aes(x = 1, y = cv.error))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(colour = c("orangered2"), width = 0.1)+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

# Creamos el histograma:
histograma <- ggplot(data = data.frame(cv.error = cv.error), aes(x = cv.error)) +
geom_histogram(color = "grey", fill = "orangered2")

grid.arrange(boxplot, histograma, ncol = 2)

dev.off()

# Resultado:
"Como resultado de las 100 iteraciones, la estimación del test error oscila entre el 40,37% y el 48,99%, con una media del 44,43%, por lo que el porcentaje de acierto es del 55,57%."


## ------------------------------------------------------------------------------------
# Creamos una tabla de porcentajes (de 0 a 1), redondeada a 3 digitos
print("# Creamos una tabla de porcentajes (de 0 a 1), redondeada a 3 digitos")
print(prop.table(table(Weekly$Direction)) %>% round(digits = 3))

# Conclusión:
"Si siempre se predijera Direction = “Up”, el porcentaje de aciertos sería aproximadamente del 56%, por lo que este es el porcentaje mínimo que debería superar el modelo. En este caso, no se supera."

sink()
