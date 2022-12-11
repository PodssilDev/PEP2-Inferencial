
library(boot)
library(dplyr)
library(ggpubr)
library(tidyr)
library(rcompanion)

# En el trabajo de título de una estudiante del DIINF
# se reportan tiempos de ejecución (en milisegundos) y
# la cercanía con la solución óptima (en por ciento)
# de la mejor solución encontrada con tres versiones
# de un algoritmo genético para resolver instancias
# del problema del vendedor viajero disponibles en
# repositorios públicos. Ahora debe enfrentar el
# análisis de estos datos, por que está solicitando
# ayuda de los estudiantes de Estadística Inferencial.

cat("\n\n")
cat("Pregunta 1\n")
cat("==========\n")
cat("\n\n")
# Observando los datos, la memorista sospecha que hay
# diferencias significativas en el tiempo de ejecución
# entre las versiones A y C del algoritmo cuando las
# instancias tienen 100 o más nodos. ¿Los datos respaldan
# la intuición de la memorista?
# Para responder, filtren los datos para tener las
# instancias con 100 o más nodos y seleccionen las
# columnas de los tiempos de ejecución de las versiones
# A y C en formato ancho. Usando como semilla el valor 13,
# obtengan muestras aleatorias independientes de 20 tiempos
# registrados por la versión A y 18 tiempos registrados
# por la versión C del algoritmo. Lleven los datos a
# formato largo y utilicen la escalera de potencias de
# Tukey para analizar las muestras obtenidas.

# Obtenemos los datos en formato ancho
dir <- "~/../Downloads"
src.basename <- "EP07 Datos.csv"
src.file <- file.path(dir, src.basename)
datos <- read.csv(src.file)

# Primero, filtramos para quedarnos con las instancias que
# nos interesan y quitar las columnas que no necesitamos.
dw <- datos %>%
  filter(n.nodos >= 100) %>%
  select(instancia, tiempo.A, tiempo.C)

# Ahora tomamos la muestra solicitada.
# Es importante que lo hagamos con una sola llamada
# a la función sample(), para evitar que los algoritmos
# compartan alguna instancia, asegurando así muestras
# independientes.
set.seed(13)
i <- sample(1:nrow(dw), 20 + 18)
seleccion <- dw[i, ]
muestra1 <- seleccion[["tiempo.A"]][1:20]
muestra2 <- seleccion[["tiempo.C"]][21:38]

# Creamos una versión larga de los datos
dl <- data.frame(
  instancia = seleccion[["instancia"]],
  algoritmo = c(rep("A", 20), rep("C", 18)),
  tiempo = c(muestra1, muestra2)
)
dl[["instancia"]] <- factor(dl[["instancia"]])
dl[["algoritmo"]] <- factor(dl[["algoritmo"]])

# Revisemos un histograma de los datos
p1 <- gghistogram(
  dl,
  x = "tiempo",
  xlab = "algoritmo",
  color = "algoritmo", fill = "algoritmo",
  bins = 5
)
p1 <- p1 + facet_grid(~ algoritmo)
print(p1)

# Podemos ver que las muestras no parecen tomadas desde una
# distribución normal.

# Se nos pide utilizar la escalera de potencias de Tukey
# para analizar las muestras obtenidas.

# Utilizamos la función del paquete "rcompanion"
tr <- transformTukey(
  dl[["tiempo"]],
  plotit = TRUE,
  verbose = FALSE
)

# Vemos que la función anterior buscó y aplicó una
# trasformación de forma que los datos ahora muestran
# un comportamiento normal en un histograma y un gráfico
# Q-Q, con una prueba Shapiro-Wilks no significativa.
 
# Recuperamos las muestras transformadas
transf1 <- tr[1:20]
transf2 <- tr[21:38]

# Ahora podemos hacer la prueba paramétrica que nos permita
# comparar ambas muestras. En este caso, una prueba t de
# Student con la corrección de Welch (no tenemos por qué
# suponer que ambas muestras tienen igual varianza).
wt <- t.test(transf1, transf2)

# Como no se nos han dado instrucciones al respecto,
# mantenemos los parámetros por defecto (alfa = 0,05).
print(wt)

# Podemos concluir, entonces, que existe fuerte evidencia
# en contra de la hipótesis nula, por lo que la rechazamos
# en favor de la alternativa. Esto es, los algorimos no
# tardan tiempos similares en resolver instancias del
# problema del vendedor viajero. Mirando los gráficos de
# los datos originales y transformados, podemos sugerir
# que el algoritmo C requiere, en promedio, significativa-
# mente menos tiempo de procesamiento.199.660
 


cat("\n\n")
cat("Pregunta 2\n")
cat("==========\n")
cat("\n\n")

# La memorista también sospecha que, al comparar las mismas
# instancias, las mejores soluciones encontradas por las
# versiones B y C tienen rendimientos distintos.
# ¿Estará en lo cierto?
# Para responder, filtren los datos para tener las instancias
# con 100 o más nodos y seleccionen las columnas con el mejor
# rendimiento de las versiones B y C en formato ancho. Usando
# como semilla el valor 117, obtengan una muestra aleatoria de
# 24 instancias. Lleven los datos a formato largo y utilicen
# una prueba no paramétrica apropiada para analizar las
# muestras obtenidas.

# Obtenemos la muestra de datos en formato ancho.
# Como tenemos que comparar los resultados obtenidos por los
# algoritmos con *las mismas instancias*, debemos obtener
# una muestra apareada de 24 observaciones, las que obtenemos
# usando las funciones de tidyverse.

set.seed(117)
dw2 <- datos %>%
  filter(n.nodos >= 100) %>%
  select(instancia, mejor.B, mejor.C) %>%
  sample_n(24)

# Recordemos que nos están pidiendo usar una prueba *no*
# paramétrica. Como se trata de dos muestras pareadas, se
# debe usar una alternativa a la prueba t de Student para
# muestras pareadas, es decir, la prueba de rangos con signos
# de Wilcoxon. Recordemos las condiciones:
# 1. Los pares de observaciones son independientes.
#    Efectivamente, si el experimento fue realizado correc-
#    tamente por la memorista, cómo se desempeña un algorit-
#    mo no debería tener influencia en cómo rinde el segundo.
# 2. La escala de medición empleada para ambas muestras debe
#    ser a lo menos ordinal.
#    Valores porcentuales cumplen esta condición, pues podemos
#    compararlos y ordenarlos.
# 3. La escala de medición empleada para las observaciones
#    es intrínsecamente continua.
#    No debemos olvidar que la "cercanía" con la solución
#    óptima está medida como un porcentaje. Luego, a pesar de 
#    ser una variable numérica en que un valor puede, en 
#    teoría, tomar un número arbitrario de decimales, estos 
#    están limitados a un intervalo fijo (por ser, en el 
#    fondo, proporciones). Esto hace que las observaciones en
#    la muestra obtenida violen esta condición.
#    En estos casos, se recomienda transformarlos a una escala
#    abierta. Es común usar el arcoseno de la raíz cuadrada de 
#    la proporción original (también llamada transformación
#    angular), aunque últimamente se está prefiriendo la
#    función logit (que estudiaremos en más detalle más
#    adelante) que relaciones proporciones con valores Z [ver
#    por ejemplo http://strata.uga.edu/8370/rtips/proportions.html].
#    Aquí usaremos la función logit() implementada en el
#    paquete 'boot'.

dw3 <- dw2 %>%
  select(instancia, mejor.B, mejor.C) %>%
  mutate(
    mejor.B = logit(mejor.B / 100),
    mejor.C = logit(mejor.C / 100),
  )
  
# Llevamos los datos a formato largo
dl2 <- dw3 %>%
  pivot_longer(
    cols = c("mejor.B", "mejor.C"),
    names_to = "algoritmo",
    values_to = "resultado"
  )
dl2[["instancia"]] <- factor(dl2[["instancia"]])
dl2[["algoritmo"]] <- factor(dl2[["algoritmo"]])

# Revisemos los datos con un diagrama de cajas
p2 <- ggboxplot(
  dl2,
  x = "algoritmo", y = "resultado",
  xlab = "algoritmo",
  color = "algoritmo"
)
print(p2)

# Vemos que los datos transformados presentan una leve
# asimetría y al menos un par de valores atípicos.
# Procedamos con la prueba no paramétrica, ya que se cumplen
# las condiciones para aplicar la prueba de rangos con signo
# de Wilcoxon.

wsrt <- wilcox.test(
  x = dw3[["mejor.B"]],
  y = dw3[["mejor.C"]],
  paired = TRUE
)
print(wsrt)

# El resultado de la prueba indica que hay suficiente
# evidencia (p = 0,034) para descartar la hipótesis nula
# en favor de la alternativa. En conjunto con el gráfico de
# cajas observado anteriormente, pareciera que el algoritmo
# C consigue mejores resultados que los que se obtienen con
# el algoritmo B.


