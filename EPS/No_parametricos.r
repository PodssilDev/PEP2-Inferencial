# Ñami-Ñam, compañía dedicada a la elaboración y comercialización de golosinas,
# se prepara para lanzar una nueva línea de productos al mercado. Para asegurar
# el éxito comercial, ha solicitado a varias empresas de diseño la creación de
# un empaque para cada uno de los nuevos productos. A fin de decidir qué envase
# es mejor para cada producto y evaluar un contrato permanente con una de las
# empresas de diseño, Ñami-Ñam ha reclutado a 2.000 voluntarios de todo el país.
# Cada participante debe puntuar las distintas alternativas de envase para un
# producto mediante una escala Likert de 7 puntos, donde: 1: el envase es muy
# poco atractivo y 7: el envase es muy atractivo.

# Fijar carpeta de trabajo.
setwd("D:/Dropbox/Inferencia/Ejercicios prácticos 1-2022/EP10")

# Cargar paquetes
library(tidyverse)

# Cargar datos
datos <- read.csv2("EP10 Datos.csv", stringsAsFactors = TRUE)
datos[["Id"]] <- factor(datos[["Id"]])



################################################################################
# ¿Existe diferencia en la puntuación obtenida por los envases diseñados por
# KoolDesign según las evaluaciones realizadas por jóvenes y adultos?
################################################################################

# En este caso se requiere inferir acerca de las medias de dos muestras
# independientes, por lo que la primera alternativa corresponde a una prueba t
# de Student.

# Seleccionar datos de interés
datos.anchos <- datos %>% pivot_wider(names_from = "Diseno", values_from = "Puntaje")
jovenes <- datos.anchos[datos.anchos[["Edad"]] == "Joven",][["KoolDesign"]]
adultos <- datos.anchos[datos.anchos[["Edad"]] == "Adulto",][["KoolDesign"]]

# Verificamos las condiciones:
# Del enunciado se desprende que las observaciones son independientes entre sí,
# pues los voluntarios fueron seleccionados de manera aleatoria.
# Sin embargo, no es posible comprobar el supuesto de normalidad dado que la
# escala de la variable es discreta y no es de intervalos iguales, aunque sí es
# ordinal. En consecuencia, es más adecuado usar la prueba de suma de rangos de
# Wilcoxon.

# Formulamos las hipótesis:
# H0: No hay diferencia en la percepción de jóvenes y adultos de cuán atractivos
#     resultan los envases diseñados por KoolDesign.
# HA: Jóvenes y adultos difieren en la percepción de cuán atractivos resultan
#     los envases diseñados por KoolDesign.

# Aplicamos la prueba.
alfa <- 0.05

prueba.1 <- wilcox.test(jovenes, adultos, paired = FALSE,
                        alternative = "two.sided", conf.level = 1-alfa)

print(prueba.1)

# Puesto que el valor p obtenido (p < 2.2e-16) es menor que el nivel de
# significación, se rechaza H0 en favor de HA. En consecuencia, podemos concluir
# con 95% de confianza que jóvenes y adultos perciben distinto atractivo en los
# envases diseñados por KoolDesign.



################################################################################
# ¿Existe diferencia entre la puntuación obtenida por los envases diseñados por
# LaKajita y PackPro?
################################################################################

# En este caso se requiere inferir acerca de las medias de dos muestras
# pareadas, por lo que la primera alternativa corresponde a una prueba t
# de Student.

# Verificamos las condiciones:
# Del enunciado se desprende que las observaciones son independientes entre sí,
# pues los voluntarios fueron seleccionados de manera aleatoria.
# Sin embargo, no es posible comprobar el supuesto de normalidad dado que la
# escala de la variable es discreta y no es de intervalos iguales, aunque sí es
# ordinal. En consecuencia, es más adecuado usar la prueba de rangos con signo
# de Wilcoxon.

# Formulamos las hipótesis:
# H0: Los voluntarios no perciben diferencia en los atractivos que resultan los
#     envases diseñados por LaKAjita y PackPro.
# HA: Los voluntarios sí perciben diferencia en los atractivos que resultan los
#     envases diseñados por LaKAjita y PackPro.

# Aplicamos la prueba.
alfa <- 0.05

prueba.2 <- wilcox.test(datos.anchos[["LaKajita"]], datos.anchos[["PackPro"]],
                        paired = TRUE, alternative = "two.sided",
                        conf.level = 1-alfa)

print(prueba.2)

# Puesto que el valor p obtenido (p = 0,596) es mayor que el nivel de
# significación, se falla al rechazar H0. En consecuencia, podemos concluir con
# 95% de confianza que los voluntarios consideran los envases diseñados por
# LaKajita y PackPro igualmente atractivos.



################################################################################
# ¿Existen diferencias entre las puntuaciones obtenidas para los diferentes
# envases de cuchuflí? De ser así, ¿cuál(es) envase(s) se diferencia(n) de los
# demás?
################################################################################

# Puesto que cada voluntario evaluó 4 envases para un mismo producto, la primera
# opción sería usar el procedimiento ANOVA de una vía para muestras
# correlacionadas.

# Seleccionamos los datos de interés:
datos.2 <- datos %>% filter(Producto == "Cuchufli")
datos.2 <- droplevels(datos.2)

# Verificamos las condiciones:
# Puesto que cada participante tiene percepciones diferentes y criterios
# subjetivos para asignar una puntuación, no se cumple que la variable
# dependiente tenga una escala de intervalos iguales, aún siendo ordinal.
# Como ya discutimos en las preguntas anteriores, se verifica que las mediciones
# son independientes al interior de cada grupo.

# En consecuencia, es más adecuado usar la prueba de Friedman, con las
# siguientes hipótesis:
# H0: Los envases de cuchuflí tienen percepciones similares en cuanto a su
#     atractivo.
# HA: Los envases de cuchuflí tienen percepciones diferentes en cuanto a su
#     atractivo.

# Aplicamos la prueba.
alfa <- 0.05
prueba.3 <- friedman.test(Puntaje ~ Diseno | Id , data = datos.2)
print(prueba.3)

# Puesto que el valor p obtenido (p = 0,5536) es mayor que el nivel de
# significación, se falla al rechazar H0. En consecuencia, podemos concluir con
# 95% de confianza que los voluntarios consideran todos los envases de cuchuflí
# igualmente atractivos.



################################################################################
# ¿Existe diferencias en las puntuaciones obtenidas para el envase de galletas
# diseñado por KoolDesign según la edad de los evaluadores? De ser así,
# ¿cuál(es) grupo(s) de evaluador(es) se diferencia(n) de los demás?
################################################################################

# Puesto que cada rango de edades tiene distintos participantes, esta pregunta
# la primera alternativa en este caso sería usar ANOVA de una vía para muestras
# independientes.

# Seleccionamos los datos de interés:
datos.3 <- datos %>% filter(Producto == "Galletas", Diseno == "KoolDesign")
datos.3 <- droplevels(datos.3)

# Verificamos las condiciones:
# Puesto que cada participante tiene percepciones diferentes y criterios
# subjetivos para asignar una puntuación, no se cumple que la variable
# dependiente tenga una escala de intervalos iguales, aún siendo ordinal.
# Como ya discutimos en las preguntas anteriores, se verifica que las mediciones
# son independientes al interior de cada grupo.

# En consecuencia, es más adecuado usar la prueba de Kruskal-Wallis, con las
# siguientes hipótesis:
# H0: El envase de galletas diseñado por KoolDesign es igualmente atractivo para
#     todas las edades.
# HA: El envase de galletas diseñado por KoolDesign no es igualmente atractivo
#     para todas las edades.

# Aplicamos la prueba.
alfa <- 0.05
prueba.4 <- kruskal.test(Puntaje ~ Edad , data = datos.3)
print(prueba.4)

# Puesto que el valor p obtenido (p < 2.2e-16) es menor que el nivel de
# significación, se rechaza H0 en favor de HA. En consecuencia, podemos concluir
# con 95% de confianza que los voluntarios, según su edad, difieren en cuanto al
# atractivo del envase para galletas de KoolDesign.

# Puesto que la prueba ómnibus de Kruskal-Wallis detecta diferencias, debemos
# hacer ahora un procedimiento post-hoc. COnsideraremos el ajuste de Holm, por
# tener mayor poder estadístico que el de Bonferroni.
post.hoc4 <- pairwise.wilcox.test(datos[["Puntaje"]], datos[["Edad"]],
                                  p.adjust.method = "holm", paired = FALSE)

print(post.hoc4)

# El procedimiento post-hoc no encuentra diferencias significativas entre los
# diferentes pares de grupos.


