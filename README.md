
# =========================================================
# CUADRO LATINO 4x4 - LOCALIDAD CLEMSON
# Analisis de varianza (ANOVA)
# =========================================================

# 1. Crear los vectores de datos

fila <- c(
  1, 1, 1, 1,
  2, 2, 2, 2,
  3, 3, 3, 3,
  4, 4, 4, 4
)

columna <- c(
  1, 2, 3, 4,
  1, 2, 3, 4,
  1, 2, 3, 4,
  1, 2, 3, 4
)

genotipo <- c(
  "Poinsett", "Sprint", "Dasher", "Guardian",
  "Sprint", "Guardian", "Poinsett", "Dasher",
  "Guardian", "Dasher", "Sprint", "Poinsett",
  "Dasher", "Poinsett", "Guardian", "Sprint"
)

rendimiento <- c(
  11.5, 15.1, 44.2, 33.0,
  20.3, 13.6, 22.4, 54.1,
  44.1, 47.2, 41.3, 30.3,
  36.7, 21.5, 35.8, 27.1
)

# 2. Convertir los vectores en factores

fila <- factor(fila)
columna <- factor(columna)

genotipo <- factor(
  genotipo,
  levels = c("Dasher", "Guardian", "Poinsett", "Sprint")
)

# 3. Construir la base de datos

datos <- data.frame(
  Fila = fila,
  Columna = columna,
  Genotipo = genotipo,
  Rendimiento = rendimiento
)

# Mostrar la base de datos
print(datos)

# 4. Ajustar el modelo de cuadro latino

modelo <- aov(
  Rendimiento ~ Fila + Columna + Genotipo,
  data = datos
)

# 5. Mostrar la tabla ANOVA

tabla_anova <- anova(modelo)
print(tabla_anova)

# 6. Mostrar las medias de los genotipos

medias <- aggregate(
  Rendimiento ~ Genotipo,
  data = datos,
  FUN = mean
)

print(medias)

# 7. Verificar los grados de libertad

cat("Grados de libertad residuales:",
    df.residual(modelo), "\n")

# 8. Mostrar el resumen del modelo

summary(modelo)



# =========================================================
# PRUEBA DE NORMALIDAD SHAPIRO-WILK
# Cuadro latino 4x4 - Localidad Clemson
# =========================================================

# Obtener los residuos del modelo ANOVA
residuos <- residuals(modelo)

# Aplicar la prueba de Shapiro-Wilk
shapiro.test(residuos)

# Graficar el QQ-Plot
qqnorm(residuos,
       main = "QQ-Plot de los residuos - Clemson")

qqline(residuos, col = "red", lwd = 2)




# =========================================================
# PRUEBA DE LEVENE
# Cuadro latino 4x4 - Localidad Clemson
# =========================================================



# Cargar el paquete
library(car)

# =========================================================
# PRUEBA DE LEVENE
# Cuadro latino 4x4 - Localidad Clemson
# =========================================================

library(car)

# 1. Prueba de Levene para los genotipos
levene_genotipo <- leveneTest(
  Rendimiento ~ Genotipo,
  data = datos,
  center = median
)

cat("\n--- PRUEBA DE LEVENE: GENOTIPOS ---\n")
print(levene_genotipo)


# 2. Prueba de Levene para las filas
levene_fila <- leveneTest(
  Rendimiento ~ Fila,
  data = datos,
  center = median
)

cat("\n--- PRUEBA DE LEVENE: FILAS ---\n")
print(levene_fila)


# 3. Prueba de Levene para las columnas
levene_columna <- leveneTest(
  Rendimiento ~ Columna,
  data = datos,
  center = median
)

cat("\n--- PRUEBA DE LEVENE: COLUMNAS ---\n")
print(levene_columna)

