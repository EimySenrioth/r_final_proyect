# ============================================================
# PROYECTO FINAL - MINERÍA DE DATOS 
# ÁRBOL DE DECISIÓN
# ============================================================

install.packages("dplyr")
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)

# 1. CARGA Y PREPARACIÓN DE DATOS
unirMaster1 <- read.csv("C:/Users/kcanc/Downloads/unir_master.csv")
unirMaster2 <- read.csv("C:/Users/kcanc/Downloads/Unirmaster_dos.csv")

# quitar todas las columnas que estén en NULL o NA
um1.not.na.o.null <- unirMaster1 %>% select(where(~ !any(is.na(.) | . == "NULL")))

# quitar las columnas que tienen más del 15% de faltantes
um1.not.na.0.15 <- um1.not.na.o.null %>% select(where(~ mean(is.na(.)) <= 0.15))

# Renombrar columna para unir datasets
head(um1.not.na.0.15)
um1.renombrado.col.id_alumno <- um1.not.na.0.15 %>% rename(id_estudiante = id_alumno)

# al unir2 lo uno con el unir 1
join.unir1.unir2 <- unirMaster2 %>% left_join(um1.renombrado.col.id_alumno, by = "id_estudiante")

# por curso contar los ceros, si son mayor al 15% significa que son faltantes
join.unir1.unir2.no.0.15 <- um1.not.na.o.null %>% select(where(~ mean(. == 0) <= 0.15))

cat("\n=== DIMENSIONES DEL DATASET FINAL ===\n")
cat("Número de registros:", nrow(join.unir1.unir2), "\n")
cat("Número de variables:", ncol(join.unir1.unir2), "\n")


# ============================================================
# PARTE 2: ESTADÍSTICAS DESCRIPTIVAS
# ============================================================
data("mtcars")
names(mtcars)

# Función para calcular la moda
calcular_moda <- function(x) {
  x <- na.omit(x)
  if(length(x) == 0) return(NA)
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
# Función para calcular todas las estadísticas de una variable numérica
estadisticas_completas <- function(data, variable) {
  var_data <- data[[variable]]
  var_data <- var_data[!is.na(var_data)]
  
  if(length(var_data) == 0) {
    cat("No hay datos válidos para", variable, "\n")
    return(NULL)
  }
  
  resultado <- data.frame(
    Variable = variable,
    N = length(var_data),
    
    # Medidas de tendencia central
    Media = mean(var_data, na.rm = TRUE),
    Mediana = median(var_data, na.rm = TRUE),
    Moda = calcular_moda(var_data),
    
    # Medidas de dispersión
    Varianza = var(var_data, na.rm = TRUE),
    Desviacion_Estandar = sd(var_data, na.rm = TRUE),
    Rango = max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE),
    Minimo = min(var_data, na.rm = TRUE),
    Maximo = max(var_data, na.rm = TRUE),
    
    # Cuartiles
    Q1 = quantile(var_data, 0.25, na.rm = TRUE),
    Q2_Mediana = quantile(var_data, 0.50, na.rm = TRUE),
    Q3 = quantile(var_data, 0.75, na.rm = TRUE),
    IQR = IQR(var_data, na.rm = TRUE),
    
    # Percentiles
    P10 = quantile(var_data, 0.10, na.rm = TRUE),
    P25 = quantile(var_data, 0.25, na.rm = TRUE),
    P50 = quantile(var_data, 0.50, na.rm = TRUE),
    P75 = quantile(var_data, 0.75, na.rm = TRUE),
    P90 = quantile(var_data, 0.90, na.rm = TRUE),
    P95 = quantile(var_data, 0.95, na.rm = TRUE),
    P99 = quantile(var_data, 0.99, na.rm = TRUE)
  )
  
  return(resultado)
}

# Identificar columnas numéricas en el dataset unido
columnas_numericas <- names(join.unir1.unir2)[sapply(join.unir1.unir2, is.numeric)]

cat("Columnas numéricas encontradas:", length(columnas_numericas), "\n")
print(columnas_numericas)
cat("\n")

# Calcular estadísticas para cada columna numérica
lista_estadisticas <- list()
for(col in columnas_numericas) {
  stats <- estadisticas_completas(join.unir1.unir2, col)
  if(!is.null(stats)) {
    lista_estadisticas[[col]] <- stats
  }
}

# Combinar todas las estadísticas en un dataframe
todas_estadisticas <- bind_rows(lista_estadisticas)

cat("\n=== RESUMEN ESTADÍSTICO DE TODAS LAS VARIABLES NUMÉRICAS ===\n")
print(todas_estadisticas, width = 1000)

# ============================================================
# ESTADÍSTICAS PARA VARIABLES CLAVE
# ============================================================

# Si existe una columna de calificaciones
if("calificacion" %in% names(join.unir1.unir2)) {
  cat("\n=== ESTADÍSTICAS DETALLADAS PARA EVALUACIÓN INDIVIDUAL ===\n")
  
  estadisticas_calificacion <- join.unir1.unir2 %>%
    summarise(
      N = n(),
      Media = mean(evaluacion_individual, na.rm = TRUE),
      Mediana = median(evaluacion_individual, na.rm = TRUE),
      Moda = calcular_moda(evaluacion_individual),
      Varianza = var(evaluacion_individual, na.rm = TRUE),
      Desviacion_Estandar = sd(evaluacion_individual, na.rm = TRUE),
      Rango = max(evaluacion_individual, na.rm = TRUE) - min(evaluacion_individual, na.rm = TRUE),
      Minimo = min(evaluacion_individual, na.rm = TRUE),
      Maximo = max(evaluacion_individual, na.rm = TRUE),
      Q1 = quantile(evaluacion_individual, 0.25, na.rm = TRUE),
      Q2 = quantile(evaluacion_individual, 0.50, na.rm = TRUE),
      Q3 = quantile(evaluacion_individual, 0.75, na.rm = TRUE),
      IQR = IQR(evaluacion_individual, na.rm = TRUE),
      P10 = quantile(evaluacion_individual, 0.10, na.rm = TRUE),
      P90 = quantile(evaluacion_individual, 0.90, na.rm = TRUE),
      P95 = quantile(evaluacion_individual, 0.95, na.rm = TRUE)
    )
  
  print(estadisticas_calificacion)
  
  # Resumen con summary()
  cat("\n--- Resumen con summary() ---\n")
  print(summary(join.unir1.unir2$evaluacion_individual))
}
if("curso" %in% names(join.unir1.unir2) && "evaluacion_individual" %in% names(join.unir1.unir2)) {
  cat("\n=== ESTADÍSTICAS POR CURSO ===\n")
  
  estadisticas_por_curso <- join.unir1.unir2 %>%
    group_by(curso) %>%
    summarise(
      N = n(),
      Media = mean(evaluacion_individual, na.rm = TRUE),
      Mediana = median(evaluacion_individual, na.rm = TRUE),
      Moda = calcular_moda(evaluacion_individual),
      Desviacion_Estandar = sd(evaluacion_individual, na.rm = TRUE),
      Varianza = var(evaluacion_individual, na.rm = TRUE),
      Rango = max(evaluacion_individual, na.rm = TRUE) - min(evaluacion_individual, na.rm = TRUE),
      Minimo = min(evaluacion_individual, na.rm = TRUE),
      Maximo = max(evaluacion_individual, na.rm = TRUE),
      Q1 = quantile(evaluacion_individual, 0.25, na.rm = TRUE),
      Q2 = quantile(evaluacion_individual, 0.50, na.rm = TRUE),
      Q3 = quantile(evaluacion_individual, 0.75, na.rm = TRUE),
      P90 = quantile(evaluacion_individual, 0.90, na.rm = TRUE)
    ) %>%
    arrange(desc(Media))
  
  print(estadisticas_por_curso, n = Inf)
}

# ============================================================
# ANÁLISIS DE COLUMNAS CON PATRÓN r1, r2, ..., r24
# ============================================================

columnas_r <- grep("^r[0-9]+$", names(join.unir1.unir2), value = TRUE)

if(length(columnas_r) > 0) {
  cat("\n=== ANÁLISIS DE COLUMNAS r1 A r", length(columnas_r), " ===\n", sep = "")
  
  # Calcular promedio por estudiante
  join.unir1.unir2 <- join.unir1.unir2 %>%
    rowwise() %>%
    mutate(promedio_r = mean(c_across(all_of(columnas_r)), na.rm = TRUE)) %>%
    ungroup()
  
  estadisticas_promedio_r <- join.unir1.unir2 %>%
    summarise(
      N = n(),
      Media = mean(promedio_r, na.rm = TRUE),
      Mediana = median(promedio_r, na.rm = TRUE),
      Moda = calcular_moda(promedio_r),
      Desviacion_Estandar = sd(promedio_r, na.rm = TRUE),
      Varianza = var(promedio_r, na.rm = TRUE),
      Rango = max(promedio_r, na.rm = TRUE) - min(promedio_r, na.rm = TRUE),
      Q1 = quantile(promedio_r, 0.25, na.rm = TRUE),
      Q2 = quantile(promedio_r, 0.50, na.rm = TRUE),
      Q3 = quantile(promedio_r, 0.75, na.rm = TRUE),
      P10 = quantile(promedio_r, 0.10, na.rm = TRUE),
      P90 = quantile(promedio_r, 0.90, na.rm = TRUE)
    )
  
  print(estadisticas_promedio_r)
}

# ============================================================
# VISUALIZACIONES
# ============================================================

if("evaluacion_individual" %in% names(join.unir1.unir2)) {
  
  # Histograma con líneas de media y evaluacion_individual
  p1 <- ggplot(join.unir1.unir2, aes(x = evaluacion_individual)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean(evaluacion_individual, na.rm = TRUE)), 
               color = "red", linetype = "dashed", size = 1.2) +
    geom_vline(aes(xintercept = median(evaluacion_individual, na.rm = TRUE)), 
               color = "green", linetype = "dashed", size = 1.2) +
    labs(title = "Distribución de Evaluaciones",
         subtitle = "Línea roja = Media | Línea verde = Mediana",
         x = "Evaluación Individual",
         y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  print(p1)
  
  # Boxplot
  p2 <- ggplot(join.unir1.unir2, aes(y = evaluacion_individual)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
                 fill = "red", color = "red") +
    labs(title = "Boxplot de Evaluaciones",
         subtitle = "El rombo rojo indica la media",
         y = "Evaluaciones") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  print(p2)
  
  # Si existe curso, hacer boxplot por curso
  if("curso" %in% names(join.unir1.unir2)) {
    p3 <- ggplot(join.unir1.unir2, aes(x = curso, y = evaluacion_individual, fill = curso)) +
      geom_boxplot(alpha = 0.7) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
                   fill = "white", color = "black") +
      labs(title = "Distribución de Evaluaciones por Curso",
           subtitle = "El rombo blanco indica la media",
           x = "Curso",
           y = "Evaluación") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p3)
  }
}

# ============================================================
# medias de dispercion de rango, Media recortada, desviazcion tipica, 
#asimetria de los datos en histograma, diagrama de dispercion prom tarea vz evaluacion infividual
# ============================================================

var_num <- "evaluacion_individual"
if (var_num %in% names(join.unir1.unir2)) {
  datos <- join.unir1.unir2[[var_num]]
  datos <- datos[!is.na(datos)]
  
  # Medidas de dispersión de rango
  rango <- max(datos) - min(datos)
  cat("Rango:", rango, "\n")
  
  # Media recortada (10%)
  media_recortada <- mean(datos, trim = 0.1)
  cat("Media recortada (10%):", media_recortada, "\n")
  
  # Desviación típica 
  desv_tipica <- sd(datos)
  cat("Desviación típica:", desv_tipica, "\n")
  
  # Asimetría de los datos en histograma
  library(e1071)
  asimetria <- skewness(datos)
  cat("Asimetría (skewness):", asimetria, "\n")
  hist(datos, main = "Histograma de Evaluación Individual", xlab = var_num, col = "skyblue", border = "white")
  abline(v = mean(datos), col = "red", lwd = 2, lty = 2)
  abline(v = median(datos), col = "green", lwd = 2, lty = 2)
  legend("topright", legend = c("Media", "Mediana"), col = c("red", "green"), lty = 2, lwd = 2)
  
  # Diagrama de dispersión del dataset (ejemplo: Prom_tarea vs Contero_tarea)
  if ("Contero_tarea" %in% names(join.unir1.unir2)) {
    contero_tarea <- as.numeric(as.character(join.unir1.unir2$Contero_tarea))
    plot(contero_tarea, join.unir1.unir2[[var_num]],
         main = "Diagrama de Dispersión: Contero_tarea vs Evaluación Individual",
         xlab = "Contero_tarea", ylab = var_num, pch = 19, col = rgb(0,0,1,0.5))
    abline(lm(join.unir1.unir2[[var_num]] ~ contero_tarea), col = "red", lwd = 2)
  }
}

# ============================================================
# PARTE 3: ÁRBOL DE DECISIÓN
# (Agregar después de las estadísticas descriptivas)
# ============================================================

cat("\n\n")
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║        CONSTRUCCIÓN DE ÁRBOL DE DECISIÓN            ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

# ============================================================
# FUNCIONES PARA CÁLCULO DE ENTROPÍA Y GANANCIA
# (Ecuaciones 1, 2, 3 del documento)
# ============================================================

# Función para calcular la entropía I(p,n) - Ecuación 1
calcular_entropia <- function(p, n) {
  total <- p + n
  if (total == 0 || p == 0 || n == 0) return(0)
  
  prob_p <- p / total
  prob_n <- n / total
  
  entropia <- -prob_p * log2(prob_p) - prob_n * log2(prob_n)
  return(entropia)
}

# Función para calcular entropía de un atributo E(A) - Ecuación 2
calcular_entropia_atributo <- function(datos, atributo, clase) {
  total <- nrow(datos)
  valores_unicos <- unique(datos[[atributo]])
  valores_unicos <- valores_unicos[!is.na(valores_unicos)]
  
  entropia_total <- 0
  
  for (valor in valores_unicos) {
    subset_datos <- datos[datos[[atributo]] == valor & !is.na(datos[[atributo]]), ]
    
    clases <- unique(datos[[clase]])
    clases <- clases[!is.na(clases)]
    
    if(length(clases) >= 2) {
      p <- sum(subset_datos[[clase]] == clases[1], na.rm = TRUE)
      n <- sum(subset_datos[[clase]] == clases[2], na.rm = TRUE)
      
      peso <- nrow(subset_datos) / total
      entropia_total <- entropia_total + peso * calcular_entropia(p, n)
    }
  }
  
  return(entropia_total)
}

# Función para calcular ganancia Ganancia(A) = I(p,n) - E(A) - Ecuación 3
calcular_ganancia <- function(datos, atributo, clase) {
  clase_factor <- factor(datos[[clase]])
  niveles <- levels(clase_factor)
  
  if(length(niveles) < 2) return(list(entropia_inicial = 0, entropia_atributo = 0, ganancia = 0))
  
  p_total <- sum(clase_factor == niveles[1], na.rm = TRUE)
  n_total <- sum(clase_factor == niveles[2], na.rm = TRUE)
  
  entropia_inicial <- calcular_entropia(p_total, n_total)
  entropia_atributo <- calcular_entropia_atributo(datos, atributo, clase)
  ganancia <- entropia_inicial - entropia_atributo
  
  return(list(
    entropia_inicial = entropia_inicial,
    entropia_atributo = entropia_atributo,
    ganancia = ganancia
  ))
}

cat("✓ Funciones de entropía y ganancia cargadas\n\n")

# ============================================================
# PREPARACIÓN DE DATOS PARA ÁRBOL DE DECISIÓN
# ============================================================

cat("=== PREPARACIÓN DE DATOS ===\n")

# Crear variable objetivo: RENDIMIENTO basado en evaluacion_individual
datos_arbol <- join.unir1.unir2 %>%
  filter(!is.na(evaluacion_individual) & evaluacion_individual > 0) %>%
  mutate(
    # Variable objetivo (4 categorías)
    rendimiento = case_when(
      evaluacion_individual >= 90 ~ "Excelente",
      evaluacion_individual >= 80 ~ "Bueno",
      evaluacion_individual >= 70 ~ "Regular",
      TRUE ~ "Bajo"
    ),
    rendimiento = factor(rendimiento, levels = c("Bajo", "Regular", "Bueno", "Excelente")),
    
    # Variable binaria para cálculo de entropía
    rendimiento_binario = ifelse(evaluacion_individual >= 80, "Aprobado", "NoAprobado"),
    rendimiento_binario = factor(rendimiento_binario),
    
    # Categorizar variables
    categoria_satisfaccion = case_when(
      promedio_r >= 4.5 ~ "Alta",
      promedio_r >= 3.5 ~ "Media",
      TRUE ~ "Baja"
    ),
    categoria_satisfaccion = factor(categoria_satisfaccion, levels = c("Baja", "Media", "Alta")),
    
    # Convertir Contero_tarea a numérico
    Contero_tarea_num = as.numeric(as.character(Contero_tarea)),
    
    categoria_tareas = case_when(
      Contero_tarea_num >= 15 ~ "Alto",
      Contero_tarea_num >= 10 ~ "Medio",
      TRUE ~ "Bajo"
    ),
    categoria_tareas = factor(categoria_tareas, levels = c("Bajo", "Medio", "Alto")),
    
    estatus_simple = case_when(
      Estatus %in% c("Titulado", "Graduado") ~ "Completado",
      Estatus %in% c("Activo", "Egresado", "Prácticas profesionales") ~ "Activo",
      TRUE ~ "Inactivo"
    ),
    estatus_simple = factor(estatus_simple)
  )

cat("Observaciones para el árbol:", nrow(datos_arbol), "\n")
cat("\nDistribución de RENDIMIENTO:\n")
print(table(datos_arbol$rendimiento))

# ============================================================
# CÁLCULO DE GANANCIA DE INFORMACIÓN
# (Como en el documento - páginas 266-277)
# ============================================================

cat("\n\n=== ANÁLISIS DE GANANCIA DE INFORMACIÓN ===\n")
cat("(Ecuaciones del documento: I(p,n), E(A), Ganancia)\n\n")

# Dataset para análisis
datos_ganancia <- datos_arbol %>%
  select(rendimiento_binario, categoria_satisfaccion, categoria_tareas, estatus_simple) %>%
  filter(complete.cases(.))

cat("Clase objetivo: Rendimiento Binario\n")
cat("  P (Aprobado):", sum(datos_ganancia$rendimiento_binario == "Aprobado"), "\n")
cat("  N (NoAprobado):", sum(datos_ganancia$rendimiento_binario == "NoAprobado"), "\n\n")

# Calcular ganancia para cada atributo
atributos <- c("categoria_satisfaccion", "categoria_tareas", "estatus_simple")
resultados_ganancia <- data.frame()

for (atributo in atributos) {
  resultado <- calcular_ganancia(datos_ganancia, atributo, "rendimiento_binario")
  
  cat("──────────────────────────────────────\n")
  cat("Atributo:", atributo, "\n")
  cat("  I(p,n) =", round(resultado$entropia_inicial, 4), "\n")
  cat("  E(A)   =", round(resultado$entropia_atributo, 4), "\n")
  cat("  Ganancia =", round(resultado$ganancia, 4), "\n")
  
  resultados_ganancia <- rbind(resultados_ganancia, data.frame(
    Atributo = atributo,
    Entropia_Inicial = round(resultado$entropia_inicial, 4),
    Entropia_Atributo = round(resultado$entropia_atributo, 4),
    Ganancia = round(resultado$ganancia, 4)
  ))
}

resultados_ganancia <- resultados_ganancia %>% arrange(desc(Ganancia))

cat("\n═══════════════════════════════════════\n")
cat("RESUMEN DE GANANCIAS\n")
cat("═══════════════════════════════════════\n")
print(resultados_ganancia)
cat("\n>>> NODO RAÍZ:", resultados_ganancia$Atributo[1], 
    "(Mayor ganancia:", resultados_ganancia$Ganancia[1], ")\n\n")

# Gráfico de ganancias
ggplot(resultados_ganancia, aes(x = reorder(Atributo, Ganancia), y = Ganancia)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = round(Ganancia, 4)), hjust = -0.2, size = 5) +
  coord_flip() +
  labs(title = "Ganancia de Información por Atributo",
       subtitle = "El atributo con mayor ganancia forma el nodo raíz",
       x = "Atributo", y = "Ganancia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# ============================================================
# CONSTRUCCIÓN DEL ÁRBOL DE DECISIÓN
# ============================================================

cat("\n=== CONSTRUCCIÓN DEL ÁRBOL ===\n")

# Preparar dataset final
datos_modelo <- datos_arbol %>%
  select(rendimiento, categoria_satisfaccion, categoria_tareas, 
         estatus_simple, promedio_r, Contero_tarea_num) %>%
  filter(complete.cases(.))

cat("Total observaciones:", nrow(datos_modelo), "\n\n")

# PASO 1: División entrenamiento/prueba (70%/30%)
set.seed(123)
indices_train <- createDataPartition(datos_modelo$rendimiento, p = 0.7, list = FALSE)
datos_train <- datos_modelo[indices_train, ]
datos_test <- datos_modelo[-indices_train, ]

cat("Entrenamiento (70%):", nrow(datos_train), "observaciones\n")
cat("Prueba (30%):", nrow(datos_test), "observaciones\n\n")

# PASO 2: Construir el árbol
cat("Construyendo árbol con algoritmo CART...\n")
arbol_modelo <- rpart(
  rendimiento ~ .,
  data = datos_train,
  method = "class",
  control = rpart.control(
    minsplit = 20,
    minbucket = 7,
    cp = 0.01,
    maxdepth = 10
  )
)

cat("✓ Árbol construido\n")
cat("  Nodos:", nrow(arbol_modelo$frame), "\n\n")

# PASO 3: Visualizar el árbol
rpart.plot(
  arbol_modelo,
  type = 4,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Árbol de Decisión: Rendimiento Académico",
  box.palette = "RdYlGn",
  shadow.col = "gray",
  nn = TRUE,
  tweak = 1.2
)

# ============================================================
# IMPORTANCIA DE VARIABLES
# ============================================================

cat("\n=== IMPORTANCIA DE VARIABLES ===\n")
importancia <- arbol_modelo$variable.importance
if(length(importancia) > 0) {
  importancia_df <- data.frame(
    Variable = names(importancia),
    Importancia = importancia,
    Porcentaje = round((importancia / sum(importancia)) * 100, 2)
  ) %>% arrange(desc(Importancia))
  
  print(importancia_df)
  
  # Gráfico
  ggplot(importancia_df, aes(x = reorder(Variable, Importancia), y = Importancia)) +
    geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
    geom_text(aes(label = paste0(Porcentaje, "%")), hjust = -0.2, size = 4) +
    coord_flip() +
    labs(title = "Importancia de Variables en el Árbol",
         x = "Variable", y = "Importancia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# ============================================================
# EVALUACIÓN - MATRIZ DE CONFUSIÓN
# (Como en el documento - páginas 282-292)
# ============================================================

cat("\n\n=== EVALUACIÓN DEL MODELO ===\n")

# Predicciones
predicciones <- predict(arbol_modelo, datos_test, type = "class")
matriz_conf <- confusionMatrix(predicciones, datos_test$rendimiento)

cat("\n*** MATRIZ DE CONFUSIÓN ***\n\n")
print(matriz_conf$table)

# Métricas
accuracy <- matriz_conf$overall['Accuracy']
error <- 1 - accuracy

cat("\n*** MÉTRICAS PRINCIPALES ***\n")
cat("─────────────────────────────\n")
cat("Accuracy (Exactitud):", round(accuracy * 100, 2), "%\n")
cat("Error:", round(error * 100, 2), "%\n")
cat("Kappa:", round(matriz_conf$overall['Kappa'], 4), "\n\n")

# Métricas por clase
metricas_clase <- as.data.frame(matriz_conf$byClass)
metricas_resumen <- data.frame(
  Clase = rownames(metricas_clase),
  Sensibilidad = round(metricas_clase$Sensitivity * 100, 2),
  Especificidad = round(metricas_clase$Specificity * 100, 2),
  Precision = round(metricas_clase$Precision * 100, 2),
  F1 = round(metricas_clase$F1 * 100, 2)
)

cat("*** MÉTRICAS POR CLASE ***\n")
print(metricas_resumen)

# Visualización matriz de confusión
matriz_df <- as.data.frame(matriz_conf$table)
colnames(matriz_df) <- c("Prediccion", "Real", "Frecuencia")

ggplot(matriz_df, aes(x = Real, y = Prediccion, fill = Frecuencia)) +
  geom_tile(color = "white", size = 1.5) +
  geom_text(aes(label = Frecuencia), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Matriz de Confusión",
       subtitle = paste0("Exactitud: ", round(accuracy * 100, 2), "%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 12, face = "bold"))

# ============================================================
# PODA DEL ÁRBOL
# (Paso 4 del algoritmo - página 262)
# ============================================================

cat("\n\n=== PODA DEL ÁRBOL ===\n")
cat("(Eliminar ramas que representan ruido)\n\n")

# Tabla de complejidad
cat("Parámetro de Complejidad (CP):\n")
printcp(arbol_modelo)

# Gráfico CP
plotcp(arbol_modelo)

# Mejor CP
mejor_cp <- arbol_modelo$cptable[which.min(arbol_modelo$cptable[,"xerror"]), "CP"]
cat("\n>>> Mejor CP:", mejor_cp, "\n")

# Podar
arbol_podado <- prune(arbol_modelo, cp = mejor_cp)
cat("Nodos después de podar:", nrow(arbol_podado$frame), "\n\n")

# Visualizar árbol podado
rpart.plot(
  arbol_podado,
  type = 4,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Árbol de Decisión PODADO",
  box.palette = "RdYlGn",
  shadow.col = "gray",
  nn = TRUE,
  tweak = 1.2
)

# Evaluar árbol podado
predicciones_podado <- predict(arbol_podado, datos_test, type = "class")
matriz_conf_podado <- confusionMatrix(predicciones_podado, datos_test$rendimiento)

cat("\n*** COMPARACIÓN ***\n")
comparacion <- data.frame(
  Modelo = c("Original", "Podado"),
  Nodos = c(nrow(arbol_modelo$frame), nrow(arbol_podado$frame)),
  Exactitud = c(
    round(matriz_conf$overall['Accuracy'] * 100, 2),
    round(matriz_conf_podado$overall['Accuracy'] * 100, 2)
  )
)
print(comparacion)

# ============================================================
# PREDICCIÓN DE NUEVOS CASOS
# ============================================================

cat("\n\n=== PREDICCIÓN DE NUEVOS ESTUDIANTES ===\n\n")

# Crear casos ejemplo
nuevos <- data.frame(
  categoria_satisfaccion = factor(c("Alta", "Baja", "Media"), 
                                  levels = levels(datos_train$categoria_satisfaccion)),
  categoria_tareas = factor(c("Alto", "Bajo", "Medio"), 
                            levels = levels(datos_train$categoria_tareas)),
  estatus_simple = factor(c("Activo", "Activo", "Completado"), 
                          levels = levels(datos_train$estatus_simple)),
  promedio_r = c(4.8, 3.0, 4.2),
  Contero_tarea_num = c(18, 6, 14)
)

predicciones_nuevos <- predict(arbol_podado, nuevos, type = "class")
probabilidades <- predict(arbol_podado, nuevos, type = "prob")

for(i in 1:nrow(nuevos)) {
  cat("─── Estudiante", i, "───\n")
  cat("  Satisfacción:", as.character(nuevos$categoria_satisfaccion[i]), "\n")
  cat("  Tareas:", as.character(nuevos$categoria_tareas[i]), "\n")
  cat("  Estatus:", as.character(nuevos$estatus_simple[i]), "\n")
  cat("  → Predicción:", as.character(predicciones_nuevos[i]), "\n")
  cat("  → Probabilidades:\n")
  for(j in 1:ncol(probabilidades)) {
    cat("     ", colnames(probabilidades)[j], ":", 
        round(probabilidades[i,j] * 100, 1), "%\n")
  }
  cat("\n")
}

# ============================================================
# RESUMEN FINAL
# ============================================================

cat("\n")
cat("╔════════════════════════════════════════════════════╗\n")
cat("║           RESUMEN DEL ANÁLISIS COMPLETO           ║\n")
cat("╚════════════════════════════════════════════════════╝\n\n")

cat("1. GANANCIA DE INFORMACIÓN\n")
cat("   Mejor atributo:", resultados_ganancia$Atributo[1], "\n")
cat("   Ganancia:", resultados_ganancia$Ganancia[1], "\n\n")

cat("2. MODELO\n")
cat("   Datos entrenamiento:", nrow(datos_train), "\n")
cat("   Datos prueba:", nrow(datos_test), "\n")
cat("   Nodos árbol podado:", nrow(arbol_podado$frame), "\n\n")

cat("3. RENDIMIENTO\n")
cat("   Exactitud:", round(matriz_conf_podado$overall['Accuracy'] * 100, 2), "%\n")
cat("   Kappa:", round(matriz_conf_podado$overall['Kappa'], 4), "\n\n")

cat("4. CONCLUSIÓN\n")
if(matriz_conf_podado$overall['Accuracy'] >= 0.80) {
  cat("   ✓ Modelo con buen rendimiento\n")
} else if(matriz_conf_podado$overall['Accuracy'] >= 0.70) {
  cat("   ⚠ Modelo aceptable, puede mejorarse\n")
} else {
  cat("   ✗ Modelo requiere mejoras\n")
}

cat("\n╔════════════════════════════════════════════════════╗\n")
cat("║              ANÁLISIS COMPLETADO                  ║\n")
cat("╚════════════════════════════════════════════════════╝\n\n")

# Guardar modelo
cat("Para guardar el modelo:\n")
cat('  saveRDS(arbol_podado, "modelo_arbol_rendimiento.rds")\n\n')

