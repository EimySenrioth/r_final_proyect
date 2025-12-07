library(readxl)
# Actualizar rutas para cargar los archivos Excel
unir_master <- readxl::read_excel("c:/Users/itano/Desktop/r_documents/proyecto_final_r/unir_master (1).xlsx")
Unirmaster_dos <- readxl::read_excel("c:/Users/itano/Desktop/r_documents/proyecto_final_r/Unirmaster_dos.xlsx")

View(Unirmaster_dos)
View(unir_master)
data("mtcars")
names(mtcars)
library(dplyr)
library(ggplot2)
# quitar todas las columnas que estén en NULL o NA
um1.not.na.o.null <- unir_master %>% select(where(~ !any(is.na(.) | . == "NULL")))

# quitar las columnas que tienen más del 15% de faltantes
um1.not.na.0.15 <- um1.not.na.o.null %>% select(where(~ mean(is.na(.)) <= 0.15))

head(um1.not.na.0.15)
um1.renombrado.col.id_alumno <- um1.not.na.0.15 %>% rename(id_estudiante = id_alumno)

# al unir2 lo uno con el unir 1
join.unir1.unir2 <- Unirmaster_dos %>% left_join(um1.renombrado.col.id_alumno, by = "id_estudiante")

# por curso contar los ceros, si son mayor al 15% significa que son faltantes
join.unir1.unir2.no.0.15 <- um1.not.na.o.null %>% select(where(~ mean(. == 0) <= 0.15))

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
# ESTADÍSTICAS Y GRÁFICOS DESCRIPTIVOS AVANZADOS DEL DATASET UNIDO
# ============================================================

# Variable numérica de interés
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

  # Desviación típica (igual a desviación estándar)
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

  # Diagrama de dispersión del dataset unido (ejemplo: Prom_tarea vs evaluacion_individual)
  if ("Prom_tarea" %in% names(join.unir1.unir2)) {
    plot(join.unir1.unir2$Prom_tarea, join.unir1.unir2[[var_num]],
         main = "Diagrama de Dispersión: Prom_tarea vs Evaluación Individual",
         xlab = "Prom_tarea", ylab = var_num, pch = 19, col = rgb(0,0,1,0.5))
    abline(lm(join.unir1.unir2[[var_num]] ~ join.unir1.unir2$Prom_tarea), col = "red", lwd = 2)
  }
}

# ============================================================
# medias de dispercion de rango, Media recortada, desviazcion tipica, 
#asimetria de los datos en histograma, diagrama de dispercion prom tarea vz evaluacion infividual
# ============================================================

var_num <- "evaluacion_individual"

# Diagrama de dispersión del dataset (promedio_final vs evaluacion_individual redondeada)
if ("promedio_final" %in% names(join.unir1.unir2)) {
  promedio_final <- as.numeric(as.character(join.unir1.unir2$promedio_final))
  eval_ind_rd <- round(join.unir1.unir2$evaluacion_individual, 2)
  plot(promedio_final, eval_ind_rd,
       main = "Diagrama de Dispersión: Promedio final vs Evaluación Individual (redondeada)",
       xlab = "Promedio final", ylab = "Evaluación Individual (redondeada)",
       pch = 19, col = rgb(0,0,1,0.5))
  abline(lm(eval_ind_rd ~ promedio_final), col = "red", lwd = 2)
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