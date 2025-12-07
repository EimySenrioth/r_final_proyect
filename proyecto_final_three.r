#cargar librerías para árbol de clasificación
library(summarytools)
library(psych)
library(dplyr)
library(skimr)
library(rpart.plot)
library(caret)

# Instalar y cargar librerías para lectura de archivos Excel
if (!require("readxl")) install.packages("readxl", repos = "https://cloud.r-project.org")
library(readxl)

# Define la ruta de la carpeta & cargar los archivos Excel
ruta_carpeta <- "c:/Users/itano/Desktop/r_documents/proyecto_final_r"

df1 <- read_excel(file.path(ruta_carpeta, "unir_master (1).xlsx"))
df2 <- read_excel(file.path(ruta_carpeta, "Unirmaster_dos.xlsx"))

# Explora los datos cargados
head(df1)
head(df2)

# Calcular cantidad y porcentaje de valores NA por columna para df1
na_por_col_df1 <- colSums(is.na(df1))
porc_na_por_col_df1 <- na_por_col_df1 / nrow(df1) * 100
resumen_na_col_df1 <- data.frame(
  columna = names(df1),
  na_count = na_por_col_df1,
  na_percent = porc_na_por_col_df1
)
write.csv(resumen_na_col_df1, file = file.path(ruta_carpeta, "na_columnas_df1.csv"), row.names = FALSE)

# Calcular cantidad y porcentaje de valores NA por columna para df2
na_por_col_df2 <- colSums(is.na(df2))
porc_na_por_col_df2 <- na_por_col_df2 / nrow(df2) * 100
resumen_na_col_df2 <- data.frame(
  columna = names(df2),
  na_count = na_por_col_df2,
  na_percent = porc_na_por_col_df2
)
write.csv(resumen_na_col_df2, file = file.path(ruta_carpeta, "na_columnas_df2.csv"), row.names = FALSE)

# Contar valores NA desde la columna r25_1 en df1
col_inicio <- which(names(df1) == "r25_1")
na_por_col_r25_1_df1 <- colSums(is.na(df1[, col_inicio:ncol(df1)]))
porc_na_por_col_r25_1_df1 <- na_por_col_r25_1_df1 / nrow(df1) * 100
resumen_na_col_r25_1_df1 <- data.frame(
  columna = names(df1)[col_inicio:ncol(df1)],
  na_count = na_por_col_r25_1_df1,
  na_percent = porc_na_por_col_r25_1_df1
)
write.csv(resumen_na_col_r25_1_df1, file = file.path(ruta_carpeta, "na_columnas_r25_1_df1.csv"), row.names = FALSE)

#Unir los datasets

#Árboles de desición - Modelo de clasificaciòn

