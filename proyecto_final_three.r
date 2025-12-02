if (!require("readxl")) install.packages("readxl", repos = "https://cloud.r-project.org")
library(readxl)

# Define la ruta de la carpeta & cargar los archivos Excel
ruta_carpeta <- "c:/Users/itano/Desktop/r_documents/proyecto_final_r"

df1 <- read_excel(file.path(ruta_carpeta, "unir_master (1).xlsx"))
df2 <- read_excel(file.path(ruta_carpeta, "Unirmaster_dos.xlsx"))

# Explora los datos cargados
head(df1)
head(df2)

# Realiza un análisis exploratorio básico