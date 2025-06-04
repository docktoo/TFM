# paquetes
library(readxl)
library(purrr)
library(writexl)
library(raster)
library(dplyr)
library(ggplot2)
library(moments)
library(nortest)
library(FactoMineR) 
library(factoextra)
library(biomod2)
library(tidyr)
library(gridExtra)
library(inspectdf)
library(DataExplorer)



# Define la ruta del archivo CSV
ruta_archivo <- "C:/Users/smoya/Desktop/gbif.csv"

datos_GBIF <- read_delim(ruta_archivo, delim = "\t")

# Crear una copia del dataframe original
data_modificado <- datos_GBIF

# Especificar las columnas a eliminar
columnas_a_eliminar <- c(
  "datasetKey", "occurrenceID", "kingdom", "phylum", "class", "order", 
  "family", "genus", "infraspecificEpithet", "taxonRank", "scientificName", 
  "verbatimScientificName", "verbatimScientificNameAuthorship", 
  "occurrenceStatus", "individualCount", "publishingOrgKey", 
  "coordinateUncertaintyInMeters", "coordinatePrecision", "elevation", 
  "elevationAccuracy", "depth", "depthAccuracy", 
  colnames(datos_GBIF)[34:50]
)

# Eliminar las columnas especificadas del dataframe modificado
data_modificado <- data_modificado[, !(colnames(data_modificado) %in% columnas_a_eliminar)]

# Guardar el dataframe modificado en un nuevo archivo CSV
write_csv(data_modificado, "datos_modificados.csv")

# Guardar el dataframe modificado en un archivo XLSX
write_xlsx(data_modificado, "datos_modificados_excel.xlsx")

# Mostrar las primeras filas del dataframe modificado
head(data_modificado)
getwd()

#----------------------------------------------------------------------------
###### EDA ######
#----------------------------------------------------------------------------

# cargamos la información del QGIS
base_datos<- read_excel("C:/Users/smoya/Desktop/TFM/bio_qgis/ES/resultado_combinado.xlsx")
# Pequeña visualizacion
head(base_datos)
# Una vez combinado todo procedemos con el EDA
# Se buscan los valores faltantes o NA
colSums(is.na(base_datos))

# Eliminamos las columnas que no nos interesan como bioXcount
columnas_a_eliminar <- paste0("bio", 1:19, "count")
base_datos <- base_datos[, !names(base_datos) %in% columnas_a_eliminar]

#Elimino tambien bioxsum
elm_biosum <- paste0("bio", 1:19, "sum")
base_datos <- base_datos[, !names(base_datos) %in% elm_biosum]

# creamos un nuevo data frame con las variables a analizar exlcuyendo el id y las coordenadas y otr informacion georreferenciada
columnas_a_excluir <- c("id", "left", "top", "right", "bottom", "centro_x", "centro_y", "row_index", "col_index")
base_datos_analisis <- base_datos %>% select(-all_of(columnas_a_excluir))

# Reporte completo de EDA con visualizaciones automáticas
create_report(base_datos_analisis, output_file = "eda_reporte.html")

#resumen estadistico
resumen <- base_datos_analisis %>%
  summarise(across(where(is.numeric), list(
    n = ~length(.x),
    na = ~sum(is.na(.x)),
    media = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    IQR = ~IQR(.x, na.rm = TRUE),
    Skewness = ~skewness(.x, na.rm = TRUE),
    Kurtosis = ~kurtosis(.x, na.rm = TRUE),
    percentil_25 = ~quantile(.x, 0.25, na.rm = TRUE),
    percentil_50 = ~quantile(.x, 0.50, na.rm = TRUE),
    percentil_75 = ~quantile(.x, 0.75, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

resumen_largo <- resumen %>%
  pivot_longer(
    cols = everything(),
    names_pattern = "(.+?)_(n|na|media|sd|max|min|IQR|Skewness|Kurtosis|percentil_25|percentil_50|percentil_75)",
    names_to = c("Variable", "Metrica")
  ) %>%
  pivot_wider(
    names_from = "Metrica",
    values_from = "value",
    values_fn = function(x) x[1] # Extrae solo el primer valor, evita listas
  ) 

# Ver resultado
print(resumen)

library(inspectdf)
df_simple <- as.data.frame(base_datos_analisis)

inspect_types(df_simple) %>%
  plot() +
  labs(title = "Figura 15. Tipos de Variables (Discretas vs Continuas)")
# Tipos de columnas (todas deberían ser continuas en este caso)
inspect_types(base_datos_analisis) %>%
  plot() +
  labs(title = "Figura 15. Tipos de Variables (Discretas vs Continuas)")

inspect_types(df_simple) -> tipos_df
plot_inspect(tipos_df) +
  labs(title = "Figura 15. Tipos de Variables (Discretas vs Continuas)")

# Crear la tabla base_datos# Crear la tabla con el número de puntos y la cantidad de veces que aparece
tabla_frecuencia <- as.data.frame(table(base_datos_analisis$numero_puntos_total))
colnames(tabla_frecuencia) <- c("numero_puntos_total", "frecuencia")

# Filtrar solo los valores presentes en el data frame
tabla_frecuencia <- tabla_frecuencia[tabla_frecuencia$frecuencia > 0, ]

# Mostrar la tabla
print(tabla_frecuencia)


ggplot(base_datos_grupo, aes(x = as.factor(numero_puntos_total))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Frecuencia de numero_puntos_total",
       x = "Número de puntos (presencia)",
       y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras con frecuencia sobre las barras
ggplot(base_datos_grupo, aes(x = categoria_puntos)) +
  geom_bar(fill = "steelblue", color = "black", width = 0.7) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  # Añadir texto sobre las barras
  labs(title = "Frecuencia de Categorías de Número de Puntos",
       x = "Categoría de Número de Puntos",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Seleccionar solo las variables deseadas
variables_seleccionadas <- base_datos_analisis %>%
  select(bio9mean, bio13mean, bio12mean, bio4mean)

# Convertir a formato largo
variables_largo <- variables_seleccionadas %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Graficar histogramas
ggplot(variables_largo, aes(x = Valor)) +
  geom_histogram(bins = 30, fill = "darkcyan", color = "white") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histogramas de bio9, bio13, bio12 y bio4")


# Definir grupos de variables (puedes ajustar esto según las variables de tu base de datos)
temperatura_variables <- c("bio1mean", "bio2mean", "bio3mean", "bio4mean", "bio5mean","bio6mean","bio7mean", "bio8mean","bio9mean","bio10mean","bio11mean")  
precipitacion_variables <- c("bio12mean", "bio13mean", "bio14mean","bio15mean" , "bio16mean", "bio17mean","bio18mean","bio19mean")  


# Histograma para las variables de temperatura
histograms_temp <- list()
for (col in temperatura_variables) {
  p <- ggplot(base_datos_analisis, aes(x = .data[[col]])) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = paste("Histograma de", col), x = col, y = "Frecuencia")
  histograms_temp[[col]] <- p
}

# Histograma para las variables de precipitación
histograms_precip <- list()
for (col in precipitacion_variables) {
  p <- ggplot(base_datos_analisis, aes(x = .data[[col]])) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = paste("Histograma de", col), x = col, y = "Frecuencia")
  histograms_precip[[col]] <- p
}

# Mostrar los histogramas agrupados por tipo de variable (Temperatura y Precipitación)
# Gráfico de temperatura
do.call(grid.arrange, c(histograms_temp, ncol = 3))

# Gráfico de precipitación
do.call(grid.arrange, c(histograms_precip, ncol = 3))

# Boxplots para las variables de temperatura vs numero_puntos_total
boxplots_temp <- list()
for (col in temperatura_variables) {
  p <- ggplot(base_datos_analisis, aes(x = .data[[col]], y = as.factor(numero_puntos_total))) +
    geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot de", col, "vs numero_puntos_total"), x = col, y = "Número de Puntos") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  boxplots_temp[[col]] <- p
}

# Boxplots para las variables de precipitación vs numero_puntos_total
boxplots_precip <- list()
for (col in precipitacion_variables) {
  p <- ggplot(base_datos_analisis, aes(x = .data[[col]], y = as.factor(numero_puntos_total))) +
    geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot de", col, "vs numero_puntos_total"), x = col, y = "Número de Puntos") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  boxplots_precip[[col]] <- p
}

# Mostrar los boxplots agrupados por tipo de variable (Temperatura y Precipitación)
# Boxplots de temperatura
do.call(grid.arrange, c(boxplots_temp, ncol = 3))

# Boxplots de precipitación
do.call(grid.arrange, c(boxplots_precip, ncol = 3))

# Diagrama de dispersión para análisis bivariado de temperatura
scatterplots_temp <- list()
for (col in temperatura_variables) {
  p <- ggplot(base_datos_analisis, aes(x = .data[[col]], y = numero_puntos_total)) +
    geom_point(color = "blue") +
    labs(title = paste("Dispersión de numero_puntos_total vs", col), x = col, y = "numero_puntos_total") +
    theme_minimal()
  scatterplots_temp[[col]] <- p
}

# Diagrama de dispersión para análisis bivariado de precipitación
scatterplots_precip <- list()
for (col in precipitacion_variables) {
  p <- ggplot(base_datos_analisis, aes(x = .data[[col]], y = numero_puntos_total)) +
    geom_point(color = "blue") +
    labs(title = paste("Dispersión de numero_puntos_total vs", col), x = col, y = "numero_puntos_total") +
    theme_minimal()
  scatterplots_precip[[col]] <- p
}

# Mostrar los diagramas de dispersión agrupados por tipo de variable (Temperatura y Precipitación)
# Dispersión de temperatura
do.call(grid.arrange, c(scatterplots_temp, ncol = 3))

# Dispersión de precipitación
do.call(grid.arrange, c(scatterplots_precip, ncol = 3))


# Crear una nueva variable 'base_datos_grupo' para agrupar número de puntos
base_datos_grupo <- base_datos_analisis %>%
  mutate(categoria_puntos = case_when(
    numero_puntos_total == 0 ~ "0",
    numero_puntos_total >= 1 & numero_puntos_total <= 5 ~ "1-5",
    numero_puntos_total >= 6 & numero_puntos_total <= 10 ~ "6-10",
    numero_puntos_total > 10 ~ "Más de 10"
  ))

# Convertir a factor para asegurar el orden de las categorías
base_datos_grupo$categoria_puntos <- factor(base_datos_grupo$categoria_puntos, 
                                            levels = c("0", "1-5", "6-10", "Más de 10"))

boxplot_grupo <- list()

# Definir las variables de temperatura y precipitación (ajusta según tus nombres)
variables_temperatura <- grep("^bio[1-9]mean$", names(base_datos_grupo), value = TRUE)  # Variables de temperatura
variables_precipitacion <- grep("^bio[10-19]mean$", names(base_datos_grupo), value = TRUE)  # Variables de precipitación
# Definir las variables que deseas graficar
variables_deseadas <- c("bio12mean", "bio13mean", "bio4mean", "bio8mean")

# Filtrar las variables que realmente están presentes en tu dataset
variables_seleccionadas_boxplot <- intersect(variables_deseadas, names(base_datos_grupo))

# Crear listas vacías para los gráficos de temperatura y precipitación
boxplot_grupo_temp <- list()
boxplot_grupo_prec <- list()
boxplot_grupo_selec <- list()


# Crear boxplots para las variables de temperatura vs categoria_puntos
for (col in variables_temperatura) {
  plot <- ggplot(base_datos_grupo, aes(x = categoria_puntos, y = .data[[col]])) +
    geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot de", col, "vs Categoría de Número de Puntos"),
         x = "Categoría de Número de Puntos", y = col) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Mejora la legibilidad
  boxplot_grupo_temp[[col]] <- plot
}

# Crear boxplots para las variables de precipitación vs categoria_puntos
for (col in precipitacion_variables) {
  plot <- ggplot(base_datos_grupo, aes(x = categoria_puntos, y = .data[[col]])) +
    geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot de", col, "vs Categoría de Número de Puntos"),
         x = "Categoría de Número de Puntos", y = col) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Mejora la legibilidad
  boxplot_grupo_prec[[col]] <- plot
}

# Crear boxplots para las variables seleccionadas vs categoria_puntos
for (col in variables_seleccionadas_boxplot) {
  plot <- ggplot(base_datos_grupo, aes(x = categoria_puntos, y = .data[[col]])) +
    geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot de", col, "vs Categoría de Número de Puntos"),
         x = "Categoría de Número de Puntos", y = col) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Mejora la legibilidad
  boxplot_grupo_selec[[col]] <- plot
}


# Mostrar todos los boxplots de temperatura juntos
do.call(grid.arrange, c(boxplot_grupo_temp, ncol = 2))

# Mostrar todos los boxplots de precipitación juntos
do.call(grid.arrange, c(boxplot_grupo_prec, ncol = 2))

# Mostrar todos los boxplots de temperatura juntos
do.call(grid.arrange, c(boxplot_grupo_selec, ncol = 2))

# Instalar y cargar el paquete necesario
if (!require(nortest)) {
  install.packages("nortest")
}
library(nortest)

# Lista para almacenar los resultados
resultados_ks <- list()
resultados_ad <- list()

# Realizar las pruebas de normalidad para todas las columnas numéricas
for (col in names(base_datos_analisis)) {
  if (is.numeric(base_datos_analisis[[col]])) {
    # Prueba de Kolmogorov-Smirnov
    ks_test <- lillie.test(base_datos_analisis[[col]])
    resultados_ks[[col]] <- ks_test
    
    # Prueba de Anderson-Darling
    ad_test <- ad.test(base_datos_analisis[[col]])
    resultados_ad[[col]] <- ad_test
  }
}

# Mostrar los resultados
print("Resultados de la prueba de Kolmogorov-Smirnov:")
print(resultados_ks)

print("Resultados de la prueba de Anderson-Darling:")
print(resultados_ad)

# Calcular la matriz de correlación de Spearman entre las variables numéricas
correlaciones <- cor(base_datos_analisis[, sapply(base_datos_analisis, is.numeric)], 
                     method = "spearman", use = "complete.obs")

# Imprimir la matriz de correlación
print(correlaciones)

# Visualizar la matriz de correlación con un mapa de calor (opcional)
install.packages("corrplot")  # Solo la primera vez
library(corrplot)

# Crear el mapa de calor de la matriz de correlación con círculos y números
corrplot(correlaciones, method = "circle", type = "upper", tl.cex = 0.8, 
         tl.col = "black", diag = FALSE, addCoef.col = "black", 
         number.cex = 0.7, cl.pos = "n")  # cl.pos = "n" quita la leyenda de color

correlaciones <- cor(base_datos_analisis, use = "complete.obs")

corrplot.mixed(correlaciones, 
               upper = "number", 
               lower = "circle", 
               tl.cex = 0.8, 
               tl.col = "black", 
               number.cex = 0.7, 
               diag = "n",
               tl.pos = "lt",  # Posiciona los nombres en la parte superior y a la izquierda
               tl.srt = 45)    # Ajusta la rotación de los nombres

# Calcular el VIF

library(olsrr)
modelo_vif <- lm(y ~ ., data = base_datos_analisis[, -1])
ols_vif_tol(modelo_vif)

# Crear un gráfico de barras de los VIF
ggplot(vif_tabla, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +  # Para voltear las barras y hacer que los nombres de las variables sean legibles
  labs(title = "Gráfico de VIF para Variables del Modelo", x = "Variable", y = "VIF") +
  theme_minimal()

#--------------------------------------------------------------
# Analsis de componentes principales
#-------------------------------------------------------------
# Crear una nueva variable sin la columna numero_puntos_total
datos_ACP <- base_datos_analisis %>% select(-numero_puntos_total)

dim(datos_scaled)
#Estandarizar variables media=0 desv=1
datos_scaled <- scale(datos_ACP)

# Realizar el ACP
acp <- PCA(datos_scaled, graph = FALSE)

# Varianza explicada
fviz_eig(acp, addlabels = TRUE, barfill = "steelblue", barcolor = "black")

#Contribucion de las variables
fviz_pca_var(acp, col.var = "cos2", gradient.cols = c("blue", "red"), repel = TRUE)

# Mostrar los eigenvalues (valores propios)
eigenvalues <- acp$eig
print(eigenvalues)

# peso de las variables
fviz_pca_var(acp, 
             col.var = "blue",      # Color de las variables
             repel = TRUE,          # Evitar el solapamiento de etiquetas
             title = "Correlación entre las variables")  # Título

# Visualizar la contribución de las variables a los primeros dos componentes
fviz_contrib(acp, choice = "var", axes = c(1, 2, 3, 4), top = 19)


contribuciones <- acp$var$contrib
# Mostrar contribución de las variables a los dos primeros componentes
contribuciones_pc4 <- contribuciones[, c("Dim.1", "Dim.2", "Dim.3", "Dim.4")]
print(contribuciones_pc4)
# Contribución de las variables a las primeras 4 dimensiones
contribuciones_4dim <- acp$var$contrib[, 1:4]  # Obtener contribución a las 4 primeras dimensiones

# Visualizar la contribución de las variables a los primeros cuatro componentes
fviz_pca_var(acp, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(
    title = "Contribution of variables to Dim-1-4",
    x = "Dimensions",
    y = "Variables"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )
# Obtener las contribuciones de las variables a los componentes principales
contrib <- get_pca_var(acp)$contrib

# Seleccionar las contribuciones de los primeros cuatro componentes
contrib_matrix <- contrib[, 1:4]
# Crear el gráfico de contribuciones
corrplot(contrib_matrix, is.corr = FALSE, method = "circle", col = colorRampPalette(c("#00AFBB", "#E7B800", "#FC4E07"))(200),
         title = "Contribution of variables to Dim-1-4", tl.cex = 0.8, cl.cex = 0.8)

# Obtener los primeros 4 componentes principales
pc_data <- as.data.frame(acp$ind$coord[, 1:4])
# Añadir la columna de presencia/ausencia
pc_data$presence <- base_datos_analisis$numero_puntos_total
# Añadir la columna de coordenadas originales
pc_data$centro_x <- base_datos$centro_x
pc_data$centro_y <- base_datos$centro_y
# Transformar la columna numero_puntos_total en una variable binaria en el nuevo DataFrame
pc_data$presence <- ifelse(pc_data$presence > 0, 1, 0)

# -------------------------------
# Formateo de datos SPP126
# -------------------------------

# Script para unir los excel que genera el qgis las 19 variables ambientales de los diferentes sppp
#unir excel del qgis, ir cambiando la ruta del ruta_qgis
ruta_qgis_126<- list.files(path = "C:/Users/smoya/Desktop/TFM/bio_qgis/ES_126", pattern = "*.xlsx", full.names = TRUE)
# Leer todos los archivos Excel y almacenarlos en una lista
lista_qgist <- lapply(ruta_qgis_126, read_excel)
#Combinar las listas partiendo de la base que tienen el mismo id y coordendas
datos_combinados_spp126 <- lista_qgist %>%
  reduce(function(x, y) full_join(x, y, by = c("id", "left", "top", "right", "bottom", "row_index", "col_index","NUMPOINTS")))

# Verifica el resultado
head(datos_combinados_spp126)

#Guardo el excel y hago las columnas de coordenas x e y
write_xlsx(datos_combinados_spp126, "C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp126.xlsx")

# Cargo archivo y escalo los datos
datos_126<-read_excel("C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp126.xlsx")

datos_126_acp<- datos_126 %>% select(-id,-left,-top,-right,-bottom,-centro_x,-centro_y,-row_index,-col_index,-NUMPOINTS)

pc_126<-scale(datos_126_acp)

# Asignar los mismos nombres de columna que los datos originales escalados
colnames(pc_126) <- colnames(datos_scaled)
  
# Proyectar los datos futuros sobre los componentes del ACP original
proyeccion_futuros <- predict(acp, newdata = pc_126)

# Extraer los primeros 4 componentes principales
pc_futuros_126 <- as.data.frame(proyeccion_futuros$coord[, 1:4])

# Añadir la columna de presencia/ausencia
pc_futuros_126$presence <- datos_126$NUMPOINTS
# Añadir la columna de coordenadas originales
pc_futuros_126$centro_x <- datos_126$centro_x
pc_futuros_126$centro_y <- datos_126$centro_y
# Transformar la columna numero_puntos_total en una variable binaria en el nuevo DataFrame
pc_futuros_126$presence <- ifelse(pc_futuros_126$presence > 0, 1, 0)
#eliminar NA
pc_futuros_126 <- na.omit(pc_futuros_126)

# -------------------------------
# Formateo de datos SPP245
# -------------------------------

#unir excel del qgis, ir cambiando la ruta del ruta_qgis
ruta_qgis_245<- list.files(path = "C:/Users/smoya/Desktop/TFM/bio_qgis/ES_245", pattern = "*.xlsx", full.names = TRUE)
# Leer todos los archivos Excel y almacenarlos en una lista
lista_qgist_245 <- lapply(ruta_qgis_245, read_excel)
#Combinar las listas partiendo de la base que tienen el mismo id y coordendas
datos_combinados_spp245 <- lista_qgist_245 %>%
  reduce(function(x, y) full_join(x, y, by = c("id", "left", "top", "right", "bottom", "row_index", "col_index","NUMPOINTS")))

# Verifica el resultado
head(datos_combinados_spp245)

#Guardo el excel y hago las columnas de coordenas x e y
write_xlsx(datos_combinados_spp245, "C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp245.xlsx")

# Cargo archivo y escalo los datos
datos_245<-read_excel("C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp245.xlsx")

datos_245_acp<- datos_245 %>% select(-id,-left,-top,-right,-bottom,-centro_x,-centro_y,-row_index,-col_index,-NUMPOINTS)

pc_245<-scale(datos_245_acp)

# Asignar los mismos nombres de columna que los datos originales escalados
colnames(pc_245) <- colnames(datos_scaled)

# Proyectar los datos futuros sobre los componentes del ACP original
proyeccion_futuros_245 <- predict(acp, newdata = pc_245)

# Extraer los primeros 4 componentes principales
pc_futuros_245 <- as.data.frame(proyeccion_futuros_245$coord[, 1:4])

# Añadir la columna de presencia/ausencia
pc_futuros_245$presence <- datos_245$NUMPOINTS
# Añadir la columna de coordenadas originales
pc_futuros_245$centro_x <- datos_245$centro_x
pc_futuros_245$centro_y <- datos_245$centro_y
# Transformar la columna numero_puntos_total en una variable binaria en el nuevo DataFrame
pc_futuros_245$presence <- ifelse(pc_futuros_245$presence > 0, 1, 0)
#eliminar NA
pc_futuros_245 <- na.omit(pc_futuros_245)


# -------------------------------
# Formateo de datos SPP370
# -------------------------------

#unir excel del qgis, ir cambiando la ruta del ruta_qgis
ruta_qgis_370<- list.files(path = "C:/Users/smoya/Desktop/TFM/bio_qgis/ES_370", pattern = "*.xlsx", full.names = TRUE)
# Leer todos los archivos Excel y almacenarlos en una lista
lista_qgist_370 <- lapply(ruta_qgis_370, read_excel)
#Combinar las listas partiendo de la base que tienen el mismo id y coordendas
datos_combinados_spp370 <- lista_qgist_370 %>%
  reduce(function(x, y) full_join(x, y, by = c("id", "left", "top", "right", "bottom", "row_index", "col_index","NUMPOINTS")))

# Verifica el resultado
head(datos_combinados_spp370)

#Guardo el excel y hago las columnas de coordenas x e y
write_xlsx(datos_combinados_spp370, "C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp370.xlsx")

# Cargo archivo y escalo los datos
datos_370<-read_excel("C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp370.xlsx")

datos_370_acp<- datos_370 %>% select(-id,-left,-top,-right,-bottom,-centro_x,-centro_y,-row_index,-col_index,-NUMPOINTS)

pc_370<-scale(datos_370_acp)

# Asignar los mismos nombres de columna que los datos originales escalados
colnames(pc_370) <- colnames(datos_scaled)

# Proyectar los datos futuros sobre los componentes del ACP original
proyeccion_futuros_370 <- predict(acp, newdata = pc_370)

# Extraer los primeros 4 componentes principales
pc_futuros_370 <- as.data.frame(proyeccion_futuros_370$coord[, 1:4])

# Añadir la columna de presencia/ausencia
pc_futuros_370$presence <- datos_370$NUMPOINTS
# Añadir la columna de coordenadas originales
pc_futuros_370$centro_x <- datos_370$centro_x
pc_futuros_370$centro_y <- datos_370$centro_y
# Transformar la columna numero_puntos_total en una variable binaria en el nuevo DataFrame
pc_futuros_370$presence <- ifelse(pc_futuros_370$presence > 0, 1, 0)
#eliminar NA
pc_futuros_370 <- na.omit(pc_futuros_370)


# -------------------------------
# Formateo de datos SPP548
# -------------------------------
#unir excel del qgis, ir cambiando la ruta del ruta_qgis
ruta_qgis_548<- list.files(path = "C:/Users/smoya/Desktop/TFM/bio_qgis/ES_548", pattern = "*.xlsx", full.names = TRUE)
# Leer todos los archivos Excel y almacenarlos en una lista
lista_qgist_548 <- lapply(ruta_qgis_548, read_excel)
#Combinar las listas partiendo de la base que tienen el mismo id y coordendas
datos_combinados_spp548 <- lista_qgist_548 %>%
  reduce(function(x, y) full_join(x, y, by = c("id", "left", "top", "right", "bottom", "row_index", "col_index","NUMPOINTS")))

# Verifica el resultado
head(datos_combinados_spp548)

#Guardo el excel y hago las columnas de coordenas x e y
write_xlsx(datos_combinados_spp548, "C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp548.xlsx")

# Cargo archivo y escalo los datos
datos_548<-read_excel("C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp548.xlsx")

datos_548_acp<- datos_548 %>% select(-id,-left,-top,-right,-bottom,-centro_x,-centro_y,-row_index,-col_index,-NUMPOINTS)

pc_548<-scale(datos_548_acp)

# Asignar los mismos nombres de columna que los datos originales escalados
colnames(pc_548) <- colnames(datos_scaled)

# Proyectar los datos futuros sobre los componentes del ACP original
proyeccion_futuros_548 <- predict(acp, newdata = pc_548)

# Extraer los primeros 4 componentes principales
pc_futuros_548 <- as.data.frame(proyeccion_futuros_548$coord[, 1:4])

# Añadir la columna de presencia/ausencia
pc_futuros_548$presence <- datos_548$NUMPOINTS
# Añadir la columna de coordenadas originales
pc_futuros_548$centro_x <- datos_548$centro_x
pc_futuros_548$centro_y <- datos_548$centro_y
# Transformar la columna numero_puntos_total en una variable binaria en el nuevo DataFrame
pc_futuros_548$presence <- ifelse(pc_futuros_548$presence > 0, 1, 0)
#eliminar NA
pc_futuros_548 <- na.omit(pc_futuros_548)

# -------------------------------
# 1. Formateo de datos para biomod2
# -------------------------------

Biomod_presente <- BIOMOD_FormatingData(
  resp.var = pc_data$presence,
  expl.var = pc_data[, 1:4],  # Dim.1-Dim.4 de la PCA
  resp.xy = pc_data[, c("centro_x", "centro_y")],
  resp.name = "H. halys"
)

# -------------------------------
# 2. Configuración y entrenamiento de modelos
# -------------------------------

Biomod_model_presente <- BIOMOD_Modeling(
  Biomod_presente,
  models = c('GLM', 'RF', 'GBM'), 
  models.options = BIOMOD_ModelingOptions(),
  NbRunEval = 3,              # Repeticiones
  DataSplit = 70,             # 70% entrenamiento
  VarImport = 3,
  models.eval.meth = c('TSS', 'ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE
)

# -------------------------------
# 3. Evaluación de los modelos
# -------------------------------

Biomod_evalu_presente <- get_evaluations(Biomod_model_presente)
print(Biomod_evalu_presente)

TSS_scores <- Biomod_evalu_presente["TSS", , , ]
boxplot(TSS_scores, main="Distribución de TSS por modelo")

# -------------------------------
# 4. Proyección para el presente (con coordenadas)
# -------------------------------

Biomod_proyeccion_presente <- BIOMOD_Projection(
  modeling.output = Biomod_model_presente,
  new.env = pc_data[, 1:4],  # variables PCA
  proj.name = 'presente',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  build.clamping.mask = FALSE,
  xy = pc_data[, c("centro_x", "centro_y")]
)

# -------------------------------
# 5. Proyección para el futuro SPP126
# -------------------------------

Biomod_proyeccion_futuro_126 <- BIOMOD_Projection(
  modeling.output = Biomod_model_presente,
  new.env = pc_futuros_126[, 1:4],
  proj.name = "futuro_126",
  selected.models = "all",
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE,
  xy = pc_futuros_126[, c("centro_x", "centro_y")]
)
# -------------------------------
# 5.1. Proyección para el futuro SPP245
# -------------------------------

Biomod_proyeccion_futuro_245 <- BIOMOD_Projection(
  modeling.output = Biomod_model_presente,
  new.env = pc_futuros_245[, 1:4],
  proj.name = "futuro_245",
  selected.models = "all",
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE,
  xy = pc_futuros_245[, c("centro_x", "centro_y")]
)
# -------------------------------
# 5.2. Proyección para el futuro SPP370
# -------------------------------

Biomod_proyeccion_futuro_370 <- BIOMOD_Projection(
  modeling.output = Biomod_model_presente,
  new.env = pc_futuros_370[, 1:4],
  proj.name = "futuro_370",
  selected.models = "all",
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE,
  xy = pc_futuros_370[, c("centro_x", "centro_y")]
)
# -------------------------------
# 5.3. Proyección para el futuro SPP548
# -------------------------------

Biomod_proyeccion_futuro_548 <- BIOMOD_Projection(
  modeling.output = Biomod_model_presente,
  new.env = pc_futuros_548[, 1:4],
  proj.name = "futuro_548",
  selected.models = "all",
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE,
  xy = pc_futuros_548[, c("centro_x", "centro_y")]
)

# -------------------------------
# 6. Creación de modelos en conjunto (ensemble)
# -------------------------------

myBiomodEnsembleModel <- BIOMOD_EnsembleModeling(
  modeling.output = Biomod_model_presente,
  chosen.models = 'all',
  em.by = 'all',
  eval.metric = c('TSS', 'ROC'),
  eval.metric.quality.threshold = c(0.7, 0.8),  # Tenías una coma mal antes (0,8 -> 0.8)
  prob.mean = TRUE,
  prob.cv = TRUE,
  prob.ci = TRUE,
  prob.ci.alpha = 0.05,
  prob.median = TRUE,
  committee.averaging = TRUE,
  prob.mean.weight = TRUE,
  prob.mean.weight.decay = 'proportional'
)

Biomod_ensemble_presente <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEnsembleModel,
  projection.output = Biomod_proyeccion_presente,
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE
)

# Visualización
plot(Biomod_ensemble_presente)

# -------------------------------
# 7. Ensemble Forecasting para el futuro SPP126
# -------------------------------

Biomod_ensemble_futuro_126 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEnsembleModel,
  projection.output = Biomod_proyeccion_futuro_126,
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE
)
# -------------------------------
# 7.1. Ensemble Forecasting para el futuro SPP245
# -------------------------------

Biomod_ensemble_futuro_245 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEnsembleModel,
  projection.output = Biomod_proyeccion_futuro_245,
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE
)
# -------------------------------
# 7.2. Ensemble Forecasting para el futuro SPP370
# -------------------------------

Biomod_ensemble_futuro_370 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEnsembleModel,
  projection.output = Biomod_proyeccion_futuro_370,
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE
)
# -------------------------------
# 7.2. Ensemble Forecasting para el futuro SPP548
# -------------------------------

Biomod_ensemble_futuro_548 <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEnsembleModel,
  projection.output = Biomod_proyeccion_futuro_548,
  binary.meth = "TSS",
  compress = "xz",
  build.clamping.mask = FALSE
)
# -------------------------------
# 8. Visualización de proyecciones
# -------------------------------

plot(Biomod_proyeccion_presente)
plot(Biomod_proyeccion_futuro_126)
plot(Biomod_proyeccion_futuro_245)
plot(Biomod_proyeccion_futuro_370)
plot(Biomod_proyeccion_futuro_548)
plot(Biomod_ensemble_futuro_126)
plot(Biomod_ensemble_futuro_245)
plot(Biomod_ensemble_futuro_370)
plot(Biomod_ensemble_futuro_548)


get_evaluations(myBiomodEnsembleModel)

var_importancia <- get_variables_importance(Biomod_model_presente)
var_importancia


# Obtener todas las predicciones 
all_preds <- get_predictions(Biomod_model_presente)

# Verifica las dimensiones y contenido
str(all_preds)

par(mfrow = c(3, 3))  # 3x3 gráficos por página

modelos <- dimnames(all_preds)[[2]]
corridas <- dimnames(all_preds)[[3]]
obs <- get_formal_data(Biomod_model_presente, "resp.var")

for (mod in modelos) {
  for (run in corridas) {
    preds <- all_preds[, mod, run, 1]
    roc_obj <- roc(obs, preds)
    plot(roc_obj, main = paste("ROC -", mod, run))
  }
}

get_evaluations(myBiomodEnsembleModel)

