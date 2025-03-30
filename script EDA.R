# paquetes
library(readxl)
library(purrr) #para la función reduce (juntar los excel qgis)
library(writexl)
library(dplyr)
library(ggplot2)
library(moments)
library(nortest)
library(FactoMineR) # para ACP
library(factoextra)

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

####### Parte 2######
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
print(resumen_largo)

# Crear la tabla con el número de puntos y la cantidad de veces que aparece
tabla_frecuencia <- as.data.frame(table(base_datos_analisis$numero_puntos_total))
colnames(tabla_frecuencia) <- c("numero_puntos_total", "frecuencia")

# Filtrar solo los valores presentes en el data frame
tabla_frecuencia <- tabla_frecuencia[tabla_frecuencia$frecuencia > 0, ]

# Mostrar la tabla
print(tabla_frecuencia)

# Lista para almacenar los gráficos de histogramas
histograms <- list()

# Lista para almacenar los gráficos de boxplots
boxplots <- list()

# Lista para almacenar los gráficos de dispersión
scatterplots <- list()

# Histograma para todas las columnas numéricas
for (col in names(base_datos_analisis)) {
  if (is.numeric(base_datos_analisis[[col]])) {
    p <- ggplot(base_datos_analisis, aes(x = .data[[col]])) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      labs(title = paste("Histograma de", col), x = col, y = "Frecuencia")
    histograms[[col]] <- p
  }
}

# Boxplot para cada variable numérica vs numero_puntos_total
for (col in names(base_datos_analisis)) {
  if (col != "numero_puntos_total" && is.numeric(base_datos_analisis[[col]])) {
    p <- ggplot(base_datos_analisis, aes(x = .data[[col]], y = as.factor(numero_puntos_total))) +
      geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
      labs(title = paste("Boxplot de", col, "vs numero_puntos_total"), x = col, y = "Número de Puntos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    boxplots[[col]] <- p
  }
}
# Diagrama de puntos para análisis bivariado
for (col in names(base_datos_analisis)) {
  if (is.numeric(base_datos_analisis[[col]]) && col != "numero_puntos_total") {
    p <- ggplot(base_datos_analisis, aes(x = .data[[col]], y = numero_puntos_total)) +
      geom_point(color = "blue") +
      labs(title = paste("Dispersión de numero_puntos_total vs", col), x = col, y = "numero_puntos_total") +
      theme_minimal()
    scatterplots[[col]] <- p
  }
}

# Boxplot para cada variable numérica vs numero_puntos_total
for (col in names(base_datos_analisis)) {
  if (col != "numero_puntos_total" && is.numeric(base_datos_analisis[[col]])) {
    p <- ggplot(base_datos_analisis, aes(x = as.factor(numero_puntos_total), y = .data[[col]])) +
      geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
      labs(title = paste("Boxplot de", col, "vs numero_puntos_total"), x = "Número de Puntos", y = col) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    boxplots[[col]] <- p
  }
}

# Mostrar todos los histogramas juntos
do.call(grid.arrange, c(histograms, ncol = 2))

# Mostrar todos los boxplots juntos
do.call(grid.arrange, c(boxplots, ncol = 2))

# Mostrar todos los diagramas de puntos juntos
do.call(grid.arrange, c(scatterplots, ncol = 2))


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

# Crear boxplots para cada variable numérica vs categoria_puntos en 'base_datos_grupo'
for (col in names(base_datos_grupo)) {
  if (col != "numero_puntos_total" && col != "categoria_puntos" && is.numeric(base_datos_grupo[[col]])) {  
    plot <- ggplot(base_datos_grupo, aes(x = categoria_puntos, y = .data[[col]])) +
      geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
      labs(title = paste("Boxplot de", col, "vs Categoría de Número de Puntos"),
           x = "Categoría de Número de Puntos", y = col) +
      theme_minimal()
    
    boxplot_grupo[[col]] <- plot
  }
}

# Mostrar todos los boxplots juntos
do.call(grid.arrange, c(boxplot_grupo, ncol = 2))

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
# Analsis de componentes principales
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



# Script para unir los excel que genera el qgis las 19 variables ambientales de los diferentes sppp
#unir excel del qgis, ir cambiando la ruta del ruta_qgis
ruta_qgis<- list.files(path = "C:/Users/smoya/Desktop/TFM/bio_qgis/ES_126", pattern = "*.xlsx", full.names = TRUE)
# Leer todos los archivos Excel y almacenarlos en una lista
lista_qgist <- lapply(ruta_qgis, read_excel)
#Combinar las listas partiendo de la base que tienen el mismo id y coordendas
datos_combinados_spp126 <- lista_qgist %>%
  reduce(function(x, y) full_join(x, y, by = c("id", "left", "top", "right", "bottom", "row_index", "col_index","NUMPOINTS")))

# Verifica el resultado
head(datos_combinados_spp126)

#Guardo el excel y hago las columnas de coordenas x e y
write_xlsx(datos_combinados_spp126, "C:/Users/smoya/Desktop/TFM/bio_qgis/resultado_combinado_ssp126.xlsx")





# Formatear los datos para biomod2
Biomod_presente <- BIOMOD_FormatingData(
  resp.var = pc_data$presence,
  expl.var = pc_data[, 1:4],
  resp.xy = pc_data[, c("centro_x", "centro_y")],
  resp.name = "H. halys"
)
# Configuración de los modelos
Biomod_model_presente <- BIOMOD_Modeling(
  Biomod_presente,
  models = c('GLM', 'RF', 'GBM'), 
  models.options = BIOMOD_ModelingOptions(),
  NbRunEval = 3, # 3 runs o ciclos
  DataSplit = 70, # 70% entrenamiento 30%test
  VarImport = 3,
  models.eval.meth = c('TSS', 'ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE
)
# Evaluación de los modelos
Biomod_evalu_presente <- get_evaluations(Biomod_model_presente)
# Visualizar los resultados de la evaluación
print(Biomod_evalu_presente)  # Ver resultados en consola
TSS_scores <- Biomod_evalu_presentel["TSS", , , ]  # Extraer TSS por modelo
boxplot(TSS_scores, main="Distribución de TSS por modelo")

# Proyección de los modelos
Biomod_proyeccion <- BIOMOD_Projection(
  modeling.output = Biomod_model_presente,
  new.env = pc_data[, 1:4],
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  build.clamping.mask = FALSE,
  xy = pc_data[, c("centro_x", "centro_y")]
)

# Para añadir las diferentes SPP,
future_env <- stack("ruta_a_datos_futuros.tif")
myBiomodFutureProjection <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = future_env,
  proj.name = 'future', #Aqui cambiar al spp que corresponda
  selected.models = 'all'
)
# Creación de modelos en conjunto (ensemble models)
myBiomodEnsembleModel <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',
  em.by = 'all',
  eval.metric = c('TSS', 'ROC'),
  eval.metric.quality.threshold = c(0.7, 0,8),
  prob.mean = TRUE,
  prob.cv = TRUE,
  prob.ci = TRUE,
  prob.ci.alpha = 0.05,
  prob.median = TRUE,
  committee.averaging = TRUE,
  prob.mean.weight = TRUE,
  prob.mean.weight.decay = 'proportional'
)

# Proyección de los modelos en conjunto, aqui tmb se pude comparar escenarios actuales y futuros
myBiomodEnsembleProjection <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEnsembleModel,
  projection.output = myBiomodProjection,
  binary.meth = 'TSS',
  compress = 'xz',
  build.clamping.mask = FALSE
)

# Proyección de los modelos con coordenadas XY
myBiomodProjection <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = expl_var,
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  build.clamping.mask = FALSE,
  xy = nuevo_base_datos[, c("centro_x", "centro_y")] # Proporcionar las coordenadas XY
)

# Visualizar las proyecciones
plot(Biomod_proyeccion)





