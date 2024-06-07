# LIBRERÍAS Y DATOS

library(tidytext)
library(dplyr)
library(ggplot2)
library(syuzhet)
library(tidyr)
library(reactable)
library(readxl)
library(lubridate)

load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/datos.rds")
datos$id <- 1:2740
datos$anyo <- substr(datos$fechas, nchar(datos$fechas) - 3, nchar(datos$fechas))
datos <- datos[,-2]
datos$cuerpo <- substring(datos$cuerpo, regexpr("\n", datos$cuerpo) + 1)
datos$cuerpo <- gsub("\n", "", datos$cuerpo)


# COMPONENTE ECONÓMICA ############################################################################################

Indice_parte_economica <- read_excel("Datos/Indice_parte_economica.xlsx")
str(Indice_parte_economica)
resultados_economica <- numeric(nrow(Indice_parte_economica))

# DESCRIPTIVO ###################################################################################################

# Crear un dataframe largo para facilitar la visualización
options(scipen = 95)
df_long1 <- reshape2::melt(Indice_parte_economica, measure.vars=c("ROE_2022", "ROE_2023", "ROA_2022", "ROA_2023","Margen_beneficio_2022", "Margen_beneficio_2023"))
df_long1$año <- gsub("\\D", "", df_long1$variable)
df_long2 <- reshape2::melt(Indice_parte_economica, measure.vars=c("Activos_totales_2022", "Activos_totales_2023"))
df_long2 <- df_long2[,-c(2:7)]
df_long2$año <- substr(df_long2$variable, 17, 22)

# Identificar los outliers
outliers <- df_long1 %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(value > quantile(value, 0.75) + 1.5 * IQR(value) | value < quantile(value, 0.25) - 1.5 * IQR(value), as.character(Empresa), NA))

# Graficar los boxplots juntos con etiquetas de outliers
boxplot <- ggplot(outliers, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(aes(group = variable), color = "black", alpha = 0.5, width = 0.5) +  # Modificar el ancho de las cajas
  geom_text(data = outliers[!is.na(outliers$outlier), ], aes(label = outlier), vjust = -1.5, hjust = 0.5, size = 3) +  # Agregar etiquetas de outliers
  scale_fill_viridis_d() +  # Usar la paleta de colores viridis
  labs(title = "Gráfico 8. Distribución de variables económicas",
       x = NULL, y = NULL, caption = "Fuente: elaboración propia a partir de Orbis") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, hjust = 1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "black"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20), color = "black"),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black")) +
    coord_flip()



df_long2 <- df_long2 %>% arrange(variable, desc(value))

barras <- ggplot(df_long2, aes(x=reorder(Empresa,value), y= value ,fill=variable)) +
  geom_bar(stat = "identity", color="black", alpha=0.5, width=0.5) +
  scale_fill_viridis_d() +
  scale_y_log10() +  # Aplicar escala logarítmica al eje y
  labs(title="Gráfico 9. Distribución de Activos por Empresa",
       subtitle = "Expresado en miles de euros",
       x=NULL, y="Logaritmo de Activos", caption="Fuente: elaboración propia a partir de Orbis") +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "black"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20), color = "black"),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas del eje x
        strip.text.x = element_text(size = 11, face = "bold"),
        strip.background = element_rect(color = "black", fill = "white")) +
  facet_wrap(~ año, scales = "free") +
  coord_flip()

# Normalización de datos: (Valor- media) / sd. ##################################################################

# PRIMERA FORMA DE NORMALIZACIÓN - Creo que me gusta más esta si pongo una restricción de solo valores positivos.

for(i in names(Indice_parte_economica)[-1]) {
    Indice_parte_economica[[i]] <- (Indice_parte_economica[[i]] - median(Indice_parte_economica[[i]])) / sd(Indice_parte_economica[[i]])
}

# Lo hago con la mediana ya que es menos sensible a valores atípicos.

for(i in 1:nrow(Indice_parte_economica)) {
    resultados_economica[i] <- sum(Indice_parte_economica[i, -1]) / 8
}

# Como hay resultados negativos reescalamos sumando el valor absoluto más negativo a cada empresa.
minimo <- min(resultados_economica)

for (i in 1:length(resultados_economica)) {
    resultados_economica[i] <- abs(minimo) + resultados_economica[i]

}

resultados_economica

# Utilizo el valor mayor en resultados_economica como divisor para que los valores resultantes sean menores que 3.
Componente_economica <- (3*resultados_economica)/max(resultados_economica)
Componente1 <- data.frame(Indice_parte_economica$Empresa,Componente_economica)

# COMPONENTE DE PERCEPCIÓN #######################################################################################

load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/datos.rds")
datos$id <- 1:2740
datos$anyo <- substr(datos$fechas, nchar(datos$fechas) - 3, nchar(datos$fechas))
datos <- datos[,-2]
datos$cuerpo <- substring(datos$cuerpo, regexpr("\n", datos$cuerpo) + 1)
datos$cuerpo <- gsub("\n", "", datos$cuerpo)


datos <- tibble(datos)
datos1 <- datos %>% unnest_tokens(word,cuerpo) %>% tibble()
Tokens_por_empresa <- datos1 %>% group_by(Empresa) %>% summarise(total = n())

# Diccionario Loughran ##########################################################################################

# Cargo el diccionario filtrado

diccionario <- read_excel("Diccionarios/Loughran_Macdonald_Definitivo.xlsx")
diccionario$Palabra <- tolower(diccionario$Palabra)
diccionario <- diccionario %>% distinct()
colnames(diccionario) <- c("word", "Sentimiento")

# Lo junto con los datos
datos2 <- datos1 %>%
  inner_join(diccionario, by = "word")

datos2$Valor <- ifelse(datos2$Sentimiento == "Negativo", -1,1)

datos2 %>% group_by(Sentimiento) %>% summarise(n())

# Calculo el numero de sentimientos positivos y negativos que hay en el dataframe datos2

Polaridad_absoluta_por_empresa <- datos2 %>%
  count(Id = id, Titulo = titulo,Empresa,Sentimiento) %>%
  pivot_wider(names_from = Sentimiento,
              values_from = n,
              values_fill = 0) %>%
  mutate(Polaridad_absoluta = Positivo - Negativo)

Polaridad_empresa <- Polaridad_absoluta_por_empresa %>% group_by(Empresa) %>% summarise(Polaridad = sum(Polaridad_absoluta))

Porcentaje_polaridad_empresa2 <- Polaridad_empresa$Polaridad/Tokens_por_empresa$total
Empresas <- c("AENA", "BBVA", "CAIXABANK", "ENDESA", "FERROVIAL", "GRIFOLS", "IBERDROLA", "INDITEX", "REPSOL", "BANCO SANTANDER")
Indice_percepcion <- data.frame(Empresas, Porcentaje_polaridad_empresa2)


resultados_percepcion <- numeric(nrow(Indice_percepcion))

for(i in names(Indice_percepcion)[-1]) {
    Indice_percepcion[[i]] <- (Indice_percepcion[[i]] - median(Indice_percepcion[[i]])) / sd(Indice_percepcion[[i]])
}

resultados_percepcion <- Indice_percepcion$Porcentaje_polaridad_empresa

# Como hay resultados negativos reescalamos sumando el valor absoluto más negativo a cada empresa.
minimo <- min(resultados_percepcion)

for (i in 1:length(resultados_percepcion)) {
    resultados_percepcion[i] <- abs(minimo) + resultados_percepcion[i]

}


resultados_percepcion

# Utilizo el valor mayor en resultados_economica como divisor para que los valores resultantes sean menores que 7.
Componente_percepcion <- (7*resultados_percepcion)/max(resultados_percepcion)
Componente2 <- data.frame(Empresas,Componente_percepcion)
Componente2



# GRÁFICOS ANÁLISIS DE SENTIMIENTOS

palabras <- datos2 %>%
  count(word, Sentimiento, sort = TRUE) %>%
  ungroup() %>%
  group_by(Sentimiento) %>%
  slice_max(n, n = 8) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

palabras$Sentimiento <- toupper(palabras$Sentimiento)

ggplot(palabras, aes(x = reorder(word,n), y = n)) +
  geom_point(color = "#2E86C1", size = 5) +
  geom_segment(aes(xend = reorder(word,n), yend = 0), color = "grey", alpha = 0.5, size = 1.5) +
  coord_flip() +
  labs(title = "Gráfico 10. Términos que más contribuyen al sentimiento",x = NULL, y = "Frecuencia",
      caption = "Fuente: elaboración propia a partir de Gale One File News") +
    facet_wrap(~Sentimiento, scales = "free") +
    theme_minimal() +
     theme(legend.position="none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "black"),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas del eje x
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.background = element_rect(color = "black", fill = "white"))

str(datos_comparar)

Total_sentimientos_empresa <- datos2 %>% group_by(Empresa, Sentimiento, anyo) %>% summarise(total = n())
Total_sentimientos_empresa$Sentimiento <- toupper(Total_sentimientos_empresa$Sentimiento)



ggplot(Total_sentimientos_empresa, aes(x = reorder(Empresa, total), y = total, fill = Sentimiento)) +
  geom_col(position = "dodge", color = "black", width = 0.7) +
  scale_fill_manual(values = c("NEGATIVO" = "#FF5733", "POSITIVO" = "#33FF57"),
                    labels = c("Negativo", "Positivo")) +
  labs(title = "Gráfico 11. Total de sentimientos por empresa",x = NULL, y = NULL,
      caption = "Fuente: elaboración propia a partir de Gale One File News")  +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "black"),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black"),  # Rotar las etiquetas del eje x
        strip.text.x = element_text(size = 10, face = "bold")) +
  guides(fill = guide_legend(reverse = TRUE)) + coord_flip()


ggplot(Total_sentimientos_empresa, aes(x = reorder(Empresa, total), y = total, fill = Sentimiento)) +
  geom_col(position = "dodge", color = "black", width = 0.7) +
  scale_fill_manual(values = c("NEGATIVO" = "#FF5733", "POSITIVO" = "#33FF57"),
                    labels = c("Negativo", "Positivo")) +
  labs(title = "Gráfico 12. Evolución del sentimiento por empresa",x = NULL, y = NULL,
      caption = "Fuente: elaboración propia a partir de Gale One File News")  +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "black"),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black"),  # Rotar las etiquetas del eje x
        strip.text.x = element_text(size = 10, face = "bold")) +
  guides(fill = guide_legend(reverse = TRUE)) + coord_flip() + facet_wrap(~ anyo, scales = "free")



Polaridad_absoluta_por_anyo <- datos2 %>%
  count(Id = id, Titulo = titulo,Sentimiento,fechas, Empresa) %>%
  pivot_wider(names_from = Sentimiento,
              values_from = n,
              values_fill = 0) %>%
  mutate(Polaridad_absoluta = Positivo - Negativo)

Polaridad_absoluta_por_anyo$anyo <- substr(Polaridad_absoluta_por_anyo$fechas, nchar(Polaridad_absoluta_por_anyo$fechas) - 3, nchar(Polaridad_absoluta_por_anyo$fechas))
Polaridad_absoluta_por_anyo$dia_mes <- substr(Polaridad_absoluta_por_anyo$fechas, nchar(Polaridad_absoluta_por_anyo$fechas) - 7, nchar(Polaridad_absoluta_por_anyo$fechas)-6)
Polaridad_absoluta_por_anyo$mes <- substr(Polaridad_absoluta_por_anyo$fechas, 1,4)
Polaridad_absoluta_por_anyo$Date_mensual <- substr(Polaridad_absoluta_por_anyo$fechas, 1,8)
Polaridad_absoluta_por_anyo$Date_mensual  <- gsub(",", "", Polaridad_absoluta_por_anyo$Date_mensual)

Polaridad_absoluta_por_anyo <- Polaridad_absoluta_por_anyo %>%
  mutate(Date_diario = dmy(paste(dia_mes, mes, anyo, sep = " ")))

# Crear el gráfico mejorado
ggplot(Polaridad_absoluta_por_anyo, aes(x = Date_diario, y = Polaridad_absoluta)) +
  geom_line(color = "steelblue", size = 1) +  # Línea azul
  geom_point(color = "darkred", size = 1) +  # Puntos rojos
  labs(title = "Evolución Temporal de la Polaridad",
       subtitle = "Años 2022, 2023 y 2024",
       x = "Fecha",
       y = "Polaridad Absoluta") +
  theme_minimal()

# UNIFICACIÓN DE RESULTADOS

colnames(Componente1) <- c("Empresas", "Componente_económica")
resultados <- left_join(Componente1, Componente2, by = "Empresas")
resultados$Indice_recomendación <- resultados$Componente_económica + resultados$Componente_percepcion
str(resultados)

resultados$Indice_recomendación <- round(resultados$Indice_recomendación,2)
resultados$Componente_económica <- round(resultados$Componente_económica,2)
resultados$Componente_percepcion <-  round(resultados$Componente_percepcion,2)

# TABLA RESULTADOS DEL ÍNDICE

library(kableExtra)

tabla <- resultados %>%
  kable("html", caption = "Tabla 4") %>%
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c(" ", "Componentes" = 2, "Índice" = 1)) %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1) %>%
    add_footnote("Fuente: elaboración propia a partir de Orbis y Gale One File News")

# Mostrar la tabla
tabla



