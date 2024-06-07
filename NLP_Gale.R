############################ NLP PARA CREACIÓN ÍNDICE INVERSIÓN ######################################

# LIBRERIAS

library(tidyverse)
library(quanteda)
library(tm)
library(stringr)
library(tidytext)
library(readtext)


# CARGA Y UNIFICACIÓN DE DATAFRAMES --------------------------------------------------------------------------

load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Iberdrola.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/AENA.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Caixabank.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Santander.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/BBVA.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Grifols.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Ferrovial.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Repsol.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Endesa.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Inditex.rds")

AENA_df$Empresa <- "AENA"
Santander_df$Empresa <- "Santander"
BBVA_df$Empresa <- "BBVA"
Caixabank_df$Empresa <- "Caixabank"
Grifols_df$Empresa <- "Grifols"
Ferrovial_df$Empresa <- "Ferrovial"
Iberdrola_df$Empresa <- "Iberdrola"
Repsol_df$Empresa <- "Repsol"
Endesa_df$Empresa <- "Endesa"
Inditex_df$Empresa <- "Inditex"

lista_dataframes <- list(AENA_df, BBVA_df, Santander_df, Caixabank_df, Endesa_df, Ferrovial_df,
    Grifols_df, Inditex_df, Repsol_df, Iberdrola_df)

datos <- do.call(rbind, lista_dataframes)
save(datos, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/datos.rds")

# CONSTRUCCIÓN DEL CORPUS -----------------------------------------------------------------------------------

load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/datos.rds")
datos$id <- 1:2740
datos$anyo <- substr(datos$fechas, nchar(datos$fechas) - 3, nchar(datos$fechas))
datos <- datos[,-2]
datos$cuerpo <- substring(datos$cuerpo, regexpr("\n", datos$cuerpo) + 1)
datos$cuerpo <- gsub("\n", "", datos$cuerpo)
datos <- datos %>%
  mutate(sector = case_when(
    Empresa %in% c("Iberdrola", "Repsol", "Endesa") ~ "Sector energético",
    Empresa %in% c("BBVA", "Caixabank", "Santander") ~ "Sector bancario",
    Empresa == "Inditex" ~ "Sector textil",
    Empresa == "AENA" ~ "Transporte",
    Empresa == "Ferrovial" ~ "Construcción",
    Empresa == "Grifols" ~ "Sector farmacéutico",
    TRUE ~ "Otro"
  ))
corpus <- corpus(datos, docid_field="id",text_field="cuerpo")




# ANÁLISIS DESCRIPTIVO---------------------------------------------------------------------------------------

toks <- c("compañía","ante", "bajo", "cabe", "con", "contra", "de", "desde", "durante", "en", "entre", "hacia",
    "hasta", "mediante", "para", "por", "según", "sin", "sobre", "tras","millones","euros","año","años","parte","dos",
    "pasado","negocio", "empresa", "empresas", "si", "no","dias", "además", "aunque", "así", "después", "hace", "puede",
    "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "españa")

dtm_sinSW <- corpus %>% tokens(remove_punct=TRUE,
                                      remove_numbers =TRUE,
                                      remove_symbols = TRUE,
                                      remove_url = TRUE) %>%
  tokens_remove(stopwords("es")) %>%
  tokens_remove(toks) %>%
  tokens_tolower() %>%
  dfm()

dtm_sinSW


# G1: Palabras más frecuentes por año

textfreq <- textstat_frequency(dtm_sinSW,n=8,groups = anyo) # agrupamos por año (metadatos en corpus)
ggplot(data = textfreq, aes(x = factor(nrow(textfreq):1), y = frequency)) +
  geom_bar(stat = "identity",fill="darkred") +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_discrete(breaks = nrow(textfreq):1,
                   labels = textfreq$feature) +
  labs(x = NULL, y = "")+
  ggtitle(label='Gráfico 1: Palabras más frecuentes por año', subtitle='Corpus noticias financieras') +
  labs(caption='Fuente: elaboración propia a partir de Gale One File News')+
  theme_minimal() +
    theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20), color = "black"),
    plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text.x = element_text(size = 11, face = "bold"),
    strip.background = element_rect(color = "black", fill = "white"))


# G2: Palabras más repetidas por sector ##############################################################################

textfreq <- textstat_frequency(dtm_sinSW,n=10,groups = sector)
ggplot(data = textfreq, aes(x = factor(nrow(textfreq):1), y = frequency)) +
  geom_bar(stat = "identity", fill = "darkred") +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_discrete(breaks = nrow(textfreq):1,
                   labels = textfreq$feature) +
  labs(x = NULL, y = "") +
  ggtitle(label = 'Gráfico 2. Palabras más frecuentes por sector',
          subtitle = 'Corpus de noticias financieras') +
  theme_minimal() +  # Estilo minimalista
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
        strip.text.x = element_text(size = 11, face = "bold"),
        strip.background = element_rect(color = "black", fill = "white"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 12, margin = margin(r = 10))) +
  labs(caption = 'Fuente: elaboración propia a partir de Gale One File News')


# G3: Matriz coocurrencia de terminos ##############################################################################

dtm_trim <- dfm_trim(dtm_sinSW, min_docfreq = 700)
topfeatures(dtm_trim)

fcm_delta <- fcm(dtm_trim)
fcm_delta

textplot_network(fcm_delta,
                 fontsize = 6,          # Reducir el tamaño de la fuente
                   edge_color = "steelblue", # Cambiar el color de los bordes
                   node_color = "black",   # Cambiar el color de los nodos
                   min_distance = 0.1,
    edge_alpha = 0.3) +
    labs(title = "Gráfico 3: Red de términos principales",
       subtitle = "Corpus noticias financieras",
       caption = "Fuente: elaboración propia a partir de Gale One File News") +
    theme_minimal() +
    theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20), color = "black"),
    plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black"),
    axis.title.x = element_blank(), axis.title.y = element_blank())


