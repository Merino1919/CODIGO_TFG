#################################### LDA TFG #####################################################################

# Carga de paquetes y datos

library(tidyverse)
library(quanteda)
library(tidytext)
library(topicmodels)
library(stringr)
library(reshape2)
library(topicdoc)
library(ldatuning)
library(lda)

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


# Conversión a estructura de preprocesamiento

corpus <- corpus(datos, docid_field="id",text_field="cuerpo")
summary(corpus)

toks <- c("compañía","ante", "bajo", "cabe", "con", "contra", "de", "desde", "durante", "en", "entre", "hacia",
    "hasta", "mediante", "para", "por", "según", "sin", "sobre", "tras","millones","euros","año","años","parte","dos",
    "pasado","negocio", "empresa", "empresas", "si", "no","dias", "además", "aunque", "así", "después", "hace", "puede",
    "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")

dtm_sinSW <- corpus %>% tokens(remove_punct=TRUE,
                                      remove_numbers =TRUE,
                                      remove_symbols = TRUE,
                                      remove_url = TRUE) %>%
  tokens_remove(stopwords("es")) %>%
  tokens_tolower() %>%
  tokens_remove(toks) %>%
  dfm()

dtm_sinSW


# Estimación LDA

dtm_topicM <- convert(dtm_sinSW,to="topicmodels")
dtm_topicM

lda <- LDA(dtm_topicM,k=6,control = list(seed=123))
lda_post <- posterior(lda)

beta <- lda_post$terms
beta[1,1:10]

theta <- lda_post$topics
theta[1,]

# Interpretación de tópicos

terms(lda,10)
top5terminosPorTopico <- terms(lda, 5)
nombreTopicos <- apply(top5terminosPorTopico, 2, paste, collapse=".")
nombreTopicos

lda_top_terminos <- tidy(lda,matrix="beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

LDA1 <- lda_top_terminos %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE, width = 0.5) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered(name = "Términos") +
  theme_minimal() +
  labs(title = "Gráfico 4: Primera iteración LDA",
       caption = "Fuente: elaboración propia a partir de Gale One File News") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "black"),
    plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black"),
    axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave("LDA1.pdf", LDA1, width = 9, height = 8, dpi = 300)



# Interpretación de documentos (No sé si interesa hacerlo, debería ser un análisis por empresa o por año,
# es decir, analizar qué temas son más comunes por año o por empresa)


# Diagnóstico de tópicos

topic_diagnostics(lda,dtm_topicM,top_n_tokens = 5)

# Topic size: Número de tokens por tópico

# Mean_token_length: Número medio de caracteres para los top tokens del tópico

# Dist_from_corpus: distancia de la distribución  de tokens de un tópico con la distribución de tokens del
# corpus (distancia de Hellinger: medida de similitud entre dos distribuciones de probabilidad). Valor
# máximo es 1. Valores mayores indica mayor distancia/diferencia del tema con el corpus.

# Tf_df_dist: distancia entre la distribución de frecuencias de tokens en un tema y distribución
# document freq. (nota no normalizado). Valores más altos indican tópico incorpora términos que son
# poco frecuentes en la dist de documentos.

# Doc_prominence: Número de documentos únicos en los que aparece un tema.

# Topic_coherence: Medida de la frecuencia en que los top tokens en cada tema aparecen juntos en
# el mismo documento

# Topic_exclusivity: Medida del grado en que los top tokens en un tópico no lo son de otros tópicos
# (es decir cuan 'exclusivas' son las palabras top de un tópico/tema).


# Elección de la k óptima

# La elección del número de temas es clave en LDA
# Si el número de temas seleccionados es demasiado pequeño, el significado de
# cada tema será demasiado amplio; si el número de temas seleccionados es
# demasiado grande (sobreajuste) conducirá a temas inútiles o con demasiada similitud.

# Función perplexity en topicmodels
# La perplejidad se usa comúnmente para evaluar la calidad de los modelos
# bondad predictiva para la muestra del modelo probabilístico LDA estimado
# Comparable a AIC: menor perplejidad mejor ajuste

lda4 <- LDA(dtm_topicM,k=4,control = list(seed=123))
lda9 <- LDA(dtm_topicM,k=9,control = list(seed=123))
lda10 <- LDA(dtm_topicM,k=10,control = list(seed=123))
lda12 <- LDA(dtm_topicM,k=12,control = list(seed=123))
lda15 <- LDA(dtm_topicM,k=15,control = list(seed=123))
lda20 <- LDA(dtm_topicM,k=20,control = list(seed=123))

lapply(c(lda4,lda9,lda10,lda12,lda15,lda20),perplexity)

# Según la perplejidad, el número ideal de temas es 20

# Otra forma de hacerlo: FindTopicsNumber.


result <- FindTopicsNumber(
  dtm_topicM,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Según FindTopicsNumber el número ideal de temas es 17.


# Análisis de los tópicos con la k buena:

lda2 <- LDA(dtm_topicM,k=17,control = list(seed=123))
lda_post2 <- posterior(lda2)

beta <- lda_post2$terms
theta <- lda_post2$topics

attributes(lda_post2)

lda2_top_terminos <- tidy(lda2,matrix="beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 7) %>%
  ungroup() %>%
  arrange(topic, -beta)

x <- lda2_top_terminos %>%
  mutate(term = reorder_within(term, beta, topic))

x <- ggplot(lda2_top_terminos, aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE, width = 0.4) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +  # Cambiar el número de columnas según sea necesario
  scale_y_reordered() +
  theme_minimal() +
  labs(title = "Gráfico 6: Segunda iteración LDA con k óptima",
       caption = "Fuente: elaboración propia a partir de Gale One File News", y = NULL, x = "Beta") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5), color = "black"),
    plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10), color = "black"),
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    axis.text.y = element_text(size = 13))

# Guardamos el gráfico y le damos una resolución buena:
ggsave("G6.pdf", x, width = 20, height = 12, dpi = 300)


# Comparación final entre k:

comparacion1 <- topic_diagnostics(lda,dtm_topicM,top_n_tokens = 5)
comparacion2 <- topic_diagnostics(lda2,dtm_topicM,top_n_tokens = 5)

# Hacer 1 gráfico de comparación aquí con el doc_prominence y el topic size y toda la pesca


# PRIMERA PRUEBA DE COMPARACIÓN

ggplot() +
  geom_point(data = comparacion1, aes(x = topic_size, y = doc_prominence), color = 'blue', size = 3, alpha = 0.6) +
  geom_point(data = comparacion2, aes(x = topic_size, y = doc_prominence), color = 'red', size = 3, alpha = 0.6) +
  labs(title = 'Gráfico 7. Comparación de la Calidad de las Iteraciones de LDA',
       x = 'Tamaño del Tópico',
       y = 'Prominencia de Documentos',
      caption = "Fuente: elaboración propia a partir de Gale One File News") +
  scale_color_manual(values = c('blue', 'red')) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5), color = "black"),
    legend.position = "top") + annotate("text", x = 10, y = 12, label = "Iteracion 1")

annot
# SEGUNDA PRUEBA DE COMPARACIÓN

datos <- rbind(comparacion1, comparacion2)

# Derretir el dataframe para facilitar la creación del gráfico
datos_melted <- melt(datos, id.vars = c("topic_num"), measure.vars = c("topic_size", "mean_token_length", "dist_from_corpus", "tf_df_dist", "doc_prominence", "topic_coherence", "topic_exclusivity"))

# Crear el gráfico
ggplot(datos_melted, aes(x = topic_num, y = value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Comparación de la calidad de las iteraciones LDA", x = "Número de Tópico", y = "Valor de Métrica", color = "Iteración") +
  theme_minimal()

# Datos de la primera iteración
topic_num1 <- c(1, 2, 3, 4, 5, 6, 7)
topic_size1 <- c(7716.1, 6634.967, 4847.854, 6333.94, 4944.97, 6008.847, 5372.322)
doc_prominence1 <- c(412, 660, 544, 409, 741, 611, 489)

# Datos de la segunda iteración
topic_num2 <- 1:17
topic_size2 <- c(3058.732, 3276.299, 1955.056, 2657.898, 2260.744, 2125.603, 2439.526, 2285.088,
                 3395.887, 2128.702, 1872.69, 1509.409, 2447.576, 2423.687, 2883.614, 2542.849, 2595.641)
doc_prominence2 <- c(152, 365, 336, 183, 156, 360, 274, 189, 326, 347, 224, 206, 151, 167, 205, 203, 245)

# Crear dataframes
df_iter1 <- data.frame(topic_num = topic_num1, topic_size = topic_size1, doc_prominence = doc_prominence1)
df_iter2 <- data.frame(topic_num = topic_num2, topic_size = topic_size2, doc_prominence = doc_prominence2)

# Añadir columna para la leyenda
df_iter1$iteration <- 'Iteración 1 (6 tópicos)'
df_iter2$iteration <- 'Iteración 2 (17 tópicos)'

# Combinar dataframes
df_combined <- rbind(df_iter1, df_iter2)

# Crear el gráfico
library(ggplot2)

ggplot(df_combined, aes(x = topic_size, y = doc_prominence, color = iteration)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = 'Gráfico 7. Comparación de la Calidad de las Iteraciones de LDA',
       x = 'Tamaño del Tópico',
       y = 'Prominencia de Documentos',
       caption = "Fuente: elaboración propia a partir de Gale One File News",
      color = NULL) +
  theme_minimal() +
  xlim(0, 8000) +
  scale_color_manual(values = c('Iteración 1 (6 tópicos)' = 'blue', 'Iteración 2 (17 tópicos)' = 'red')) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5), color = "black"),
      legend.position = "top")






