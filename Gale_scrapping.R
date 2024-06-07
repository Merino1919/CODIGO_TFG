################### GALE SCRAPPING ###############################################################

## librerias
library(rvest)
library(readr)
library(jsonlite)
library(dplyr)

# IBERDROLA -----------------------------------------------------------------------------------------------
Iberdrola_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

## creacion parametro para bucle
# Crear el vector de números
numeros <- seq(1, 444, by = 20)
# Crear el dataframe
df <- data.frame(Numero = numeros)

for (i in 1:23) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Economista%2C+Spain%22+Or+%22El+Pais%2C+Spain%22+Or+%22El+Mundo%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7EIberdrola%7E%7ERB%7E366+5&searchId=R1&searchType=AdvancedSearchForm&currentPosition=", parametro, "&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(paste0(i, " de 23"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  Iberdrola_df <- bind_rows(Iberdrola_df, df_temporal)
}


## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:444) {
  url_1 <- Iberdrola_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 444"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}

Iberdrola_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(Iberdrola_df$cuerpo))
Iberdrola_df <- subset(Iberdrola_df, !(1:nrow(Iberdrola_df) %in% indices_duplicados))
length(unique(Iberdrola_df$cuerpo))
save(Iberdrola_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Iberdrola.rds")





# Inditex -------------------------------------------------------------------------------------------------

Inditex_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 251, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:13) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Pais%2C+Spain%22+Or+%22El+Economista%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7Einditex%7E%7ERB%7E366+5&searchId=R2&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(paste0(i, " de 13"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  Inditex_df <- bind_rows(Inditex_df, df_temporal)
}


## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:251) {
  url_1 <- Inditex_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 251"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}


Inditex_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(Inditex_df$cuerpo))
Inditex_df <- subset(Inditex_df, !(1:nrow(Inditex_df) %in% indices_duplicados))
length(unique(Inditex_df$cuerpo))
save(Inditex_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Inditex.rds")







# BBVA ----------------------------------------------------------------------------------------------------

BBVA_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 351, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:18) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Economista%2C+Spain%22+Or+%22El+Pais%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7EBBVA%7E%7ERB%7E366+5&searchId=R6&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND"
)
  print(paste0(i, " de 18"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  BBVA_df <- bind_rows(BBVA_df, df_temporal)
}


## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:351) {
  url_1 <- BBVA_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 351"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}


BBVA_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(BBVA_df$cuerpo))
BBVA_df <- subset(BBVA_df, !(1:nrow(BBVA_df) %in% indices_duplicados))
length(unique(BBVA_df$cuerpo))
save(BBVA_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/BBVA.rds")










# AENA ----------------------------------------------------------------------------------------------------

AENA_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 153, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:8) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Pais%2C+Spain%22+Or+%22El+Economista%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7EAena%7E%7ERB%7E366+5&searchId=R2&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(i)
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  AENA_df <- bind_rows(AENA_df, df_temporal)
}

unique(AENA_df$titulo)

## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:153) {
  url_1 <- AENA_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 153"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}

AENA_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(AENA_df$cuerpo))
AENA_df <- subset(AENA_df, !(1:nrow(AENA_df) %in% indices_duplicados))
length(unique(AENA_df$cuerpo))
save(AENA_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/AENA.rds")









# FERROVIAL -----------------------------------------------------------------------------------------------

Ferrovial_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 299, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:15) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Economista%2C+Spain%22+Or+%22El+Pais%2C+Spain%22+Or+%22El+Mundo%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7EFerrovial%7E%7ERB%7E366+5&searchId=R18&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(paste0(i, " de 15"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  Ferrovial_df <- bind_rows(Ferrovial_df, df_temporal)
}


## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:299) {
  url_1 <- Ferrovial_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 299"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}

Ferrovial_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(Ferrovial_df$cuerpo))
Ferrovial_df <- subset(Ferrovial_df, !(1:nrow(Ferrovial_df) %in% indices_duplicados))
length(unique(Ferrovial_df$cuerpo))
save(Ferrovial_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Ferrovial.rds")








# REPSOL --------------------------------------------------------------------------------------------------

Repsol_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 308, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:16) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Pais%2C+Spain%22+Or+%22El+Economista%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7ERepsol%7E%7ERB%7E366+5&searchId=R2&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(paste0(i, " de 16"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  Repsol_df <- bind_rows(Repsol_df, df_temporal)
}

## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:308) {
  url_1 <- Repsol_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 308"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}

Repsol_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(Repsol_df$cuerpo))
Repsol_df <- subset(Repsol_df, !(1:nrow(Repsol_df) %in% indices_duplicados))
length(unique(Repsol_df$cuerpo))
save(Repsol_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Repsol.rds")





# Endesa --------------------------------------------------------------------------------------------

Endesa_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 177, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:9) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Economista%2C+Spain%22+Or+%22El+Pais%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7Eendesa%7E%7ERB%7E366+5&searchId=R2&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(paste0(i, " de 9"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  Endesa_df <- bind_rows(Endesa_df, df_temporal)
}


## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:177) {
  url_1 <- Endesa_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 177"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}

Endesa_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(Endesa_df$cuerpo))
Endesa_df <- subset(Endesa_df, !(1:nrow(Endesa_df) %in% indices_duplicados))
length(unique(Endesa_df$cuerpo))
save(Endesa_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Endesa.rds")








# Grifols ---------------------------------------------------------------------------

Grifols_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 217, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:11) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Economista%2C+Spain%22+Or+%22El+Pais%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7E%22grifols%22%7E%7ERB%7E366+5&searchId=R6&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(paste0(i, " de 11"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  Grifols_df <- bind_rows(Grifols_df, df_temporal)
}


## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:217) {
  url_1 <- Grifols_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 217"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}

Grifols_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(Grifols_df$cuerpo))
Grifols_df <- subset(Grifols_df, !(1:nrow(Grifols_df) %in% indices_duplicados))
length(unique(Grifols_df$cuerpo))
save(Grifols_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Grifols.rds")








# Caixabank ---------------------------------------------------------------------------

Caixabank_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 244, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:13) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Economista%2C+Spain%22+Or+%22El+Pais%2C+Spain%22&searchResultsType=SingleTab&qt=TI%7ECaixabank%7E%7ERB%7E366+5&searchId=R9&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(paste0(i, " de 13"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  Caixabank_df <- bind_rows(Caixabank_df, df_temporal)
}


## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:244) {
  url_1 <- Caixabank_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 244"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}

Caixabank_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(Caixabank_df$cuerpo))
Caixabank_df <- subset(Caixabank_df, !(1:nrow(Caixabank_df) %in% indices_duplicados))
length(unique(Caixabank_df$cuerpo))
save(Caixabank_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Caixabank.rds")






# Santander ---------------------------------------------------------------------------

Santander_df <- data.frame(titulo = character(), url = character(), stringsAsFactors = FALSE)

numeros <- seq(1, 398, by = 20)
df <- data.frame(Numero = numeros)

for (i in 1:20) {

  parametro <- df$Numero[i]
  url_go <- paste0("https://go.gale.com/ps/paginate.do?tabID=T004&lm=AC%7Ey%7E%7EPU%7E%22El+Economista%2C+Spain%22+Or+%22El+Pais%2C+Spain%22%7E%7ETX%7EBanco&searchResultsType=SingleTab&qt=TI%7E%22santander%22%7E%7ERB%7E366+5&searchId=R5&searchType=AdvancedSearchForm&currentPosition=", parametro,"&userGroupName=univ&inPS=true&sort=Relevance&prodId=STND")
  print(paste0(i, " de 20"))
  webpage <- read_html(url_go)

  titulo <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event') %>%
    html_text()
  titulos <- gsub("^\\s+|\\s+$", "", titulo)

  enlace <- webpage %>%
    html_nodes('a.title__link.documentLink.js-tts-hover.gtm-click-event')  %>%
    html_attr("href")
  enlaces <- gsub("retrieve.do", "https://go.gale.com/ps/retrieve.do", enlace)

  fecha <- webpage %>%
    html_nodes('span.js-metadata-box.metadata-box.citation-pubDate') %>%
    html_text()
  fecha

  editorial <- webpage %>%
    html_nodes('span.citation-details') %>%
    html_text()
  editorial

  df_temporal <- data.frame(titulo = titulos, url=enlaces, fechas = fecha, editorial = editorial)
  Santander_df <- bind_rows(Santander_df, df_temporal)
}


## INSIDE
resumen_df <- data.frame(texto = character(), stringsAsFactors = FALSE)

for (i in 1:398) {
  url_1 <- Santander_df$url[i]
  webpage <- read_html(url_1)

  resumen <- webpage %>%
    html_nodes('div.document-text') %>%
    html_text()
  print(paste0(i, " de 398"))

  temporal_df <- data.frame(texto=resumen)
  resumen_df <- bind_rows(resumen_df, temporal_df)

}

Santander_df$cuerpo <- resumen_df$texto
indices_duplicados <- which(duplicated(Santander_df$cuerpo))
Santander_df <- subset(Santander_df, !(1:nrow(Santander_df) %in% indices_duplicados))
length(unique(Santander_df$cuerpo))
save(Santander_df, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Santander.rds")













# Vamos cargando lo scrappeado para el análisis:

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









