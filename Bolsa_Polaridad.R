
library(quantmod)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidytext)
library(lubridate)
library(readxl)

# Define el rango de fechas
start_date <- as.Date("2022-05-17")
end_date <- as.Date("2024-03-05")

# Lista de símbolos de las acciones
symbols <- c("AENA.MC", "BBVA.MC", "CABK.MC", "SAN.MC", "ELE.MC",
             "REP.MC", "GRF.MC", "FER.MC", "ITX.MC", "IBE.MC")

# Crea una lista para almacenar los datos
data_list <- list()

# Descarga los datos de las acciones
for (symbol in symbols) {
  getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
  data_list[[symbol]] <- get(symbol)
}

# Convierte la lista en un dataframe
all_data <- do.call(merge, data_list)

long_data <- all_data %>%
  as.data.frame() %>%
  rownames_to_column(var = "Date") %>%
  pivot_longer(cols = -Date, names_to = "Company", values_to = "Price")

long_data$Acciones <- substr(long_data$Company, 8, nchar(long_data$Company))
long_data$Acciones <- gsub("\\.", "", long_data$Acciones)

long_data$Company <- substr(long_data$Company, 1, 4)
long_data$Company <- gsub("\\.", "", long_data$Company)
long_data$Company <- gsub("CABK", "Caixabank", long_data$Company)
long_data$Company <- gsub("SAN", "Santander", long_data$Company)
long_data$Company <- gsub("ELE", "Endesa", long_data$Company)
long_data$Company <- gsub("REP", "Repsol", long_data$Company)
long_data$Company <- gsub("GRF", "Grifols", long_data$Company)
long_data$Company <- gsub("FER", "Ferrovial", long_data$Company)
long_data$Company <- gsub("ITX", "Inditex", long_data$Company)
long_data$Company <- gsub("IBE", "Iberdrola", long_data$Company)

save(long_data, file = "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/datos_bolsa.rds")
load("C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/datos_bolsa.rds")
long_data_variacion <- filter(long_data, Acciones == "Volume" | Acciones == "Open" | Acciones == "Close")

Acciones_AENA <- filter(long_data_variacion, Company == "AENA")
Acciones_BBVA <- filter(long_data_variacion, Company == "BBVA")
Acciones_Caixabank <- filter(long_data_variacion, Company == "Caixabank")
Acciones_Endesa <- filter(long_data_variacion, Company == "Endesa")
Acciones_Grifols <- filter(long_data_variacion, Company == "Grifols")
Acciones_Iberdrola <- filter(long_data_variacion, Company == "Iberdrola")
Acciones_Ferrovial <- filter(long_data_variacion, Company == "Ferrovial")
Acciones_Repsol <- filter(long_data_variacion, Company == "Repsol")
Acciones_Inditex <- filter(long_data_variacion, Company == "Inditex")
Acciones_Santander <- filter(long_data_variacion, Company == "Santander")

# IBEX 35

# Fechas
start_date <- as.Date("2022-05-17")
end_date <- as.Date("2024-03-05")

symbol <- c("^IBEX")
getSymbols("^IBEX", src = "yahoo", from = start_date, to = end_date)

IBEX <- IBEX %>%
  as.data.frame() %>%
  rownames_to_column(var = "Date")


# TASAS DE VARIACIÓN #####################################################################

writexl::write_xlsx(Acciones_AENA,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_AENA.xlsx")
writexl::write_xlsx(Acciones_BBVA,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_BBVA.xlsx")
writexl::write_xlsx(Acciones_Caixabank,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_Caixabank.xlsx")
writexl::write_xlsx(Acciones_Endesa,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_Endesa.xlsx")
writexl::write_xlsx(Acciones_Ferrovial,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_Ferrovial.xlsx")
writexl::write_xlsx(Acciones_Grifols,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_Grifols.xlsx")
writexl::write_xlsx(Acciones_Iberdrola,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_Iberdrola.xlsx")
writexl::write_xlsx(Acciones_Inditex,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_Inditex.xlsx")
writexl::write_xlsx(Acciones_Repsol,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_Repsol.xlsx")
writexl::write_xlsx(Acciones_Santander,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/Acciones_Santander.xlsx")




# POLARIDAD ############################################################################################

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


Polaridad_absoluta_por_anyo <- datos2 %>%
  count(Id = id, Titulo = titulo,Sentimiento,fechas, Empresa) %>%
  pivot_wider(names_from = Sentimiento,
              values_from = n,
              values_fill = 0) %>%
  mutate(Polaridad_absoluta = Positivo - Negativo)

Polaridad_absoluta_por_anyo$anyo <- substr(Polaridad_absoluta_por_anyo$fechas, nchar(Polaridad_absoluta_por_anyo$fechas) - 3, nchar(Polaridad_absoluta_por_anyo$fechas))
Polaridad_absoluta_por_anyo$dia_mes <- substr(Polaridad_absoluta_por_anyo$fechas, nchar(Polaridad_absoluta_por_anyo$fechas) - 7, nchar(Polaridad_absoluta_por_anyo$fechas)-6)
Polaridad_absoluta_por_anyo$mes <- substr(Polaridad_absoluta_por_anyo$fechas, 1,4)
Polaridad_absoluta_por_anyo <- Polaridad_absoluta_por_anyo %>%
  mutate(Date_diario = dmy(paste(dia_mes, mes, anyo, sep = " ")))



Polaridad_absoluta_por_anyo <- Polaridad_absoluta_por_anyo[,c(4,5,6,7,11)]
Numero_pos_neg_empresa <- datos2 %>% group_by(Sentimiento, Empresa) %>% summarise(total = n())


# TODOS LOS SENTIMIENTOS

Polaridad_Grifols <- filter(Polaridad_absoluta_por_anyo, Empresa == "Grifols")
Polaridad_Grifols <- Polaridad_Grifols %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_Grifols) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_AENA <- filter(Polaridad_absoluta_por_anyo, Empresa == "AENA")
Polaridad_AENA <- Polaridad_AENA %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo), Positivos = sum(Positivo),
    Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_AENA) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_Santander <- filter(Polaridad_absoluta_por_anyo, Empresa == "Santander")
Polaridad_Santander <- Polaridad_Santander %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_Santander) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_Repsol <- filter(Polaridad_absoluta_por_anyo, Empresa == "Repsol")
Polaridad_Repsol <- Polaridad_Repsol %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_Repsol) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_Ferrovial <- filter(Polaridad_absoluta_por_anyo, Empresa == "Ferrovial")
Polaridad_Ferrovial <- Polaridad_Ferrovial %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_Ferrovial) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_Iberdrola <- filter(Polaridad_absoluta_por_anyo, Empresa == "Iberdrola")
Polaridad_Iberdrola <- Polaridad_Iberdrola %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_Iberdrola) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_Endesa <- filter(Polaridad_absoluta_por_anyo, Empresa == "Endesa")
Polaridad_Endesa <- Polaridad_Endesa %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_Endesa) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_Caixabank <- filter(Polaridad_absoluta_por_anyo, Empresa == "Caixabank")
Polaridad_Caixabank <- Polaridad_Caixabank %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_Caixabank) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_Inditex <- filter(Polaridad_absoluta_por_anyo, Empresa == "Inditex")
Polaridad_Inditex <- Polaridad_Inditex %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_Inditex) <- c("Date", "Negativos", "Positivos", "Polaridad")

Polaridad_BBVA <- filter(Polaridad_absoluta_por_anyo, Empresa == "BBVA")
Polaridad_BBVA <- Polaridad_BBVA %>% group_by(Date_diario) %>% summarise(Negativos = sum(Negativo),
    Positivos = sum(Positivo), Polaridad = sum(Polaridad_absoluta))
colnames(Polaridad_BBVA) <- c("Date", "Negativos", "Positivos", "Polaridad")


Acciones_Santander <- read_excel("Datos/Acciones_Santander.xlsx")
Acciones_Repsol <- read_excel("Datos/Acciones_Repsol.xlsx")
Acciones_Inditex <- read_excel("Datos/Acciones_Inditex.xlsx")
Acciones_Iberdrola <- read_excel("Datos/Acciones_Iberdrola.xlsx")
Acciones_Grifols <- read_excel("Datos/Acciones_Grifols.xlsx")
Acciones_Ferrovial <- read_excel("Datos/Acciones_Ferrovial.xlsx")
Acciones_Endesa <- read_excel("Datos/Acciones_Endesa.xlsx")
Acciones_Caixabank <- read_excel("Datos/Acciones_Caixabank.xlsx")
Acciones_BBVA <- read_excel("Datos/Acciones_BBVA.xlsx")
Acciones_AENA <- read_excel("Datos/Acciones_AENA.xlsx")

Acciones_Grifols$Date <- as.Date(Acciones_Grifols$Date)
Acciones_Ferrovial$Date <- as.Date(Acciones_Ferrovial$Date)
Acciones_Caixabank$Date <- as.Date(Acciones_Caixabank$Date)
Acciones_Repsol$Date <- as.Date(Acciones_Repsol$Date)
Acciones_Iberdrola$Date <- as.Date(Acciones_Iberdrola$Date)
Acciones_Endesa$Date <- as.Date(Acciones_Endesa$Date)
Acciones_Santander$Date <- as.Date(Acciones_Santander$Date)
Acciones_AENA$Date <- as.Date(Acciones_AENA$Date)
Acciones_BBVA$Date <- as.Date(Acciones_BBVA$Date)
Acciones_Inditex$Date <- as.Date(Acciones_Inditex$Date)


# Acciones_BBVA$tasa_variacion <- diff(Acciones_BBVA$Price) / lag(Acciones_BBVA$Price)
# Acciones_BBVA$tasa_variacion <- replace(Acciones_BBVA$tasa_variacion, is.na(Acciones_BBVA$tasa_variacion), 0)

evaluacion_Grifols <- inner_join(Polaridad_Grifols, Acciones_Grifols, by = "Date")
evaluacion_Ferrovial <- inner_join(Polaridad_Ferrovial, Acciones_Ferrovial, by = "Date")
evaluacion_AENA <- inner_join(Polaridad_AENA, Acciones_AENA, by = "Date")
evaluacion_Santander <- inner_join(Polaridad_Santander, Acciones_Santander, by = "Date")
evaluacion_Iberdrola <- inner_join(Polaridad_Iberdrola, Acciones_Iberdrola, by = "Date")
evaluacion_Endesa <- inner_join(Polaridad_Endesa, Acciones_Endesa, by = "Date")
evaluacion_Repsol <- inner_join(Polaridad_Repsol, Acciones_Repsol, by = "Date")
evaluacion_Caixabank <- inner_join(Polaridad_Caixabank, Acciones_Caixabank, by = "Date")
evaluacion_Inditex <- inner_join(Polaridad_Inditex, Acciones_Inditex, by = "Date")
evaluacion_BBVA <- inner_join(Polaridad_BBVA, Acciones_BBVA, by = "Date")


# CORRELACION TASA DE VARIACIÓN DEL CLOSE CON NEGATIVOS #################################################

cor(evaluacion_AENA$Negativos, evaluacion_AENA$Tasa_var_cotizacion)
cor(evaluacion_BBVA$Negativos, evaluacion_BBVA$Tasa_var_cotizacion)
cor(evaluacion_Caixabank$Negativos, evaluacion_Caixabank$Tasa_var_cotizacion)
cor(evaluacion_Endesa$Negativos, evaluacion_Endesa$Tasa_var_cotizacion)
cor(evaluacion_Ferrovial$Negativos, evaluacion_Ferrovial$Tasa_var_cotizacion)
cor(evaluacion_Grifols$Negativos, evaluacion_Grifols$Tasa_var_cotizacion)
cor(evaluacion_Iberdrola$Negativos, evaluacion_Iberdrola$Tasa_var_cotizacion)
cor(evaluacion_Inditex$Negativos, evaluacion_Inditex$Tasa_var_cotizacion)
cor(evaluacion_Repsol$Negativos, evaluacion_Repsol$Tasa_var_cotizacion)
cor(evaluacion_Santander$Negativos, evaluacion_Santander$Tasa_var_cotizacion)


# CORRELACION TASA DE VARIACIÓN DEL CLOSE CON POSITIVOS

cor(evaluacion_AENA$Positivos, evaluacion_AENA$Tasa_var_cotizacion)
cor(evaluacion_BBVA$Positivos, evaluacion_BBVA$Tasa_var_cotizacion)
cor(evaluacion_Caixabank$Positivos, evaluacion_Caixabank$Tasa_var_cotizacion)
cor(evaluacion_Endesa$Positivos, evaluacion_Endesa$Tasa_var_cotizacion)
cor(evaluacion_Ferrovial$Positivos, evaluacion_Ferrovial$Tasa_var_cotizacion)
cor(evaluacion_Grifols$Positivos, evaluacion_Grifols$Tasa_var_cotizacion)
cor(evaluacion_Iberdrola$Positivos, evaluacion_Iberdrola$Tasa_var_cotizacion)
cor(evaluacion_Inditex$Positivos, evaluacion_Inditex$Tasa_var_cotizacion)
cor(evaluacion_Repsol$Positivos, evaluacion_Repsol$Tasa_var_cotizacion)
cor(evaluacion_Santander$Positivos, evaluacion_Santander$Tasa_var_cotizacion)


# CORRELACION TASA DE VARIACIÓN DEL CLOSE CON POLARIDAD

cor(evaluacion_AENA$Polaridad, evaluacion_AENA$Tasa_var_cotizacion)
cor(evaluacion_BBVA$Polaridad, evaluacion_BBVA$Tasa_var_cotizacion)
cor(evaluacion_Caixabank$Polaridad, evaluacion_Caixabank$Tasa_var_cotizacion)
cor(evaluacion_Endesa$Polaridad, evaluacion_Endesa$Tasa_var_cotizacion)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Tasa_var_cotizacion)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Tasa_var_cotizacion)
cor(evaluacion_Iberdrola$Polaridad, evaluacion_Iberdrola$Tasa_var_cotizacion)
cor(evaluacion_Inditex$Polaridad, evaluacion_Inditex$Tasa_var_cotizacion)
cor(evaluacion_Repsol$Polaridad, evaluacion_Repsol$Tasa_var_cotizacion)
cor(evaluacion_Santander$Polaridad, evaluacion_Santander$Tasa_var_cotizacion)


# CORRELACION TASA DE VARIACIÓN DEL VOLUMEN CON NEGATIVOS
# CORRELACION TASA DE VARIACIÓN DEL VOLUMEN CON POSITIVOS
# CORRELACION TASA DE VARIACIÓN DEL VOLUMEN CON POLARIDAD




# CORRELACION TASA DE VARIACIÓN MEDIA DEL CLOSE CON NEGATIVOS ##########################################

cor(evaluacion_AENA$Negativos, evaluacion_AENA$Tasa_media_cotizacion)
cor(evaluacion_BBVA$Negativos, evaluacion_BBVA$Tasa_media_cotizacion)
cor(evaluacion_Caixabank$Negativos, evaluacion_Caixabank$Tasa_media_cotizacion)
cor(evaluacion_Endesa$Negativos, evaluacion_Endesa$Tasa_media_cotizacion)
cor(evaluacion_Ferrovial$Negativos, evaluacion_Ferrovial$Tasa_media_cotizacion)
cor(evaluacion_Grifols$Negativos, evaluacion_Grifols$Tasa_media_cotizacion)
cor(evaluacion_Iberdrola$Negativos, evaluacion_Iberdrola$Tasa_media_cotizacion)
cor(evaluacion_Inditex$Negativos, evaluacion_Inditex$Tasa_media_cotizacion)
cor(evaluacion_Repsol$Negativos, evaluacion_Repsol$Tasa_media_cotizacion)
cor(evaluacion_Santander$Negativos, evaluacion_Santander$Tasa_var_cotizacion)

# CORRELACION TASA DE VARIACIÓN MEDIA DEL CLOSE CON POSITIVOS

cor(evaluacion_AENA$Positivos, evaluacion_AENA$Tasa_media_cotizacion)
cor(evaluacion_BBVA$Positivos, evaluacion_BBVA$Tasa_media_cotizacion)
cor(evaluacion_Caixabank$Positivos, evaluacion_Caixabank$Tasa_media_cotizacion)
cor(evaluacion_Endesa$Positivos, evaluacion_Endesa$Tasa_media_cotizacion)
cor(evaluacion_Ferrovial$Positivos, evaluacion_Ferrovial$Tasa_media_cotizacion)
cor(evaluacion_Grifols$Positivos, evaluacion_Grifols$Tasa_media_cotizacion)
cor(evaluacion_Iberdrola$Positivos, evaluacion_Iberdrola$Tasa_media_cotizacion)
cor(evaluacion_Inditex$Positivos, evaluacion_Inditex$Tasa_media_cotizacion)
cor(evaluacion_Repsol$Positivos, evaluacion_Repsol$Tasa_media_cotizacion)
cor(evaluacion_Santander$Positivos, evaluacion_Santander$Tasa_media_cotizacion)


# CORRELACION TASA DE VARIACIÓN MEDIA DEL CLOSE CON POLARIDAD

cor(evaluacion_AENA$Polaridad, evaluacion_AENA$Tasa_media_cotizacion)
cor(evaluacion_BBVA$Polaridad, evaluacion_BBVA$Tasa_media_cotizacion)
cor(evaluacion_Caixabank$Polaridad, evaluacion_Caixabank$Tasa_media_cotizacion)
cor(evaluacion_Endesa$Polaridad, evaluacion_Endesa$Tasa_media_cotizacion)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Tasa_media_cotizacion)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Tasa_media_cotizacion)
cor(evaluacion_Iberdrola$Polaridad, evaluacion_Iberdrola$Tasa_media_cotizacion)
cor(evaluacion_Inditex$Polaridad, evaluacion_Inditex$Tasa_media_cotizacion)
cor(evaluacion_Repsol$Polaridad, evaluacion_Repsol$Tasa_media_cotizacion)
cor(evaluacion_Santander$Polaridad, evaluacion_Santander$Tasa_media_cotizacion)










# CORRELACION TASA DE VARIACIÓN EMPRESA-IBEX CON NEGATIVOS #############################################

cor(evaluacion_AENA$Negativos, evaluacion_AENA$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_BBVA$Negativos, evaluacion_BBVA$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Caixabank$Negativos, evaluacion_Caixabank$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Endesa$Negativos, evaluacion_Endesa$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Ferrovial$Negativos, evaluacion_Ferrovial$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Grifols$Negativos, evaluacion_Grifols$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Iberdrola$Negativos, evaluacion_Iberdrola$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Inditex$Negativos, evaluacion_Inditex$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Repsol$Negativos, evaluacion_Repsol$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Santander$Negativos, evaluacion_Santander$Tasa_var_media_IBEX_Empresa)

# CORRELACION TASA DE VARIACIÓN MEDIA EMPRESA-IBEX CON POSITIVOS

cor(evaluacion_AENA$Positivos, evaluacion_AENA$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_BBVA$Positivos, evaluacion_BBVA$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Caixabank$Positivos, evaluacion_Caixabank$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Endesa$Positivos, evaluacion_Endesa$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Ferrovial$Positivos, evaluacion_Ferrovial$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Grifols$Positivos, evaluacion_Grifols$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Iberdrola$Positivos, evaluacion_Iberdrola$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Inditex$Positivos, evaluacion_Inditex$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Repsol$Positivos, evaluacion_Repsol$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Santander$Positivos, evaluacion_Santander$Tasa_var_media_IBEX_Empresa)

# CORRELACION TASA DE VARIACIÓN MEDIA EMPRESA-IBEX CON POLARIDAD

cor(evaluacion_AENA$Polaridad, evaluacion_AENA$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_BBVA$Polaridad, evaluacion_BBVA$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Caixabank$Polaridad, evaluacion_Caixabank$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Endesa$Polaridad, evaluacion_Endesa$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Iberdrola$Polaridad, evaluacion_Iberdrola$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Inditex$Polaridad, evaluacion_Inditex$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Repsol$Polaridad, evaluacion_Repsol$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Santander$Polaridad, evaluacion_Santander$Tasa_var_media_IBEX_Empresa)










# CORRELACION RATIO NEGATIVOS CON TASA DE VARIACION CLOSE ################################################


cor(evaluacion_AENA$Negativos, evaluacion_AENA$Tasa_var_cotizacion)
cor(evaluacion_BBVA$Negativos, evaluacion_BBVA$Tasa_var_cotizacion)
cor(evaluacion_Caixabank$Negativos, evaluacion_Caixabank$Tasa_var_cotizacion)
cor(evaluacion_Endesa$Negativos, evaluacion_Endesa$Tasa_var_cotizacion)
cor(evaluacion_Ferrovial$Negativos, evaluacion_Ferrovial$Tasa_var_cotizacion)
cor(evaluacion_Grifols$Negativos, evaluacion_Grifols$Tasa_var_cotizacion)
cor(evaluacion_Iberdrola$Negativos, evaluacion_Iberdrola$Tasa_var_cotizacion)
cor(evaluacion_Inditex$Negativos, evaluacion_Inditex$Tasa_var_cotizacion)
cor(evaluacion_Repsol$Negativos, evaluacion_Repsol$Tasa_var_cotizacion)
cor(evaluacion_Santander$Negativos, evaluacion_Santander$Tasa_var_cotizacion)

# CORRELACION RATIO POSITIVOS CON TASA DE VARIACION DE CLOSE ###########################################


cor(evaluacion_AENA$Positivos, evaluacion_AENA$Tasa_var_cotizacion)
cor(evaluacion_BBVA$Positivos, evaluacion_BBVA$Tasa_var_cotizacion)
cor(evaluacion_Caixabank$Positivos, evaluacion_Caixabank$Tasa_var_cotizacion)
cor(evaluacion_Endesa$Positivos, evaluacion_Endesa$Tasa_var_cotizacion)
cor(evaluacion_Ferrovial$Positivos, evaluacion_Ferrovial$Tasa_var_cotizacion)
cor(evaluacion_Grifols$Positivos, evaluacion_Grifols$Tasa_var_cotizacion)
cor(evaluacion_Iberdrola$Positivos, evaluacion_Iberdrola$Tasa_var_cotizacion)
cor(evaluacion_Inditex$Positivos, evaluacion_Inditex$Tasa_var_cotizacion)
cor(evaluacion_Repsol$Positivos, evaluacion_Repsol$Tasa_var_cotizacion)
cor(evaluacion_Santander$Positivos, evaluacion_Santander$Tasa_var_cotizacion)


# CORRELACION RATIO POLARIDAD CON TASA DE VARIACION DE CLOSE ###########################################

cor(evaluacion_AENA$Polaridad, evaluacion_AENA$Tasa_var_cotizacion)
cor(evaluacion_BBVA$Polaridad, evaluacion_BBVA$Tasa_var_cotizacion)
cor(evaluacion_Caixabank$Polaridad, evaluacion_Caixabank$Tasa_var_cotizacion)
cor(evaluacion_Endesa$Polaridad, evaluacion_Endesa$Tasa_var_cotizacion)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Tasa_var_cotizacion)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Tasa_var_cotizacion)
cor(evaluacion_Iberdrola$Polaridad, evaluacion_Iberdrola$Tasa_var_cotizacion)
cor(evaluacion_Inditex$Polaridad, evaluacion_Inditex$Tasa_var_cotizacion)
cor(evaluacion_Repsol$Polaridad, evaluacion_Repsol$Tasa_var_cotizacion)
cor(evaluacion_Santander$Polaridad, evaluacion_Santander$Tasa_var_cotizacion)






# CORRELACION TASA VARIACION POLARIDAD CON TASA VARIACION CLOSE ############################################

cor(evaluacion_AENA$Tasa_var_Polaridad, evaluacion_AENA$Tasa_var_cotizacion)
cor(evaluacion_BBVA$Tasa_var_polaridad, evaluacion_BBVA$Tasa_var_cotizacion)
cor(evaluacion_Caixabank$Tasa_var_Polaridad, evaluacion_Caixabank$Tasa_var_cotizacion)
cor(evaluacion_Endesa$Tasa_var_polaridad, evaluacion_Endesa$Tasa_var_cotizacion)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Tasa_var_cotizacion)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Tasa_var_cotizacion)


cor(evaluacion_AENA$Tasa_var_Negativos, evaluacion_AENA$Tasa_var_cotizacion)
cor(evaluacion_BBVA$Tasa_var_negativos, evaluacion_BBVA$Tasa_var_cotizacion)
cor(evaluacion_Caixabank$Tasa_var_negativos, evaluacion_Caixabank$Tasa_var_cotizacion)
cor(evaluacion_Endesa$Tasa_var_negativos, evaluacion_Endesa$Tasa_var_cotizacion)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Tasa_var_cotizacion)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Tasa_var_cotizacion)




# CORRELACION TASA VARIACION POLARIDAD CON TASA VARIACION MEDIA

cor(evaluacion_AENA$Tasa_var_Polaridad, evaluacion_AENA$Tasa_media_cotizacion)
cor(evaluacion_BBVA$Tasa_var_polaridad, evaluacion_BBVA$Tasa_media_cotizacion)
cor(evaluacion_Caixabank$Tasa_var_Polaridad, evaluacion_Caixabank$Tasa_media_cotizacion)
cor(evaluacion_Endesa$Tasa_var_polaridad, evaluacion_Endesa$Tasa_media_cotizacion)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Tasa_media_cotizacion)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Tasa_media_cotizacion)

# CORRELACION TASA VARIACION POLARIDAD CON TASA VARIACION IBEX

cor(evaluacion_AENA$Tasa_var_Polaridad, evaluacion_AENA$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_BBVA$Tasa_var_polaridad, evaluacion_BBVA$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Caixabank$Tasa_var_Polaridad, evaluacion_Caixabank$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Endesa$Tasa_var_polaridad, evaluacion_Endesa$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Tasa_var_media_IBEX_Empresa)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Tasa_var_media_IBEX_Empresa)














# PRUEBA CON EL IBEX

IBEX$Date <- as.Date(IBEX$Date)
IBEX <- IBEX[,c(1,5,6)]

writexl::write_xlsx(IBEX,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/IBEX.xlsx")

evaluacion_IBEX <- inner_join(IBEX, Negativos, by = "Date")

writexl::write_xlsx(evaluacion_IBEX, "C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_IBEX.xlsx")
evaluacion_IBEX <- read_excel("Datos/evaluacion_IBEX.xlsx")

cor(evaluacion_IBEX$Negativos, evaluacion_IBEX$IBEX.Close)






writexl::write_xlsx(evaluacion_AENA,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_AENA.xlsx")
writexl::write_xlsx(evaluacion_BBVA,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_BBVA.xlsx")
writexl::write_xlsx(evaluacion_Caixabank,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_Caixabank.xlsx")
writexl::write_xlsx(evaluacion_Endesa,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_Endesa.xlsx")
writexl::write_xlsx(evaluacion_Ferrovial,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_Ferrovial.xlsx")
writexl::write_xlsx(evaluacion_Grifols,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_Grifols.xlsx")
writexl::write_xlsx(evaluacion_Iberdrola,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_Iberdrola.xlsx")
writexl::write_xlsx(evaluacion_Inditex,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_Inditex.xlsx")
writexl::write_xlsx(evaluacion_Repsol,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_Repsol.xlsx")
writexl::write_xlsx(evaluacion_Santander,"C:/Users/34656/OneDrive/Escritorio/BIA/4o curso/TFG/Datos/evaluacion_Santander.xlsx")



evaluacion_AENA <- read_excel("Datos/evaluacion_AENA.xlsx")
evaluacion_BBVA <- read_excel("Datos/evaluacion_BBVA.xlsx")
evaluacion_Caixabank <- read_excel("Datos/evaluacion_Caixabank.xlsx")
evaluacion_Inditex <- read_excel("Datos/evaluacion_Inditex.xlsx")
evaluacion_Ferrovial <- read_excel("Datos/evaluacion_Grifols.xlsx")
evaluacion_Grifols <- read_excel("Datos/evaluacion_Ferrovial.xlsx")
evaluacion_Endesa <- read_excel("Datos/evaluacion_Endesa.xlsx")
evaluacion_Santander<- read_excel("Datos/evaluacion_Santander.xlsx")
evaluacion_Repsol <- read_excel("Datos/evaluacion_Repsol.xlsx")
evaluacion_Iberdrola <- read_excel("Datos/evaluacion_Iberdrola.xlsx")

evaluacion_Grifols$anyo <- substr(evaluacion_Grifols$Date,1,4)
evaluacion_Grifols_2022 <- filter(evaluacion_Grifols, anyo == "2022")
evaluacion_Grifols_2023 <- filter(evaluacion_Grifols, anyo == "2023")

evaluacion_Ferrovial$anyo <- substr(evaluacion_Ferrovial$Date,1,4)
evaluacion_Ferrovial_2022 <- filter(evaluacion_Ferrovial, anyo == "2022")
evaluacion_Ferrovial_2023 <- filter(evaluacion_Ferrovial, anyo == "2023")

# CORRPLOT

cor(evaluacion_AENA$Polaridad, evaluacion_AENA$Volume)
cor(evaluacion_BBVA$Polaridad, evaluacion_BBVA$Volume)
cor(evaluacion_Caixabank$Polaridad, evaluacion_Caixabank$Volume)
cor(evaluacion_Endesa$Polaridad, evaluacion_Endesa$Volume)
cor(evaluacion_Ferrovial$Polaridad, evaluacion_Ferrovial$Volume)
cor(evaluacion_Grifols$Polaridad, evaluacion_Grifols$Volume)
cor(evaluacion_Iberdrola$Polaridad, evaluacion_Iberdrola$Volume)
cor(evaluacion_Inditex$Polaridad, evaluacion_Inditex$Volume)
cor(evaluacion_Repsol$Polaridad, evaluacion_Repsol$Volume)
cor(evaluacion_Santander$Polaridad, evaluacion_Santander$Volume)


cor(evaluacion_AENA$Negativos, evaluacion_AENA$Volume)
cor(evaluacion_BBVA$Negativos, evaluacion_BBVA$Volume)
cor(evaluacion_Caixabank$Negativos, evaluacion_Caixabank$Volume)
cor(evaluacion_Endesa$Negativos, evaluacion_Endesa$Volume)
cor(evaluacion_Ferrovial$Negativos, evaluacion_Ferrovial$Volume)
cor(evaluacion_Grifols$Negativos, evaluacion_Grifols$Volume)
cor(evaluacion_Iberdrola$Negativos, evaluacion_Iberdrola$Volume)
cor(evaluacion_Inditex$Negativos, evaluacion_Inditex$Volume)
cor(evaluacion_Repsol$Negativos, evaluacion_Repsol$Volume)
cor(evaluacion_Santander$Negativos, evaluacion_Santander$Volume)


cor(evaluacion_AENA$Positivos, evaluacion_AENA$Volume)
cor(evaluacion_BBVA$Positivos, evaluacion_BBVA$Volume)
cor(evaluacion_Caixabank$Positivos, evaluacion_Caixabank$Volume)
cor(evaluacion_Endesa$Positivos, evaluacion_Endesa$Volume)
cor(evaluacion_Ferrovial$Positivos, evaluacion_Ferrovial$Volume)
cor(evaluacion_Grifols$Positivos, evaluacion_Grifols$Volume)
cor(evaluacion_Iberdrola$Positivos, evaluacion_Iberdrola$Volume)
cor(evaluacion_Inditex$Positivos, evaluacion_Inditex$Volume)
cor(evaluacion_Repsol$Positivos, evaluacion_Repsol$Volume)
cor(evaluacion_Santander$Positivos, evaluacion_Santander$Volume)


