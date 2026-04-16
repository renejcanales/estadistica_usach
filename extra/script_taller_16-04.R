################################################################################
###############      Estadística -EEII USACH- Taller 2     ######################
###############            16 de abril de 2026             ######################
################################################################################


############################ Ejemplos de clase #################################

# Fijamos el directorio para cargar la base de datos

 setwd("")
 getwd()

# Instalar paquetes que no tengan con:
install.packages("fBasics")

# Cargar paquetes necesarios
library(dplyr)    # manipulación
library(ggplot2)  # gráficos
library(readr)    # importar
library(car)      # recodificar
library(fBasics)  # descriptivos
library(DT)       # tablas interactivas
library(tidyr)    # limpieza adicional
library(here)     # manejo de rutas

# Configuración de visualización
theme_set(theme_bw())
options(scipen = 999) # Evita notación científica

# Usamos la función base read.csv, especificando parámetros clave:
datos <- read.csv("ruta/a/tu/cep95.csv", 
                  sep = ",",   # ¡Ojo! Si exportaste de un Excel en español, a veces es ";"
                  encoding = "UTF-8",  # VITAL: Asegura que las 'ñ' y tildes no se rompan
                  na.strings = c("NA", "", " ", "-99"), # Define qué cosas debe considerar como "Perdidos"
                  stringsAsFactors = FALSE)

# Limpieza rápida: Convertir variables clave a factores para mejores gráficos
# Esto evita que variables como 'región' se traten como números
cep <- cep %>%
  mutate(zona_u_r = as.factor(zona_u_r),
         iden_pol_2 = as.factor(iden_pol_2))

# Ver las primeras filas
head(cep)

# Revisar los nombres de las variables disponibles
names(cep)

# Dimensiones de la base (filas = casos, columnas = variables)
dim(cep)

# Resumen rápido de todas las variables
glimpse(cep)

# Revisión inicial de la variable
table(cep$zona_u_r)


### Variable dictómica: Zona Urbana/Rural

# Tabla de frecuencias completa
data_zona <- select(cep, zona_u_r) %>% na.omit()

tabla_zona <- data_zona %>%
  group_by(zona_u_r) %>%
  summarise(Frec_abs = n()) %>%
  mutate(
    Frec_rel       = prop.table(Frec_abs),
    Frec_porc      = prop.table(Frec_abs) * 100,
    Frec_acum      = cumsum(Frec_abs),
    Frec_acum_rel  = cumsum(Frec_rel),
    Frec_acum_porc = cumsum(Frec_porc)
  )

tabla_zona

# Recodificamos la variable a formato numérico (1 = Urbano, 0 = Rural)
cep$zona_num <- as.numeric(car::recode(as.character(cep$zona_u_r),
                                       "'Urbano'=1; 'Rural'=0"))

# Calculamos la media (= proporción de encuestados urbanos)
mean(cep$zona_num, na.rm = TRUE)

# Variable politómica: Identificación política (`iden_pol_2`)

# Revisión inicial
table(cep$iden_pol_2)

# Tabla de frecuencias
data_idenpol <- select(cep, iden_pol_2) %>% na.omit()

tabla_idenpol <- data_idenpol %>%
  group_by(iden_pol_2) %>%
  summarise(Frec_abs = n()) %>%
  mutate(
    Frec_rel       = prop.table(Frec_abs),
    Frec_porc      = prop.table(Frec_abs) * 100,
    Frec_acum      = cumsum(Frec_abs),
    Frec_acum_rel  = cumsum(Frec_rel),
    Frec_acum_porc = cumsum(Frec_porc)
  ) %>%
  arrange(desc(Frec_abs))

tabla_idenpol

# Gráficos para variables categóricas 

theme_set(theme_bw())

# Gráfico básico
g1a <- ggplot(cep, aes(x = iden_pol_2)) +
  geom_bar()
g1a

# Gráfico con porcentajes y personalización
theme_set(theme_bw())

g1b <- ggplot(cep, aes(x = iden_pol_2)) +
  geom_bar(aes(y = (after_stat(count)) / sum(after_stat(count)) * 100),
           color = "black", fill = "steelblue") +
  labs(
    y       = "Porcentaje",
    x       = "Identificación política",
    title   = "Distribución de encuestados/as según identificación política",
    caption = "Fuente: Encuesta CEP N°95"
  ) +
  coord_flip()

g1b


## Ejercicio 1: Construye una tabla de frecuencias y un gráfico de barras para la variable `region`. Personaliza el gráfico usando un color distinto a los ejemplos anteriores.

# Tabla de frecuencias
data_region <- select(cep, region_3) %>% na.omit()

tabla_region <- data_region %>%
  group_by(region_3) %>%
  summarise(Frec_abs = n()) %>%
  mutate(
    Frec_porc = prop.table(Frec_abs) * 100
  ) %>%
  arrange(desc(Frec_abs))

tabla_region

# Gráfico de barras
theme_set(theme_bw())

g_ej1 <- ggplot(cep, aes(x = region_3)) +
  geom_bar(color = "red", fill = "lightblue") +
  labs(y = "Frecuencia", x = "Región", title = "Distribución de encuestados/as según región") +
  coord_flip()

g_ej1

## Gráficos para variables continuas: Edad

library(fBasics)

# Medidas de tendencia central y dispersión
mean(cep$edad,   na.rm = TRUE)   # media
median(cep$edad, na.rm = TRUE)   # mediana
min(cep$edad,    na.rm = TRUE)   # mínimo
max(cep$edad,    na.rm = TRUE)   # máximo
sd(cep$edad,     na.rm = TRUE)   # desviación estándar

# Cuartiles
quantile(cep$edad, prob = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Quintiles
quantile(cep$edad, prob = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)

# Deciles
quantile(cep$edad, prob = seq(0, 1, length = 11), na.rm = TRUE)

# Tabla resumen con basicStats()
tab_edad <- select(cep, edad) %>% na.omit()

tab_edad <- basicStats(tab_edad)[c("Minimum", "Maximum", "Mean",
                                  "Median", "Stdev",
                                  "1. Quartile", "3. Quartile",
                                  "Skewness", "Kurtosis", "NAs"), ]

# Convertir y transponer ANTES de nombrar
tab_edad <- as.data.frame(t(as.matrix(tab_edad)))

# Ahora sí: nombres de columnas
colnames(tab_edad) <- c("Mínimo", "Máximo", "Media", "Mediana",
                        "DS", "Q1", "Q3", "Asimetría", "Curtosis", "NAs")

# Nombre de fila
rownames(tab_edad) <- "Edad"

# Redondeo
tab_edad[, 1:9] <- round(tab_edad[, 1:9], digits = 1)

# Tabla interactiva con botón para exportar a Excel
DT::datatable(tab_edad,
  caption    = "Tabla 1: Estadísticos descriptivos — Edad",
  extensions = "Buttons",
  options    = list(dom = "Bfrtip", buttons = c("excel"))
)

## Histogramas 

theme_set(theme_bw())

g2a <- ggplot(cep, aes(x = edad)) +
  geom_histogram() +
  labs(y = "Frecuencia", x = "Edad",
       title = "Distribución de la variable Edad")
g2a

theme_set(theme_bw())

g2b <- ggplot(cep, aes(x = edad)) +
  geom_histogram(binwidth = 5, color = "black", fill = "steelblue") +
  labs(
    y       = "Frecuencia",
    x       = "Edad (años)",
    title   = "Distribución de la variable Edad",
    caption = "Fuente: Encuesta CEP N°95"
  )
g2b

## Boxplot

theme_set(theme_bw())

g3a <- ggplot(cep, aes(x = "", y = edad)) +
  geom_boxplot(fill = "steelblue", outliers = TRUE) +
  labs(
    x       = "",
    y       = "Edad (años)",
    title   = "Distribución de la variable Edad",
    caption = "Fuente: Encuesta CEP N°95"
  )
g3a

theme_set(theme_bw())

g3b <- ggplot(cep, aes(x = "", y = edad)) +
  geom_boxplot(fill = "steelblue", outliers = TRUE) +
  labs(
    x       = "",
    y       = "Edad (años)",
    title   = "Distribución de la variable Edad",
    caption = "Fuente: Encuesta CEP N°95"
  ) +
  coord_flip()
g3b

## Ejercicio 2: Calcule los estadísticos descriptivos y elabore un histograma y un boxplot para la variable `edad` según la variable `zona_u_r`.

# 1. Preparamos un subconjunto de datos limpio
datos_boxplot <- cep %>%
  filter(!is.na(zona_u_r), !is.na(edad)) %>% # Quitamos los casos sin respuesta
  mutate(zona_categoria = as.factor(zona_u_r)) # Convertimos a factor (1=Urbano, 2=Rural)

# 2. Creamos el gráfico
g_box <- ggplot(datos_boxplot, aes(x = zona_categoria, y = edad, fill = zona_categoria)) +
  geom_boxplot(
    alpha = 0.7,               # Hace los colores un poco más suaves
    outlier.color = "red",     # Resalta los valores atípicos en rojo
    outlier.size = 2           # Hace los valores atípicos más visibles
  ) +
  labs(
    title = "Distribución de la Edad según Zona de Residencia",
    subtitle = "Los puntos rojos representan edades atípicas (outliers)",
    x = "Zona (1 = Urbano, 2 = Rural)",
    y = "Edad (años)",
    fill = "Zona"
  ) +
  theme_bw() +
  theme(legend.position = "none") # Quitamos la leyenda porque el eje X ya lo explica

g_box

