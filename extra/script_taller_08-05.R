################################################################################
##############     Estadística - EEII USACH - Clase 5      #####################
##############         René Canales Sellés               ####################
##############           08 de mayo de 2026                 ####################
################################################################################


# ─────────────────────────────────────────────────────────────────────────────
# 0. PREPARACIÓN
# ─────────────────────────────────────────────────────────────────────────────

# Fijar directorio de trabajo
## setwd("C:/ruta/a/tu/carpeta")
getwd()

# Instalar paquetes si es necesario (correr solo una vez):
## install.packages("dplyr")
## install.packages("here")
## install.packages("car")

# Cargar paquetes
library(dplyr)   # manipulación de datos
library(here)    # rutas relativas
library(car)     # recodificación de variables

# Cargar datos
cep <- read.csv(file = here("bases", "raw", "cep95.csv"),
                sep = ",",
                encoding = "UTF-8",
                stringsAsFactors = FALSE)

# Ver las primeras filas
head(cep)

# Revisar los nombres de variables disponibles
names(cep)


# ─────────────────────────────────────────────────────────────────────────────
# 1. REPASO: CARGA Y RECODIFICACIÓN DE VARIABLES
# ─────────────────────────────────────────────────────────────────────────────

# --- Exploración inicial -----------------------------------------------------

# Dimensiones de la base (filas = casos, columnas = variables)
dim(cep)

# Resumen rápido de todas las variables
glimpse(cep)

# PREGUNTA: ¿Cuántas personas fueron encuestadas? ¿Cuántas variables tiene la base?


# --- Variable dicotómica: Zona urbana/rural (zona_u_r) -----------------------

# Revisión inicial de la variable
table(cep$zona_u_r)

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
# Esto nos permite calcular la proporción de encuestados urbanos con la media
cep$zona_num <- as.numeric(car::recode(as.character(cep$zona_u_r),
                                       "'1'=1; '2'=0"))

# Verificamos la recodificación
table(cep$zona_num)

# La media de una variable dummy equivale a la proporción de la categoría = 1
mean(cep$zona_num, na.rm = TRUE)

# PREGUNTA: ¿Qué porcentaje de los encuestados/as reside en zona urbana?
# ¿Qué representa la media de la variable recodificada?


# --- Variable politómica: Identificación política (iden_pol_2) ---------------

# Revisión inicial
table(cep$iden_pol_2)

# Convertimos a factor para trabajar correctamente con la variable
cep <- cep %>%
  mutate(iden_pol_2 = as.factor(iden_pol_2))

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

# PREGUNTA: ¿Cuál es la identificación política más frecuente en la muestra?
# ¿Qué porcentaje acumulado representan las dos categorías más frecuentes?


# --- Variable continua: Edad -------------------------------------------------

# Medidas de tendencia central y dispersión
mean(cep$edad,   na.rm = TRUE)
median(cep$edad, na.rm = TRUE)
min(cep$edad,    na.rm = TRUE)
max(cep$edad,    na.rm = TRUE)
sd(cep$edad,     na.rm = TRUE)

# Cuartiles
quantile(cep$edad, prob = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# PREGUNTA: ¿Cuál es la edad promedio?
# ¿Por qué es importante comparar la media con la mediana?


# --- Ejercicio de repaso -----------------------------------------------------
# Construye una tabla de frecuencias para region_3 y recodifica zona_u_r
# asignando 2 a Urbano y 1 a Rural. Verifica con table().

# Tabla de frecuencias para region_3
data_region <- select(cep, region_3) %>% na.omit()

tabla_region <- data_region %>%
  group_by(region_3) %>%
  summarise(Frec_abs = n()) %>%
  mutate(Frec_porc = prop.table(Frec_abs) * 100) %>%
  arrange(desc(Frec_abs))

tabla_region

# Recodificación alternativa — complete el código:
# cep$zona_num2 <- as.numeric(car::recode(as.character(cep$zona_u_r),
#                                         "'1'=___; '2'=___"))
# table(cep$zona_num2)


# ─────────────────────────────────────────────────────────────────────────────
# 2. PUNTAJE Z
# ─────────────────────────────────────────────────────────────────────────────

# Fórmula: z = (x - media) / sd

# --- Ejercicio 1 -------------------------------------------------------------
# Una encuesta del Ministerio de Salud encontró que el promedio de horas de
# ejercicio físico semanal es 1.5 con SD = 1.
# ¿Qué porcentaje de jóvenes practica MENOS de una hora a la semana?

media <- 1.5
sd    <- 1
valor <- 1

z <- (valor - media) / sd
z

# Probabilidad asociada (área a la izquierda del valor)
pnorm(z)

# Interpretación: el ___% de los jóvenes practica menos de una hora a la semana.


# --- Ejercicio 2 -------------------------------------------------------------
# El gobierno entregará un bono al 10% de familias con mayor puntaje en la
# ficha social. Variable normal con media = 250 y SD = 48.
# ¿Desde qué puntaje hacia arriba obtienen el bono?

media <- 250
sd    <- 48

# Buscamos el valor que deja el 10% en la cola superior
puntaje_corte <- qnorm(0.90, mean = media, sd = sd)
puntaje_corte

# Interpretación: las familias con puntaje >= ___ recibirían el bono.


# --- Ejercicio 3 -------------------------------------------------------------
# Puntajes de un examen de historia: media = 80, SD = 6.
# ¿Cuál es el puntaje z de un estudiante que obtuvo 75?

media <- 80
sd    <- 6
valor <- 75

z <- (valor - media) / sd
z

# Interpretación: el estudiante está ___ desviaciones estándar por debajo de la media.


# ─────────────────────────────────────────────────────────────────────────────
# 3. INTERVALOS DE CONFIANZA PARA MEDIAS
# ─────────────────────────────────────────────────────────────────────────────

# Fórmula: IC = x_barra ± t(α/2) * (s / sqrt(n))

# Valores t para muestras grandes (n > 120):
#   90% confianza  → t = 1.645
#   95% confianza  → t = 1.96
#   99% confianza  → t = 2.58


# --- Ejemplo: Variable Edad --------------------------------------------------

# Estadísticos descriptivos
summary(cep$edad)
sd(cep$edad, na.rm = TRUE)

# n sin casos perdidos
n_edad <- sum(!is.na(cep$edad))
n_edad

# Cálculo paso a paso al 95% de confianza
media_edad <- mean(cep$edad, na.rm = TRUE)
sd_edad    <- sd(cep$edad, na.rm = TRUE)
n_edad     <- sum(!is.na(cep$edad))

# Error estándar
se_edad <- sd_edad / sqrt(n_edad)
se_edad

# Valor crítico
t_critico <- 1.96

# Margen de error
me_edad <- t_critico * se_edad
me_edad

# Límites del intervalo
li_edad <- media_edad - me_edad
ls_edad <- media_edad + me_edad

cat("IC al 95%: [", round(li_edad, 2), ",", round(ls_edad, 2), "]\n")

# Cálculo automático con t.test()
t.test(x = cep$edad, conf.level = 0.95)
t.test(x = cep$edad, conf.level = 0.99)

# Interpretación: con un 95% de confianza, la edad promedio de los chilenos/as
# se encuentra entre ___ y ___ años.


# --- Ejercicio 4 -------------------------------------------------------------
# Calcule e interprete el IC al 90% y al 99% para la variable edad.
# ¿Qué ocurre con la amplitud del intervalo al aumentar el nivel de confianza?

t.test(x = cep$edad, conf.level = 0.90)
t.test(x = cep$edad, conf.level = 0.99)

# PREGUNTA: ¿Cuál intervalo es más amplio? ¿Por qué ocurre esto?


# --- Ejercicio 5 -------------------------------------------------------------
# Calcule el IC al 95% para OTRA variable continua de la base CEP.
# Reporte: estimador puntual, error estándar, margen de error, IC e interpretación.

# Escriba aquí su código:
# summary(cep$_____)
# sd(cep$_____, na.rm = TRUE)
# t.test(x = cep$_____, conf.level = 0.95)


# --- Ejercicio 6 (Desafío) ---------------------------------------------------
# Calcule el IC al 95% para edad separado por zona urbana y rural.
# ¿Los intervalos se solapan? ¿Qué conclusión podemos extraer?

# Filtramos por zona (1 = Urbano, 2 = Rural)
edad_urbana <- cep %>% filter(zona_u_r == 1) %>% pull(edad)
edad_rural  <- cep %>% filter(zona_u_r == 2) %>% pull(edad)

# IC para zona urbana
t.test(x = edad_urbana, conf.level = 0.95)

# IC para zona rural
t.test(x = edad_rural, conf.level = 0.95)

# PREGUNTAS:
# 1. ¿Cuál es la edad promedio en zona urbana y en zona rural?
# 2. ¿Los intervalos se solapan? ¿Qué nos dice eso sobre las diferencias entre grupos?
# 3. ¿Qué limitaciones tiene este análisis?


################################################################################
#                              FIN DEL SCRIPT                                  #
################################################################################
