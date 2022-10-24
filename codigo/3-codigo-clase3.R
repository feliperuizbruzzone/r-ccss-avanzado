# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - OCTUBRE 2022
# PROFESOR: FELIPE RUIZ

# ---- 0. PAQUETES A UTILIZAR ----

install.packages("pacman")
pacman::p_load(readxl, haven, tidyverse)

# ---- 1. RECOMBINACIÓN DE BASES DE DATOS ----

#_________ CARGAR BASES ORIGINALES_______________
# Iremos editando en objetos nuevos para preparar fusión
# Base "datos" a secas, se irá sobre escribiendo a partir de llave. 
# Al final la nombraremos de forma definitiva

#Cargar base de datos SIMCE octavo por establecimiento 
datos <- read_excel("datos/3-simce2m2017_rbd_publica_final.xlsx")

# Sobreescribimos seleccionando variables de interés: 
# promedio puntaje lenguaje, gse, rol en base datos (ID) y zona.
datos <- select(datos, lenguaje=prom_lect2m_rbd, gse=cod_grupo, llave=rbd,
                zona=cod_rural_rbd)

#Cargar base de datos SIMCE indicadores promedio desarrollo personal por establecimiento
datos2 <- read_excel("datos/3-idps2m2017_rbd_final.xlsx")

# Sobreescribir para seleccionar variables de interés a fusionar
# codigo region (cod_reg_rbd), codigo comuna (cod_com_rbd),
# y código dependencia (cod_depe2)
# Definir mismo nombre para llave (rbd).

datos2 <- select(datos2, llave = rbd, region = cod_reg_rbd, comuna = cod_com_rbd, dependencia = cod_depe2)

#Cargar base de datos SIMCE por estudiante
datos3 <- read_spss("datos/3-simce nivel estudiante.sav")

# Ver diferencias en estructura y tamaño de las bases de datos

#Seleccionar variables a fusionar, rbd (llave), la media de "puedo hacer las tareas y trabajos difíciles" (cest_p0_01)
#                      la media de "puntaje matemáticas" (ptje_mate2m_alu)
datos3 <- select(datos3, llave = rbd, matematicas =  ptje_mate2m_alu, dificiles = "cest_p01_01")

#_________ FUSIONAR DATOS A NIVEL COLEGIO _______________

#Agregar desde datos2 a "simce2M2017_colegio" codigo region (cod_reg_rbd), codigo comuna (cod_com_rbd),
# y código dependencia (cod_depe2)

#left_join mantiene la cantidad más grande de casos, y rellena casos sin equivalencia con NA.
#Si no se especifica argumento "by" la función usa las variables de nombre común. 
#Para no usarlo, asegurar que variables de nombre igual sean exclusivamente llave.

datos_A <- left_join(datos, datos2)
datos_A <- left_join(datos, datos2, by = "llave")

#inner_join mantiene sólo los casos efectivamente compartidos
datos_B <- inner_join(datos, datos2)
datos_B <- inner_join(datos, datos2, by = "llave")

#_________ FUSIONAR DATOS A NIVEL COLEGIO DESDE NIVEL ESTUDIANTE _______________

#Redimensionar base datos3 (estudiantes) según variable llave
# Reescalar variables, la media de "puedo hacer las tareas y trabajos difíciles" (cest_p0_01) por establecimiento
#                      la media de "puntaje matemáticas" (ptje_mate2m_alu) por establecimiento

# Presentación de un "pipe" y principio de "no repetir código"

colegio <- datos3 %>% 
  group_by(llave) %>%  
  summarize(media_mates = mean(matematicas, na.rm = T), 
            media_dificiles = mean(dificiles, na.rm = T))

# Ver similitud en cantidad de observaciones!

#Dejar atributos de llave igual a las otras bases de datos (numeric y no atomic)
colegio$llave <- as.numeric(colegio$llave)

#_________ CONSTRUIR Y GUARDAR BASE DEFINITIVA _______________

#Fundir "datos_A" (datos+datos2) con "colegio" (datos3 reescalada)

simce2017_colegio <- left_join(datos_A, colegio)

# Guardar base de datos  reescalada como archivo de datos R (formato RDS más útil para datos que .RData)

saveRDS(simce2017_colegio, file = "datos/3-simce2017colegio.rds")

