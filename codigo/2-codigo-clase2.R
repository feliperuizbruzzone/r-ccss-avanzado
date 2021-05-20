# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - MAYO 2021
# PROFESOR: FELIPE RUIZ

# ---- 0. PAQUETES A UTILIZAR ---- 

# Ejecutar en  modo off-line
# install.packages(c("haven","srvyr","dplyr"))

library(haven)
library(srvyr)
library(dplyr)

# ---- 1. CARGA DE BASE DE DATOS (DESDE SPSS)  ----

casen_2017 <- read_sav("datos/Casen 2017.sav")

casen_2017 <- readRDS("datos/2-casen.RDS") # Alternativa ante no lectura SPSS


# ---- 2. CÁLCULO INDICADOR POBREZA MULTIDIMENSIONAL: NIVEL MUESTRAL ----

table(casen_2017$pobreza_multi_5d) # 1 = 44.972 // Variable original, coincide con libro de códigos

casen_2017$pobreza_multi_5d <- as.factor(casen_2017$pobreza_multi_5d)


# ¿Nos sirve este valor para conocer la situación nacional?

# ---- 3. CÁLCULO INDICADOR POBREZA MULTIDIMENSIONAL: NIVEL POBLACIONAL ----

#DEFINIR MUESTRA COMPLEJA PARA DATOS DE NIVEL NACIONAL/REGIONAL
casen_pond <- casen_2017  %>% as_survey_design(ids = 1, strata = varstrat, weights = expr)

#Definir variables CASEN: expr (factor de expansión regional), varstrat (error estratos de varianza)
#Definir argmentos as_survey_desig: id (Formula or data frame specifying cluster ids from largest level to smallest level, ~0 or ~1 is a formula for no clusters), 
#strata (Formula or vector specifying strata), weights (Formula or vector specifying sampling weights as an alternative to prob),

#CÁLCULO POBREZA MULTI5D NACIONAL
pobrezamulti <- casen_pond %>% 
  group_by(pobreza_multi_5d) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # Variable original
# Pobres multidimensionales
# Estimación de punto ponderada 19,828% (*)- Intervalo de confianza --> Li: 19,607% - Ls: 20,048%
# (*) Coincide con Cifra expandida de pobres multidimensionales publicada por CASEN: 3.530.889
# Coincide número oficial con cálculo propio: tasa es 20,7 (total = pobres+no pobres) y absoluto 3.530.889
3530889/(3530889+13529811) 


# DISEÑO MUESTRAL SIN INCORPORAR ERROR DE CADA ESTRATO
casen_pond2 <- casen_2017 %>% as_survey_design(ids = 1, weights = expr)

# Pobreza multidimensional: frecuencias expandidas + proporción ponderada + IC proporción NC-95% + n muestral
pobrezamulti2 <- casen_pond2 %>% 
  group_by(pobreza_multi_5d) %>%
  summarize(frecuencias = survey_total(),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
# Pobres multidimensionales
# Estimación de punto ponderada 19,828% (*)- Intervalo de confianza --> Li: 19,603% - Ls: 20,053%
### INTERVALOS CALCULADOS SON DIFERENTES


# ----- 4. CONSTRUIR BASE PARA UTILIZAR OFF LINE

casen_seleccion <- select(casen_2017, pobreza_multi_5d, expr, expc, varstrat, region, comuna)

# Para guardar CASEN completa en formato RDS (habiendo cargado desde SPSS)
saveRDS(casen_2017, file = "datos/2-casen_2017.RDS")

# Guardar CASEN sólo con variables de interés para análisis en formato RDS
saveRDS(casen_seleccion, file = "datos/2-casen_pobreza.RDS")