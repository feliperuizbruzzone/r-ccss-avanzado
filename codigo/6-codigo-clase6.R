# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - ENERO 2021
# PROFESOR: FELIPE RUIZ
# ---- 0. PAQUETES A UTILIZAR ---- 

# Evaluar instalación y/o carga de paquetes
pacman::p_load(tidyverse, haven, texreg)

# El único nuevo es "texreg"

## (*) Pasos de preparación de datos ya realizados

# ---- 1. CARGA DE BASE DE DATOS (DESDE SPSS) (*)  ----

casen_2017 <- read_sav("data/Casen 2017.sav")

# ---- 2. PREPARACIÓN DE DATOS PARA ANÁLISIS ----

# ---- Crear objeto que contenga sólo personas que reciben ingresos salariales (*)

casen_regresion <- filter(casen_2017, y1 > 0)

# ---- Seleccionar variables de interés (*)

casen_regresion <- select(casen_regresion, ingreso = y1,
                          edad, escolaridad = esc, educ_madre = r12a,
                          educ_padre = r12b)

# ---- Guardar como base de datos en formato RDS para análisis (*)

saveRDS(casen_regresion, file = "data/6-casen-regresion.rds")

# ---- Cargar base desde formato RDS

casen <- readRDS("datos/6-casen-regresion.rds")

# Recodificar estableciendo variables como numéricas, asignando casos perdidos y dejando sólo casos completos
# No corresponde a ingreso, edad, ni escolaridad, solo educ_madre y padre
casen_regresion <- casen %>%
  mutate_at(vars(1:5), ~as.numeric(.)) %>%
  mutate_at(vars(1,4:5), funs(car::recode(. ,"77 = NA; 99 = NA"))) %>%
  drop_na()

summary(casen_regresion)

# ---- 3. ANÁLISIS DE REGRESIÓN MÚLTIPLE - MÉTODO DE LOS MÍNIMOS CUADRADOS ----

correlaciones  <- cor(casen_regresion)

write.csv2(as.data.frame(correlaciones), file = "resultados/6-correlaciones.csv")

## Incorporación parcial de variables

r1 <- lm(ingreso ~ escolaridad, 
         data = casen_regresion)

r2 <- lm(ingreso ~ escolaridad + edad,
         data = casen_regresion)

r3 <- lm(ingreso ~ escolaridad +  edad + educ_padre,
         data = casen_regresion)

r4 <- lm(ingreso ~ escolaridad +  edad + educ_padre + educ_madre,
         data = casen_regresion)

# Resultados básicos con el paquete R base
summary(r4)

# ---- 4. RESULTADOS CON FORMATO Y COMPARATIVOS ----

# Combinamos modelos como una lista
modelos<-list(r1,r2,r3,r4)

# Podemos configurar los resultados para que comas sean decimales
options(OutDec= ",") 

# Función 'screenreg' permite "imprimir" una tabla de resultados más editada
screenreg(modelos,
          custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),  
          custom.coef.names = c("Constante", "Escolaridad", "Edad", "Educación padre", "Educación madre"))


# Función 'htmlreg' permite "imprimir" tabla de resultados editada a un documento
htmlreg(modelos,
          custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),  
          custom.coef.names = c("Constante", "Escolaridad", "Edad", "Educación padre", "Educación madre"),
        file = "resultados/6-tabla-modelos.doc")
