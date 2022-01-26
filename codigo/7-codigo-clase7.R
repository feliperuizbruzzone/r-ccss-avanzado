# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - ENERO 2022
# PROFESOR: FELIPE RUIZ

# ---- 0. PAQUETES A UTILIZAR ---- 

### Evaluar instalación y/o carga de paquetes
pacman::p_load(psych, MVN, GPArotation, semPlot, tidyverse)

# ---- 1. LECTURA BASE DE DATOS AD HOC -----

## Cargar base sólo con variables de interés
datos <- read.csv2("datos/7-baseAFE.csv")

## Explorar datos
summary(datos)

## Recodificar escala para asignar mayor número a valoración "esencial"
## Asignar casos perdidos y eliminar casos con info incompleta (se pierden 16 casos)
datos <- datos %>%
  mutate_at(vars(1:10), funs(car::recode(. ,"5=1;4=2;3=3;2=4;1=5;88=NA;99=NA"))) %>%
  drop_na()

## Verificar datos
summary(datos)

## Observar base
View(datos)

# ---- 2. COMPROBACIÓN DE SUPUESTOS ESTADÍSTICOS -----

## ---- Normalidad multivariante y univariante

## Test de Mardia.
mvn <- mvn(datos) # MultiVariant Normality

## Imprimir tabla 
write.csv2(mvn$Descriptives, file="resultados/7-distribucion.csv")

## Solicitar test con gráficas 
mvn(datos, univariatePlot = "histogram", 
    multivariatePlot = "qq")

## Colinealidad.
correlaciones <- cor(datos)
matriz_cor <- as.data.frame(correlaciones)
correlaciones
write.csv2(matriz_cor, file = "resultados/7-correlaciones.csv")

## Multicolinealidad
det(correlaciones)
kmo<-KMO(datos) # aplicar prueba y guardar resultados como objeto
kmo # Visualizar resultados
## Imprimir indicador por variable
write.csv2(kmo$MSAi, file = "resultados/7-MSA-univariado.csv")

# ---- 3. CONSTRUCCIÓN DEL MODELO -----

## ---- Determinar el número de factores a extraer
scree(datos, pc = F) #Se elimina la graficación de componente principal

## --- CÁLCULO MODELO 2 factores (no rotado)
m1 <- fa(datos,fm="ml", nfactors=2, rotate="none")
print(m1)

## Autovalores modelo no rotado
m1$e.values

# Cargas factoriales modelo no rotado
m1$loadings

## Comunalidades modelo no rotado
m1$communalities

## --- CÁLCULO MODELO 2 FACTORES (rotado, método varimax)
m1rot <- fa(datos,fm="ml", nfactors=2, rotate="varimax")
print(m1rot)

## Autovalores modelo rotado
m1rot$e.values
write.csv2(m1rot$e.values, file = "resultados/7-autovalores.csv")

## Cargas factoriales modelo rotado
m1rot$loadings
write.csv2(m1rot$loadings, file = "resultados/7-cargas-factoriales-rotadas.csv")

## Comunalidades modelo rotado
m1rot$communalities
write.csv2(m1rot$communalities, file = "resultados/7-comunalidades.csv")

## Diagrama modelo dos factores rotados
fa.diagram(m1rot, e.size = 0.1, rsize = 0.3)
