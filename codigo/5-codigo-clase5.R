# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - NOVIEMBRE 2022
# PROFESOR: FELIPE RUIZ

# ---- 0. PAQUETES A UTILIZAR ---- 

# Evaluar instalación y/o carga de paquetes
pacman::p_load(tidyverse, haven, car, labelled)

# (*) Pasos de análisis ya realizados (no los aplicaremos ahora)

# ---- 1. CARGA DE BASE DE DATOS (DESDE SPSS) (*)  ----

casen_2017 <- read_sav("datos/Casen 2017.sav")

# ---- 2. PREPARACIÓN DE DATOS PARA ANÁLISIS ----

# ---- Crear objeto que contenga sólo provincia de santiago (*)

casen_santiago <- filter(casen_2017, provincia == 131)

# Según libro de códigos son 31.868 casos

# ---- Seleccionar variables de interés (*)

casen_santiago <- select(casen_santiago, comuna, cuidado_enfermos = r7a,
                         ciudado_dependientes = r7b, vehiculo = r7c,
                         dinero = r7d, tramites = r7e, tecnologias = r7f,
                         reparaciones_hogar = r7g, trabajo = r7h,
                         consejos = r7i, idiomas = r7j, estudios = r7k)

# ---- Guardar base en formato RDS (*)

saveRDS(casen_santiago, file = "datos/5-casen_santiago.rds")

# ---- Cargar base de Casen recortada
casen_santiago <- readRDS("datos/5-casen-santiago.rds")

# ---- Recodificar variables para construir indicador común

# Ver tipo de objeto y categorías de variable comuna
class(casen_santiago$comuna)
table(casen_santiago$comuna)

# Transformación variable comuna desde SPSS a formato 'character'
casen_santiago <- mutate(casen_santiago, 
                         comunaf = labelled::to_character(casen_santiago$comuna,
                                                         levels = "labels",
                                                       drop_unused_labels = T))
# Elminamos etiqueta de variable
var_label(casen_santiago$comunaf) <- NULL

# Verificamos tabla con etiquetas
table(casen_santiago$comunaf)

# Recodificación módulo "r7" CASEN
# sección R: Identidades, redes y participación
# módulo de preguntas r7: ¿Alguien en su hogar, conoce a una persona que...?
# 1. Sí, alguien fuera del hogar / 2. Sí, alguien dentro del hogar / 3. Ambas
# 4. No conoce / 9. No sabe
names(casen_santiago) # Detectar columnas de interés (2 a 12)
casen_santiago[,c(2:12)] # Observar columnas de interés

# ---- Recodificación sistemática variables de módulo "r"

# "mutuate_at" y "pipe" de "tidyverse" permite aplicar instrucciones encadenadas 
# sobre varias variables

# Primero, se codifican como numéricas todas las variables.
# Segundo, se recodifican sus valores dejando como 2 los valores "no" conoce/sabe
# Tercero, se eliminan todos los casos con NA dejando sólo a respondientes efectivos (jefes de hogar)

df2 <- casen_santiago %>%
  mutate_at(vars(2:12), ~as.numeric(.)) %>%
  mutate_at(vars(2:12), funs(car::recode(. ,"1:3=1;4:9=2"))) %>%
  drop_na()

View(df2)

# ---- Reducir dimensionalidad calculando variables a nivel comuna.

# El sentido del código se aprecia analizando la primera línea del 'summarise'
# Esto todavía se podría automatizar más, pero implicaría programación funcional (¿otro curso?).
# Dimensionalidad se reduce, calculando cada variable como proporción de categoría de referencia

comunas <- df2 %>%
  group_by(comunaf) %>%
  summarise(enfermos = sum(cuidado_enfermos == "1")/length(cuidado_enfermos),
            dependientes = sum(ciudado_dependientes == "1")/length(ciudado_dependientes),
            vehiculo = sum(vehiculo == "1")/length(vehiculo),
            dinero = sum(dinero == "1")/length(dinero),
            tramites = sum(tramites == "1")/length(tramites),
            tecnologias = sum(tecnologias == "1")/length(tecnologias),
            reparaciones_hogar = sum(reparaciones_hogar == "1")/length(reparaciones_hogar),
            trabajo = sum(trabajo == "1")/length(trabajo),
            consejos = sum(consejos == "1")/length(consejos),
            idiomas = sum(idiomas == "1")/length(idiomas),
            estudios = sum(estudios == "1")/length(estudios))

View(comunas)

# ---- 3. ANÁLISIS DE CONGLOMERADOS - MÉTODO JERÁRQUICO ----

#Se usará la distancia euclidea: hay más distancias (ver ?dist)
#Se usarán variables no estandarizadas pues ya están como % (columnas 2:12)

# Cálculo de distancias (euclídea al cuadrado) con todas las variables incluídas
d <- dist(comunas[c(2:12)])^2 # distancia euclídea al cuadrado (^2) 
# Error posible (teórico): no tenemos una predefinición de por qué importa cada variable

# Mostrar matriz de distancia
print(d,digits=1)


## Creación del MODELO JERARQUICO: "hclust" --> Hierarquical Clustering
# hay más algoritmos: ver ?hclust -> centroid, single, ward.D, etc.
# Se crea un modelo indicando matriz de distancias y algoritmo de clasificación.

hc <- hclust(d, method= "ward.D") # hc = 'hirarchical clustering' (algoritmo aglomerativo)
                                  # método de clasificación: "distancia de Ward"
                                  # Resultado se guarda como objeto 'lista' en entorno

# RESULTADO 1: historial de conglomeración
historial <- data.frame(hc[2:1]) # extracción historial de conglomeración
write.csv2(historial, file ="resultados/5-historial_hc.csv")
# Salto de mayor relevancia parece estar en solución de 3
# Elementos negativos son casos no conglomerados hasta ese paso (detección de atípicos).
View(historial)

# RESULTADO 2: dendograma

plot(hc, labels=comunas$comunaf, main = "Dendograma método jerárquico",
     xlab = "Comuna",
     ylab = "Distancia") 

# dendograma con grupos marcados
rect.hclust (hc, k=3, border="red") 

# RESULTADO 3: extraer conglomerado de pertenencia

#Para extraer conglomerado de pertenencia
pertenencia_hc <- cutree(hc, k=3) #Corte de grupos "k"

# Tabla de cantidad de casos por conglomerado
table(pertenencia_hc) 

# Agregar solución de 3 conglomerados como variable de clasificación en base
comunas <- mutate(comunas, hc_3 = pertenencia_hc)
names(comunas)
View(comunas)

## RESULTADO 4: perfiles variables conglomeración según comunas (medias)
hc <- comunas %>%
  group_by(hc_3) %>%
  summarise(enfermos = mean(enfermos),
            dependientes = mean(dependientes),
            vehiculo = mean(vehiculo),
            dinero = mean(dinero),
            tramites = mean(tramites),
            tecnologias = mean(tecnologias),
            reparaciones_hogar = mean(reparaciones_hogar),
            trabajo = mean(trabajo),
            consejos = mean(consejos),
            idiomas = mean(idiomas),
            estudios = mean(estudios))

# imprimir tabla de caracterización de variables
write.csv2(hc, file = "resultados/5-medias-variables-conglomeración-j.csv")

#Remover todo excepto "comunas" del entorno

# ---- 4. ANÁLISIS DE CONGLOMERADOS - MÉTODO NO JERÁRQUICO ---- 

## Creación del MODELO NO JERARQUICO: 
#Algoritmo K-Means, es muy utilizado, hay otros

# nstart= escoge el mejor punto de partida(aleat)
#         *poner num elevado
# centers= cantidad de conglomerados k
# iter.max= iteraciones/ pasos

km<-kmeans(comunas[2:12], centers=3, iter.max = 10, nstart = 200)

#Ver todos los resultados
km
km$iter #cantidad de interaciones
km$size #tamaño conglomerados
km$centers #centros conglomerados finales
km$cluster #conglomerado de pertenencia por caso

pertenencia_nh <-cbind(comunas[1], km[1]) #tabla con conglomerad de pertencia y comunas
View(pertenencia_nh)

#Agregar solución no jerárquica como variable de clasificación.
comunas <- mutate(comunas, km_3 = km$cluster)

# Variables de conglomeración en modelo no jerárquico
km$centers #centros conglomerados finales

# Exportar resultados a planilla
write.csv2(km$centers, file = "resultados/5-medias-variables-conglomeración-nj.csv")

# ----- GUARDAR BASE DE DATOS CON VARIABLES DE CONGLOMERACIÓN

saveRDS(comunas, file = "datos/5-cluster_comunas_casen.rds")
