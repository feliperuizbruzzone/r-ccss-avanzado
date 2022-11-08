# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - OCTUBRE 2022
# PROFESOR: FELIPE RUIZ

# ---- 1. INSTALAR Y CARGAR PAQUETES A UTILIZAR, CARGAR DATOS ----

# DETALLE PAQUETES
# ca: específico para análisis de correspondencias
# haven: sirve para leer SPSS y otros formatos de bases de datos
# dplyr: sirve para manipular bases de datos
# ggplot2: construcción de gráficos avanzados

# Evaluar instalación y/o carga de paquetes
pacman::p_load(ca, haven, dplyr, summarytools, ggplot2)

# ---- CARGAR BASE DE DATOS

# Cargar base original desde formato SPSS
# datos <- read_sav("datos/4-PNUD_2015.sav") no lo usaremos pues en proyecto hay base en formato RDS

# Cargar base en formato R (.rds)
datos <- readRDS("datos/4-PNUD_2015.rds")

# Seleccionar variables de interés
PNUD <- select(datos, cambios, conflictos, manifestaciones, involucramiento, edad, NSE)

#SUPUESTOS: Asociación entre variables (hacer tablas de contingencias para verificarlo
#previamente, para este caso asumiremos asociación)

#SUPUESTOS ACS y ACM : Variables categóricas o cualitativas (cuantitativas recodificadas)

#IMPORTANTE.
# 1. Recodificaremos las variables de interés como factor.
# 2. Modificaremos las etiquetas originales para adecuarlas al mapa.
# 3. Ejecutaremos el análisis con ese sub objeto.

# ---- 2. VALIDAR VARIABLES: RECODIFICAR Y CONFIGURAR COMO FACTOR ----

# ---- Revisar variables 

summary(PNUD) # Función básica
dfSummary(PNUD) # Función de "summarytools"

# ----- Recodificaciones
# Sentido de factorizar, y edición de categorías de resupuesta

PNUD$cambios<-factor(PNUD$cambios,levels=c(1,2,3),labels=c("Radicales","Graduales","Sin"))

PNUD$conflictos<-factor(PNUD$conflictos,levels=c(1,2),labels=c("Mostrar","Evitar"))

PNUD$manifestaciones<-factor(PNUD$manifestaciones,levels=c(1,2,3),labels=c("Positivas","Negativas", "Neutras"))

PNUD$involucramiento<-factor(PNUD$involucramiento,levels=c(1,2,3,4,5),
                        labels=c("Retraídos","Observadores","Ritualistas",
                                 "Comprometidos/Involucrados","Colectivistas"))

PNUD$edad<-factor(PNUD$edad,levels=c(1,2,3),labels=c("Jóvenes","Adultos","Adultos mayores"))

PNUD$NSE<-factor(PNUD$NSE,levels=c(1,2,3,4),labels=c("ABC1","C2","C3","D y E"))

# ---- Verificar clase de variables en entorno

# ---- Revisar recodificación 

summary(PNUD) # Función básica
dfSummary(PNUD) # Función de "summarytools"

# ---- 3. ANÁLISIS DE CORRESPONDENCIA MÚLTIPLES ---- 

# ---- Utilizaremos el comando "mjca" del paquete "ca"

# NOTA: "mjca" usado sin especificación emplea por defecto la opción ajustada 
# ("adjusted") de la matriz de Burt. Este nos entrega una parte de los estadísticos resumen 
# y las puntuaciones de cada categoría en las dos dimensiones a graficar. 
# Permite determinar la importancia de las categorías con las dimensiones del  plano

mjca(PNUD)


# El comando "summary" proporciona la información de las dimensiones articulando 
# los autovalores con el porcentaje de varianza explicada y un gráfico de 
# sedimentación.
# Luego, por cada fila y columna proporciona: 
# i) la coordenada en la dimensión (k=1, k=2), 
# ii)la correlación al cuadrado ("cor") y 
# iii)la contribución ("ctr") a cada dimensión

summary(mjca(PNUD))

#Luego para generar el mapa percepctual ejecutamos el comando "plot"
plot(mjca(PNUD))

# Ambos resultados parecen ser insuficientes en términos de formato

# ---- 4. EXPORTAR RESULTADOS PARA ANÁLISIS ----

# Guardar tabla de resumen como objeto 
resultados <- summary(mjca(PNUD)) # Observar lista de elementos en entorno

# Exportar tabla como archivo de planilla CSV hacia carpeta "resultados" del proyecto
write.csv2(resultados$columns, file = "resultados/4-resultados_acm.csv")

# ----5. EDITAR MAPA PERCEPTUAL ----

# Mejoramos el gráfico para hacerlo presentable y claro. Utilizamos otro sistema de gráficos en R: ggplot2

# Creamos un objeto del ACM y creamos los nombres de sus categorías para hacer una base de datos con las coordenadas
mca <- mjca(PNUD)
cats <- apply(PNUD, 2, function(x) nlevels(as.factor(x)))

mca_df <- data.frame(mca$colcoord, Variable = rep(names(cats), cats))
rownames(mca_df) = mca$levelnames

# Hacemos el gráfico con ggplot2 (la lógica es distinta a  los gráficos del paquete base)

p<- ggplot(data = mca_df, aes(x = X1, y = X2, label = rownames(mca_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(x = X1, y = X2)) +
  theme(axis.text.x=element_text(size=7), axis.text.y=element_text(size=7))+
  theme_bw()
p 

# Quitamos la leyenda, fijamos los ejes en el mismo rango para hacer simétrico el mapa, etiquetamos los ejes

p + guides(col="none") + geom_point(aes(x = X1, y = X2), shape = 16, size = 2) + 
  geom_text(aes(colour = Variable), size=5, hjust = 0, nudge_y = 0.06) +
  xlim(-2.5,2.5) + ylim(-3,3) + xlab("D1   47,8%") + ylab("D2   8,6%")


