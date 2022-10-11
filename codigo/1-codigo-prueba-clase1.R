
# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - OCTUBRE 2022
# PROFESOR: FELIPE RUIZ

# CARGAR BASE DE DATOS
datos <- read.csv2("datos/1-paraguay.csv")

# GUARDAR BASE DE DATOS EN FORMATO R (EN CARPETA DATOS)

saveRDS(datos, file = "datos/1-paraguay.rds")


# GUARDAR RESULTADO DE UN ANÁLISIS (EN CARPETA RESULTADOS)
resultado1 <- summary(datos$edad)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(resultado1, file = "resultados/1-ejemplo-resultado.xlsx")
