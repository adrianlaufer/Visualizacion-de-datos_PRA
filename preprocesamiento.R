library(dplyr)

residuos <- read.csv('residuos.csv', sep = ";")

# Eliminamos los primeros números
residuos$Comunidades.y.Ciudades.Autónomas <- gsub("[0-9]", "", residuos$Comunidades.y.Ciudades.Autónomas)

# Convertimos en numero
residuos$Total <- gsub(",", "", residuos$Total)
residuos$Total <- as.numeric(residuos$Total)

# ELiminamos los numeros de Tipo de resiudos
residuos$Tipo.de.residuo <- gsub("[0-9.]", "", residuos$Tipo.de.residuo)

# Eliminamos la columna Total.Nacional
residuos <- subset(residuos, select = -Total.Nacional)

residuos <- residuos %>% rename(Periodo = periodo)

poblacion <- read.csv('poblacion.csv', sep = ";")

poblacion$Comunidades.y.Ciudades.Autónomas <- gsub("[0-9]", "", poblacion$Comunidades.y.Ciudades.Autónomas)

poblacion <- poblacion %>% filter(Tamaño.de.los.municipios == "Total")

poblacion <- subset(poblacion, select = -Tamaño.de.los.municipios)
poblacion$Total <- gsub("\\.", "", poblacion$Total)
poblacion$Total <- as.numeric(poblacion$Total)

poblacion <- poblacion %>% rename(Poblacion = Total)

datos <- merge(residuos, poblacion, by = c("Comunidades.y.Ciudades.Autónomas", "Periodo"))

datos <- datos %>% rename(Comunidad = Comunidades.y.Ciudades.Autónomas)

datos <- datos %>% rename(Residuo = Tipo.de.residuo)
datos$Residuo <- gsub("^[^A-Z]+", "", datos$Residuo)
datos$Comunidad <- gsub("^[^A-Z]+", "", datos$Comunidad)
datos$Porcentaje_por_poblacion <- round(100* datos$Total / datos$Poblacion,2)

datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Asturias, Principado de", "Principado de Asturias"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "País Vasco", "País Vasco/Euskadi"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Rioja, La", "La Rioja"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Navarra, Comunidad Foral de", "Comunidad Foral de Navarra"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Balears, Illes", "Illes Balears"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Cataluña", "Cataluña/Catalunya"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Madrid, Comunidad de", "Comunidad de Madrid"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Navarra, Comunidad Foral de", "Comunidad Foral de Navarra"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Castilla - La Mancha", "Castilla-La Mancha"))
datos <- datos %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Murcia, Región de", "Región de Murcia"))



pib <- read.csv('pib.csv', sep = ",")

pib <- pib %>% filter(pib$MEDIDAS_CODE == "GDP_PRODUCTION_PC")

pib <- pib %>% rename(Comunidad = TERRITORIO.es)
pib <- pib %>% rename(Periodo = TIME_PERIOD.es)
pib <- pib %>% rename(PIB_PC = OBS_VALUE)

pib <- subset(pib, select=c(Comunidad, Periodo, PIB_PC))

pib <- pib %>%
  filter(Comunidad != "Resto de España", Comunidad != "España", Periodo >= 2010)

pib <- pib %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Illes Balears / Islas Baleares", "Illes Balears"))
pib <- pib %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Cataluña", "Cataluña/Catalunya"))
pib <- pib %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Castilla - La Mancha", "Castilla-La Mancha"))
pib <- pib %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "País Vasco", "País Vasco/Euskadi"))
pib <- pib %>% 
  mutate(Comunidad = replace(Comunidad, Comunidad == "Comunidad Valenciana", "Comunitat Valenciana"))

datos <- merge(datos, pib, by = c("Comunidad", "Periodo"))


write.csv(datos, "datos.csv", row.names = FALSE)
