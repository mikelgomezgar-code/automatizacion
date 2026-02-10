#MEDIMOS LA PERCEPCIÓN DE PELIGRO DE DROGAS e3n 2023----

library(dplyr)
library(stringr)
library(tidyverse)

#1. cargamos csv----
per_cocaina <- read.csv("C:/Proyectos_R/drogas_eitb/src/per_cocaina.csv",
                        sep = ";", fileEncoding = "UTF-8", header = TRUE)
per_hachis <- read.csv("C:/Proyectos_R/drogas_eitb/src/per_hachis_mes.csv",
                       sep = ";", fileEncoding = "UTF-8", header = TRUE)
per_opioides <- read.csv("C:/Proyectos_R/drogas_eitb/src/per_opioides.csv",
                       sep = ";", fileEncoding = "UTF-8", header = TRUE)
per_sedantes <- read.csv("C:/Proyectos_R/drogas_eitb/src/per_sedantes.csv",
                       sep = ";", fileEncoding = "UTF-8", header = TRUE)
per_tabaco <- read.csv("C:/Proyectos_R/drogas_eitb/src/per_tabaco.csv",
                       sep = ";", fileEncoding = "UTF-8", header = TRUE)

#2. Seleccionemos la columans que queremos----
per_cocaina <- per_cocaina %>%
  select(Categorías, Indicador.CAS, Total)
per_hachis <- per_hachis %>%
  select(Categorías, Indicador.CAS, Total)
per_opioides <- per_opioides %>%
  select(X, X.1, Ambos.sexos.3)
per_sedantes <- per_sedantes %>%
  select(X, X.1, Ambos.sexos.3)
per_tabaco <- per_tabaco %>%
  select(X, X.1, Ambos.sexos.3)

#en opioides sedantes y tabaco hay que cambiar nombres de columnas y eliminar fila 1
per_opioides <- per_opioides %>%
  rename(Categorías = X,
         indicador = X.1,
         Total = Ambos.sexos.3)
per_opioides <- per_opioides %>%
  slice(-1)

per_sedantes <- per_sedantes %>%
  rename(Categorías = X,
         indicador = X.1,
         Total = Ambos.sexos.3)
per_sedantes <- per_sedantes %>%
  slice(-1)

per_tabaco <- per_tabaco %>%
  rename(Categorías = X,
         indicador = X.1,
         Total = Ambos.sexos.3)
per_tabaco <- per_tabaco %>%
  slice(-1)


#A cocaina y hachis hay que cambiar nombre de columna indicador----
per_cocaina <- per_cocaina %>%
  rename(indicador = Indicador.CAS)
per_hachis <- per_hachis %>%
  rename(indicador = Indicador.CAS)


#3. unificamos DF----
riesgo <- bind_rows(per_cocaina, per_hachis, per_opioides, per_sedantes, per_tabaco)


#4. cambiamos nombresw de valores para simplificarlos----

riesgo <- riesgo %>%
  mutate(
    indicador = case_when(
      indicador == "Consumir cocaína u otras drogas ilegales una vez o menos al mes" ~ "Cocaina",
      indicador == "Fumar hachís o marihuana (cannabis)una vez o menos al mes" ~ "Cannabis",
      indicador == "Consumir analgésicos opioides una vez por semana o más" ~ "Opioides",
      indicador == "Tomar tranquilizantes / sedantes o somníferos una vez por semana o más" ~"Sedantes",
      indicador == "Fumar un paquete de tabaco diario" ~ "Tabaco",
      TRUE ~ indicador
    )
  )

#5. tenemos que widear categorias----
riesgo <- riesgo %>%
  pivot_wider(names_from = Categorías,
              values_from = Total)

#6. exportamos----

write.csv(riesgo, "C:/Proyectos_R/drogas_eitb/output/riesgo.csv")
