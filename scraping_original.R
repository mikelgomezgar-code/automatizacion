#ESCRAPEO----
#1.vamos a trabajar con rvest----
##1.1 cargamos rvest----
library(rvest)
library(tidyverse)
library(dplyr)
library(stringr)
##1.2. extraemos tabla de encuestas desde un sitio web----
url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Spanish_general_election"
encuesta <-  session(url) |>
  read_html()

encuesta
tabla <- encuesta |>
  html_table()

tabla_1 <- tabla[[1]]
tabla_2 <- tabla[[2]]
tabla_3 <- tabla[[3]]


#LIMPIEZA DE TABLAS----
#2 TRabajamos sobre la tabla 1----
#tengo que cargar lubridate
library(lubridate)
##2.1 elimminar fila 1----
tabla_1 <- tabla_1[-1,]

##2.2 cambiamos nombres de columnas----

names(tabla_1)[5:18] <- c("pp", "psoe", "vox", "sumar", "erc", "junts", "eh_bildu", "pnv", "bng", "cc", "upn", "podemos", "SALF", "AC")
tabla_1 <- rename(tabla_1, encuestadora = `Polling firm/Commissioner`,
                  fecha = `Fieldwork date`,
                  tamaño_muestra = `Sample size`)
##2.3eliminamos columnas "Turnout"----

tabla_1 <- tabla_1 %>%
  select(-Turnout)

##2.4 eliminamos caracteres no numericos a partir de la columna 5----
tabla_1 <- tabla_1 %>%
  mutate(across(3:ncol(tabla_1), ~ as.numeric(gsub("[^0-9.-]", "", .x))))


##2.5 convertimos desde columna 5 en numericas----
tabla_1 <- tabla_1 %>%
  mutate(across(3:ncol(tabla_1), as.numeric))

##2.6 eliminamos los valores que corresponden a escaños columna a columna----
tabla_1 <- tabla_1%>%
  mutate(pp = substr(pp, 1, 4))
tabla_1 <- tabla_1%>%
  mutate(psoe = substr(psoe, 1, 4))
tabla_1 <- tabla_1%>%
  mutate(vox = substr(vox, 1, 4))
tabla_1 <- tabla_1%>%
  mutate(sumar = substr(sumar, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(erc = substr(erc, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(junts = substr(junts, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(eh_bildu = substr(eh_bildu, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(pnv = substr(pnv, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(bng = substr(bng, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(cc = substr(cc, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(upn = substr(upn, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(podemos = substr(podemos, 1, 3))
tabla_1 <- tabla_1%>%
  mutate(SALF = substr(SALF, 1, 3))
tabla_1 <- tabla_1 %>%
  mutate(AC = substr(AC, 1, 3))

##2.7 limpiamos la columna fecha----
###eliminamos los caracteres antes de -
tabla_1 <- tabla_1 %>%
  mutate(fecha = str_replace(fecha, ".*–", ""))

### convertimos a formato fecha deseado
tabla_1 <- tabla_1 %>%
  mutate(
    fecha = str_replace_all(fecha, "[[:space:]]+", " "),
    fecha = str_replace_all(fecha, "\u00A0", " "),
    fecha = str_trim(fecha),
    fecha = paste0(fecha, " 2026"),
    fecha = str_replace_all(fecha, "Sept", "Sep"),
    fecha = suppressWarnings(parse_date_time(fecha, orders = c("d b Y", "d B Y"))),
    fecha = as.Date(fecha)
  )


##2.8 limpiamos columna encuestadora----
tabla_1 <- tabla_1 %>%
  mutate(encuestadora = str_remove_all(encuestadora, "\\[.*?\\]"))
##2.9 cambiamos el nombre al DF----
encuestas_2026 <- tabla_1

#3 trabajamos sobre tabla 2----
##3.1 elimminar fila 1----
tabla_2 <- tabla_2[-1,]

##3.2 cambiamos nombres de columnas----
names(tabla_2)[5:18] <- c("pp", "psoe", "vox", "sumar", "erc", "junts", "eh_bildu", "pnv", "bng", "cc", "upn", "podemos", "SALF", "AC")
tabla_2 <- rename(tabla_2, encuestadora = `Polling firm/Commissioner`,
                  fecha = `Fieldwork date`,
                  tamaño_muestra = `Sample size`)

##3.3eliminamos columnas "Turnout"----

tabla_2 <- tabla_2 %>%
  select(-Turnout)

##3.4 eliminamos caracteres no numericos a partir de la columna 3----
tabla_2 <- tabla_2 %>%
  mutate(across(3:ncol(tabla_2), ~ as.numeric(gsub("[^0-9.-]", "", .x))))

##3.5 convertimos desde columna 3 en numericas----
tabla_2 <- tabla_2 %>%
  mutate(across(3:ncol(tabla_1), as.numeric))

##3.6 eliminamos los valores que corresponden a escaños ciolumna a columna----
tabla_2 <- tabla_2%>%
  mutate(pp = substr(pp, 1, 4))
tabla_2 <- tabla_2%>%
  mutate(psoe = substr(psoe, 1, 4))
tabla_2 <- tabla_2%>%
  mutate(vox = substr(vox, 1, 4))
tabla_2 <- tabla_2%>%
  mutate(sumar = substr(sumar, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(erc = substr(erc, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(junts = substr(junts, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(eh_bildu = substr(eh_bildu, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(pnv = substr(pnv, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(bng = substr(bng, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(cc = substr(cc, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(upn = substr(upn, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(podemos = substr(podemos, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(SALF = substr(SALF, 1, 3))
tabla_2 <- tabla_2%>%
  mutate(AC = substr(AC, 1, 3))

##3.7 limpiamos la columna fecha----
###eliminamos los caracteres antes de ----
tabla_2 <- tabla_2 %>%
  mutate(fecha = str_replace(fecha, ".*–", ""))

### convertimos a formato fecha deseado----
tabla_2 <- tabla_2 %>%
  mutate(
    fecha = str_replace_all(fecha, "[[:space:]]+", " "),
    fecha = str_replace_all(fecha, "\u00A0", " "),
    fecha = str_trim(fecha),
    fecha = paste0(fecha, " 2025"),
    fecha = str_replace_all(fecha, "Sept", "Sep"),
    fecha = suppressWarnings(parse_date_time(fecha, orders = c("d b Y", "d B Y"))),
    fecha = as.Date(fecha)
  )



##3.8 limpiamos columa encuestadora----
tabla_2 <- tabla_2 %>%
  mutate(encuestadora = str_remove_all(encuestadora, "\\[.*?\\]"))
##3.9 cambiamos el nombre al DF----
encuestas_2025 <- tabla_2

#4 trabajamos sobre tabla 3----
##4.1 elimminar fila 1----
tabla_3 <- tabla_3[-1,]

which(names(tabla_3) == "" | is.na(names(tabla_3)))

##3.2 cambiamos nombres de columnas----
names(tabla_3)[5:17] <- c("pp", "psoe", "vox", "sumar", "erc", "junts", "eh_bildu", "pnv", "bng", "cc", "upn", "podemos", "SALF")
tabla_3 <- rename(tabla_3, encuestadora = `Polling firm/Commissioner`,
                  fecha = `Fieldwork date`,
                  tamaño_muestra = `Sample size`)

##3.3eliminamos columnas "Turnout"----

tabla_3 <- tabla_3 %>%
  select(-Turnout)

##3.4 eliminamos caracteres no numericos a partir de la columna 3----
tabla_3 <- tabla_3 %>%
  mutate(across(3:ncol(tabla_3), ~ as.numeric(gsub("[^0-9.-]", "", .x))))

##3.5 convertimos desde columna 3 en numericas----
tabla_3 <- tabla_3 %>%
  mutate(across(3:ncol(tabla_3), as.numeric))

##3.6 eliminamos los valores que corresponden a escaños ciolumna a columna----
tabla_3 <- tabla_3%>%
  mutate(pp = substr(pp, 1, 4))
tabla_3 <- tabla_3%>%
  mutate(psoe = substr(psoe, 1, 4))
tabla_3 <- tabla_3%>%
  mutate(vox = substr(vox, 1, 4))
tabla_3 <- tabla_3%>%
  mutate(sumar = substr(sumar, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(erc = substr(erc, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(junts = substr(junts, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(eh_bildu = substr(eh_bildu, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(pnv = substr(pnv, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(bng = substr(bng, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(cc = substr(cc, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(upn = substr(upn, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(podemos = substr(podemos, 1, 3))
tabla_3 <- tabla_3%>%
  mutate(SALF = substr(SALF, 1, 3))


##3.7 limpiamos la columna fecha----
###eliminamos los caracteres antes de ----
tabla_3 <- tabla_3 %>%
  mutate(fecha = str_replace(fecha, ".*–", ""))

### convertimos a formato fecha deseado----
tabla_3 <- tabla_3 %>%
  mutate(
    fecha = str_replace_all(fecha, "[[:space:]]+", " "),
    fecha = str_replace_all(fecha, "\u00A0", " "),
    fecha = str_trim(fecha),
    fecha = paste0(fecha, " 2024"),
    fecha = str_replace_all(fecha, "Sept", "Sep"),
    fecha = suppressWarnings(parse_date_time(fecha, orders = c("d b Y", "d B Y"))),
    fecha = as.Date(fecha)
  )



##3.8 limpiamos columa encuestadora----
tabla_3 <- tabla_3 %>%
  mutate(encuestadora = str_remove_all(encuestadora, "\\[.*?\\]"))
##3.9 cambiamos el nombre al DF----
encuestas_2024 <- tabla_3

#FUSIÓN DE TABLAS LIMPIAS----
#4 Unimos las encuestas----
encuestas <- bind_rows(encuestas_2026, encuestas_2025, encuestas_2024)


#VISUALIZACIÓN----
#13 Trabajamos la visualización----
#13.1 Instalamosl y cargamos librerias para visualizar----

library(ggplot2)
library(plotly)
library(tibble)



##13.2 Antes hay que longear por porcentajes----
encuestas_long <- encuestas %>%
  pivot_longer(cols = pp:AC, 
               names_to = "partido", values_to = "porcentaje")
##13.3 Organizamos datos----

encuestas_long <- encuestas_long %>%
  filter(!is.na(porcentaje)) %>%      
  arrange(partido, fecha)   

encuestas_long <- encuestas_long %>%
  mutate(porcentaje = as.numeric(porcentaje))

##13.3.1 Cambiamos nombres de partidos----
encuestas_long <- encuestas_long %>%
  mutate(
    partido = recode(
      partido,
      "pp" = "PP",
      "psoe" = "PSOE",
      "bng" = "BNG",
      "cc" = "CC",
      "eh_bildu" = "EH Bildu",
      "erc" = "ERC",
      "junts" = "Junts",
      "pnv" = "PNV",
      "podemos" = "Podemos",
      "sumar" = "Sumar",
      "vox" = "VOX",
      "upn" = "UPN",
      "SALF" = "SALF",
      "AC" = "AC"
    )
  )



##13.4 Empezamos con el codigo del gráfico----

###13.4.1Antes vamos a generar una lista con los colores de los partidos----
colores_partidos <- 
  c("PP" = "#21599e",
    "PSOE" = "#f31912",
    "VOX" = "#7cbd2a",
    "Sumar" = "#e51c55",
    "ERC" = "#f95838",
    "Junts" = "#00c3b2",
    "EH Bildu" = "#00c19f",
    "PNV"= "#499e37",
    "BNG" = "#558ab6",
    "CC" = "#33bbff",
    "UPN" = "#f03131",
    "Podemos" = "#9169f4",
    "SALF" = "#71523f",
    "AC" = "#0f4c81")

###Vamos a generar una lista con orden de loos partidos----

orden_partidos <- c(
  "PSOE", "PP", "VOX",
  "Sumar", "Podemos",
  "PNV", "EH Bildu",
  "ERC", "Junts",
  "BNG", "CC", "UPN", "SALF", "AC"
)

###Vamos a generar una lista de hitos a apuntar (versión Beta)----
hitos <- tibble(
  evento = c("Imputación Cerdán", "Incendios Castilla y Leon", "DANA Valencia",
             "Casos de acoso PSOE", "Escandalo mamografías", "Accidente Adamuz", "Elecciones Extremadura"),
  fecha = as.Date(c("2025-06-05", "2025-08-10", "2024-10-29", "2025-12-12", "2025-10-01",
                    "2026-01-18", "2025-12-21")),
  y = c(44, 48, 46, 42, 40,48, 46)
  )

p <- ggplot(
  encuestas_long,
  aes(
    x = fecha,
    y = porcentaje,
    color = factor(partido, levels = orden_partidos),
    fill  = factor(partido, levels = orden_partidos),
    group = factor(partido, levels = orden_partidos),
  text = paste0(
    "<b>", partido, "</b>",
    "<br>Fecha: ", format(fecha, "%d-%m-%Y"),
    "<br>Porcentaje: ", sprintf("%.1f", porcentaje), "%"
  )
  )
)+
  geom_point(alpha = 0.3) +
  geom_smooth(se = TRUE, span = 0.1, linewidth = 0.6, alpha = 0.18) +
  labs(title = "Evolución de las encuestas", x = "Fecha", y = "Porcentaje", color = "Partido") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(limits = c(0, 50))+
  scale_color_manual(values = colores_partidos,
                     labels = c("pp" = "PP",
                                "psoe" = "PSOE",
                                "bng" = "BNG",
                                "cc" = "CC",
                                "eh_bildu" = "EH Bildu",
                                "erc" = "ERC",
                                "junts" = "Junts",
                                "pnv" = "PNV",
                                "podemos" = "Podemos",
                                "sumar" = "Sumar",
                                "vox" = "VOX",
                                "upn" = "UPN")
                     ) +
  geom_vline(
    data = hitos,
    aes(xintercept = fecha),
    linetype = "dotted",
    linewidth = 0.2,
    inherit.aes = FALSE,
    color = "#222222"
  ) +
  geom_text(
    data = hitos,
    aes(x = fecha, y = y, label = evento),
    angle = 90,
    vjust = -0.4,
    hjust = 0,
    size = 3.2,
    inherit.aes = FALSE,
    color = "#222222",
    family = "PP Neue Montreal"
  ) +
  scale_fill_manual(values = colores_partidos)+
  theme_minimal() +
  theme(legend.position = "right")
p

pl <- ggplotly(p, tooltip = "text") %>% #Usa la estetica de ggplot
  style(hoverinfo = "text") %>% #Ignora x, y, color, etc. Muestra solo lo que definiremos
  layout(
    font = list(
      family = "PP Neue Montreal, Arial, sans-serif",
      size = 13,
      color = "#222222"
    ), #valores por defecto en caso de que algo no se defina
    
    title = list(
      text = "Evolución de las encuestas",
      x = 0.5,
      xanchor = "center",
      font = list(family = "PP Neue Montreal, Arial, sans-serif", size = 22)
    ), #Titulo del grafico
    
    xaxis = list(
      title = list(text = "Fecha", font = list(family = "PP Neue Montreal", size = 15)),
      tickfont = list(family = "PP Neue Montreal", size = 12),
      tickangle = -45
    ), #Eje X
    
    yaxis = list(
      title = list(text = "Porcentaje", font = list(family = "PP Neue Montreal", size = 15)),
      tickfont = list(family = "PP Neue Montreal", size = 12),
      range = c(0, 50)
    ), #eje Y
    
    legend = list(
      orientation = "h",
      x = 0.5, xanchor = "center",
      y = -0.28, yanchor = "top",
      font = list(family = "PP Neue Montreal", size = 12),
      title = list(text = "Partido")
    ), #Leyenda
    
    hoverlabel = list(
      namelength = 0,
      font = list(family = "PP Neue Montreal", size = 12),
      align = "left"
    ), #caja del popoup
    
    margin = list(l = 70, r = 30, t = 80, b = 120)
  ) #margenes

pl

