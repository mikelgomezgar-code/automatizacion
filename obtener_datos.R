# Script de automatización para GitHub Actions
# Solo obtiene y guarda datos, sin generar gráficos

# Cargar librerías necesarias
library(rvest)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)

# Función para obtener y limpiar las encuestas
obtener_encuestas <- function() {
  
  # SCRAPING
  url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Spanish_general_election"
  
  tryCatch({
    encuesta <- session(url) |>
      read_html()
    
    tabla <- encuesta |>
      html_table()
    
    tabla_1 <- tabla[[1]]
    tabla_2 <- tabla[[2]]
    tabla_3 <- tabla[[3]]
    
    # LIMPIEZA TABLA 1 (2026)
    tabla_1 <- tabla_1[-1,]
    names(tabla_1)[5:18] <- c("pp", "psoe", "vox", "sumar", "erc", "junts", "eh_bildu", "pnv", "bng", "cc", "upn", "podemos", "SALF", "AC")
    tabla_1 <- rename(tabla_1, 
                      encuestadora = `Polling firm/Commissioner`,
                      fecha = `Fieldwork date`,
                      tamaño_muestra = `Sample size`)
    tabla_1 <- tabla_1 %>% select(-Turnout)
    tabla_1 <- tabla_1 %>%
      mutate(across(3:ncol(tabla_1), ~ as.numeric(gsub("[^0-9.-]", "", .x))))
    
    # Limpiar valores de partidos
    partidos <- c("pp", "psoe", "vox", "sumar", "erc", "junts", "eh_bildu", 
                  "pnv", "bng", "cc", "upn", "podemos", "SALF", "AC")
    for(p in partidos) {
      max_chars <- ifelse(p %in% c("pp", "psoe", "vox"), 4, 3)
      tabla_1[[p]] <- substr(tabla_1[[p]], 1, max_chars)
    }
    
    # Limpiar fecha
    tabla_1 <- tabla_1 %>%
      mutate(
        fecha = str_replace(fecha, ".*–", ""),
        fecha = str_replace_all(fecha, "[[:space:]]+", " "),
        fecha = str_replace_all(fecha, "\u00A0", " "),
        fecha = str_trim(fecha),
        fecha = paste0(fecha, " 2026"),
        fecha = str_replace_all(fecha, "Sept", "Sep"),
        fecha = suppressWarnings(parse_date_time(fecha, orders = c("d b Y", "d B Y"))),
        fecha = as.Date(fecha)
      )
    
    tabla_1 <- tabla_1 %>%
      mutate(encuestadora = str_remove_all(encuestadora, "\\[.*?\\]"))
    
    encuestas_2026 <- tabla_1
    
    # LIMPIEZA TABLA 2 (2025)
    tabla_2 <- tabla_2[-1,]
    names(tabla_2)[5:18] <- c("pp", "psoe", "vox", "sumar", "erc", "junts", "eh_bildu", "pnv", "bng", "cc", "upn", "podemos", "SALF", "AC")
    tabla_2 <- rename(tabla_2, 
                      encuestadora = `Polling firm/Commissioner`,
                      fecha = `Fieldwork date`,
                      tamaño_muestra = `Sample size`)
    tabla_2 <- tabla_2 %>% select(-Turnout)
    tabla_2 <- tabla_2 %>%
      mutate(across(3:ncol(tabla_2), ~ as.numeric(gsub("[^0-9.-]", "", .x))))
    
    for(p in partidos) {
      max_chars <- ifelse(p %in% c("pp", "psoe", "vox"), 4, 3)
      tabla_2[[p]] <- substr(tabla_2[[p]], 1, max_chars)
    }
    
    tabla_2 <- tabla_2 %>%
      mutate(
        fecha = str_replace(fecha, ".*–", ""),
        fecha = str_replace_all(fecha, "[[:space:]]+", " "),
        fecha = str_replace_all(fecha, "\u00A0", " "),
        fecha = str_trim(fecha),
        fecha = paste0(fecha, " 2025"),
        fecha = str_replace_all(fecha, "Sept", "Sep"),
        fecha = suppressWarnings(parse_date_time(fecha, orders = c("d b Y", "d B Y"))),
        fecha = as.Date(fecha)
      )
    
    tabla_2 <- tabla_2 %>%
      mutate(encuestadora = str_remove_all(encuestadora, "\\[.*?\\]"))
    
    encuestas_2025 <- tabla_2
    
    # LIMPIEZA TABLA 3 (2024)
    tabla_3 <- tabla_3[-1,]
    names(tabla_3)[5:17] <- c("pp", "psoe", "vox", "sumar", "erc", "junts", "eh_bildu", "pnv", "bng", "cc", "upn", "podemos", "SALF")
    tabla_3 <- rename(tabla_3, 
                      encuestadora = `Polling firm/Commissioner`,
                      fecha = `Fieldwork date`,
                      tamaño_muestra = `Sample size`)
    tabla_3 <- tabla_3 %>% select(-Turnout)
    tabla_3 <- tabla_3 %>%
      mutate(across(3:ncol(tabla_3), ~ as.numeric(gsub("[^0-9.-]", "", .x))))
    
    partidos_2024 <- c("pp", "psoe", "vox", "sumar", "erc", "junts", "eh_bildu", 
                       "pnv", "bng", "cc", "upn", "podemos", "SALF")
    for(p in partidos_2024) {
      max_chars <- ifelse(p %in% c("pp", "psoe", "vox"), 4, 3)
      tabla_3[[p]] <- substr(tabla_3[[p]], 1, max_chars)
    }
    
    tabla_3 <- tabla_3 %>%
      mutate(
        fecha = str_replace(fecha, ".*–", ""),
        fecha = str_replace_all(fecha, "[[:space:]]+", " "),
        fecha = str_replace_all(fecha, "\u00A0", " "),
        fecha = str_trim(fecha),
        fecha = paste0(fecha, " 2024"),
        fecha = str_replace_all(fecha, "Sept", "Sep"),
        fecha = suppressWarnings(parse_date_time(fecha, orders = c("d b Y", "d B Y"))),
        fecha = as.Date(fecha)
      )
    
    tabla_3 <- tabla_3 %>%
      mutate(encuestadora = str_remove_all(encuestadora, "\\[.*?\\]"))
    
    encuestas_2024 <- tabla_3
    
    # FUSIONAR TABLAS
    encuestas <- bind_rows(encuestas_2026, encuestas_2025, encuestas_2024)
    
    return(encuestas)
    
  }, error = function(e) {
    message("Error al obtener datos: ", e$message)
    return(NULL)
  })
}

# Obtener datos nuevos
message("Obteniendo datos de encuestas...")
datos_nuevos <- obtener_encuestas()

if (!is.null(datos_nuevos)) {
  
  # Verificar si hay datos antiguos
  archivo_datos <- "datos/encuestas.csv"
  
  if (file.exists(archivo_datos)) {
    datos_antiguos <- read_csv(archivo_datos, show_col_types = FALSE)
    
    # Comparar si hay cambios
    if (!identical(datos_nuevos, datos_antiguos)) {
      write_csv(datos_nuevos, archivo_datos)
      message("✓ Datos actualizados correctamente")
      message("Total de encuestas: ", nrow(datos_nuevos))
    } else {
      message("○ No hay cambios en los datos")
    }
  } else {
    # Primera ejecución
    write_csv(datos_nuevos, archivo_datos)
    message("✓ Datos guardados por primera vez")
    message("Total de encuestas: ", nrow(datos_nuevos))
  }
  
  # Guardar fecha de última actualización
  fecha_actualizacion <- data.frame(
    ultima_actualizacion = Sys.time(),
    total_encuestas = nrow(datos_nuevos)
  )
  write_csv(fecha_actualizacion, "datos/ultima_actualizacion.csv")
  
} else {
  message("✗ No se pudieron obtener los datos")
  quit(status = 1)
}