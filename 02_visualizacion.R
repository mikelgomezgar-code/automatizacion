# ============================================
# SCRIPT 02: VISUALIZACIÓN DE ENCUESTAS
# Genera gráfico interactivo con Plotly
# ============================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(readr)

message("📊 Iniciando generación de gráficos...")

# 1. LEER DATOS ----
encuestas <- read_csv("datos/encuestas.csv", show_col_types = FALSE)

if (nrow(encuestas) == 0) {
  stop("❌ No hay datos para graficar")
}

message("✓ Datos cargados: ", nrow(encuestas), " encuestas")

# 2. TRANSFORMAR A FORMATO LARGO ----
encuestas_long <- encuestas %>%
  pivot_longer(
    cols = pp:AC, 
    names_to = "partido", 
    values_to = "porcentaje"
  ) %>%
  filter(!is.na(porcentaje)) %>%
  arrange(partido, fecha) %>%
  mutate(porcentaje = as.numeric(porcentaje))

# 3. CAMBIAR NOMBRES DE PARTIDOS ----
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

# 4. DEFINIR COLORES Y ORDEN ----
colores_partidos <- c(
  "PP" = "#21599e",
  "PSOE" = "#f31912",
  "VOX" = "#7cbd2a",
  "Sumar" = "#e51c55",
  "ERC" = "#f95838",
  "Junts" = "#00c3b2",
  "EH Bildu" = "#00c19f",
  "PNV" = "#499e37",
  "BNG" = "#558ab6",
  "CC" = "#33bbff",
  "UPN" = "#f03131",
  "Podemos" = "#9169f4",
  "SALF" = "#71523f",
  "AC" = "#0f4c81"
)

orden_partidos <- c(
  "PSOE", "PP", "VOX",
  "Sumar", "Podemos",
  "PNV", "EH Bildu",
  "ERC", "Junts",
  "BNG", "CC", "UPN", "SALF", "AC"
)

# 5. DEFINIR HITOS ----
hitos <- tibble(
  evento = c("Imputación Cerdán", "Incendios Castilla y León", "DANA Valencia",
             "Casos de acoso PSOE", "Escándalo mamografías", "Flotilla de la libertad"),
  fecha = as.Date(c("2025-06-05", "2025-08-10", "2024-10-29", 
                    "2025-12-12", "2025-10-01", "2025-10-03")),
  y = c(44, 48, 46, 42, 40, 46)
)

# 6. CREAR GRÁFICO BASE ----
p <- ggplot(
  encuestas_long,
  aes(
    x = fecha,
    y = porcentaje,
    color = factor(partido, levels = orden_partidos),
    fill = factor(partido, levels = orden_partidos),
    group = factor(partido, levels = orden_partidos),
    text = paste0(
      "<b>", partido, "</b>",
      "<br>Fecha: ", format(fecha, "%d-%m-%Y"),
      "<br>Porcentaje: ", sprintf("%.1f", porcentaje), "%"
    )
  )
) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = TRUE, span = 0.1, linewidth = 0.6, alpha = 0.18) +
  labs(
    title = "Evolución de las encuestas", 
    x = "Fecha", 
    y = "Porcentaje", 
    color = "Partido"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(limits = c(0, 50)) +
  scale_color_manual(values = colores_partidos) +
  scale_fill_manual(values = colores_partidos) +
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
    color = "#222222"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# 7. CONVERTIR A PLOTLY ----
pl <- ggplotly(p, tooltip = "text") %>%
  style(hoverinfo = "text") %>%
  layout(
    font = list(
      family = "Arial, sans-serif",
      size = 13,
      color = "#222222"
    ),
    title = list(
      text = "Evolución de las encuestas electorales en España",
      x = 0.5,
      xanchor = "center",
      font = list(family = "Arial, sans-serif", size = 22)
    ),
    xaxis = list(
      title = list(text = "Fecha", font = list(family = "Arial", size = 15)),
      tickfont = list(family = "Arial", size = 12),
      tickangle = -45
    ),
    yaxis = list(
      title = list(text = "Porcentaje", font = list(family = "Arial", size = 15)),
      tickfont = list(family = "Arial", size = 12),
      range = c(0, 50)
    ),
    legend = list(
      orientation = "h",
      x = 0.5, xanchor = "center",
      y = -0.28, yanchor = "top",
      font = list(family = "Arial", size = 12),
      title = list(text = "Partido")
    ),
    hoverlabel = list(
      namelength = 0,
      font = list(family = "Arial", size = 12),
      align = "left"
    ),
    margin = list(l = 70, r = 30, t = 80, b = 120)
  )

# 8. GUARDAR GRÁFICO ----
htmlwidgets::saveWidget(
  pl, 
  "graficos/evolucion_encuestas.html",
  selfcontained = TRUE,
  title = "Evolución Encuestas España"
)

message("✅ Gráfico guardado en: graficos/evolucion_encuestas.html")
message("📈 Total de observaciones: ", nrow(encuestas_long))
