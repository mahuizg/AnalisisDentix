library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(purrr)   # para iterar y mapear funciones
library(forcats) # para manejar factores (categóricos)
library(here)  


# conectamos a la base
conn <- dbConnect(RSQLite::SQLite(), "dentix_ult.sqlite")
tables <- dbListTables(conn)
sql_query <- "SELECT * FROM raw_data" # seleccionamos todo de la tabla unificada
df_full <- dbGetQuery(conn, sql_query)
dbDisconnect(conn)
print(paste("registros:", nrow(df_full)))


# separación de variables

# excluimos 'transaction_id'
categorical_vars <- df_full %>%
  select(where(is.character), -any_of("transaction_id")) %>%
  names()

numeric_vars <- df_full %>%
  select(where(is.numeric), -any_of("transaction_id")) %>%
  names()

output_dir <- here("graficos_univariados")
dir.create(output_dir)

vars_to_log <- c("activos", "cuota_credito", "cuota_mensual", 
                   "gastos_sostenimiento", "ingresos_fijos", "pasivos",
                   "monto_desembolso", "saldo_capital", "saldo_vencido", "total_egresos")


# GRAFICOSSSS

# 1. histogramas
walk(numeric_vars, function(var_name) {

  plot_data <- df_full
  plot_title <- paste("Distribución de", var_name)
  plot_x_label <- var_name
  plot_aes <- aes(x = .data[[var_name]])
  plot_subtitle <- NULL
  plot_zoom_limit <- NULL # variable para almacenar el límite del zoom (si aplica)

  # transformación logarítmica
  if (var_name %in% vars_to_log) {
    
    # filtramos NAs y valores negativos
    plot_data <- df_full %>% 
      filter(!is.na(.data[[var_name]]), .data[[var_name]] >= 0)
    
    # usamos log1p() que es log(x + 1) para manejar los ceros de forma segura
    plot_aes <- aes(x = log1p(.data[[var_name]])) 
    plot_title <- paste("Distribución de", var_name)
    plot_x_label <- paste(var_name)
    plot_subtitle <- "Valores transformados con log(x+1)"

  # truncamiento para todas las demás
  } else {
    # calculamos el percentil 99 para truncar las colas largas
    limit_val <- quantile(plot_data[[var_name]], 0.99, na.rm = TRUE)
    
    # solo aplicamos el límite si es un valor válido y mayor a 0
    if (!is.na(limit_val) && limit_val > 0) {
      plot_zoom_limit <- limit_val
    }
  }

  # construimos los histogramas
  plot_hist_freq <- ggplot(plot_data, plot_aes) +
    geom_histogram(
      bins = 33, 
      fill = "cornflowerblue",
      alpha = 0.9,
      color = "aliceblue"
    ) +
    labs(
      title = plot_title,
      x = plot_x_label,
      y = "Frecuencia", 
      subtitle = plot_subtitle
    ) +
    theme_minimal()

  # aplicar el zoom si se calculó
  if (!is.null(plot_zoom_limit)) {
    plot_hist_freq <- plot_hist_freq + 
      coord_cartesian(xlim = c(NA, plot_zoom_limit)) # zoom de 0 hasta 'limit_val'
  }
  ggsave(
    filename = file.path(output_dir, paste0(var_name,".jpg")), plot = plot_hist_freq, width = 8, height = 6, dpi = 150)
})

# 2. bar plots
mapeo <- c(
  "Actividades de apoyo a la educación." = "Educación",
  "Comercio al por mayor a cambio de una retribución o por contrata." = "Comercio mayorista",
  "Otros tipos de comercio al por menor no realizado en establecimientos, puestos de venta o mercados." = "Comercio minorista",
  "Otras actividades de servicios personales n.c.p." = "Servicios personales",
  "Actividades de apoyo a la agricultura." = "Agricultura",
  "Rentistas de Capital, solo para personas naturales." = "Rentista de capital",
  "Actividades no diferenciadas de los hogares individuales como productores de bienes para uso propio." = "Producción de bienes"
)

# lo aplicamos
if ("actividad_economica" %in% names(df_full)) {
  df_full <- df_full %>%
    mutate(
      actividad_economica = recode(
        actividad_economica, 
        !!!mapeo, 
        .default = actividad_economica # mantiene las demás sin cambios
      )
    )
}

walk(categorical_vars, function(var_name) {
  
  # contamos y agrupamos el top de clases más frecuentes y el resto en otros
  data_plot <- df_full %>%
    count(.data[[var_name]], sort = TRUE, name = "count") %>%
    mutate(
      "{var_name}" := fct_lump_n(
        factor(.data[[var_name]]), 
        n = 20, 
        other_level = "Otros",
        w = count 
      )
    ) %>%
    group_by(.data[[var_name]]) %>%
    summarise(count = sum(count), .groups = 'drop') %>%
    filter(!is.na(.data[[var_name]])) # omitimos nulos

  # construimos el bar plot
  plot_barplot <- ggplot(
      data_plot, 
      # reorder() ordena el eje x basado en count
      aes(x = reorder(.data[[var_name]], -count), y = count)
    ) +
    geom_bar(stat = "identity", fill = "cornflowerblue", alpha = 0.9) +
    geom_text(aes(label = count), vjust = -0.5, size = 3) + 
    labs(
      title = paste("Frecuencia de", var_name),
      x = var_name,
      y = "Conteo"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1) 
    )
  
  # guardamos
  ggsave(
    filename = file.path(output_dir, paste0(var_name, ".jpg")),
    plot = plot_barplot,
    width = 11, height = 8, 
    dpi = 150
  )
})

