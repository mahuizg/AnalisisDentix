
create_df <- function() {
  library(vcd)
  library(RSQLite)
  library(dplyr)
  library(ggplot2) 
  library(scales)
  library(here)
  library(FactoMineR) 

  conn <- dbConnect(RSQLite::SQLite(), "dentix_ult.sqlite")
  tables <- dbListTables(conn)
  sql_query <- "SELECT * FROM raw_data;" 
  df <- dbGetQuery(conn, sql_query)
  dbDisconnect(conn)

  #df <- df %>%
    #mutate(
     # ratio_cuota_ingreso = cuota_mensual / (ingresos_fijos + 1)
    #)

  # excluir columnas
  # df <- df %>% select(-clinica, -comercial, -transaction_id) 

  df$plazo <- case_when(
    df$plazo %in% c(3, 4) ~ "3",
    df$plazo == 6 ~ "6",
    df$plazo == 9 ~ "9",
    df$plazo == 12 ~ "12",
    df$plazo == 18 ~ "18",
    df$plazo == 24 ~ "24",
    df$plazo == 30 ~ "30",
    df$plazo == 36 ~ "36",
    df$plazo == 48 ~ "48",
    df$plazo == 60 ~ "60",
    TRUE ~ as.character(df$plazo)
  )
  df$plazo <- as.factor(df$plazo)

  breaks_tasa <- quantile(df$tasa, probs = c(0, 0.333, 0.666, 1), na.rm = TRUE)
  df$tasa <- cut(
    df$tasa, 
    breaks = breaks_tasa, 
    labels = c("Baja", "Media", "Alta"), 
    include.lowest = TRUE 
  )

  breaks_act <- quantile(df$tiempo_actividad, probs = 0:4/4, na.rm = TRUE)
  df$tiempo_actividad <- cut(
    df$tiempo_actividad, 
    breaks = breaks_act, 
    labels = c("Rookie", "Intermedio", "Moderado", "Veterano"), 
    include.lowest = TRUE
  )

  breaks_res <- quantile(df$tiempo_residencia, probs = 0:4/4, na.rm = TRUE)
  df$tiempo_residencia <- cut(
    df$tiempo_residencia, 
    breaks = breaks_res, 
    labels = c("Rookie", "Intermedio", "Moderado", "Veterano"), 
    include.lowest = TRUE
  )

  df <- df %>%
    mutate(
      cuota_credito_bin = as.factor(ifelse(cuota_credito > 0, "Si", "No")),
      activos_bin = as.factor(ifelse(activos > 0, "Si", "No")),
      pasivos_bin = as.factor(ifelse(pasivos > 0, "Si", "No"))
    )

  # transformación logarítmica a las variables financieras
  vars_to_log <- c("activos", "cuota_credito", "cuota_mensual", 
                  "gastos_sostenimiento", "ingresos_fijos", "monto_desembolso", 
                  "pasivos", "saldo_capital", "saldo_vencido", "total_egresos",
                  "ratio_cuota_ingreso") 
  vars_to_log <- intersect(vars_to_log, names(df))
  df <- df %>%
    mutate(
      across(all_of(vars_to_log), ~ log1p(as.numeric(.)))
    )

  mapeo <- c(
    "Actividades de apoyo a la educación." = "Educación",
    "Comercio al por mayor a cambio de una retribución o por contrata." = "Comercio mayorista",
    "Otros tipos de comercio al por menor no realizado en establecimientos, puestos de venta o mercados." = "Comercio minorista",
    "Otras actividades de servicios personales n.c.p." = "Servicios personales",
    "Actividades de apoyo a la agricultura." = "Agricultura",
    "Rentistas de Capital, solo para personas naturales." = "Rentista de capital",
    "Actividades no diferenciadas de los hogares individuales como productores de bienes para uso propio." = "Producción de bienes"
  )

  if ("actividad_economica" %in% names(df)) {
    df <- df %>%
      mutate(
        actividad_economica = dplyr::recode(
          actividad_economica, 
          !!!mapeo, 
          .default = actividad_economica
        )
      )
  }

  df <- df %>%
    mutate(
      across(where(is.character), as.factor)
    )

  mca_vars <- c("tipo_contrato", "actividad_economica", "ocupacion")

  mca_data_prepared <- df %>%
    select(all_of(mca_vars), transaction_id) %>% # metemos el id
    na.omit()

  mca_res <- MCA(
    mca_data_prepared[, mca_vars],  # obvio no le aplicamos mca al id
    graph = FALSE
  )

  # creamos un df con los ids y los scores de la dimensión 1
  mca_scores_df <- data.frame(
    transaction_id = mca_data_prepared$transaction_id, # usamos el id para saber a qn le pertenece
    estabilidad = mca_res$ind$coord[, 1] 
  )

  # unimos los scores de vuelta al df original usando transaction_id
  df <- left_join(df, mca_scores_df, by = "transaction_id")
  # print(summary(df$estabilidad))

  return(df)
}

df <- create_df()
