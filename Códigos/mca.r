library(RSQLite)
library(dplyr)
library(ggplot2) 
library(FactoMineR) # para ejecutar mca
library(factoextra) # para visualizar mca
library(fastDummies) 
library(car)
library(vcd)

conn <- dbConnect(RSQLite::SQLite(), "dentix_ult.sqlite")
tables <- dbListTables(conn)
sql_query <- "SELECT * FROM raw_data;" 
df <- dbGetQuery(conn, sql_query)
# excluir columnas
df <- df %>% select(-clinica, -comercial, -transaction_id) 
dbDisconnect(conn)

# añadimos una variable que es cuota_mensual / ingresos_totales
df <- df %>%
  mutate(
    ratio_cuota_ingreso = cuota_mensual / (ingresos_fijos + 1)
  )

# a factores

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

# separación de variables
df <- df %>%
  mutate(
    cuota_credito_bin = as.factor(ifelse(cuota_credito > 0, "Si", "No")),
    activos_bin = as.factor(ifelse(activos > 0, "Si", "No")),
    pasivos_bin = as.factor(ifelse(pasivos > 0, "Si", "No"))
  )

# transformación logarítmica
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
        .default = actividad_economica # mantiene las demás sin cambios
      )
    )
}

# factorizamos todo lo demás
df <- df %>%
  mutate(
    across(where(is.character), as.factor)
  )


# multi correspondence analysis!!
mca_vars <- c("tipo_contrato", "actividad_economica", "ocupacion")

# filtramos nulos ya que no soporta
mca_data <- df %>%
  select(all_of(mca_vars)) %>%
  na.omit()

mca_res <- MCA(mca_data, graph = FALSE)
print(mca_res$eig)

var_labels <- c(
  tipo_contrato = "tipo de contrato",
  actividad_economica = "actividad_economica",
  ocupacion = "ocupacion"
)

# extraemos las etiquetas de las categorías
var_labels <- rownames(mca_res$var$coord)

map_category_to_var <- function(category, data) {
  for (var in names(data)) {
    if (category %in% unique(data[[var]])) {
      return(var)
    }
  }
  return(NA)
}

mapped_vars <- sapply(var_labels, map_category_to_var, data = mca_data)
lb_vector <- var_labels[mapped_vars]

# biplot
fviz_mca_biplot(
  mca_res,
  repel = TRUE,
  ggtheme = theme_minimal(),
  col.var = lb_vector,
  alpha.var = 0.7,
  label = "var",
  geom.ind = "point",
  col.ind = "gray70",
  alpha.ind = 0,
  title = "MCA"
)

# análisis de contribución

# dim1
fviz_contrib(mca_res, 
             choice = "var", 
             axes = 1, 
             top = 15,
             title = "Contribución de variables a la dimensión 1")
