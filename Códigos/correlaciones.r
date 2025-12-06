library(vcd)
library(RSQLite)
library(dplyr)
library(corrplot)
library(ggplot2) 
library(scales)  # para etiquetas de porcentaje en geom_bar
library(here)

# preprocesamiento y carga de datos

conn <- dbConnect(RSQLite::SQLite(), "dentix_ult.sqlite")
tables <- dbListTables(conn)
sql_query <- "SELECT *  FROM raw_data;" 
df <- dbGetQuery(conn, sql_query)
df <- df %>% select(-clinica, -comercial, -transaction_id) # esto es para columnas que queramos eliminar y esto va a variar en cada análisis
dbDisconnect(conn)

# transformaciones a cats

# plazo
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

# tasa
breaks_tasa <- quantile(df$tasa, probs = c(0, 0.333, 0.666, 1), na.rm = TRUE)
df$tasa <- cut(
  df$tasa, 
  breaks = breaks_tasa, 
  labels = c("Baja", "Media", "Alta"), 
  include.lowest = TRUE 
)

# tiempo_actividad
breaks_act <- quantile(df$tiempo_actividad, probs = 0:4/4, na.rm = TRUE)
df$tiempo_actividad <- cut(
  df$tiempo_actividad, 
  breaks = breaks_act, 
  labels = c("Rookie", "Intermedio", "Moderado", "Veterano"), 
  include.lowest = TRUE
)

# tiempo_residencia
breaks_res <- quantile(df$tiempo_residencia, probs = 0:4/4, na.rm = TRUE)
df$tiempo_residencia <- cut(
  df$tiempo_residencia, 
  breaks = breaks_res, 
  labels = c("Rookie", "Intermedio", "Moderado", "Veterano"), 
  include.lowest = TRUE
)

# separacion de variables basado en el hurdle

df <- df %>%
  mutate(
    cuota_credito_bin = as.factor(ifelse(cuota_credito > 0, "Si", "No")),
    activos_bin = as.factor(ifelse(activos > 0, "Si", "No")),
    pasivos_bin = as.factor(ifelse(pasivos > 0, "Si", "No"))
  )

# transformación logarítmica a las variables financieras

vars_to_log <- c("activos", "cuota_credito", "cuota_mensual", 
                 "gastos_sostenimiento", "ingresos_fijos", "monto_desembolso", 
                 "pasivos", "saldo_capital", "saldo_vencido", "total_egresos")

# solo se aplica a columnas que existen (por si acashore)
vars_to_log <- intersect(vars_to_log, names(df))

df <- df %>%
  mutate(
    # Usamos across() y log1p() que es básicamente aplicar log(x+1) para los ceros
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

# lo aplicamos
if ("actividad_economica" %in% names(df)) {
  df <- df %>%
    mutate(
      actividad_economica = recode(
        actividad_economica, 
        !!!mapeo, 
        .default = actividad_economica # mantiene las demás sin cambios
      )
    )
}

# factorizamos todo lo demás que sea cat

df <- df %>%
  mutate(
    across(where(is.character), as.factor)
  )

# hasta aquí todo esto se lo pueden copiar y modificar para tener el df que necesiten

# análisis de correlación

# omitir nulos
df_complete <- na.omit(df) 
df_complete$total_egresos <- NULL # se elimina después de transformar
print(paste("Registrsos sin nulos", nrow(df_complete)))

# separamos las variables de acuerdo a su tipo
categorical_vars <- names(df_complete[sapply(df_complete, is.factor)])
numeric_vars <- names(df_complete[sapply(df_complete, is.numeric)])

# pasamos las variables a df
numeric_df <- df_complete[, numeric_vars, drop = FALSE]
categorical_df <- df_complete[, categorical_vars, drop = FALSE]

# "olvidar" los niveles que ya no existen
categorical_df[] <- lapply(categorical_df, droplevels)

# definimos las funciones de correlaçao

# v de cramer para cat-cat
cramers_v <- function(x, y) {
  tbl <- table(x, y)
  if (all(dim(tbl) > 1)) {
    return(assocstats(tbl)$cramer)
  } else {
    return(NA_real_)
  }
}

# eta para num-cat
correlation_ratio <- function(categories, values) {
  categories <- as.factor(categories)
  values <- as.numeric(values)
  if (length(unique(categories)) < 2) return(NA_real_)
  
  valid_cases <- !is.na(categories) & !is.na(values)
  if(sum(valid_cases) == 0) return(NA_real_)
  
  categories <- categories[valid_cases]
  values <- values[valid_cases]
  
  grand_mean <- mean(values, na.rm = TRUE)
  ss_between <- sum(tapply(values, categories, function(g) {
    n <- sum(!is.na(g))
    if (n == 0) return(0)
    (mean(g, na.rm = TRUE) - grand_mean)^2 * n
  }), na.rm = TRUE)
  
  ss_total <- sum((values - grand_mean)^2, na.rm = TRUE)
  if (ss_total == 0) return(0)
  return(sqrt(ss_between / ss_total))
}

# cálculo de las matrices

# matriz de pearson num-num
pearson_mat <- cor(numeric_df, use = "pairwise.complete.obs", method = "pearson")

# matriz de cramer
n_cat_vars <- length(categorical_vars)
cramer_mat <- matrix(NA_real_, nrow = n_cat_vars, ncol = n_cat_vars,
                     dimnames = list(categorical_vars, categorical_vars))
if (n_cat_vars > 0) {
  for (i in seq_len(n_cat_vars)) {
    for (j in i:n_cat_vars) {
      cramer_mat[i, j] <- cramers_v(categorical_df[[i]], categorical_df[[j]])
      cramer_mat[j, i] <- cramer_mat[i, j]
    }
  }
}

# matriz de eta
n_num_vars <- length(numeric_vars)
eta_mat <- matrix(NA_real_, nrow = n_cat_vars, ncol = n_num_vars,
                  dimnames = list(categorical_vars, numeric_vars))
if (n_cat_vars > 0 && n_num_vars > 0) {
  for (i in seq_len(n_cat_vars)) {
    for (j in seq_len(n_num_vars)) {
      eta_mat[i, j] <- correlation_ratio(categorical_df[[i]], numeric_df[[ j ]])
    }
  }
}

# juntar todo
all_cols <- c(numeric_vars, categorical_vars)
K <- length(all_cols)
hybrid_mat <- matrix(NA_real_, nrow = K, ncol = K, dimnames = list(all_cols, all_cols))
hybrid_mat[numeric_vars, numeric_vars] <- pearson_mat
hybrid_mat[categorical_vars, categorical_vars] <- cramer_mat
for (i in seq_len(n_cat_vars)) {
  for (j in seq_len(n_num_vars)) {
    v <- eta_mat[i, j]
    hybrid_mat[categorical_vars[i], numeric_vars[j]] <- v
    hybrid_mat[numeric_vars[j], categorical_vars[i]] <- v
  }
}

# diagonal de unos
diag(hybrid_mat) <- 1

# exportamos como csv
hybrid_df <- as.data.frame(hybrid_mat)
write.csv(hybrid_df, "correlaciones.csv", row.names = TRUE)

# matriz de corr 
corrplot(as.matrix(hybrid_df),
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 90,
         col = colorRampPalette(c("cornflowerblue", "white", "hotpink"))(200),
         is.corr = FALSE) # <- importante: no es de -1 a 1

output_dir_bv <- here("graficos_bivariados")
dir.create(output_dir_bv)

# gráficos bivariados

# activos vs region
h1_plot <- df %>%
  filter(!is.na(activos), !is.na(region)) %>%
  ggplot(aes(x = region, y = activos)) +
  geom_boxplot(fill = "gray85", outlier.alpha = 0.2) +
  labs(title = "Varianza de activos por región",
       x = "Región",
       y = "Activos") +
  theme_minimal()
ggsave(file.path(output_dir_bv, "activos_vs_region.jpg"), h1_plot, width = 10, height = 6)

# edad vs ocupacion
h2_plot <- df %>%
  filter(!is.na(edad), !is.na(ocupacion)) %>%
  ggplot(aes(x = ocupacion, y = edad)) +
  geom_violin(fill = "gray85", alpha = 0.7, draw_quantiles = 0.5) +
  labs(title = "Distribución de Edad por Ocupación",
       x = "Ocupación",
       y = "Edad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(output_dir_bv, "edad_vs_ocupacion.jpg"), h2_plot, width = 12, height = 7)


# monto_desembolso vs plazo
h3_plot <- df %>%
  filter(!is.na(monto_desembolso), !is.na(plazo)) %>%
  ggplot(aes(x = plazo, y = monto_desembolso)) +
  geom_boxplot(fill = "gray85", outlier.alpha = 0.2) +
  labs(title = "Monto de Desembolso por Plazo",
       x = "Plazo",
       y = "Monto Desembolso") +
  theme_minimal()
ggsave(file.path(output_dir_bv, "monto_vs_plazo.jpg"), h3_plot, width = 10, height = 6)


# activos_bin vs pasivos_bin
h4_plot <- df %>%
  filter(!is.na(activos_bin), !is.na(pasivos_bin)) %>%
  ggplot(aes(x = activos_bin, fill = pasivos_bin)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("gray60", "gray80","gray30")) +
  labs(title = "Proporción de 'Tiene Pasivos' por 'Tiene Activos'",
       x = "Tiene Activos",
       y = "Proporción de Clientes",
       fill = "Tiene Pasivos") +
  theme_minimal()
ggsave(file.path(output_dir_bv, "activos_bin_vs_pasivos_bin.jpg"), h4_plot, width = 8, height = 6)


# edad vs tipo_contrato 
h5_plot <- df %>%
  filter(!is.na(edad), !is.na(tipo_contrato)) %>%
  ggplot(aes(x = tipo_contrato, y = edad)) +
  geom_boxplot(fill = "gray85", outlier.alpha = 0.2) +
  labs(title = "Distribución de edad por tipo de contrato",
       x = "Tipo de Contrato",
       y = "Edad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(output_dir_bv, "edad_vs_contrato.jpg"), h5_plot, width = 10, height = 7)


# score vs tiempo_actividad
h6_plot <- df %>%
  filter(!is.na(score), !is.na(tiempo_actividad)) %>%
  ggplot(aes(x = tiempo_actividad, y = score)) +
  geom_boxplot(fill = "gray85", outlier.alpha = 0.2) +
  labs(title = "Distribución de score por tiempo laboral",
       x = "Tiempo de Actividad",
       y = "Score") +
  theme_minimal()
ggsave(file.path(output_dir_bv, "score_vs_actividad.jpg"), h6_plot, width = 10, height = 6)


# score vs tasa y score vs plazo
h7_plot_tasa <- df %>%
  filter(!is.na(score), !is.na(tasa)) %>%
  ggplot(aes(x = tasa, y = score)) +
  geom_boxplot(fill = "gray85", outlier.alpha = 0.2) +
  labs(title = "Distribución de score por tasa",
       x = "Tasa",
       y = "Score") +
  theme_minimal()
ggsave(file.path(output_dir_bv, "score_vs_tasa.jpg"), h7_plot_tasa, width = 8, height = 6)

h7_plot_plazo <- df %>%
  filter(!is.na(score), !is.na(plazo)) %>%
  ggplot(aes(x = plazo, y = score)) +
  geom_boxplot(fill = "gray85", outlier.alpha = 0.2) +
  labs(title = "Distribución de score por plazo",
       x = "Plazo",
       y = "Score") +
  theme_minimal()
ggsave(file.path(output_dir_bv, "score_vs_plazo.jpg"), h7_plot_plazo, width = 10, height = 6)


# ocupacion vs actividad_economica 
plot_data_h8 <- df %>%
  filter(!is.na(ocupacion), !is.na(actividad_economica)) %>%
  count(ocupacion, actividad_economica, name = "conteo")

h8_plot <- ggplot(plot_data_h8, aes(x = ocupacion, y = actividad_economica, fill = conteo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = conteo), size = 3, color = "black") +
  scale_fill_gradient(low = "gray85", high = "gray50") +
  labs(title = "Ocupación vs Actividad Económica",
       x = "Ocupación",
       y = "Actividad Económica",
       fill = "Conteo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(file.path(output_dir_bv, "ocupacion_vs_actividad.jpg"), h8_plot, width = 12, height = 8)


# tipo_contrato vs ocupacion
h9_plot <- df %>%
  filter(!is.na(tipo_contrato), !is.na(ocupacion)) %>%
  ggplot(aes(x = ocupacion, fill = tipo_contrato)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70", "gray90")) +
  labs(title = "Proporción de tipo de contrato por ocupación",
       x = "Ocupación",
       y = "Proporción",
       fill = "Tipo de Contrato") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(output_dir_bv, "contrato_vs_ocupacion.jpg"), h9_plot, width = 12, height = 7)


# plazo vs tasa
plot_data_h10 <- df %>%
  count(plazo, tasa, name = "conteo") %>%
  filter(!is.na(plazo), !is.na(tasa))

h10_plot <- ggplot(plot_data_h10, aes(x = plazo, y = tasa, fill = conteo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = conteo), size = 3, color = "black") +
  scale_fill_gradient(low = "gray85", high = "gray50") +
  labs(title = "Plazo vs Tasa",
       x = "Plazo",
       y = "Tasa",
       fill = "Conteo") +
  theme_minimal()
ggsave(file.path(output_dir_bv, "plazo_vs_tasa.jpg"), h10_plot, width = 10, height = 6)


# actividad_economica vs tipo_contrato
h11_plot <- df %>%
  filter(!is.na(actividad_economica), !is.na(tipo_contrato)) %>%
  ggplot(aes(x = actividad_economica, fill = tipo_contrato)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70", "gray90")) +
  labs(title = "Proporción de contrato por actividad económica",
       x = "Actividad Económica",
       y = "Proporción",
       fill = "Tipo de Contrato") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(output_dir_bv, "actividad_vs_contrato.jpg"), h11_plot, width = 12, height = 7)

# pruebas de hipótesis

# en todas estas usamos kruskal wallis
# H0: las medianas de la variable numérica son iguales en todos los grupos categóricos
# H1: al menos una mediana es diferente

# activos vs region
test_h1_data <- df %>% filter(!is.na(activos), !is.na(region), region != "Insular")
test_h1 <- kruskal.test(activos ~ region, data = test_h1_data)
print(test_h1)

# edad vs ocupacion
test_h2_data <- df %>% filter(!is.na(edad), !is.na(ocupacion))
test_h2 <- kruskal.test(edad ~ ocupacion, data = test_h2_data)
print(test_h2)

# monto_desembolso vs plazo
test_h3_data <- df %>% filter(!is.na(monto_desembolso), !is.na(plazo))
test_h3 <- kruskal.test(monto_desembolso ~ plazo, data = test_h3_data)
print(test_h3)

# edad vs tipo_contrato
test_h5_data <- df %>% filter(!is.na(edad), !is.na(tipo_contrato))
test_h5 <- kruskal.test(edad ~ tipo_contrato, data = test_h5_data)
print(test_h5)

# score vs tiempo_actividad
test_h6_data <- df %>% filter(!is.na(score), !is.na(tiempo_actividad))
test_h6 <- kruskal.test(score ~ tiempo_actividad, data = test_h6_data)
print(test_h6)

# score vs tasa
test_h7a_data <- df %>% filter(!is.na(score), !is.na(tasa))
test_h7a <- kruskal.test(score ~ tasa, data = test_h7a_data)
print(test_h7a)

# score vs plazo
test_h7b_data <- df %>% filter(!is.na(score), !is.na(plazo))
test_h7b <- kruskal.test(score ~ plazo, data = test_h7b_data)
print(test_h7b)

# pruebas de independencia chi cuadrada
# H0: las dos variables categóricas son independientes.
# H1: las variables no son independientes (están asociadas).

# activos_bin vs pasivos_bin
test_h4_data <- df %>% filter(!is.na(activos_bin), !is.na(pasivos_bin))
test_h4_table <- table(test_h4_data$activos_bin, test_h4_data$pasivos_bin)
test_h4 <- chisq.test(test_h4_table)
print(test_h4)

# ocupacion vs actividad_economica
test_h8_data <- df %>% filter(!is.na(ocupacion), !is.na(actividad_economica))
test_h8_table <- table(test_h8_data$ocupacion, test_h8_data$actividad_economica)
test_h8 <- chisq.test(test_h8_table)
print(test_h8)

# tipo_contrato vs. ocupacion
test_h9_data <- df %>% filter(!is.na(tipo_contrato), !is.na(ocupacion))
test_h9_table <- table(test_h9_data$tipo_contrato, test_h9_data$ocupacion)
test_h9 <- chisq.test(test_h9_table)
print(test_h9)

# plazo vs. tasa
test_h10_data <- df %>% filter(!is.na(plazo), !is.na(tasa))
test_h10_table <- table(test_h10_data$plazo, test_h10_data$tasa)
test_h10 <- chisq.test(test_h10_table)
print(test_h10)

# actividad_economica vs. tipo_contrato
test_h11_data <- df %>% filter(!is.na(actividad_economica), !is.na(tipo_contrato))
test_h11_table <- table(test_h11_data$actividad_economica, test_h11_data$tipo_contrato)
test_h11 <- chisq.test(test_h11_table)
print(test_h11)
