library(Rtsne) 
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dbscan) 
library(tidyr)

source("base.r")
df <- create_df() 
df <- df %>% mutate(temp_join_id = row_number())

# seleccionamos las variables de perfil/producto y la variable mora_franja para usarla como label
df_tsne_input <- df %>%
  select(
    # numéricas de perfil
    personas_a_cargo, ingresos_fijos, cuota_credito, gastos_sostenimiento, 
    activos, pasivos, score, edad, estabilidad,
    # numéricas de producto
    monto_desembolso, cuota_mensual,
    # categóricas ordinales
    estrato, plazo, tasa, tiempo_actividad, tiempo_residencia,
    # pa colorear
    mora_franja, temp_join_id
  )

# mapeo d efactores a números 

# definimos los niveles explícitamente para asegurar el orden correcto
df_tsne_numeric <- df_tsne_input %>%
  mutate(
    estrato = factor(estrato, 
                     levels = c("Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4", "Estrato 5", "Estrato 6"), 
                     ordered = TRUE),
    plazo = factor(plazo, 
                   levels = c("3","6","9","12","18","24","30","36","48","60"), 
                   ordered = TRUE),
    tasa = factor(tasa, 
                  levels = c("Baja", "Media", "Alta"), 
                  ordered = TRUE),
    tiempo_actividad = factor(tiempo_actividad, 
                              levels = c("Rookie", "Intermedio", "Moderado", "Veterano"), 
                              ordered = TRUE),
    tiempo_residencia = factor(tiempo_residencia, 
                               levels = c("Rookie", "Intermedio", "Moderado", "Veterano"), 
                               ordered = TRUE),
    mora_franja = factor(mora_franja,
                         levels = c("Al dia", "1 a 30", "31 a 60", "61 a 90", "91 a 120", "121 a 150", "151 a 180", "181 a 360", "360+"),
                         ordered = TRUE)
  ) %>%
  # convertimos todo a numérico para la matriz
  mutate(across(everything(), as.numeric))

# eliminamos nulos
df_tsne_clean <- na.omit(df_tsne_numeric)

# separamos las variables de input de mora_franja
matrix <- df_tsne_clean %>%
  select(-mora_franja, -temp_join_id)

# datos estandarizados
scaled_matrix <- scale(matrix)

# corremos t-SNE
set.seed(22072004)

# primero usamos PCA looool
pca_res <- prcomp(scaled_matrix, center = FALSE, scale. = FALSE)
print(summary(pca_res)) # tenemos que cortar en el 15avo componente

# usamos los primeros 15 PCs
tsne_input <- pca_res$x[, 1:15]

tsne_res <- Rtsne(
  tsne_input, 
  dims = 2, 
  pca = FALSE, # no sabía que ya tenía esta opción pero ya equis
  perplexity = 50, 
  check_duplicates = FALSE 
)

# Creamos un dataframe con los resultados de t-SNE que van a ser nuestros ejes
tsne_plot_data <- data.frame(
  tsne_1 = tsne_res$Y[, 1],
  tsne_2 = tsne_res$Y[, 2]
)

# unimos los resultados con los datos originales
tsne_plot_data <- bind_cols(tsne_plot_data, df_tsne_clean)

# re convertimos mora_franja y tasa a factores para los gráficos
tsne_plot_data <- tsne_plot_data %>%
  mutate(
    mora_franja_label = factor(mora_franja,
                               levels = 1:9,
                               labels = c("Al dia", "1 a 30", "31 a 60", "61 a 90", "91 a 120", "121 a 150", "151 a 180", "181 a 360", "360+")),
    tasa_label = factor(tasa,
                        levels = 1:3,
                        labels = c("Baja", "Media", "Alta"))
  )

# formas de los perfiles
g_forma <- ggplot(tsne_plot_data, aes(x = tsne_1, y = tsne_2)) +
  geom_point(alpha = 0.2, color = "black") +
  labs(title = "Perfiles de Clientes") +
  theme_minimal()

print(g_forma)

# para probar la hipótesis de 2 productos (coloreado por monto)
g_producto <- ggplot(tsne_plot_data, aes(x = tsne_1, y = tsne_2, color = monto_desembolso)) +
  geom_point(alpha = 0.3) +
  scale_color_gradient(low = "hotpink", high = "blue") +
  labs(title = "Segmentación de préstamos",
       subtitle = "Coloreado por monto_desembolso",
       color = "monto_desembolso") +
  theme_minimal()

print(g_producto)

# para analizar el riesgo de los grupos
g_riesgo <- ggplot(tsne_plot_data, aes(x = tsne_1, y = tsne_2, color = mora_franja_label)) +
  geom_point(alpha = 0.4) +
  scale_color_brewer(palette = "RdYlGn", direction = -1) + 
  labs(title = "Análisis de Riesgo",
       color = "mora_franja") +
  theme_minimal()

print(g_riesgo)

# reducimos las cats de mora
tsne_plot_data <- tsne_plot_data %>%
  mutate(
    mora_franja_simple = case_when(
      mora_franja == 1 ~ "Al dia",   
      mora_franja == 2 ~ "1 a 30", 
      mora_franja >= 3 ~ "30+",
      TRUE ~ NA_character_
    ),
    # la convertimos a factor con el orden correcto para la leyenda
    mora_franja_label = factor(mora_franja_simple, levels = c("Al dia", "1 a 30", "30+"))
  )

# para ver bien el riesgo de los grupos
g_riesgo <- ggplot(tsne_plot_data, aes(x = tsne_1, y = tsne_2, color = mora_franja_label, alpha = mora_franja_label)) +
  geom_point() +

  # colores muy diferentes
  scale_color_manual(values = c("Al dia" = "darkolivegreen2", "1 a 30" = "darkorange", "30+" = "blue")) + 
  
  # opacidad para las cats
  scale_alpha_manual(values = c("Al dia" = 0.2, "1 a 30" = 0.4, "30+" = 0.8),
                     na.value = 0.1) +
  
  labs(title = "Análisis de Riesgo",
       color = "mora_franja",
       alpha = "mora_franja") +
  theme_minimal()

print(g_riesgo)

# clustering

# scree plot para eps 
kNNdistplot(tsne_input, k = 50)
abline(h = 4.1, lty = 2, col = "red")

# ejecutamos el algoritmo sobre los 15 PCs
set.seed(22072004)
db_res <- dbscan(tsne_input, eps = 2.3, minPts = 50)

# conteo por cluster (el 0 son outliers)
print(table(db_res$cluster))

# visualización

# añadimos las etiquetas del cluster al dataframe de visualización
tsne_plot_data$cluster <- as.factor(db_res$cluster)

# creamos el gráfico final
g_dbscan_clusters <- ggplot(tsne_plot_data, aes(x = tsne_1, y = tsne_2, color = cluster)) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set1", 
                     name = "cluster",
                     labels = c("outlier", "1", "2", "3")) + 
  labs(title = "DBSCAN sobre mapa t-SNE") +
  theme_minimal()

print(g_dbscan_clusters)

# estadística de los clusters

vars_demograficas <- c("estrato", "personas_a_cargo", "edad", "estabilidad", 
                       "tiempo_actividad", "tiempo_residencia")
vars_financieras <- c("ingresos_fijos", "cuota_credito", "gastos_sostenimiento", 
                      "activos", "pasivos", "score", "monto_desembolso", 
                      "plazo", "tasa")

# renombramos clusters
tsne_plot_data <- tsne_plot_data %>%
  mutate(cluster_label = case_when(
    cluster == "0" ~ "outliers",
    cluster == "1" ~ "perfil 1",
    cluster == "2" ~ "perfil 2",
    cluster == "3" ~ "perfil 3",
    TRUE ~ cluster
  ))

# resumen estadístico
resumen_clusters <- tsne_plot_data %>%
  group_by(cluster_label) %>%
  summarise(
    n = n(),
    across(all_of(c(vars_demograficas, vars_financieras)),
           list(media = ~mean(.x, na.rm = TRUE),
                mediana = ~median(.x, na.rm = TRUE)),
           .names = "{.col}|{.fn}")
  ) %>%
  pivot_longer(cols = -c(cluster_label, n),
               names_to = c("variable", "estadistico"),
               names_sep = "\\|") %>% 
  pivot_wider(names_from = estadistico, 
              values_from = value)

# mora por cluster
resumen_mora <- tsne_plot_data %>%
  count(cluster_label, mora_franja_label) %>%
  group_by(cluster_label) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  arrange(cluster_label, mora_franja_label)


print(resumen_clusters, n = Inf)
print(resumen_mora, n = Inf)


# creamos un índice para las filas que son outliers
outlier_index <- db_res$cluster == 0

# extraemos los datos de los 15 PCs solo de los outliers para clusterizar
outlier_tsne_input <- tsne_input[outlier_index, ]

# extraemos los datos de visualización solo de los outliers
outlier_plot_data <- tsne_plot_data[outlier_index, ]

# clustering jerarquico 

# calculamos la matriz de distancia sobre los datos de los 15 PCs
dist_matrix <- dist(outlier_tsne_input, method = "euclidean")

# yawss
hclust_res <- hclust(dist_matrix, method = "ward.D2")

# xortamos el dendrograma en k=3 y crea un vector de etiquetas 1, 2, o 3
sub_clusters <- cutree(hclust_res, k = 3)

# asignamos etiquetas finales al dataframe principal

# creamos una nueva columna 'sub_cluster' en tsne_plot_data
tsne_plot_data$sub_cluster <- NA

# rellenamos esa columna solo en las filas que eran outliers con las nuevas etiquetas que encontramos.
tsne_plot_data$sub_cluster[outlier_index] <- sub_clusters

# creamos la columna del perfil final
tsne_plot_data <- tsne_plot_data %>%
  mutate(
    perf_final = case_when(
      cluster == "1" ~ "perfil 1",
      cluster == "2" ~ "perfil 2",
      cluster == "3" ~ "perfil 3",
      cluster == "0" & sub_cluster == 1 ~ "perfil 4",
      cluster == "0" & sub_cluster == 2 ~ "perfil 5",
      cluster == "0" & sub_cluster == 3 ~ "perfil 6",
      TRUE ~ "otro" 
    ),
    perf_final = as.factor(perf_final)
  )

print(table(tsne_plot_data$perf_final))


# visualizamos los nuevos clusters en el mapa t-SNE

g_final_clusters <- ggplot(tsne_plot_data, 
                           aes(x = tsne_1, 
                               y = tsne_2, 
                               color = perf_final)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(
    values = c(
      "perfil 1" = "cornflowerblue",
      "perfil 2" = "palegreen",
      "perfil 3" = "mediumpurple1", 
      "perfil 4" = "orange",
      "perfil 5" = "red3",
      "perfil 6" = "yellow",
      "Otro" = "grey50"
    )
  ) +
  labs(title = "Segmentación Final",
       color = "perfil") +
  theme_minimal()

print(g_final_clusters)

# resumen again

vars_a_unir <- c(
  "transaction_id", 
  "nivel_estudios", "estado_civil", "tipo_vivienda", "lugar_nacimiento", 
  "region", "genero",
  "cuota_credito_bin", "activos_bin", "pasivos_bin",
  "saldo_capital", "saldo_vencido", "dias_mora", "temp_join_id"
)

df_extras <- df %>% select(any_of(vars_a_unir))

tsne_plot_data <- left_join(tsne_plot_data, df_extras, by = "temp_join_id")

vars_excluir <- c("tsne_1", "tsne_2", "temp_join_id", 
                  "tasa_label", "mora_franja_simple", "cluster", "cluster_label", 
                  "sub_cluster", "perf_final", "transaction_id")

# tsne_plot_data <-  tsne_plot_data %>% select(all_of(-vars_excluir)) maybe no correr esto

# Obtenemos todas las columnas candidatas
vars_candidatas <- setdiff(names(tsne_plot_data), vars_excluir)

# Identificamos cuáles son numéricas
vars_numericas <- vars_candidatas[sapply(tsne_plot_data[vars_candidatas], is.numeric)]

# identificamos cuáles son categóricas (factor o character)
vars_categoricas <- vars_candidatas[sapply(tsne_plot_data[vars_candidatas], function(x) is.factor(x) || is.character(x))]

# resumen para variables numéricas
outliers_resumen_num <- tsne_plot_data %>%
  group_by(perf_final) %>%
  summarise(
    n = n(),
    across(all_of(vars_numericas),
           list(media = ~mean(.x, na.rm = TRUE),
                mediana = ~median(.x, na.rm = TRUE)),
           .names = "{.col}|{.fn}")
  ) %>%
  pivot_longer(cols = -c(perf_final, n),
               names_to = c("variable", "estadistico"),
               names_sep = "\\|") %>% 
  pivot_wider(names_from = estadistico, 
              values_from = value)

# resumen para variables categóricas
outliers_resumen_cat <- tsne_plot_data %>%
  group_by(perf_final) %>%
  summarise(
    n = n(),
    across(all_of(vars_categoricas),
           ~names(sort(table(.), decreasing = TRUE))[1],
           .names = "{.col}|moda")
  ) %>%
  pivot_longer(cols = -c(perf_final, n),
               names_to = "variable",
               names_pattern = "(.*)\\|moda",
               values_to = "moda")

print(outliers_resumen_num, n = Inf)
print(outliers_resumen_cat, n = Inf)

# density plot de score para perfil 6

g_score_p6 <- tsne_plot_data %>%
  filter(perf_final == "perfil 6") %>%
  ggplot(aes(x = score)) +
  geom_density(fill = "cornflowerblue", color = "cornflowerblue", alpha = 0.7) +
  labs(
    title = "Densidad de score - perfil 6",
    x = "score",
    y = "densidad"
  ) +
  theme_minimal()

print(g_score_p6)

# density plot de score para perfil 1
g_score_p1 <- tsne_plot_data %>%
  filter(perf_final == "perfil 1") %>%
  ggplot(aes(x = score)) +
  geom_density(fill = "cornflowerblue", color = "cornflowerblue", alpha = 0.7) +
  labs(
    title = "Densidad de score - perfil 1",
    x = "score",
    y = "densidad"
  ) +
  theme_minimal()

print(g_score_p1)

# density plot de cuota_credito del perfil 6 por mora

g_cuota_p6_mora <- tsne_plot_data %>%
  filter(perf_final == "perfil 6", 
         mora_franja_label %in% c("Al dia", "30+")) %>%
  ggplot(aes(x = cuota_credito, fill = mora_franja_label)) +
  geom_density(alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("Al dia" = "cornflowerblue", "30+" = "indianred")) +
  labs(
    title = "Densidad de cuotas - perfil 6 separado por mora",
    x = "cuota_credito",
    y = "densidad",
    fill = "mora"
  ) +
  theme_minimal()

print(g_cuota_p6_mora)

# density plot de score del perfil 1 por mora

g_score_p1_mora <- tsne_plot_data %>%
  filter(perf_final == "perfil 1", 
         mora_franja_label %in% c("Al dia", "30+")) %>%
  ggplot(aes(x = score, fill = mora_franja_label)) +
  geom_density(alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("Al dia" = "cornflowerblue", "30+" = "indianred")) +
  labs(
    title = "Densidad de score - perfil 1 separado por mora",
    x = "score",
    y = "densidad",
    fill = "mora"
  ) +
  theme_minimal()

print(g_score_p1_mora)

# pruebas de hipótesis

# 1, score perfil 6 vs perfil 1

score_p6 <- tsne_plot_data %>% 
  filter(perf_final == "perfil 6") %>% 
  pull(score) %>% 
  na.omit()

score_p1 <- tsne_plot_data %>% 
  filter(perf_final == "perfil 1") %>% 
  pull(score) %>% 
  na.omit()

# test 
test_score_perfiles <- wilcox.test(score_p6, score_p1, alternative = "two.sided", conf.int = TRUE)

cat(median(score_p6), median(score_p1), test_score_perfiles$statistic, test_score_perfiles$p.value)


# 2. cuota de al dia vs morosos en el perfil 6

cuota_p6_aldia <- tsne_plot_data %>% 
  filter(perf_final == "perfil 6", mora_franja_label == "Al dia") %>% 
  pull(cuota_credito) %>% 
  na.omit()

cuota_p6_mora <- tsne_plot_data %>% 
  filter(perf_final == "perfil 6", mora_franja_label == "30+") %>% 
  pull(cuota_credito) %>% 
  na.omit()

test_cuota_p6 <- wilcox.test(cuota_p6_aldia, cuota_p6_mora, alternative = "two.sided", conf.int = TRUE)

cat(median(cuota_p6_mora), median(cuota_p6_aldia), test_cuota_p6$statistic, test_cuota_p6$p.value)

# 3. score morosos vs al dia en el perfil 1

score_p1_mora <- tsne_plot_data %>% 
  filter(perf_final == "perfil 1", mora_franja_label == "30+") %>% 
  pull(score) %>% 
  na.omit()

score_p1_aldia <- tsne_plot_data %>% 
  filter(perf_final == "perfil 1", mora_franja_label == "Al dia") %>% 
  pull(score) %>% 
  na.omit()

test_score_p1 <- wilcox.test(score_p1_mora, score_p1_aldia, alternative = "two.sided",conf.int = TRUE)

cat(median(score_p1_mora), median(score_p1_aldia), test_score_p1$statistic, test_score_p1$p.value)

# 4. proporción de mora 30+ en el perfil 6 y perfil 4

n_p6 <- sum(tsne_plot_data$perf_final == "perfil 6", na.rm = TRUE)
x_p6 <- sum(tsne_plot_data$perf_final == "perfil 6" & 
            tsne_plot_data$mora_franja_label == "30+", na.rm = TRUE)
prop_p6 <- x_p6 / n_p6

n_p4 <- sum(tsne_plot_data$perf_final == "perfil 4", na.rm = TRUE)
x_p4 <- sum(tsne_plot_data$perf_final == "perfil 4" & 
            tsne_plot_data$mora_franja_label == "30+", na.rm = TRUE)
prop_p4 <- x_p4 / n_p4

prop_combinada <- (x_p6 + x_p4) / (n_p6 + n_p4)
se_diff <- sqrt(prop_combinada * (1 - prop_combinada) * (1/n_p6 + 1/n_p4))
z_stat_p6_p4 <- (prop_p6 - prop_p4) / se_diff

# p-value              # greater
p_value_p6_p4 <- pnorm(z_stat_p6_p4, lower.tail = FALSE)

cat(prop_p6, prop_p4, z_stat_p6_p4, p_value_p6_p4)


# 5. proporción de mora 30+ en el perfil 5 vs perfil 1

n_p5 <- sum(tsne_plot_data$perf_final == "perfil 5", na.rm = TRUE)
x_p5 <- sum(tsne_plot_data$perf_final == "perfil 5" & 
            tsne_plot_data$mora_franja_label == "30+", na.rm = TRUE)
prop_p5 <- x_p5 / n_p5

n_p1_prop <- sum(tsne_plot_data$perf_final == "perfil 1", na.rm = TRUE)
x_p1_prop <- sum(tsne_plot_data$perf_final == "perfil 1" & 
                 tsne_plot_data$mora_franja_label == "30+", na.rm = TRUE)
prop_p1 <- x_p1_prop / n_p1_prop

prop_combinada_p5_p1 <- (x_p5 + x_p1_prop) / (n_p5 + n_p1_prop)
se_diff_p5_p1 <- sqrt(prop_combinada_p5_p1 * (1 - prop_combinada_p5_p1) * (1/n_p5 + 1/n_p1_prop))
z_stat_p5_p1 <- (prop_p5 - prop_p1) / se_diff_p5_p1

# p-value 
p_value_p5_p1 <- pnorm(z_stat_p5_p1, lower.tail = FALSE)

cat(prop_p5, prop_p1, z_stat_p5_p1, p_value_p5_p1)

# write.csv(tsne_plot_data, "tsne_plot_data.csv", row.names = FALSE)

