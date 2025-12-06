library(xgboost)
library(tidyverse)
library(data.table)
library(caret)

# --- 1. PREPARACIÓN INICIAL Y DIVISIÓN DE DATOS ---

# Limpieza básica
df_limpio <- df %>% 
  select(-transaction_id) %>% # Quitar ID 
  na.omit()

# División de datos (70% train, 30% test)
set.seed(123)
indices_train <- createDataPartition(df_limpio$monto_desembolso, p = 0.7, list = FALSE)
train_data <- df_limpio[indices_train, ]
test_data <- df_limpio[-indices_train, ]

cat(sprintf("Datos de Entrenamiento: %d filas\n", nrow(train_data)))
cat(sprintf("Datos de Prueba: %d filas\n", nrow(test_data)))

# Estas variables solo se conocen DESPUÉS de aprobar/calcular el crédito.
vars_post_credito <- c(
  "cuota_mensual",  
  "saldo_capital",  
  "saldo_vencido",  
  "plazo",            
  "tasa",             
  "mora_franja",    
  "dias_mora",      
  "monto_desembolso" 
)
# -------------------------------------------------------------

# --- Función Auxiliar para crear matrices ---
preparar_matriz <- function(features_df, label_vector) {
  df_numeric <- features_df %>%
    mutate(across(where(is.character), as.factor)) %>% 
    mutate(across(where(is.factor), as.numeric))
  
  matrix_x <- as.matrix(df_numeric)
  dmatrix <- xgb.DMatrix(data = matrix_x, label = label_vector)
  return(dmatrix)
}

# =========================================================================
#  MODELO 1: MONTO DESEMBOLSO 
# =========================================================================

## --- 1.A. Entrenamiento con Todas las Variables (para obtener importancia) ---

target_train_monto <- train_data$monto_desembolso
target_test_monto <- test_data$monto_desembolso

features_train_monto <- train_data[, !names(train_data) %in% vars_post_credito]
features_test_monto <- test_data[, !names(test_data) %in% vars_post_credito]

# Crear DMatrix
dtrain_monto <- preparar_matriz(features_train_monto, target_train_monto)

params_monto <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  eval_metric = "mae"
)

modelo_xgb_monto_full <- xgb.train(
  params = params_monto, 
  data = dtrain_monto, 
  nrounds = 100,
  verbose = 0
)

# Obtener importancia
importancia_monto <- xgb.importance(feature_names = colnames(features_train_monto), model = modelo_xgb_monto_full)
importancia_df_monto <- as.data.frame(importancia_monto)
tabla_top10_monto <- importancia_df_monto %>% arrange(desc(Gain)) %>% head(10)
vars_top10_monto <- tabla_top10_monto$Feature

cat("--- Top 10 Variables REALES para [Monto Desembolso] ---\n")
print(tabla_top10_monto) 

## --- 1.B. Re-entrenamiento con Top 10 Variables ---

dtrain_monto_top10 <- preparar_matriz(features_train_monto[, vars_top10_monto], target_train_monto)
dtest_monto_top10 <- preparar_matriz(features_test_monto[, vars_top10_monto], target_test_monto)

modelo_xgb_monto_top10 <- xgb.train(
  params = params_monto, 
  data = dtrain_monto_top10, 
  nrounds = 100,
  verbose = 0
)

## --- 1.C. Evaluación REAL (contra el Test Set) ---
predicciones_monto <- predict(modelo_xgb_monto_top10, dtest_monto_top10)

mae_monto <- mean(abs(target_test_monto - predicciones_monto))
mape_monto <- mean(abs((target_test_monto - predicciones_monto) / target_test_monto)) * 100

cat(sprintf("\n--- Métricas Reales (Test Set) [Monto Desembolso] ---\n"))
cat(sprintf("MAE: %.2f\n", mae_monto))   
cat(sprintf("MAPE: %.2f%%\n", mape_monto)) 

# Gráfico de Predicciones vs Real (del Test Set)
comparativa_monto <- data.frame(Real = target_test_monto, Predicho = predicciones_monto)
ggplot(comparativa_monto, aes(x = Real, y = Predicho)) +
  geom_point(color = "steelblue", alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Monto Desembolso: Predicciones vs. Reales (Test Set - Corregido)",
       x = "Monto Real", y = "Monto Predicho")

# =========================================================================
#  MODELO 2: DÍAS DE MORA (TWEEDIE)
# =========================================================================

## --- 2.A. Entrenamiento con Todas las Variables (para obtener importancia) ---

target_train_mora <- train_data$dias_mora
target_test_mora <- test_data$dias_mora

features_train_mora <- train_data[, !names(train_data) %in% vars_post_credito]
features_test_mora <- test_data[, !names(test_data) %in% vars_post_credito]

# Crear DMatrix
dtrain_mora <- preparar_matriz(features_train_mora, target_train_mora)

params_tweedie <- list(
  objective = "reg:tweedie", 
  tweedie_variance_power = 1.5, 
  eta = 0.05, 
  max_depth = 6,
  eval_metric = "mae"
)

modelo_xgb_mora_full <- xgb.train(
  params = params_tweedie, 
  data = dtrain_mora, 
  nrounds = 200, 
  verbose = 0
)

# Obtener importancia
importancia_mora <- xgb.importance(feature_names = colnames(features_train_mora), model = modelo_xgb_mora_full)
importancia_df_mora <- as.data.frame(importancia_mora)
tabla_top10_mora <- importancia_df_mora %>% arrange(desc(Gain)) %>% head(10)
vars_top10_mora <- tabla_top10_mora$Feature

cat("\n--- Top 10 Variables REALES para [Días de Mora] ---\n")
print(tabla_top10_mora) 

## --- 2.B. Re-entrenamiento con Top 10 Variables ---

dtrain_mora_top10 <- preparar_matriz(features_train_mora[, vars_top10_mora], target_train_mora)
dtest_mora_top10 <- preparar_matriz(features_test_mora[, vars_top10_mora], target_test_mora)

modelo_xgb_mora_top10 <- xgb.train(
  params = params_tweedie, 
  data = dtrain_mora_top10, 
  nrounds = 200,
  verbose = 0
)

## --- 2.C. Evaluación REAL (contra el Test Set) ---
predicciones_mora <- predict(modelo_xgb_mora_top10, dtest_mora_top10)

mae_mora <- mean(abs(target_test_mora - predicciones_mora))

cat(sprintf("\n--- Métricas Reales (Test Set) [Días de Mora] ---\n"))
cat(sprintf("MAE: %.2f días\n", mae_mora)) 

# Gráfico de Predicciones vs Real (del Test Set)
comparativa_mora <- data.frame(Real = target_test_mora, Predicho = predicciones_mora)
ggplot(comparativa_mora, aes(x = Real, y = Predicho)) +
  geom_point(color = "firebrick", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Días de Mora: Predicciones vs. Reales (Test Set - Corregido)",
       x = "Días Reales", y = "Días Predichos") +
  coord_cartesian(xlim = c(0, max(target_test_mora)), ylim = c(0, max(predicciones_mora)))