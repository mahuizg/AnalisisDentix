#=========================CLASIFICACIÓN PARA FRANJA_MORA=======================#
library(randomForest)
library(ggplot2)
library(PRROC)

#===========================CARGAMOS LA BASE DE DATOS==========================#
df <- create_df()
df <- df %>% select(-clinica, -comercial, -transaction_id, -saldo_vencido, 
                    - dias_mora, - saldo_capital)
df<- na.omit(df)

df$mora_franja <- case_when(
  df$mora_franja %in% c("Al dia", "1 a 30") ~ "Sin mora o <30",
  TRUE ~ "Mora>30"
)
df$mora_franja <- as.factor(df$mora_franja)
target <- "mora_franja"
#=============SELECCIÓN DE VARIABLES CON feature_selection_cat.r===============#
set.seed(25)
compare_feature_selection(df, target)


#===================ENTRENAMIENTO DEL MODELO RANDOM FOREST====================#
df <- df %>% select(score, cuota_mensual, monto_desembolso, edad, 
                    total_egresos, plazo, activos, cuota_credito,
                    gastos_sostenimiento, estabilidad, mora_franja)
set.seed(25)
trainIndex <- sample(1:nrow(df), 0.8 * nrow(df))
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

franja <- randomForest(mora_franja ~ ., data = trainData, importance = TRUE,
                        proximity = FALSE)
predict <- predict(franja, newdata = testData)
conf <- confusionMatrix(predict, testData$mora_franja, positive = "Mora>30")
conf

#==================================MÉTRICAS====================================#
precision <- conf$byClass['Precision']
recall <- conf$byClass['Recall']
f1 <- conf$byClass['F1']
#AUC es más curiosón y no tan simple como las métricas anteriores

#Sacamos probabilidades y la probabilidad de la clase "Mora>30"
predict1_prob <- predict(franja, newdata = testData, type = "prob")
prob_mora <- predict1_prob[, "Mora>30"]
#Pasamos a ceros y unos para los cálculos
actual_mora <- ifelse(testData$mora_franja == "Mora>30", 1, 0)
pr_curve <- pr.curve(scores.class0 = prob_mora, weights.class0 = actual_mora,
                     curve = TRUE)
auc <- pr_curve$auc.integral

cat("\n MÉTRICAS \n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1:", round(f1, 3), "\n")
cat("AUC-PR:", round(auc, 3), "\n")
