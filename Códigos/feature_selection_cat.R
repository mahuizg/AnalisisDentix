compare_feature_selection <- function(datos, target) {
  library(caret)
  D <- datos
  D <- na.omit(D)
  y <- D[[target]]
  
  rf_model <- randomForest(as.formula(paste(target, "~ .")), data = D, importance = TRUE, ntree = 500)
  rf_importance <- importance(rf_model)
  
  importance_metric <- if(ncol(rf_importance) > 1) rf_importance[, ncol(rf_importance)-1] else rf_importance[, 1]
  rf_features <- names(sort(importance_metric, decreasing = TRUE))
  
  importance_df <- data.frame(Feature = names(importance_metric), Importance = importance_metric)
  
  plot <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "cornflowerblue") +
    coord_flip() +
    labs(title = "Random Forest Feature Importance", x = "Feature", y = "Importance") +
    theme_minimal()
  # Display the plot
  
  ## Random Forest Model Evaluation
  set.seed(42)
  trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
  trainData <- D[trainIndex, ]
  testData <- D[-trainIndex, ]
  
  rf_model_full <- randomForest(as.formula(paste(target, "~ .")), data = trainData, ntree = 500, importance = TRUE)
  predictions <- predict(rf_model_full, newdata = testData)
  
  if(is.factor(y)) {
    confusion <- table(Predicted = predictions, Actual = testData[[target]])
    accuracy <- sum(diag(confusion)) / sum(confusion)
    
    return(list(
      Top_Features = rf_features[1:10],  # Top 10 features
      OOB_Error = rf_model_full$err.rate[500, "OOB"],
      Test_Accuracy = accuracy,
      Confusion_Matrix = confusion,
      Feature_Plot = plot
    ))
  } else {
    mse <- mean((predictions - testData[[target]])^2)
    
    return(list(
      Top_Features = rf_features[1:10],
      OOB_Error = rf_model_full$mse[500],
      Test_MSE = mse,
      Feature_Plot = plot
    ))
  }
}
