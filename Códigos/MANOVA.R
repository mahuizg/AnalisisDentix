library(ggplot2)
library(dplyr)

df <- create_df()
df$clinica <- substr(df$clinica, start = 1, stop = 10)
res_manova <- manova(cbind(monto_desembolso, tasa) ~ clinica, data = df)
summary(res_manova, test='Wilks')
summary.aov(res_manova)

df_boxplot <- df %>%
  filter(!is.na(monto_desembolso) & !is.na(clinica)) %>%
  mutate(clinica = reorder(clinica, monto_desembolso, FUN = median))

ggplot(df_boxplot, aes(x = clinica, y = monto_desembolso)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.colour = "red", outlier.size = 1) +
  coord_flip() +  
  theme_minimal() +
  labs(title = "Distribución de Montos por Clínica",
       x = "Clínica / Sede",
       y = "Monto de Desembolso") +
  theme(axis.text.y = element_text(size = 8)) 