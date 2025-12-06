import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.feature_selection import mutual_info_classif
from sklearn.metrics import classification_report, confusion_matrix, precision_recall_curve
import xgboost as xgb
from scipy.stats import f


df = pd.read_csv("variables.csv")
df['genero'] = df['genero'].fillna('Desconocido')
df = df.dropna(subset=['lugar_nacimiento'])

# convertimos las franjas en solo dos
df['target'] = df['mora_franja_label'].apply(lambda x: 0 if x in ['Al dia', '1 a 30'] else 1)

# separación de predictoras y objetivo
drop_cols = ['mora_franja', 'mora_franja_label', 'target']
X = df.drop(drop_cols, axis=1)
y = df['target']

# transformación de categóricas
cat_cols = X.select_dtypes(include=['object']).columns.tolist()
for col in cat_cols:
    X[col] = pd.Categorical(X[col])
    X[col] = X[col].cat.codes  # convierte a números
print(X.dtypes)

# split en train y test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, stratify=y, random_state=42)

# feature selection por mutual information
mi_scores = mutual_info_classif(X_train, y_train, discrete_features='auto', random_state=42)
mi_series = pd.Series(mi_scores, index=X_train.columns).sort_values(ascending=False)

# gráficamos las variables más importantes
plt.figure(figsize=(10, 6))
mi_series.head(20).plot(kind='barh', color='blue')
plt.title('Variables por Mutual Information')
plt.gca().invert_yaxis()
plt.show()

# nos quedamos con el top 20
top_features = mi_series.head(20).index.tolist()
X_train_fs = X_train[top_features]
X_test_fs = X_test[top_features]

# scale_pos_weight
negatives = (y_train == 0).sum()
positives = (y_train == 1).sum()
scale_weight = negatives / positives
print(f"scale_pos_weight: {scale_weight:.2f}")

# entrenamiento
model = xgb.XGBClassifier(
    n_estimators=300, learning_rate=0.03,
    max_depth=5, # para capturar interacciones complejas porque los datos nomas no se dejan
    subsample=0.9, # evitar overfitting
    colsample_bytree=0.9, scale_pos_weight=scale_weight, # penalización para equilibrar clases
    objective='binary:logistic', n_jobs=-1, random_state=42, enable_categorical=True)
model.fit(X_train_fs, y_train)

# umbral del f2
y_proba = model.predict_proba(X_test_fs)[:, 1]
precisions, recalls, thresholds = precision_recall_curve(y_test, y_proba)

# calculamos el f2
f2_scores = (5 * precisions * recalls) / (4 * precisions + recalls + 1e-10)
best_idx = np.argmax(f2_scores)
best_thresh = thresholds[best_idx]
print(f"Mejor corte para f2: {best_thresh:.4f}")

# evaluación y gráficas
# predicción final usando el umbral optimizado
y_pred_opt = (y_proba >= best_thresh).astype(int)
print("Métricas")
print(classification_report(y_test, y_pred_opt))

# matriz de confusión
cm = confusion_matrix(y_test, y_pred_opt)
plt.figure(figsize=(6, 5))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', cbar=False)
plt.title('Matriz de Confusión')
plt.xlabel('Predicción')
plt.ylabel('Real')
plt.show()

# scatterplot de probabilidades
plt.figure(figsize=(10, 6))
sns.stripplot(x=y_test, y=y_proba, jitter=0.3, alpha=0.5, palette='Blues')
plt.axhline(best_thresh, color='red', linestyle='--', label=f'f2 corte: {best_thresh:.2f}')
plt.xlabel('Clase Real')
plt.ylabel('Probabilidad según el Modelo')
plt.legend()
plt.show()

# análisis forense

# usamos los índices de X_test para traer 'perf_final' y los valores originales sin escalar si fuera el caso
df_forense = df.loc[X_test.index].copy()
df_forense['prediccion'] = y_pred_opt
df_forense['probabilidad'] = y_proba
df_forense['real'] = y_test

# etiquetamos los errores
conditions = [
    (df_forense['real'] == 0) & (df_forense['prediccion'] == 1), # falso positivo
    (df_forense['real'] == 1) & (df_forense['prediccion'] == 0)  # falso negativo
]
choices = ['FP', 'FN']
df_forense['Tipo_Error'] = np.select(conditions, choices, default='Acierto')

def t2_test(group1, group2):
    n1 = len(group1)
    n2 = len(group2)
    p = group1.shape[1] # número de variables

    # medias
    mean1 = np.mean(group1, axis=0)
    mean2 = np.mean(group2, axis=0)

    # covarianzas
    cov1 = np.cov(group1, rowvar=False)
    cov2 = np.cov(group2, rowvar=False)

    # pooled cov
    # asumimos homogeneidad de covarianzas para simplificar, robusto si n1 ~ n2
    # si los tamaños son muy distintos, esto es una aproximación.
    sp = ((n1 - 1) * cov1 + (n2 - 1) * cov2) / (n1 + n2 - 2)

    # inversa de la covarianza combinada
    try:
        sp_inv = np.linalg.inv(sp)
    except np.linalg.LinAlgError:
        # Fallback si la matriz es singular (añade jitter mínimo)
        sp_inv = np.linalg.inv(sp + np.eye(p) * 1e-5)

    # diferencia de medias
    diff = mean1 - mean2

    # estadístico T2
    t2 = (n1 * n2) / (n1 + n2) * diff.T @ sp_inv @ diff

    # estadístico F transformado
    stat_f = (n1 + n2 - p - 1) / (p * (n1 + n2 - 2)) * t2
    
    # grados de libertad
    df1 = p
    df2 = n1 + n2 - p - 1

    # p-val
    p_value = f.sf(stat_f, df1, df2)

    return t2, stat_f, p_value

# los indicadores que habíamos definido que difieren en segmentación
vars_comparar = ['score', 'cuota_credito', 'monto_desembolso', 'ingresos_fijos', 'pasivos', 'estabilidad']

# falsos positivos son el perfil 6?

# agarramos los falsos positivos
data_fp = df_forense[df_forense['Tipo_Error'] == 'FP'][vars_comparar]
# agarramos al perfil 6
data_p6 = df[df['perf_final'] == 'perfil 6'][vars_comparar]
print(f"n FP:{len(data_fp)} | n perfil 6:{len(data_p6)}")

t2, f_stat, p_val = t2_test(data_fp, data_p6)

print(f"estadístico T2: {t2:.4f}")
print(f"estadístico F: {f_stat:.4f}")
print(f"p-value: {p_val:.4e}")

# centroide de los FP
centroide_fp = df_forense[df_forense['Tipo_Error'] == 'FP'][vars_comparar].mean()
# centroide del perfil 6 
centroide_p6 = df[df['perf_final'] == 'perfil 6'][vars_comparar].mean()
comparativa_1 = pd.DataFrame({'FP media': centroide_fp, 'perfil 6 media': centroide_p6})
comparativa_1['Diferencia %'] = ((comparativa_1.iloc[:,0] - comparativa_1.iloc[:,1]) / comparativa_1.iloc[:,1]) * 100
print(comparativa_1.round(2))

# en este caso concluimos que si hay diferencias estadísticas por lo que no son el perfil 6

# falsos negativos son el perfil 2?

data_fn = df_forense[df_forense['Tipo_Error'] == 'FN'][vars_comparar]
data_p2 = df[df['perf_final'] == 'perfil 2'][vars_comparar]

print(f"n FN:{len(data_fn)}) | n perfil 2:{len(data_p2)}")

t2, f_stat, p_val = t2_test(data_fn, data_p2)

print(f"Estadístico T2: {t2:.4f}")
print(f"Estadístico F transofrmado: {f_stat:.4f}")
print(f"P-Value: {p_val:.4e}")

centroide_fn = df_forense[df_forense['Tipo_Error'] == 'FN'][vars_comparar].mean()
centroide_p2 = df[df['perf_final'] == 'perfil 2'][vars_comparar].mean()

comparativa_2 = pd.DataFrame({'FN media': centroide_fn, 'perfil 2 media': centroide_p2})
comparativa_2['Diferencia %'] = ((comparativa_2.iloc[:,0] - comparativa_2.iloc[:,1]) / comparativa_2.iloc[:,1]) * 100
print(comparativa_2.round(2))

# en este caso también concluimos que los FN no son el perfil 2

df['nivel_deuda'] = (df['pasivos'] + df['cuota_credito']) / (df['ingresos_fijos'] + 1e-5)
df['estres_crediticio'] = df['score'] / (1 + df['cuota_credito'])


# definir zonas basadas en probabilidad
def asignar_zona(prob):
    if prob < 0.30: 
        return 'Bajo Riesgo'
    elif prob < 0.70:
         return 'Revisión manual'
    else: 
        return 'Alto Riesgo'

df_forense['Zona_Riesgo'] = df_forense['probabilidad'].apply(asignar_zona)

# calcular la mora real en cada zona
analisis_zonas = df_forense.groupby('Zona_Riesgo').agg(
    Total_Clientes=('target', 'count'),
    Clientes_Morosos=('target', 'sum'),
    Tasa_Mora_Real=('target', 'mean')
)

analisis_zonas['Tasa_Mora_Real'] = (analisis_zonas['Tasa_Mora_Real'] * 100).round(2)
print(analisis_zonas)

# visualización de la Zona Gris
plt.figure(figsize=(10, 6))
sns.histplot(data=df_forense, x='probabilidad', hue='target', bins=30, kde=True, palette={0: 'blue', 1: 'red'}, alpha=0.6)
plt.axvline(0.30, color='orange', linestyle='--', label='límite seguro')
plt.axvline(0.70, color='orange', linestyle='--', label='límite peligroso')
plt.title('Distribución de probabilidades de mora > 30 días')
plt.xlabel('Probabilidad de mora')
plt.ylabel('# de clientes')
plt.legend(title='Realidad')
plt.show()
