# Análisis Dentix

Las clínicas de Dentix ofrecen créditos para que sus clientes financien tratamientos odontológicos. Se observa mora y variabilidad de la mora entre clínicas y asesores. Las decisiones de aprobación y condiciones no siempre son homogéneas. Se desea anticipar qué créditos pueden caer en mora al momento de originarse, usando la base de datos proporcionada por la empresa. Este repositorio contiene el análisis de datos y modelos desarrollados para Dentix que incluyen limpieza y preparación de datos, análisis descriptivo (univariado y bivariado), gráficos y visualizaciones, selección de variables y modelos de predicción.

## Estructura general

- "Códigos/" — Scripts para limpieza, análisis, visualización, selección de variables y modelado.

- "Correlaciones/" — Gráficos de correlaciones, varianzas y distribuciones entre variables. 

- "Univariado Numéricas/" — Gráficas para las variables numéricas.

- "Univariado Categóricas/" — Gráficas para las variables categóricas.

## Objetivos

1. Realizar un diagnóstico de los factores más importantes para explicar distintas variables, como la probabilidad de mora.
2. Realizar una segmentación de clientes buscando perfiles de alto, mediano y bajo riesgo para Dentix.
3. Generar propuestas para maximizar la rentabilidad del negocio de Dentix.

## Resumen del dataset

- Observaciones: 46,329 registros de una base de datos anonimizada
- Variables:
    - Numéricas: ingresos_fijos, activos, pasivos, cuota_credito,cuota_mensual, saldo_capital, saldo_vencido, dias_mora, score, monto_desembolso, plazo, tasa,
    - Categóricas: nivel_estudios, estado_civil, tipo_vivienda, estrato, actividad_económica, tipo_contrato, ocupacion, genero, clinica, comercial, region, lugar_nacimiento, mora_franja 
edad, tiempo_residencia, tiempo_actividad, personas_a_cargo

## Metodología 

### Preprocesamiento

- Limpieza de NAs según criterio por variable.
- Transformaciones en variables con sesgo.
- Detección y tratamiento de outliers.

### Análisis exploratorio

- Estadísticas univariadas por tipo de variable.
- Análisis bivariado: correlaciones, pruebas Kruskal-Wallis, Chi-cuadrado, MANOVA .
- Visualizaciones para entender distribuciones y relaciones en los datos.

### Selección de variables

- Filtrado por correlación y varianza.
- Reducción de dimensionalidad.
- Selección usando LASSO.

### Modelado

- Clasificación para mora_franja.
- Regresión para monto_desembolso.
- Cross validation y control de sobreajuste.

## Resultados clave 

- Se identificaron 6 perfiles diferentes donde se pueden clasificar los clientes de Dentix.
- Dentix puede aumentar los montos ofrecidos, ofrecer plazos más largos o mejorar las tasas a los clientes del perfil 6 (empleados con hijos).
- Sobreestimación de clientes en el perfil 5 (empleados independientes con mal score) los cuales cuentan con un riesgo crítico. Establecer umbrales de score más altos, demostrar ingresos estables, y mayores restricciones en general.
- Cálculo de score interno para evaluación de clientes en la asignación de montos credicticios y tasas según características de los clientes que son capturadas por el score externo.

## Métricas usadas

- Pearson
- Tasa de correlación Eta
- V de Cramér
- Prueba de hipótesis de Chi cuadrada
- Z-test
- Mann-Whitney
- Precision
- Recall
- F1 score
- AUC-PR
- Matriz de confusión
- MAE
- RMSE
- R2 ajustado

## Conclusiones

Este proyecto permitió construir una visión completa del comportamiento crediticio de los pacientes de las clínicas de Dentix Colombia, generando un análsis de datos completo y la creación de distintos modelos predictivos. A lo largo del proyecto, se identificaron insights importantes que nos permitieron proponer soluciones potenciales para aumentar la rentabilidad del negocio, a la par que se reducen los riesgos. 
