# Laboratorio de Implementación 1

Rosario, 2022

---

**Semillas** usadas en los scripts: `c(777137, 664967, 507803, 387173, 536867)`

---

# Log

## Clase 1 y 2 - Tarea 1

### rpart

z101_PrimerModelo.R: data.table, rpart, prp, salida kaggle  
z102_PrimerModelo.ipynb  
z211_traintest_estratificado.r: función partición, calc. ganancia  
z222_traintest_montecarlo.r: 5 particiones, 1 por semilla. mcmapply.   
z231_mejormodelo.r: comparación 3 modelos - montercarlo - 6 semillas.   
z238_mejormodelo_analisis.r: comparación 3 modelos, montecarlo, 1000 semillas. Wilcoxon Test.  
z238_mejormodelo_analisis_2.R: basado en z238. Gráfico de densidad de las ganancias de los  3 modelos.  
z239_mejormodelo_analisis.ipynb  
**z241_gridsearch.r**: basado en z241_gridsearch_esqueleto.r. Optimización de hiperparámetros (minsplit, minbucket, cp, maxdepth)  
z242_aplicar_rpart_kaggle.r  
z271_analisis_gridsearch.r  

### Zero2Hero

0101: data.table (fread) vs dataset (read.csv)  
0102: data.table, ggplot2  
0103: Árboles: rpart, manejo de la memoria  
0104: Árboles: Colinealidad, Normalización, Tx Log, Outliers  
0105: data.table a partir de columnas, fwrite  
0106: Kaggle: cant. BAJA + 2 en 202101  
0107: Kaggle: envío archivo. Árbol sobre 202101  
0108: Árboles rpart: hiperparametros  
0109: caret: train/test estratificado  
0110: Reproducibilidad: set.seed()  
0111: rpart con train/test. Cálculo de ganancia sobre test/dataset  
0112: rpart con train/test: múltiples semillas. Función de ganancia  
0113: vectores: for vs. vectorial de R  
0114: Estimación Montecarlo  
0115: Función ArbolMontecarlo para n semillas  
