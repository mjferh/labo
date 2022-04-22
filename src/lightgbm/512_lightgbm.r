# LightGBM  cambiando algunos de los parametros

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")

# Aqui se debe poner la carpeta de la computadora local
setwd("~/MEDGC/13_LaboratorioImplementacion/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar
dataset <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors = TRUE)


# paso la clase a binaria que tome valores {0,1}  enteros
dataset[, clase01 := ifelse(clase_ternaria == "BAJA+2", 1L, 0L)]

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

ksemilla_azar <- 777137
set.seed(ksemilla_azar)

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[, campos_buenos, with = FALSE]),
  label = dataset$clase01
)

early_stopping <- as.integer(50 + 5 / 0.1) # /learning_rate

# genero el modelo con los parametros por default
modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    seed = ksemilla_azar,
    max_depth = -1, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # por ahora, lo dejo fijo
    lambda_l1 = 0.0, # por ahora, lo dejo fijo
    lambda_l2 = 0.0, # por ahora, lo dejo fijo
    max_bin = 31,
    num_iterations = 1025, # un numero muy grande, lo limita early_stopping_rounds
    force_row_wise = TRUE,
    # ---------------------------------------------
    learning_rate = 0.0102373904120051,
    feature_fraction = 0.364500143688873,
    min_data_in_leaf = 4351,
    num_leaves = 173
    # ---------------------------------------------
    #early_stopping_rounds =  early_stopping
  )
)

# aplico el modelo a los datos sin clase
dapply <- fread("./datasets/paquete_premium_202101.csv")

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)


# Genero la entrega para Kaggle
entrega <- as.data.table(list(
  "numero_de_cliente" = dapply[, numero_de_cliente],
  "Predicted" = prediccion > 1 / 60
)) # genero la salida

dir.create("./labo/exp/", showWarnings = FALSE)
dir.create("./labo/exp/KA2512/", showWarnings = FALSE)
archivo_salida <- "./labo/exp/KA2512/KA_512_t02_02.csv"

# genero el archivo para Kaggle
fwrite(entrega,
  file = archivo_salida,
  sep = ","
)


# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "./labo/exp/KA2512/KA_512_importancia_t02_01.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)
