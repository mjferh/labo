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

# ksemilla_azar <- 777137
ksemilla_azar <- 664967
set.seed(ksemilla_azar)

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[, campos_buenos, with = FALSE]),
  label = dataset$clase01
)

# genero el modelo con los parametros por default
modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    seed = ksemilla_azar,
    # Default --------------
    # num_iterations = 115,
    # learning_rate = 0.01,
    # feature_fraction = 1,
    # min_data_in_leaf = 20,
    # num_leaves = 31
    
    # BO ------------------
    max_bin=31,
    num_iterations = 1025,
    learning_rate = 0.0102373904120051,
    feature_fraction = 0.364500143688873,
    min_data_in_leaf = 4351,
    num_leaves = 173
    
    # BO MJF ---------------
    # max_bin=31,
    # num_iterations = 101,
    # learning_rate = 0.0782625510641271,
    # feature_fraction = 0.948851908323736,
    # min_data_in_leaf = 527,
    # num_leaves = 350,
    # min_gain_to_split = 0.00439696902011192,
    # lambda_l1 = 0.0984939434173004,
    # lambda_l2 = 55.0161411447029

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
  "Predicted" = prediccion
  # "Predicted" = prediccion > 1 / 60	
  # "Predicted" = prediccion > 0.0139912084156403 #Actualizar!!!
  # "Predicted" = prediccion > 0.0123866028964812
)) # genero la salida

dir.create("./labo/exp/", showWarnings = FALSE)
dir.create("./labo/exp/KA2512/", showWarnings = FALSE)
archivo_salida <- "./labo/exp/KA2512/KA_512_c3_prob.csv"

# genero el archivo para Kaggle
fwrite(entrega,
  file = archivo_salida,
  sep = ","
)


# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "./labo/exp/KA2512/KA_512_importancia_c3_prob.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)
