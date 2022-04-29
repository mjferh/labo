# LightGBM  cambiando algunos de los parametros

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")

# Aqui se debe poner la carpeta de la computadora local
setwd("~/MEDGC/13_LaboratorioImplementacion/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar
dataset <- fread("./datasets/FE4020/paquete_premium_202011_ext001.csv", stringsAsFactors = TRUE)

# paso la clase a binaria que tome valores {0,1}  enteros
# dataset[, clase01 := ifelse(clase_ternaria == "BAJA+2", 1L, 0L)]

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

ksemilla_azar <- 777137
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

    # BO FE MJF ---------------
    # max_bin=31,
    # num_iterations = 616,
    # learning_rate = 0.0102839503170953,
    # feature_fraction = 0.293310935998755,
    # min_data_in_leaf = 1792,
    # num_leaves = 799
    
    # BO FE MJF 001 ----------
    max_bin=31,
    num_iterations = 156,
    learning_rate = 0.043509034934703,
    feature_fraction = 0.200199955359218,
    min_data_in_leaf = 7995,
    num_leaves = 1024
    
  )
)

# aplico el modelo a los datos sin clase
dapply <- fread("./datasets/FE4020/paquete_premium_202101_ext001.csv")

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

#ordeno la tabla por probabilidad descendente, al comienzo quedan los registros que mas debo enviar estimulo
setorder( tb_entrega, -prob )

# Genero la entrega para Kaggle
entrega <- as.data.table(list(
  "numero_de_cliente" = dapply[, numero_de_cliente],
  # "Predicted" = prediccion > 1 / 60	
  # "Predicted" = prediccion > 0.0147209568126027
  "Predicted" = prediccion > 0.0154748670733059
)) # genero la salida

dir.create("./labo/exp/", showWarnings = FALSE)
dir.create("./labo/exp/KA2512/", showWarnings = FALSE)
archivo_salida <- "./labo/exp/KA2512/KA_512_FE_001.csv"

# genero el archivo para Kaggle
fwrite(entrega,
  file = archivo_salida,
  sep = ","
)

# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "./labo/exp/KA2512/KA_512_importancia_FE_001.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)
