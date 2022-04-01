# Grid Seach rpart

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas <- c(777137, 664967, 507803, 387173, 536867) # reemplazar por las propias semillas

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste en una
# particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
# crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(70, 30), agrupa = "clase_ternaria", seed = semilla)

  # genero el modelo
  modelo <- rpart("clase_ternaria ~ .", # quiero predecir clase_ternaria a partir del resto
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 1 / 60,
      ifelse(clase_ternaria == "BAJA+2", 59000, -1000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 1
  ) # se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}

#------------------------------------------------------------------------------

ArbolesSalida <- function(param_basicos) {
  t0 <- Sys.time()
  ganancia_promedio <- ArbolesMontecarlo(ksemillas, param_basicos)
  t1 <- Sys.time()
  delta <- as.numeric(t1 - t0, units = "secs")

  # escribo los resultados al archivo de salida
  cat(
    file = archivo_salida,
    append = TRUE,
    sep = "\t",
    c(unlist(param_basicos), round(delta,2), ganancia_promedio)
  )

  cat(file = archivo_salida, append = TRUE, "\n")
}

#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# Establezco el Working Directory
setwd("~/MEDGC/13_LaboratorioImplementacion1/")

# cargo los datos
dataset <- fread("./datasets/paquete_premium_202011.csv")

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT representa  Hiperparameter Tuning
dir.create("./labo/exp/", showWarnings = FALSE)
dir.create("./labo/exp/HT2020/", showWarnings = FALSE)

archivo_salida <- paste0(
  "./labo/exp/HT2020/gridsearch_ft",
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  ".txt"
)

# Escribo los titulos al archivo donde van a quedar los resultados
cat(
  file = archivo_salida,
  sep = "\t",
  c("min_split", "min_bucket", "cp", "max_depth", "seg", "ganancia_promedio\n")
)

# Entrenamiento con las combinanciones de parámetros
# Los valores deben estar ordenados de menor a mayor!!!
params_min_split <- c(10, 25, 50, 100, 250, 500, 1000)
params_min_bucket <- floor(params_min_split / 2)
params_cp <- c(-1, -0.75, -0.5, -0.25, seq(0, 0.01, 0.001))
params_max_depth <- c(4, 6, 8, 10, 15, 20)

# Cantidad de iteraciones (* 6 semillas)
seg_prom = 30
cant_iter = length(params_min_split) * length(params_min_bucket) * length(params_cp) * length(params_max_depth)
cat("Cantidad Iteraciones:", as.character(cant_iter))
cat("Hs estimadas:", cant_iter * seg_prom / 60 / 60)

# Setting cp to a negative amount ensures that the tree will be fully grown.
# 2*minbucket <= minsplit
# for (vmax_depth in c(4, 6, 8, 10, 12, 14))
# for (vmin_split in c(1000, 800, 600, 400, 200, 100, 50, 20, 10))

for (min_split in params_min_split) {
  i <- 1
  if (i <= length(params_min_bucket)) min_bucket <- params_min_bucket[i]

  while (i <= length(params_min_bucket) && ((2 * min_bucket) <= min_split)) {
    for (cp in params_cp) {
      for (max_depth in params_max_depth) {
        ArbolesSalida(list(
          "minsplit" = min_split, # mínima cantidad de registros en un nodo para hacer el split
          "minbucket" = min_bucket, # mínima cantidad de registros en una hoja
          "cp" = cp, # complejidad mínima
          "maxdepth" = max_depth # profundidad máxima del árbol
        ))
      }
    }

    i <- i + 1
    if (i <= length(params_min_bucket)) min_bucket <- params_min_bucket[i]
  }
}


