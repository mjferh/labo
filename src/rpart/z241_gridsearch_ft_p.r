# Grid Seach rpart
# Ejecución paralelizada para Windows v.0.1
#
# Las librerías requeridas al principio deben ser invocadas explicitamente 
# en las funciones definidas a continuación para que sean reconocidas por 
# parallel::clusterApply

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
# install.packages("filelock")
require("filelock") 

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
# Particiono estratificadamente el dataset
# MODIFICACIÓN: la versión paralelizada hace una copia del dataset - Esto 
#   degrada los tiempos aprox. en un 75% pero se compensa con los 4 entrenamientos 
#   por núcleo físico
ArbolEstimarGanancia <- function(semilla, param_basicos) {
  
  tmp_data <- data.table::copy(dataset) # Copia local
  particionar(tmp_data, division = c(70, 30), agrupa = "clase_ternaria", seed = ksemillas[1])
  #cat(file = archivo_salida,append = TRUE,"VUEVE PARTY!!!")
  
  # genero el modelo
  modelo <- rpart::rpart("clase_ternaria ~ .", # quiero predecir clase_ternaria a partir del resto
    data = tmp_data[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
                        tmp_data[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades

  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- tmp_data[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 1 / 60,
      ifelse(clase_ternaria == "BAJA+2", 59000, -1000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3
  
  # rm(tmp_data)
  
  return(ganancia_test_normalizada)
}

#------------------------------------------------------------------------------
# MODIFICACIÓN: utilizo lapply en lugar de mcmapply. Cambian el orden de los 
#   parámetros, el resultado es el mismo.
ArbolesMontecarlo <- function(semillas, param_basicos) {
 
  ganancias <- lapply(
    semillas, # paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
    ArbolEstimarGanancia,
    param_basicos = param_basicos # aqui paso el segundo parametro
  )
  
  ganancia_promedio <- mean(unlist(ganancias))
  return(ganancia_promedio)
}

#------------------------------------------------------------------------------
# Llama a ArbolesMontecarlo de acuerdo al juego de parámetros pasados. 
# Escribe en un archivo la ganancia promedio del entrenamiento para esos 
# parámetros.
# Se utiliza la librería filelock para controlar el acceso concurrente de los 
# procesos al archivo 
# Devuelve un lista con los parámetros junto con la ganancia y tiempo de ejec.
ArbolesCluster <- function(params) {
  t0 <- Sys.time()
  ganancia_promedio <- ArbolesMontecarlo(ksemillas, param_basicos=params)
  t1 <- Sys.time()
  t_delta <- as.numeric(t1 - t0, units = "secs")

  params$t0 = format(t0, "%H:%M:%S")
  params$t1 = format(t1, "%H:%M:%S")
  params$t_delta = t_delta
  params$ganancia_promedio = ganancia_promedio
   
  lock = filelock::lock(archivo_salida)  
  cat(
    file = archivo_salida,
    append = TRUE,
    sep = "\t",
    c(unlist(params))
  )
  cat(file = archivo_salida,append = TRUE,"\n")
  filelock::unlock(lock)
  
  return (params)
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

# Agrego fecha y ahora al nombre para no sobrescribir archivos
archivo_salida <- paste0(
  "./labo/exp/HT2020/gridsearch_ftp_",
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  ".txt"
)

# Escribo los títulos al archivo donde van a quedar los resultados
cat(
  file = archivo_salida,
  sep = "\t",
  c("min_split", "min_bucket", "cp", "max_depth", "t0", "t1", "t_seg", "ganancia_promedio\n")
)

# Entrenamiento con las combinaciones de parámetros
params_min_split <- c(50, 100, 250, 500, 750, 1000)
params_min_bucket <- c(10, 20, 50, 100, 200, 500)
params_cp <- c(-1, seq(0, 0.001, 0.0001))
params_max_depth <- c(4, 6, 8, 10)

# Cantidad de iteraciones (* 5 semillas)
seg_prom <- 50  #tiempo medio incluyendo los 5 entrenamientos por juego de parámetros (5 semillas)
cant_iter <- length(params_min_split) * length(params_min_bucket) * length(params_cp) * length(params_max_depth)
cat("Cantidad Modelos:", as.character(cant_iter))
cat("Hs estimadas:", cant_iter * seg_prom / 60 / 60 / 4) # 4: núcleos

# Dataframe con todas las combinaciones posibles.
param_grid <- expand.grid(
  "minsplit" = params_min_split,
  "minbucket" = params_min_bucket,
  "cp" = params_cp,
  "maxdepth" = params_max_depth
)

# Paso el dataframe a una lista (cada elemento es una fila del dataframe)
param_grid_list <- split(param_grid, seq(nrow(param_grid)))

# Detecto la cantidad de núcleos y creo el cluster
# Incluyendo los lógicos no se nota una mejora sustancial. 
ncores <- detectCores(logical = FALSE)
cl <- makeCluster(ncores)

# Hago visible variables globales y funciones a los clusters. Las librerías 
# usadas en las funciones (fuera del paquete base) deben ser referenciadas 
# explicitamente

clusterExport(cl = cl, 
              c("archivo_salida", "param_grid", "dataset", "ksemillas",
                "ArbolesMontecarlo", "ArbolEstimarGanancia", "particionar"), 
              envir=environment())
# clusterApply ejecuta tanto procesos como clusters definidos en cl y para cada 
# uno uno de los elementos de la lista pasada como parámetro (x) aplica la función 
# ArbolesCluster
result <- clusterApply(cl, x = param_grid_list, fun = ArbolesCluster)

stopCluster(cl)

