require("data.table")
require("tidyverse")

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

# Parámetros ------------------------------------------------
params <- list()
params$path_bucket <- "./bucket/exp/"
params$path_ensemble <- "./ensemble/"

params$nombre_exp <- "EN155"
params$require <- c("ZZ8410", "ZZ9412", "ZZ9414")
params$KA_start <- 9000
params$KA_end <- 13000
params$KA_step <- 500

setwd("~/MEDGC/13_LaboratorioImplementacion/")
# -----------------------------------------------------------

dt_list <- list()
for (exp in params$require) {
  ka_files <- list.files(
    path = paste0(params$path_bucket, exp, "/"),
    pattern = "futuro_prediccion_.*.csv",
    recursive = F, full.names = F
  )
  if (length(ka_files) < 1) {
    next
  }

  dt <- map_df(.x = ka_files, .f = function(f) {
    dt <- fread(paste0(params$path_bucket, exp, "/", f))
    setorder(dt, -prob)
    dt[, ":="(orden = .I, exp = exp, file = f)] # , file = f
  })

  dt_list[[exp]] <-  dt
}

dt_bind <- rbindlist(dt_list)

dt_avg <- dt_bind[, .(orden = mean(orden), n = .N), by = numero_de_cliente]
setorder(dt_avg, orden)

cortes <- seq(
  from = params$KA_start,
  to = params$KA_end,
  by = params$KA_step
)

path_ka <- paste0(params$path_ensemble, "/", params$nombre_exp, "/")
dir.create(path_ka,  showWarnings = FALSE ) 

for (corte in cortes) {
  dt_avg[, Predicted := 0L]
  dt_avg[1:corte, Predicted := 1L]

  nom_submit <- paste0(
    params$nombre_exp,
    "_",
    sprintf("%05d", corte),
    ".csv"
  )
  
  fwrite(dt_avg[, .(numero_de_cliente, Predicted)],
    file = paste0(path_ka, nom_submit),
    sep = ","
  )
}
