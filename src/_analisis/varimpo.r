require("data.table")
require("tidyverse")

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd("~/MEDGC/13_LaboratorioImplementacion/")
path_bucket <- "bucket/exp/"
path_analisis <- "labo/exp/ANALISIS/"

hts <- list.dirs(path = path_bucket, full.names = F, recursive = F)
hts <- hts[startsWith(hts, "HT") & !endsWith(hts, "BAK")]

for(ht in hts){
  impvar_files <- list.files(path = paste0(path_bucket, ht, "/"), pattern = "impo.*txt", recursive = F, full.names = F)
  if(length(impvar_files) < 1){next}
  if(file.exists(paste0(path_analisis, ht, ".csv"))){next}
  
  dt.vars <- map_df(.x = impvar_files, .f = function(f){
    dt <- fread(paste0(path_bucket, ht,"/", f))
    dt[, ':='(exp = ht, file = f)]
  })
  
  dt <- dt.vars[, .(GainMin = min(Gain), GainMean = mean(Gain), GainMax = max(Gain)), by=.(exp, Feature)]
  setorder(dt, -GainMean)
  fwrite( dt[1:30], file= paste0(path_analisis, ht, ".csv"), sep= ";", dec = ",")

}

