rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ggplot2")

setwd("~/MEDGC/13_LaboratorioImplementacion1/")

# Salida del script Z238_mejormodelo_analisis.r 
tbl  <- fread("./labo/exp/ST1005/tresmodelos.txt")

grafico  <- ggplot( tbl, aes(x=gan, fill=modelo)) + geom_density(alpha=0.25) 
print(grafico)
