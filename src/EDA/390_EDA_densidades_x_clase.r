#grafico para cada variable, las densidades de cda una de las tres clases

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ggplot2")     #para graficar las densidades

#Aqui se debe poner la carpeta de la computadora local
setwd("D:\\gdrive\\Austral2022R\\")   #Establezco el Working Directory

#cargo el dataset
dataset  <- fread( "./datasets/paquete_premium_202011.csv" )

#primero, creo la carpeta donde van los resultados
dir.create( "./labo/exp/", showWarnings= FALSE )
dir.create( "./labo/exp/ST3900", showWarnings= FALSE )
archivo  <- "./labo/exp/ST3900/390_EDA.pdf"


#creo una clase binaria,  BAJA p CONTINIUA
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "CONTINUA", "BAJA" ) ]
dataset[ , clase_ternaria := NULL ]  #elimino la clase ternaria

pdf(archivo)   #inicio la escritura al archivo  pdf

#itero sobre los campos del dataset
campos_buenos  <- setdiff( colnames( dataset ), "clase_binaria")

for( vcampo in   campos_buenos )
{
  
  cardinalidad  <-  dataset[ , length(unique(get(vcampo))) ]

  if( cardinalidad < 20 )
  {
      grafico  <- ggplot( dataset, aes_string(x= vcampo, fill= "clase_binaria") ) +
                  geom_histogram(alpha= 0.40, position= "identity", aes(y = ..density..), clolor= "black")

  } else {

    #para que los graficos se aprecien correctamente, quito ambas colas del 5%
    xlimites  <- quantile( dataset[ , get(vcampo) ],
                           prob= c(0.05,0.95),
                           type= 1,
                           na.rm= TRUE )

    grafico  <- ggplot( dataset, aes_string(x= vcampo, fill= "clase_binaria") ) +
                  scale_x_continuous(limits= xlimites )  +
                  geom_density(alpha= 0.25) 
  }
  
  print(grafico)   #imprimo el grafico
}

dev.off()  #cierro el archivo  .pdf

