# LightGBM  Motivacional con corte por cantidad de envios a Kaggle
# Se utilizan exactamente los mismos hiperparametros que el z552_lightgbm_motivacional 
#  PERO el submit a Kaggle se genera por cantidad de registros a enviar y NO por probabilidad

# se generan MUCHOS archivos para subir a Kaggle y probarlos, se van a gastar 15 de los 20 envios del dia

# utilizar la primer semilla propia

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


ksemilla  <- 102191  #poner aqui la PRIMERA de sus cinco semillas

#Aqui se debe poner la carpeta de la computadora local
setwd("D:\\gdrive\\Austral2022R\\")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )


#genero el modelo con los parametros por default
#estos hiperparametros  salieron de una Optmizacion Bayesiana
#esto es IDENTICO  al script  z552_lightgbm_motivacional
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   max_bin=              31,
                                   learning_rate=         0.067,
                                   num_iterations=      128,
                                   num_leaves=          100,
                                   min_data_in_leaf=   1700,
                                   feature_fraction=      0.37,
                                   seed=               ksemilla   #aqui se utiliza SU primer semilla
                                  )
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])  )


#guardo el resultado
#creo las carpetas
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA5530/", showWarnings = FALSE )
setwd( "./labo/exp/KA5530/" )

#imprimo varias salidas para Kaggle, cada una contiene

#genero la tabla con el numero_de_cliente  y la probabilidad que devolvio el modelo
tb_entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                    "prob" = prediccion  ) )

#ordeno la tabla por probabilidad descendente, al comienzo quedan los registros que mas debo enviar estimulo
setorder( tb_entrega, -prob )

#mensaje para @vladimiro  @JuanPabloAndres  ,  se puede probar con un "by" menor a 500
#pero estan abriendo un portal al infierno llamado  "Overfiteando el  Public Leaderboard"

for(  cantidad_estimulos  in  seq( from= 7000, to= 14000, by= 500 ) )
{
  #Genero la entrega para Kaggle
  tb_entrega[ , Predicted  := 0L ]  #pongo todos en cero
  tb_entrega[ 1:cantidad_estimulos ,  Predicted  := 1L ]  #solo van en 1 los primeros  cantidad_estimulos

  #el nombre del archivo de salida, que se le apendea al final la cantidad de 1's a enviar
  archivo_salida  <- paste0( "KA_553_" , cantidad_estimulos )

  #genero el archivo para Kaggle
  fwrite( tb_entrega[ , list( numero_de_cliente, Predicted ) ],
          file= archivo_salida,
          sep= "," )

}



#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "553_importancia_001.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )


