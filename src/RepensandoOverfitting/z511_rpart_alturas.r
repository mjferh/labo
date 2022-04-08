#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

setwd("D:\\gdrive\\Austral2022R\\" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset_entrenar  <- fread( "./datasets/paquete_premium_202011.csv")

#dejo la clase binaria
dataset_entrenar[ , clase_binaria:= ifelse( clase_ternaria=="BAJA+2", "POS","NEG" ) ]
dataset_entrenar[ , clase_ternaria:= NULL ]

#cargo los datos donde aplico el modelo
dataset_aplicar  <- fread( "./datasets/paquete_premium_202101.csv")

#donde voy a guardar los resultados
dir.create( "./labo/exp/", showWarnings= FALSE )
dir.create( "./labo/exp/KA5110", showWarnings= FALSE )
setwd( "./labo/exp/KA5110" )


#genero salidas para Kaggle probando profundidades de 6 a 30
#las ganancias sobre todo training van a ir mejorando siempre
#las ganancias en el Public Leaderboard, van a ir subiendo, llegan a un maximo, y luego descienden
# undefitting al comienzo,  overfitting luego de alcanzar el máximo

for( vmaxdepth  in 6:30 )
{

  #genero el modelo
  modelo  <- rpart(formula= "clase_binaria ~ .",  
                   data= dataset_entrenar, 
                   model= TRUE, #quiero que me devuelva el modelo
                   xval= 0,
                   cp= 0,
                   minsplit= 5,
                   maxdepth=  vmaxdepth
                  )

  #aplico el modelo a los datos en donde entrene
  prediccion_202011  <- predict( modelo, dataset_entrenar, type = "prob")
  ganancia_202011 <-  sum(  (prediccion_202011[, "POS"] > 1/60) *
                            ifelse( dataset_entrenar$clase_binaria=="POS", 59000, -1000 ) )


  cat( "maxdepth", vmaxdepth, "\t", "ganancia", ganancia_202011, "\n" )

  prediccion_202101  <- predict( modelo, dataset_aplicar, type = "prob")

  prob_pos  <- prediccion_202101[, "POS"]
  estimulo  <- as.numeric(prob_pos > 1/60)

  #el dataset en formato  Kaggle
  entrega <-  as.data.table( list(  "numero_de_cliente"= dataset_aplicar$numero_de_cliente,
                                    "Predicted"=  estimulo ) )

  #genero el archivo para Kaggle
  fwrite( entrega, 
          file= paste0("altura_", vmaxdepth, ".csv"))

}


