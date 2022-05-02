# XGBoost  sabor original ,  cambiando algunos de los parametros

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("xgboost")

#Aqui se debe poner la carpeta de la computadora local
setwd("~/MEDGC/13_LaboratorioImplementacion/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita XGBoost
dtrain  <- xgb.DMatrix( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- xgb.train( data= dtrain,
                      param= list( objective=       "binary:logistic",
                                   tree_method=     "auto",
                                   grow_policy=     "depthwise",
                                   
                                   # Default -----------------------------------
                                   # max_depth=           6,
                                   # min_child_weight=    1,
                                   # eta=                 0.3,
                                   # colsample_bytree=    1,
                                   
                                   # BO ----------------------------------------
                                   max_depth=           29,
                                   min_child_weight=    10,
                                   eta=                 0.0100619161743528,
                                   colsample_bytree=    0.760340017874888
                                   ),
                      #nrounds= 34
                      # nrounds= 3
                      nrounds= 339
                      
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
# entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
#                                  "Predicted"= prediccion > 1/60)  ) #genero la salida
                                 # "Predicted"= prediccion > 0.0148523683953432)  ) #genero la salida
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion)  ) #genero la salida

setorder(entrega, -Predicted)
entrega[, Predicted := 0]
entrega[1:11000, Predicted := 1]

dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA5610/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/KA5610/KA_561_t02_02_04.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )
