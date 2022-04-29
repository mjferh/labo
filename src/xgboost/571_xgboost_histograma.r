# XGBoost  sabor HISTOGRAMA

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
                                   tree_method=     "hist",
                                   grow_policy=     "lossguide",
                                   
                                   # Default -----------------------------------
                                   # max_leaves=          20,
                                   # min_child_weight=    1,
                                   # eta=                 0.129342891119721,
                                   # colsample_bytree=    0.3
                                   
                                   # BO ----------------------------------------
                                   max_leaves=          2,
                                   min_child_weight=    10,
                                   eta=                 0.129342891119721,
                                   colsample_bytree=    0.579870761461218
                                   ),
                      # nrounds= 138
                      nrounds= 393
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 # "Predicted"= prediccion > 1/60)
                                 "Predicted"= prediccion > 0.0195292761341395)  ) #genero la salida

dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA5710/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/KA5710/KA_571_t02_02.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )
