#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")


setwd( "D:\\gdrive\\Austral2022R\\" )  #establezco la carpeta donde voy a trabajar

#cargo el dataset
dtrain  <- fread( "./datasets/paquete_premium_202011.csv")
#ordeno para que queden en orden BAJA+1, BAJA+2, CONTINUA
#The order of the loss matrix depends on the order of your factor variable in R
setorder( dtrain, clase_ternaria )

#cargo donde voy a aplicar el modelo
dapply  <- fread( "./datasets/paquete_premium_202101.csv")

#uso esta semilla para los canaritos
set.seed(10219)

campos_originales  <- copy( colnames( dtrain ) )

#agrego  30 canaritos
for( i in 1:30)  dtrain[ , paste0("canarito", i ) :=  runif( nrow(dtrain) ) ]
for( i in 1:30)  dapply[ , paste0("canarito", i ) :=  runif( nrow(dapply) ) ]

#reodeno los campos de  dtrain de forma que los canaritos queden primeros
campos_extendidos  <- colnames( dtrain )
campos_lindos  <-  c( setdiff( campos_extendidos, campos_originales ),  campos_originales )
setcolorder(  dtrain, campos_lindos )


#la matrix de perdida,  con 59 vs 1 , definida por el problema
peso_error  <- 59
matriz_perdida  <- matrix(c( 0,peso_error,1,   1,0,1,   1,peso_error,0), nrow = 3)

#Genero un arbol sin limite
#los canaritos me van a proteger
modelo_original  <- rpart(formula= "clase_ternaria ~ . ",
                          data= dtrain,
                          xval= 0,
                          model= TRUE,
                          cp=        -1,
                          maxdepth=  30,  #lo dejo crecer lo mas posible
                          minsplit=   2,
                          minbucket=  1 )
                          #parms= list( loss= matriz_perdida)  )


#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

nodes <- as.numeric(rownames(modelo_pruned$frame))
max(rpart:::tree.depth(nodes))


prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"BAJA+2"]

entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                 "Predicted"= as.integer(  prediccion >  1/60  ) ) )


#creo la carepta donde guardo el resultado
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA5230/", showWarnings = FALSE )
setwd("D:\\gdrive\\Austral2022R\\labo\\exp\\KA5230\\")   #Establezco el Working Directory DEL EXPERIMENTO

#grabo la salida para Kaggle
fwrite( entrega, 
        file= "stopping_at_canaritos.csv",
        sep= "," ) 


#imprimo el arbol original
pdf(file = "arbol_libre.pdf", width=80, height=10)
prp(modelo_original, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()


#imprimo el arbol pruneado por canaritos
pdf(file = "stopping_at_canaritos.pdf", width=20, height=4)
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
