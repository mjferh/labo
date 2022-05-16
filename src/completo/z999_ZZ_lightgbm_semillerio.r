#Ha llegado la hora de la felicidad, al fin el semillerio
#  (lamentablemente aun no es el poder total, ya que no es el estado del arte : "Hibridacion de Semillerios"  )

# demora en correr MUCHO MAS que el ZZ original, ya que se construyen  PARAM$semillerio  modelos  ( 50 por default )

#Generacion del Final Model, Scoring y Kaggle   3-in-one
#extraigo automaticamente los mejores parametros de la Bayesian Optimization, del archivo BOlog.txt
#genero el modelo entrenando en los datos train_final  PERO  ensamblando al cambiar las semillas
# debe quedar MUY CLARO  que el entrenamiento final  se hace sobre un dataset DISTINTO a donde hice la  B.O.
#   en particular si a la B.O. la hice sobre un dataset con undersampling de los CONTINUA, ahora entreno considerando TODOS los CONTINUA
# ya NO hago Bayesian Optimization

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


require("primes")
require("lightgbm")


source( "~/labo/src/lib/exp_lib.r" )


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_iniciar( )

#genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
ksemillas  <- sample(primos)[ 1:PARAM$semillerio ]   #me quedo con PARAM$semillerio primos al azar


#cargo el dataset que tiene el dataset de training final
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dtrain_final )
dataset  <- fread( nom_arch )

dataset[ , clase01 := ifelse( get( PARAM$const$campo_clase ) %in% PARAM$clase_train_POS, 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset),
                           c( PARAM$const$campo_clase, "clase01") )


#cargo el dataset que tiene los datos del futuro
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dfuture )
dfuture  <- fread( nom_arch )


#cargo la salida de la optimizacion bayesiana
#y la ordeno por ganancia descendente
nom_arch  <- exp_nombre_archivo( PARAM$files$input$BOlog )
tb_log   <- fread( nom_arch )
setorder( tb_log,  -ganancia )


#tabla donde guardo el resultado final
tb_modelos  <- data.table( archivo=  character(),
                           iteracion_bayesiana= integer(),
                           ganancia= numeric() )

tb_predicciones  <- data.table( archivo= character(),
                                iteracion_bayesiana= integer(),
                                ganancia=  numeric() )

tb_submits  <- data.table( archivo= character(),
                           iteracion_bayesiana=  integer(),
                           ganancia=  numeric(),
                           corte=  integer() )



#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos_qty )
{
  parametros  <- as.list( copy( tb_log[ i ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana

  archivo_modelo  <- paste0( PARAM$files$output$FMmodelo ,
                             sprintf( "%03d", iteracion_bayesiana ), 
                             ".model" )

  tb_modelos  <- rbind( tb_modelos,
                        list( archivo_modelo,
                              parametros$iteracion_bayesiana,
                              parametros$ganancia ) )

  
  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( get( PARAM$const$campo_clase ) %in% PARAM$clase_test_POS, 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  iteracion_bayesiana  <- parametros$iteracion_bayesiana
  ganancia  <- parametros$ganancia

  #elimino los parametros que no son de lightgbm
  parametros$fecha       <- NULL
  parametros$prob_corte  <- NULL
  parametros$estimulos   <- NULL
  parametros$ganancia    <- NULL
  parametros$iteracion_bayesiana  <- NULL
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
  modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )

  #grabo el modelo, achivo .model
  lgb.save( modelo_final,
            file= archivo_modelo )

  #creo y grabo la importancia de variables
  tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
  fwrite( tb_importancia,
          file= paste0( PARAM$files$output$FMimportancia, 
                        sprintf( "%03d", iteracion_bayesiana ),
                        ".txt" ),
          sep= "\t" )

  fwrite( tb_modelos,
          file= PARAM$files$output$tb_modelos,
          sep= "\t" )

  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture ) )

  tb_prediccion  <- dfuture[  , PARAM$const$campos_pk,  with=FALSE ]
  tb_prediccion[ , prob := prediccion ]


  nom_pred  <- paste0( PARAM$files$output$prefijo_pred,
                       sprintf( "%03d", iteracion_bayesiana),
                       ".csv"  )

  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )

  #agrego y grabo la prediccion
  tb_predicciones  <- rbind( tb_predicciones,
                             list( nom_pred, iteracion_bayesiana, ganancia ) )

  fwrite( tb_predicciones,
          file=  PARAM$files$output$tb_predicciones,
          sep= "\t" )


  #genero los archivos para Kaggle
  cortes  <- seq( from= PARAM$KA_start,
                  to=   PARAM$KA_end,
                  by=   PARAM$KA_step )


  setorder( tb_prediccion, -prob )

  for( corte in cortes )
  {
    tb_prediccion[  , (PARAM$const$campo_pred) := 0L ]
    tb_prediccion[ 1:corte, (PARAM$const$campo_pred) := 1L ]

    nom_submit  <- paste0( EXP$experiment$name, 
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  tb_prediccion[ , c( PARAM$const$campo_id, PARAM$const$campo_pred) , with=FALSE ],
             file= nom_submit,
             sep= "," )


    #grabo la tabla de los nombres de los submits
    tb_submits  <- rbind( tb_submits,
                          list( nom_submit,
                                iteracion_bayesiana,
                                ganancia,
                                corte )  )

    fwrite( tb_submits,
            file= PARAM$files$output$tb_submits,
            sep= "\t"  )
  }

  #Hasta este punto fue exactamente lo mismo que antes
  #Pero ahora, empiezo a acumular predicciones

  tb_prediccion_semillerio  <- dfuture[  , PARAM$const$campos_pk,  with=FALSE ]
  #le asigno el ranking
  tb_prediccion_semillerio[ , pred_acumulada := as.numeric( frank(prediccion, ties.method= "random") ) ]

  #itero por las semillas acumulando la prediccion
  for( vsemilla in ksemillas )
  {
     parametros$seed  <- vsemilla
     set.seed( parametros$seed )
     modelo_final_semillerio  <- lightgbm( data= dtrain,
                                param=  parametros,
                                verbose= -100 )

    #genero la prediccion como ranking
    prediccion_semillerio  <- frank( predict( modelo_final_semillerio, data.matrix( dfuture ) ),
                                     ties.method= "random" )

    #acumulo el ranking de la prediccion
    tb_prediccion_semillerio[ , pred_acumulada := pred_acumulada + prediccion_semillerio ]
  }

  nom_pred_semillerio  <- paste0( PARAM$files$output$prefijo_pred_semillerio,
                                  sprintf( "%03d", iteracion_bayesiana),
                                  ".csv"  )

  fwrite( tb_prediccion_semillerio,
          file= nom_pred_semillerio,
          sep= "\t" )


  #ahora, a la prediccion de semillerio la corto en distintos umbrales y genero el archivo para Kaggle
  setorder( tb_prediccion_semillerio, -pred_acumulada )

  for( corte in cortes )
  {
    tb_prediccion_semillerio[  , (PARAM$const$campo_pred) := 0L ]
    tb_prediccion_semillerio[ 1:corte, (PARAM$const$campo_pred) := 1L ]

    nom_submit  <- paste0( EXP$experiment$name, 
                           "_semillerio_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  tb_prediccion_semillerio[ , c( PARAM$const$campo_id, PARAM$const$campo_pred) , with=FALSE ],
             file= nom_submit,
             sep= "," )


    #grabo la tabla de los nombres de los submits
    tb_submits  <- rbind( tb_submits,
                          list( nom_submit,
                                iteracion_bayesiana,
                                ganancia,
                                corte )  )

    fwrite( tb_submits,
            file= PARAM$files$output$tb_submits,
            sep= "\t"  )
  }



  #borro y limpio la memoria para la vuelta sigueinte del for
  rm( prediccion_semillerio  )
  rm( modelo_final_semillerio )
  rm( tb_prediccion_semillerio )
  rm( tb_prediccion )
  rm( tb_importancia )
  rm( prediccion )
  rm( modelo_final )
  rm( parametros )
  rm( dtrain )
  gc()
}


# grabo catalogo   ------------------------------------------------------------
# es lo ultimo que hago, indica que llegue a generar la salida
#no todos los archivos generados pasan al catalogo

exp_catalog_add( action= "FM",
                 type=   "file",
                 key=    "tb_modelos",
                 value = PARAM$files$output$tb_modelos  )

exp_catalog_add( action= "SC",
                 type=   "file",
                 key=    "predicciones",
                 value = PARAM$files$output$tb_predicciones  )

exp_catalog_add( action= "KA",
                 type=   "file",
                 key=    "kaggle_submits",
                 value = PARAM$files$output$tb_submits  )


#finalizo el experimento
#HouseKeeping
exp_finalizar( )
