# script para estudiantes MUY, pero MUY  avanzados
# en lugar de hacer Montecarlo Estimation con 5 semillas  se hace  usando una sola semilla  5-fold cross validation
# anecdoticamente, el resultado se almacena en un data.table en lugar de imprimirlo directamente a un archivo
# el alumno avanzado irá haciendo un fwrite de tb_resultados DENTRO del loop, no sea que se aborte RStudio y se pierda todo !
# el alumno avanzado armara los loops externos para lograr un Grid Search

#limpio la memoria
rm(list=ls())   #remove all objects
gc()            #garbage collection

require("data.table")
require("parallel")
require("rpart")


ksemilla  <- 102191  #reemplazar por la PRIMER semilla

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   particiona estratificadamente en 5 folds iguales

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}
#------------------------------------------------------------------------------
#dado un dataset ya particionado en folds, testea en el fold_test y entrena en el resto de los folds
#devuelve la ganancia SIN normalizar

ArbolSimple  <- function( fold_test, data, param, peso_error )
{
  #va la matriz de perdida,  por columnas
  matriz_perdida  <- matrix(c( 0,peso_error,1,   1,0,1,   1,peso_error,0), nrow = 3)

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",
                   data= data[ fold != fold_test, ], #training en todo MENOS  fold_test
                   xval= 0,
                   parms= list( loss= matriz_perdida),
                   control= param )

  #aplico el modelo a los datos de testing  fold==fold_test
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ], 
                          type = "prob")

  prob_baja2  <- prediccion[ , "BAJA+2"]

  ganancia_testing  <- data[ fold==fold_test ][ prob_baja2 > 1/60,  
                                                sum( ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ) )] 

  #devuelvo la ganancia SIN normalizar
  return( ganancia_testing )
}
#------------------------------------------------------------------------------
#calcula la ganancia haciendo 5-fold cross validation

ArbolesCrossValidation  <- function( data, param, peso_error, qfolds, semilla )
{
  #particiono el dataest estratificadamente en  qfolds  folds 
  divi  <- rep( 1, qfolds )   # si qfolds=5  esto devuelve  c(1,1,1,1,1)  un vector con 5 unos
  particionar( data, divi, agrupa= "clase_ternaria", seed= semilla )

  #llamo a la funcion ArbolSimple cada vez con un numero de fold distinto
  ganancias  <- mcmapply( ArbolSimple,
                          seq(qfolds), # si qfolds=5  esto da el vector  c(1,2,3,4,5)
                          MoreArgs= list( data, param, peso_error),
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a 5 si posee Linux o Mac OS

  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias )) *  qfolds )   #aqui normalizo
}
#------------------------------------------------------------------------------
#aqui comienza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("D:\\gdrive\\Austral2022R\\")   #Establezco el Working Directory

#cargo los datos donde voy a ENTRENAR el modelo
dataset  <- fread("./datasets/paquete_premium_202011.csv")

#inicializo la tabla donde voy a dejar los resultados
#humildemente, por ahora, solo experimento cambiando  maxdepth
tb_resultados  <- data.table( maxdepth= integer(),
                              vpeso_error= numeric(),
                              ganancia= numeric() )


#prueba con distintas  profundidades
for( vmaxdepth in  c( 4,5,6,7,8,9 ) )
{
for( vpeso_error  in c( 1,10,60 ) )
{
  param_basicos  <- list( "cp"=         -1,  #valor arbitrario, intencionalmente negativo para probar algo distinto a la manada
                          "minssplit"= 400,  #valor arbitrario
                          "minbucket"= 200,  #valor arbitrario
                          "maxdepth"=  vmaxdepth )  #aqui va vmaxdepth que es la variable que va cambiando en el  loop  for

  gan  <- ArbolesCrossValidation( dataset,
                                  param_basicos,    #aqui paso param_basicos
                                  vpeso_error,      #aqui paso vpeso_error
                                  qfolds= 5, # 5-fold cross validation , quizas algun heroe pruebe con  10-fold cross validation
                                  semilla= ksemilla )  # mi primer semilla

  #le agrego al final de tb_resultados el registro compuesto por  < vmaxdepth, vpeso_error, gan >
  tb_resultados  <- rbind( tb_resultados,
                           list( vmaxdepth,
                                 vpeso_error,
                                 gan ) )
}
}

#en esta tabla quedaron acumulados los resultados
tb_resultados
