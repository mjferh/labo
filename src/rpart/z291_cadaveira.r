# Este script es para resolver la duda existencial de Juan Pablo Cadaveira 
# ¿Cuantas semillas deberia utilizar para determinar precisamente cual es el mejor de dos modelos?
# lamento de corazón la respuesta ...

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

require("primes")      #para obtener numeros primos que seran semillas
require("ggplot2")     #para graficar


kcantidad_semillas  <- 1000   #cantidad de semillas con las que voy a trabajar

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(70,30), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  1/60,
                                         ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------
# le paso un vector de semillas,  los hiperparametros del arbol
# devuelve un vector, con la ganancia para cada semilla

ArbolesMontecarlo  <- function( semillas,  param_basicos )
{
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                          MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )  #se puede subir posee Linux o Mac OS

  #media de las ganancias
  return( unlist(ganancias) )
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("D:\\gdrive\\Austral2022R\\")   #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/paquete_premium_202011.csv")


#defino los modelos que voy a ccmparar
paramA  <- list( "cp"=         -1,  #complejidad minima
                 "minsplit"=  300,  #minima cantidad de registros en un nodo para hacer el split
                 "minbucket"= 150,  #minima cantidad de registros en una hoja
                 "maxdepth"=    6 ) #profundidad máxima del arbol


paramC  <- list( "cp"=         -1,  #complejidad minima
                 "minsplit"=   50,  #minima cantidad de registros en un nodo para hacer el split
                 "minbucket"=  16,  #minima cantidad de registros en una hoja
                 "maxdepth"=    6 ) #profundidad máxima del arbol


#creo un vector con los  kcantidad_semillas numeros primos que seran semillas
set.seed( 19 )               #seteo inicial para replicabilidad

primos  <- generate_primes(min=100000, max=1000000)   #genero TODOS los numeros primos entre 100k y 1M
vsemillas  <- sample(primos)[ 1:kcantidad_semillas ]  #me quedo con kcantidad_semillas primos al azar

#calculo las ganancias, aqui se hace el trabajo pesado del script
ganA  <- ArbolesMontecarlo( vsemillas, paramA )
ganC  <- ArbolesMontecarlo( vsemillas, paramC )

#---------------------------------------------
# pongo los vectores  ganA y ganC en una tabla
# y calculo el promedio acumulado,  osea  ganA_mean[5] =  (a1 + a2 + a3 + a4 + a5 ) / 5
# donde a1=ganA[1]  ,  a2=ganA[2],  etc

#primero el vector de las ganancias de los 1000 modelos con hiperparametros A
tb_resultados  <- as.data.table( list( modelo = "modeloA",
                                       qsemillas = 1:kcantidad_semillas,
                                       ganancia  = ganA
                                      ) )

#luego agrego a la tabla el vector de las ganancias de los 1000 modelos con hiperparametros C
tb_resultados  <- rbind( tb_resultados, list( modelo = "modeloB",
                                              qsemillas = 1:kcantidad_semillas,
                                              ganancia  = ganC
                                      ) )


#calculo el promedio acumulado
tb_resultados[ , gan_mean  := cumsum( ganancia ) / qsemillas ,  modelo ]

#---------------------------------------
#ahora grafico los resultados

#primero, creo la carpeta donde van los resultados
dir.create( "./labo/exp/", showWarnings= FALSE )
dir.create( "./labo/exp/ST2910", showWarnings= FALSE )
setwd( "./labo/exp/ST2910" )

#grabo la tabla
fwrite( tb_resultados,
        file= "cadaveira_modelosAC.txt",
        sep=  "\t" )



#grafico solo los primeros 10  ------------------------------------------------
# @JP Cadaveira  solo uso 6 semillas
pdf("cadaveira_modelosAC_1_10.pdf")

grafico  <- ggplot( tb_resultados[ qsemillas <= 10 ], aes( x= qsemillas, y= gan_mean)) +
              scale_x_continuous(breaks= 1:10) +
              geom_line( aes(color= modelo, linetype = modelo)) +
              scale_color_manual(values = c("darkred", "steelblue"))

print(grafico)
dev.off()



#grafico solo los primeros 100  -----------------------------------------------
pdf("cadaveira_modelosAC_1_100.pdf")

grafico  <- ggplot( tb_resultados[ qsemillas <= 100], aes( x= qsemillas, y= gan_mean)) +
              geom_line( aes(color= modelo, linetype = modelo)) +
              scale_color_manual(values = c("darkred", "steelblue"))

print(grafico)
dev.off()


#del 75 al 100 porque estan parejos  ------------------------------------------
pdf("cadaveira_modelosAC_75_100.pdf")

grafico  <- ggplot( tb_resultados[ qsemillas>= 75 & qsemillas <= 100], aes( x= qsemillas, y= gan_mean)) +
              geom_line( aes(color= modelo, linetype = modelo)) +
              scale_color_manual(values = c("darkred", "steelblue"))

print(grafico)
dev.off()


#del 900 al 1000 --------------------------------------------------------------
pdf("cadaveira_modelosAC_900_1000.pdf")

grafico  <- ggplot( tb_resultados[ qsemillas>= 900 & qsemillas <= 1000], aes( x= qsemillas, y= gan_mean)) +
              geom_line( aes(color= modelo, linetype = modelo)) +
              scale_color_manual(values = c("darkred", "steelblue"))

print(grafico)
dev.off()


#grafico  TODOS  --------------------------------------------------------------
pdf("cadaveira_modelosAC_1_1000.pdf")

grafico  <- ggplot( tb_resultados, aes( x= qsemillas, y= gan_mean)) +
              geom_line( aes(color= modelo, linetype = modelo)) +
              scale_color_manual(values = c("darkred", "steelblue"))

print(grafico)
dev.off()

