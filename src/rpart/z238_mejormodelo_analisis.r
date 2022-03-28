#Este script demora varias horas en correr
#se sugiere solo entender el codigo e interpretar los resultados que genera
#los resultados quedan en el repositorio GitHub  /exp/ST1005

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

require("primes")      #para obtener numeros primos que seran semillas
require("ggplot2")     #para grafiucar las densidades

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

ArbolesMontecarlo  <- function( semillas,  param_basicos )
{
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                          MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                          SIMPLIFY= FALSE,
                          mc.cores= 16 )  #se puede subir a 5 si posee Linux o Mac OS

  #media de las ganancias
  return(  unlist(ganancias) )
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("~/MEDGC/13_LaboratorioImplementacion1/")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/paquete_premium_202011.csv")


#defino los modelos que voy a ccmparar
paramA  <- list( "cp"=         -1,  #complejidad minima
                 "minsplit"=  300,  #minima cantidad de registros en un nodo para hacer el split
                 "minbucket"= 150,  #minima cantidad de registros en una hoja
                 "maxdepth"=    6 ) #profundidad máxima del arbol

paramB  <- list( "cp"=          0,  #complejidad minima
                 "minsplit"=   15,  #minima cantidad de registros en un nodo para hacer el split
                 "minbucket"=   5,  #minima cantidad de registros en una hoja
                 "maxdepth"=   10 ) #profundidad máxima del arbol

paramC  <- list( "cp"=         -1,  #complejidad minima
                 "minsplit"=   50,  #minima cantidad de registros en un nodo para hacer el split
                 "minbucket"=  16,  #minima cantidad de registros en una hoja
                 "maxdepth"=    6 ) #profundidad máxima del arbol



#creo un vector con 1000 numeros primos tomados al azar del intervalo [ 100k, 1M ]
kcantidad_semillas <- 1000   #cantidad de semillas con las que voy a trabajar
set.seed( 19 )               #seteo inicial para replicabilidad

primos  <- generate_primes(min=100000, max=1000000)   #genero TODOS los numeros primos entre 100k y 1M
vsemillas  <- sample(primos)[ 1:kcantidad_semillas ]  #me quedo con kcantidad_semillas primos al azar


#calculo las ganancias
ganA  <- ArbolesMontecarlo( vsemillas, paramA )
ganB  <- ArbolesMontecarlo( vsemillas, paramB )
ganC  <- ArbolesMontecarlo( vsemillas, paramC )

#---------------------------------------
#calculos directos

#primero, probabilidad que la ganancia de un modelo sea mayor a la del otro
sum( ganA > ganB ) / length(ganA)
# 0.686   el 68.6% de las veces  el modelo A performa mejor que el modelo B

sum( ganA > ganC ) / length(ganA)
# 0.5   el 50% de las veces  el modelo A performa mejor que el modelo C
# es decir A y C performan igual

sum( ganC > ganB ) / length(ganA)
# 0.708   el 70.8% de las veces  el modelo C performa mejor que el modelo B


#luego  media diferencias
mean( ganA - ganB )
# 803953.3   sin dudas  el modelo A es superior a B

mean( ganA - ganC )
# -2903.333   diferencia insignificante, se puede decir que A y C performan igual

mean( ganC - ganB )
# 806856.7    sin dudas  el modelo C es superior a B


#-------------------------------
#finalmente  test  Wilcoxon
wilcox.test( ganA, ganB ) 
#        Wilcoxon rank sum test with continuity correction

# data:  ganA and ganB
# W = 645408, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# como p-value <  0.05    ganA es distinto a ganB

wilcox.test( ganA, ganC ) 
#        Wilcoxon rank sum test with continuity correction

#data:  ganA and ganC
#W = 499360, p-value = 0.9605
#alternative hypothesis: true location shift is not equal to 0
# como p-value >> 0.05    ganA es no es distinto que ganC


#cuantas corridas hacen falta para saber que  A es mejor que B ?
for(  hasta  in  5:100 )
{
  w  <- wilcox.test( ganA[1:hasta], ganB[1:hasta] )
  cat(  hasta, "\t",  w$p.value, "\n" )
}

# 5        0.8412698
# 6        0.6991342
# 7        0.710373
# 8        0.6453768
# 9        0.6048128
# 10       0.6305289
# 11       0.6063178
# 12       0.9323005
# 13       0.8798465
# 14       0.8036128
# 15       0.9024623
# 16       0.8965332
# 17       0.8384088
# 18       0.7427978
# 19       0.6861453
# 20       0.5831142
# 21       0.636097
# 22       0.5223524
# 23       0.4074195
# 24       0.407116
# 25       0.2886601
# 26       0.3584177
# 27       0.3713059
# 28       0.3488517
# 29       0.2926881
# 30       0.2187565
# 31       0.2742744
# 32       0.231206
# 33       0.2011048
# 34       0.2382503
# 35       0.2059715
# 36       0.1729619
# 37       0.2117902
# 38       0.1905389
# 39       0.2323932
# 40       0.1939282
# 41       0.2122649
# 42       0.1351757
# 43       0.1683247
# 44       0.1186225
# 45       0.1386609
# 46       0.1239581
# 47       0.1131415
# 48       0.1168511
# 49       0.07745263
# 50       0.07875774
# 51       0.0595632
# 52       0.07380173
# 53       0.06100107
# 54       0.04998454
# 55       0.0705441
# 56       0.06854279
# 57       0.05157222
# 58       0.06122685
# 59       0.05501114
# 60       0.04223128
# 61       0.04285212
# 62       0.03855242
# 63       0.04786978
# 64       0.06241305
# 65       0.04402412 ----
# 66       0.02825976
# 67       0.0228385
# 68       0.02282957
# 69       0.01759661
# 70       0.01833151
# 71       0.01511255
# 72       0.01781914
# 73       0.01599591
# 74       0.01488216
# 75       0.01448284
# 76       0.01088699
# 77       0.007374238
# 78       0.004834983
# 79       0.00341096
# 80       0.005109289
# 81       0.002902003
# 82       0.003476924
# 83       0.003966073
# 84       0.004113668
# 85       0.005358669
# 86       0.004726279
# 87       0.003404435
# 88       0.00476147
# 89       0.002850658
# 90       0.002401621
# 91       0.001883129
# 92       0.001895918
# 93       0.001172423
# 94       0.0008875103
# 95       0.0008408663
# 96       0.0007764348
# 97       0.0005073981
# 98       0.0004142067
# 99       0.0005548294
# 100      0.0006799341


#---------------------------------------
#grafico las desnsidades

#primero, creo la carpeta donde van los resultados
dir.create( "./labo/exp/" ) 
dir.create( "./labo/exp/ST2005" ) 

#primero creo un dataset con los resultados

#cada vector a un pequeño dataset
tblA  <-  as.data.table( list( "modelo"= "A", "gan"= ganA) )
tblB  <-  as.data.table( list( "modelo"= "B", "gan"= ganB) )
tblC  <-  as.data.table( list( "modelo"= "C", "gan"= ganC) )

#concateno los datasets
tbl  <- rbind( tblA, tblB, tblC )


fwrite( tbl,
        file= "./labo/exp/ST2005/tresmodelos.txt",
        sep=  "\t" )

#grafico
pdf("./labo/exp/ST2005/tres_modelos.pdf")

grafico  <- ggplot( tbl, aes(x=gan, fill=modelo)) + geom_density(alpha=0.25) 
print(grafico)

dev.off()

