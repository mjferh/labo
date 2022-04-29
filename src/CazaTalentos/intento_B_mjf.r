# Intento de Solucion del desafio  15k
# Basado en el script intento_B.r

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

Estrategia_B  <- function()
{
  #Estrategia
  # Se juegan varias rondas (5)
  # En cada ronda, los jugadores que participan, tiran una cantidad de especificada de tiros 
  # De una ronda a la otra, solo pasan los que tuvieron mayores aciertos a la mediana de aciertos acumulados por ronda
  # Esta condición evita que el número de jugadores clasificados superen a los previstos por la estrategia
  # Los tiros de cada ronda se ajustan en función de los jugadores esperados a clasificar por ronda 
  # Si pasan menos, se aumentan los tiros para compensar la probabilidad pretendida.
  # De esta manera se respetan los tiros totales previstos por ronda
  # Se elige el mejor jugador después de la quinta ronda respecto a los tiros acumulados.

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )
  
  
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   75  tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  tiros <- 75
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := tiros ]  #registro en la planilla que tiran 70 tiros
  resultado1  <- gimnasio_tirar( ids_juegan1, tiros)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan1,  aciertos_acum := aciertos1]

  #Ronda 2 -------------------------------------------------------
  #A la mitad mejor la hago tirar 75 tiros cada uno - Si clasifican menos de 50, aumento la cantidad de tiros
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan1, median(aciertos_acum) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos_acum > mediana, id ]
  tiros <- floor((75 * 50) / length(ids_juegan2))
  
  planilla_cazatalentos[ ids_juegan2,  tiros2 := tiros ]  #registro en la planilla que tiran 70 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, tiros)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan2,  aciertos_acum := aciertos_acum + aciertos2 ]

  #Ronda 3 -------------------------------------------------------
  #A la mitad mejor la hago tirar 60 tiros cada uno - Si clasifican menos de 25, aumento la cantidad de tiros
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan2, median(aciertos_acum) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ aciertos_acum > mediana, id ]
  tiros <- floor((60 * 25) / length(ids_juegan3))
  
  planilla_cazatalentos[ ids_juegan3,  tiros3 := tiros ]  #registro en la planilla que tiran 70 tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, tiros)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan3,  aciertos_acum := aciertos_acum + aciertos3 ]

  #Ronda 4 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno - Si clasifican menos de 13, aumento la cantidad de tiros
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan3, median(aciertos_acum) ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ aciertos_acum > mediana, id ]
  tiros <- floor((70 * 13) / length(ids_juegan4))
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := tiros]  #registro en la planilla que tiran 70 tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, tiros)
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan4,  aciertos_acum := aciertos_acum + aciertos4 ]
  
  #Ronda 5 -------------------------------------------------------
  #A la mitad mejor la hago tirar 170 tiros cada uno, si clasifican menos de 7, aumento la cantidad de tiros
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan4, median(aciertos_acum) ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ aciertos_acum > mediana, id ]
  tiros <- floor((170 * 7) / length(ids_juegan5))
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := tiros ]  #registro en la planilla que tiran 70 tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, tiros)
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan5,  aciertos_acum := aciertos_acum + aciertos5 ]
  
  
   #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo acumulados en todas las rondas
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos_acum) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}


#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 777137 )  #debe ir una sola vez, ANTES de los experimentos
#set.seed( 666 )

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  veredicto  <- Estrategia_B()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total 
tasa_eleccion_correcta
