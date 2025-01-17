#intencionalmente el mejor jugador va al final de la lista de jugadores
#porque la funcion which.max() de R hace trampa
#si hay un empate ( dos máximos) se queda con el que esta primero en el vector

require("data.table")

set.seed( 777137 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

setwd("~/MEDGC/13_LaboratorioImplementacion/")
dir.create("./labo/exp/", showWarnings = FALSE)
dir.create("./labo/exp/CT/", showWarnings = FALSE)

#defino los jugadores
mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) #intencionalmente el mejor esta al final

#veo que tiene el vector
# jugadores

#hago que los 100 jugadores tiren 10 veces cada uno
# mapply(  ftirar, jugadores, 10 )

archivo_salida <- paste0(
  "./labo/exp/CT/prob_median_",
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  ".txt"
)

# c(0.5, 0.25, 0.125, 0.0625)
for (q in  c(0.03125)){ 
  for(  tiros_libres  in seq(60, 500, 5) )
  {
  
    primero_pasa  <- 0
  
    for( i in 1:10000 )  #diez mil experimentos
    {
      vaciertos  <- mapply( ftirar, jugadores, tiros_libres )
      #mejor  <- which.max( vaciertos )
      
      umbral = quantile(vaciertos, probs = 1 - q)
      # umbral = median(vaciertos)
      
      #if( mejor == 100 )  primero_ganador  <- primero_ganador + 1
      if( vaciertos[100] > umbral)  primero_pasa  <- primero_pasa + 1
    }
  
    cat(
      file = archivo_salida,
      append = TRUE,
      sep = "\t",
      c(q, tiros_libres, primero_pasa/10000))
    cat(file = archivo_salida, append = TRUE, "\n")
  }
}

