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

# setwd("~/MEDGC/13_LaboratorioImplementacion/")
# dir.create("./labo/exp/", showWarnings = FALSE)
# dir.create("./labo/exp/CT/", showWarnings = FALSE)

#defino los jugadores
mejor      <-  0.7
mejor_peor    <-  0.599
jugadores  <-  c( mejor_peor, mejor ) #intencionalmente el mejor esta al final

# archivo_salida <- paste0(
#   "./labo/exp/CT/prob_median_",
#   format(Sys.time(), "%Y%m%d_%H%M%S"),
#   ".txt"
# )



  for(  tiros_libres  in seq(10, 400, 5) )
  {
  
    primero_ganador  <- 0
  
    for( i in 1:10000 )  #diez mil experimentos
    {
      vaciertos  <- mapply( ftirar, jugadores, tiros_libres )
      mejor  <- which.max( vaciertos )
      
      if( mejor == 2 )  primero_ganador  <- primero_ganador + 1
    }
  
    cat(
      # file = archivo_salida,
      # append = TRUE,
      # sep = "\t",
      c(tiros_libres, primero_ganador/10000, "\n"))
    # cat(file = archivo_salida, append = TRUE, "\n")
  }


