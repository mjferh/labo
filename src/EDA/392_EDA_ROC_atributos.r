#atencion, demora dos minutos en correr
#la salida queda en  ./labo/exp/ST3920/392_EDA.jpg

#Curva ROC de cada atributo del dataset
#las variables con mayor AUC  son las variables mas importantes del dataset
#POS = { BAJA+2 }
#NEG = { BAJA+1, CONTINUA }

rm( list=ls() )  #Borro todos los objetos
gc()             #Garbage Collection

require("data.table")

#Aqui se debe poner la carpeta de la computadora local
setwd("D:\\gdrive\\Austral2022R\\")   #Establezco el Working Directory

#------------------------------------------------------------------------------

graficarROC  <- function( tbl,  nalast )
{
  #calculo la cantidad total de positivos y negativos de todo el dataset
  POS  <- sum( dataset$pos )
  NEG  <- sum( dataset$neg )

  
  #ordeno en forma ASCENDENTE
  orden  <- 1
  setorderv( tbl, vcampo, orden, na.last= nalast )

  #agrego el origen  (0, 0)
  tbl  <- rbind( list( -Inf, 0, 0), tbl )

  #acumulo positivos y negativos para armar la CurvaROC
  tbl[ , pos_acum := cumsum( pos ) ]
  tbl[ , neg_acum := cumsum( neg ) ]

  #Calculo de AUC   Area Under Curve , la gran metrica !!
  #suma de area de trapecios
  AUC  <- tbl[ , sum( (pos_acum + shift(pos_acum)) * ( neg_acum - shift(neg_acum)), na.rm=TRUE )  / (2*POS*NEG ) ]

  #si la curva ROC va por debajo de la diagonal, ORDENO inversamente por  vcamnpo
  if( AUC < 0.5 )
  {
    #borro el primer registro
    tbl  <- tbl[ -1 ]

    #ordeno en forma DESCENDENTE
    orden  <-  -1
    setorderv( tbl, vcampo, orden, na.last= nalast )

    #agrego primer registro
    tbl  <- rbind( list( Inf, 0, 0, 0, 0), tbl )

    #acumulo positivos y negativos para armar la CurvaROC
    tbl[  , pos_acum  := cumsum( pos ) ]
    tbl[  , neg_acum  := cumsum( neg ) ]

    #recalculo el AUC que ahora va a dar >  0.5
    AUC  <- tbl[ , sum( (pos_acum + shift(pos_acum)) * ( neg_acum - shift(neg_acum)), na.rm=TRUE )  / (2*POS*NEG ) ]
  }

  if( AUC >= 0.5 )
  {
    #genero el grafico de la curva ROC
    plot( x= tbl$neg_acum,
          y= tbl$pos_acum,
          main= paste0( vcampo,  
                        "    ROC curve, ",
                        ifelse( NAS==0," no hay NAs, ", 
                                ifelse( nalast, " NAs last, ", " NAs first, ") ),
                        "\n",
                        ifelse( orden== 1, "", " invertido, " ),
                        "AUC=", round(AUC,4) ),
          xlab= "negativos",
          ylab= "positivos",
          xaxs= "i",
          yaxs= "i",
          xlim= c(0, NEG ),
          ylim= c(0, POS ),
          col= "blue",
          type= "l",            #tipo  linea
          lwd= 6,               #ancho de la linea
          panel.first = grid()
        )

    #la diagonal
    abline( a= 0, b= POS/NEG, col= "black", lwd= 2 )
  }
}
#------------------------------------------------------------------------------
#Aqui empieza el programa


#cargo el dataset
dataset  <- fread( "./datasets/paquete_premium_202011.csv" )

#primero, creo la carpeta donde van los resultados
dir.create( "./labo/exp/", showWarnings= FALSE )
dir.create( "./labo/exp/ST3920", showWarnings= FALSE )
archivo  <- "./labo/exp/ST3920/392_EDA.pdf"

campos_buenos  <- setdiff( colnames( dataset ), "clase_binaria" )

#defino los positivos y negativos
dataset[ , pos  := as.integer( clase_ternaria == "BAJA+2" ) ]
dataset[ , neg  := as.integer( clase_ternaria != "BAJA+2" ) ]

POS  <- sum( dataset$pos )
NEG  <- sum( dataset$neg )


pdf( archivo )   #inicio la escritura al archivo pdf

#itero sobre los campos del dataset

for( vcampo in campos_buenos )
{
  #cuento la cantidad de NAs que tiene el campo  vcampo
  NAS  <- dataset[ , sum( is.na( get( vcampo) ) ) ]

  #genero la tablita minima necesaria para construir la curva ROC
  tbl  <- dataset[ ,
                   list( "pos" = sum(pos),
                         "neg" = sum(neg) ),
                   by= vcampo ]

  #Aqui grafico el primer grafico
  graficarROC( tbl, nalast= FALSE )  #aqui debe ir FALSE

  #para el caso que hay nulos en vcampo  DEBO ordenar poniendo los nulos al final
  #lo unico que cambia es el orden inicial, que ahora es na.last= TRUE
  if( NAS > 0 )  graficarROC( tbl, nalast= TRUE )  #aqui debe ir TRUE

  #libero espacio de la tablita  tbl
  rm( tbl )
  gc()
}

dev.off()
