#Feature Engineering
#creo nuevas variables dentro del mismo mes

#este script se muestra como esqueleto para que los alumnos agreguen sus propias variables
#ya sea basados en la teoria economica  o en el salvaje empiricismo
# "No es que la nueva variable creada, que funciona, no tenga sentido, lo que sucede es que yo estoy siendo capaz de encontrarselo en este momento"

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

EnriquecerDataset  <- function( dataset , arch_destino )
{

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status

  dataset[ , mv_status01 := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02 := Master_status +  Visa_status ]
  dataset[ , mv_status03 := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04 := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05 := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06 := ifelse( is.na(Visa_status), 
                                    ifelse( is.na(Master_status), 10, Master_status), 
                                    Visa_status) ]

  dataset[ , mv_status07 := ifelse( is.na(Master_status), 
                                    ifelse( is.na(Visa_status), 10, Visa_status), 
                                    Master_status) ]


  #combino MasterCard y Visa , teniendo en cuenta los NA
  dataset[ , mv_mfinanciacion_limite  := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento          := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora           := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal           := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos           := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares         := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos        := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares      := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra         := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos        := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares      := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre        := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado               := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos           := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares         := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta             := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal         := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos             := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo    := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo           := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]


  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra := Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra   := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal          := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos          := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2         := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares        := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2       := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos       := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares     := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos       := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares     := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado              := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos          := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares        := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal        := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo          := mv_mpagominimo  / mv_mlimitecompra ]
  
  # BEGIN mjfer ---------------------------
  
  #Asignar nulos en vez de cero
  dataset[ ctarjeta_visa==0 & ctarjeta_visa_trx==0, ctarjeta_visa_trx := NA ]
  dataset[ ctarjeta_master==0 & ctarjeta_master_trx==0, ctarjeta_master_trx := NA ]
  
  #Nivel tecnológico [0,1,2,3,4]
  dataset[, mjf_nivel_tec := internet + tcallcenter + thomebanking + tmobile_app]
  
  #nivel tecnológico por edad
  dataset[, mjf_nivel_tec_edad := mjf_nivel_tec * cliente_edad]
  
  #Cantidad de productos - para validar cproductos
  dataset[, mjf_cproductos := tpaquete1 + tpaquete2 + tpaquete7 + tpaquete9 + 
             tcuentas +  ccuenta_corriente + ccaja_ahorro + 
             ctarjeta_debito + ctarjeta_visa + ctarjeta_master + 
             cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios + 
             cplazo_fijo + cinversion1 + cinversion2 + ccaja_seguridad + 
             cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]
  #Cantidad operaciones/mes - similar a ctrx_quater
  dataset[, mjf_coper_mes := ctarjeta_debito_trx + ctarjeta_visa_trx + ctarjeta_master_trx + 
            cpayroll_trx + 
            ccuenta_debitos_automaticos + 
            ctarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos +
            cpagodeservicios + cpagomiscuentas + cforex +
            ctransferencias_recibidas + ctransferencias_emitidas + 
            cextraccion_autoservicio + ccheques_depositados + ccheques_emitidos +
            ccajas_depositos + ccajas_extracciones]
  # Cantidad de descuentos
  dataset[, mjf_cdescuentos := ccajeros_propios_descuentos + ctarjeta_visa_descuentos + 
            ctarjeta_master_descuentos]
  
  
  # Ratio operaciones at. personal / autoservicio
  dataset[, mjf_rautoservicio := (ccajas_trx / 
                                    (ccallcenter_trx + chomebanking_trx + cmobile_app_trx + 
                                     catm_trx + catm_trx_other))]
  # Ratio operaciones cajeros propios / otros bancos
  dataset[, mjf_rcajeros := catm_trx / catm_trx_other]
  
  # Variables Manuales 2 ------------------
  # Segunda iteración de FE
  
  PARAMS <- list()
  PARAMS$varimportantes <- c("ctrx_quarter",
                             "mtarjeta_visa_consumo",
                             "mjf_coper_mes",
                             "mcuentas_saldo",
                             "mpayroll",
                             "mcaja_ahorro",
                             "cpayroll_trx",
                             "mprestamos_personales",
                             "mpasivos_margen",
                             "mcuenta_corriente",
                             "mpayroll_sobre_edad")
  
  if( T ){ #PARAM$variablesmanuales ){
    
    # Normalizo importes en pesos - Orden por fecha
    varmontos <- colnames(dataset)
    varmontos <- varmontos[(
      (startsWith(varmontos, "m") & !startsWith(varmontos, "mjf")) |
      startsWith(varmontos, "mjf_m") |
      startsWith(varmontos, "Master_m") |
      startsWith(varmontos, "Visa_m")
    )]
    
    for(var in varmontos){
      var_name <- paste0("mjf_", var, "_orden")
      setorderv(dataset1, c("foto_mes", var))
      dataset[, (var_name) := 1:.N, by = c("foto_mes")]
    }
        
    # Calculo el cociente entre las variables indicadas como importante
    if(length(PARAMS$varimportantes) > 0){
      combinations <- combn(PARAMS$varimportantes, 2, simplify = F)  
      for(var in combinations){
        var_name <- paste0("mjf_", var[1], "-", var[2])
        dataset[ , (var_name) :=  get(var[1]) / get(var[2])  ]
      }
    }     
    
    
  }
  
  
  
  # FIN mjfer------------------------------

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply( names(dataset),
                            function(.name) dataset[ , sum(is.infinite( get(.name) )) ]  )
  
  infinitos_qty  <- sum( unlist( infinitos ) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply( names(dataset),
                       function(.name) dataset[ , sum( is.nan( get(.name) )) ] )
  
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el script!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  #FIN de la sección donde se deben hacer cambios con variables nuevas

  #grabo con nombre extendido
  fwrite( dataset,
          file= arch_destino,
          sep= "," )

}
#------------------------------------------------------------------------------

#aqui comienza el programa

#Establezco el Working Directory
setwd("~/MEDGC/13_LaboratorioImplementacion/")

#lectura de los datasets
dataset1  <- fread("./datasets/paquete_premium_202011.csv")
dataset2  <- fread("./datasets/paquete_premium_202101.csv")

dataset1[0:10 ,foto_mes:=202010]
dataset1[, .N , by=foto_mes]

#creo la carpeta donde va el experimento
# FE  representa  Feature Engineering
dir.create( "./datasets/",  showWarnings = FALSE ) 
dir.create( "./datasets/FE4020/", showWarnings = FALSE )
setwd("~/MEDGC/13_LaboratorioImplementacion/datasets/FE4020/")   #Establezco el Working Directory DEL EXPERIMENTO

EnriquecerDataset( dataset1, "paquete_premium_202011_ext002.csv" )
EnriquecerDataset( dataset2, "paquete_premium_202101_ext002.csv" )

dataset1[foto_mes == 202010, .(numero_de_cliente, mcaja_ahorro, mjf_mcaja_ahorro_orden)]

aaa <- "mcaja_ahorro"
setorderv(dataset1, c("foto_mes", aaa))


dataset1[foto_mes == 202010, .(numero_de_cliente, mcaja_ahorro, mjf_mcaja_ahorro_orden), by=mjf_mcaja_ahorro_orden]
