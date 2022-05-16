# Analsisis de resultados de z541_cluster_jerarquico.r

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("cowplot")
require("ggplot2")

# -----------------------------------------------------------------------------

generarBoxPlotsPorGrupos <- function(var_interes, df = dataset, grupos = "cluster2", outliers = T) {
  
  if(outliers == F){
    df <- as.data.table(df)
    df2 <- df[get(var_interes) < quantile(get(var_interes), probs = 0.95) & 
               get(var_interes) > quantile(get(var_interes), probs = 0.05)] 
    if(df2[,.N] > 0){
      df <- df2
    }
  }
  
  df <- data.frame(df)
  
  etiquetas <- paste(
    levels(factor(df[, grupos])), "\n(", table(df[, grupos]), ")",
    sep = ""
  )
  
  boxplot <- ggplot(
    df,
    aes(
      x = factor(get(grupos)), y = get(var_interes),
      fill = factor(get(grupos))
    )
  ) +
    geom_boxplot() +
    theme(legend.position = "none") +
    scale_x_discrete(name="", labels = etiquetas) +
    scale_y_continuous(name = paste0(var_interes)) +
    geom_hline(yintercept = median(df[, var_interes])) +
    theme(axis.text.x = element_text(size = rel(0.75)))
  
  return(boxplot)
}


# -----------------------------------------------------------------------------
setwd("~/MEDGC/13_LaboratorioImplementacion/")  #cambiar por la carpeta local

#primero, creo la carpeta donde van los resultados
# dir.create( "./labo/exp/", showWarnings= FALSE )
# dir.create( "./labo/exp/ST4610", showWarnings= FALSE )
setwd( "~/MEDGC/13_LaboratorioImplementacion/labo/exp/ST4610" )

campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_trx",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_trx", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster
dataset <- fread("cluster_de_bajas.txt")

#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

#imprimo un pdf de lboxplot por grupos (con outliers)
boxplots <- lapply(X = campos_buenos, FUN = generarBoxPlotsPorGrupos)
pdf( "cluster_jerarquico_boxplots.pdf", height = 30)
plot_grid(plotlist = boxplots, ncol = 3)
dev.off()

#imprimo un pdf de boxplots por grupos (sin outliers)
boxplots <- lapply(X = campos_buenos, FUN = generarBoxPlotsPorGrupos, outliers=F)
pdf( "cluster_jerarquico_boxplots_sinout.pdf", height = 30)
plot_grid(plotlist = boxplots, ncol = 3)
dev.off()

# dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
# dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
# dataset[  , mean(mcuentas_saldo),  cluster2 ]
# dataset[  , mean(chomebanking_trx),  cluster2 ]

# var_interes = "cpayroll_trx"
# df <- dataset[get(var_interes) < quantile(get(var_interes), probs = 0.99) & 
#            get(var_interes) > quantile(get(var_interes), probs = 0.01)] 
