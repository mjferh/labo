#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require( "data.table" )

dataset  <- fread( "~/buckets/b1/datasets/paquete_premium.csv.gz" )

fwrite( dataset[ foto_mes==202011, ],
        file= "~/buckets/b1/datasets/paquete_premium_202011.csv.gz",
        sep="\t" )

fwrite( dataset[ foto_mes==202101, ],
        file= "~/buckets/b1/datasets/paquete_premium_202101.csv.gz",
        sep="\t" )

