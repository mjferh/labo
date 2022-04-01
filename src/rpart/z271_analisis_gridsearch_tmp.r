library(data.table)
library(ggplot2)

setwd("~/MEDGC/13_LaboratorioImplementacion1/")

res001 <- fread("./labo/exp/HT2020/gridsearch_ftp_20220401_001938.txt")

ds <- res001[ganancia_promedio >= 10976666, 
       .SD, 
       by = ganancia_promedio]

ggplot(res001[ganancia_promedio >= 10976666], aes(x=min_split)) +  geom_bar()
ggplot(res001[ganancia_promedio >= 10976666], aes(x=min_bucket)) +  geom_bar()
ggplot(res001[ganancia_promedio >= 10976666], aes(x=max_depth)) +  geom_bar()
ggplot(res001[ganancia_promedio >= 10976666], aes(x=cp)) +  geom_bar()

# -----------------------------------------

files <-  list.files(path="./labo/exp/HT2020", pattern=NULL, full.names=TRUE)
list_dt <- lapply(files, fread, 
                  select = c("cp", "min_split", "min_bucket", "max_depth", "ganancia_promedio"))
dt <- rbindlist(list_dt)

dtf <- dt[cp < 0, .SD, by = .(min_split, min_bucket, max_depth)]
