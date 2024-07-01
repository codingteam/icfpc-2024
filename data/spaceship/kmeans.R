#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)

taskplot <- function(task) {
  task %>%
  mutate(n=row_number()) %>%
  ggplot(aes(x=X,y=Y,color=n)) +
    geom_point() +
    scale_x_continuous("X", minor_breaks=NULL, breaks=seq(-10000,10000,1)) +
    scale_y_continuous("Y", minor_breaks=NULL, breaks=seq(-10000,10000,1)) +
    theme_bw()
}

taskload <- function(task_id) {
  read.csv(paste0("spaceship", task_id, ".txt"), header=F, sep=" ") %>%
    bind_rows(data.frame(V1=c(0), V2=c(0))) %>%
    distinct() %>%
    mutate(n = row_number()) %>%
    rename("X"="V1", "Y"="V2")
}

clusterize_it <- function(data, ncenters) {
  tab <- data.frame(C=kmeans(data, centers = ncenters, iter.max=100)$cluster) %>%
    mutate(n = row_number())
  data %>% left_join(tab, by=c("n"))
}

savetask <- function(data, task_id, ncenters) {
  options(scipen=10)
  write.table(data %>% select(X, Y, C), paste0("spaceship", task_id, ".txt_kmeans-", ncenters, "_nosort"), row.names=FALSE, col.names=FALSE, quote=FALSE, sep=" ")
  options(scipen=0)
}

prepare <- function(task_id, nmin, nmax) {
  print(task_id)
  data <- taskload(task_id)
  for (ncenters in nmin:nmax) {
    clusters <- clusterize_it(data, ncenters)
    savetask(clusters, task_id, ncenters)
  }
}

prepare(16, 1, 100)
prepare(17, 1, 20)
prepare(18, 1, 20)
prepare(19, 8, 200)
prepare(20, 1, 100)
prepare(21, 1, 100)
prepare(22, 1, 40)

prepare(23, 40, 200)
prepare(24, 4, 200)
prepare(25, 60, 200)

