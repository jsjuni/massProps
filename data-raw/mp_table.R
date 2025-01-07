## code to prepare `massprops` dataset goes here
library(igraph)

mp_table <- read.table("data-raw/mp_table.tsv", header = TRUE, sep = "\t")

mp_edges <- as.matrix(read.table("data-raw/mp_edges.tsv", header = TRUE, sep = "\t")[, c("child", "parent")])
mp_tree <- graph_from_edgelist(mp_edges, directed = TRUE)

usethis::use_data(mp_table, overwrite = TRUE)
usethis::use_data(mp_tree, overwrite = TRUE)
