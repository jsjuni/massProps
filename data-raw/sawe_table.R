library(igraph)

sawe_table <- read.table("data-raw/sawe_table.tsv", header = TRUE, sep = "\t")

sawe_edges <- as.matrix(read.table("data-raw/sawe_edges.tsv", header = TRUE, sep = "\t")[, c("child", "parent")])
sawe_tree <- graph_from_edgelist(sawe_edges, directed = TRUE)

usethis::use_data(sawe_table, overwrite = TRUE)
usethis::use_data(sawe_tree, overwrite = TRUE)
