library(igraph)

test_table <- read.table("data-raw/test_table.tsv", header = TRUE, sep = "\t")

test_edges <- as.matrix(read.table("data-raw/test_edges.tsv", header = TRUE, sep = "\t")[, c("id", "parent")])
test_tree <- graph_from_edgelist(test_edges, directed = TRUE)

usethis::use_data(test_table, overwrite = TRUE)
usethis::use_data(test_tree, overwrite = TRUE)
