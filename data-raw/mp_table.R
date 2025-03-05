library(igraph)

mp_table <- read.table("data-raw/mp_table.tsv", header = TRUE, sep = "\t")

mp_edges <- as.matrix(read.table("data-raw/mp_edges.tsv", header = TRUE, sep = "\t")[, c("child", "parent")])
mp_tree <- graph_from_edgelist(mp_edges, directed = TRUE)

root <- V(mp_tree)[degree(mp_tree, mode = "out") == 0]
dist <- dfs(mp_tree, root, order = FALSE, dist = TRUE, mode = "in")$dist
small_top_vs <- which(dist <= 2)
small_bot_vs <- which(dist == 3)

mp_table_small <- rbind(
  mp_table[is.element(mp_table$id, names(small_top_vs)), ],
  rollup_mass_props_and_unc(mp_tree, mp_table)[is.element(mp_table$id, names(small_bot_vs)), ]
)
mp_tree_small <- induced_subgraph(mp_tree, V(mp_tree)[union(small_top_vs, small_bot_vs)])

usethis::use_data(mp_table, overwrite = TRUE)
usethis::use_data(mp_tree, overwrite = TRUE)

usethis::use_data(mp_table_small, overwrite = TRUE)
usethis::use_data(mp_tree_small, overwrite = TRUE)

