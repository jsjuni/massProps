library(igraph)

test_table <- read.table("data-raw/test_table.tsv", header = TRUE, sep = "\t")

test_edges <- as.matrix(read.table("data-raw/test_edges.tsv", header = TRUE, sep = "\t")[, c("id", "parent")])
test_tree <- graph_from_edgelist(test_edges, directed = TRUE)

test_unc_table <- Reduce(
  f = function(d, i) {
    mp <- set_poi_conv_from_target(d, i, get_mass_props(d, i))
    mp$sigma_mass <- mp$mass / 10
    mp$sigma_center_mass <- abs(mp$center_mass) / 10
    mp$sigma_inertia <- abs(mp$inertia) / 10
    set_mass_props_and_unc(d, i, mp)
  },
  x = setdiff(test_table$id, test_table$parent),
  init = test_table
)

usethis::use_data(test_table, overwrite = TRUE)
usethis::use_data(test_unc_table, overwrite = TRUE)
usethis::use_data(test_tree, overwrite = TRUE)
