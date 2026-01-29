# Validate a mass properties table

`validate_mass_props_table()` checks that the names of vertices in a
tree and the `id` values of a data frame are identical. It further
applies the checks of
[`validate_mass_props()`](https://jsjuni.github.io/massProps/reference/validate_mass_props.md)
to every row of the data frame corresponding to a leaf vertex of the
tree.

`validate_mass_props_table()` ensures that the `id` column of the table
and the vertices of the tree contain the same identifiers, and that the
mass properties of every leaf element of the table are valid.

## Usage

``` r
validate_mass_props_table(tree, df)
```

## Arguments

- tree:

  An 'igraph' tree whose vertices are named as the values of the `id`
  column of `df` and whose directed edges point from child id to parent
  id.

- df:

  A data frame with (at least) these columns: `id`, `mass`, `Cx`, `Cy`,
  `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`.

## Value

TRUE if valid, stops with an error otherwise

## Examples

``` r
validate_mass_props_table(mp_tree_small, mp_table_small)
#> [1] TRUE
```
