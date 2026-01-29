# Validate a mass properties and uncertainties table

[`validate_mass_props_and_unc()`](https://jsjuni.github.io/massProps/reference/validate_mass_props_and_unc.md)
calls
[`validate_mass_props_table()`](https://jsjuni.github.io/massProps/reference/validate_mass_props_table.md)
and further applies the checks of
[`validate_mass_props_and_unc()`](https://jsjuni.github.io/massProps/reference/validate_mass_props_and_unc.md)
to every row of the data frame corresponding to a leaf vertex of the
tree.

## Usage

``` r
validate_mass_props_and_unc_table(tree, df)
```

## Arguments

- tree:

  An 'igraph' tree whose vertices are named as the values of the `id`
  column of `df` and whose directed edges point from child id to parent
  id.

- df:

  A data frame with (at least) these columns: `id`, `mass`, `Cx`, `Cy`,
  `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
  `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`, `sigma_Ixx`,
  `sigma_Iyy`, `sigma_Izz`, `sigma_Ixy`, `sigma_Ixz`, `sigma_Iyz`.

## Value

TRUE if valid, stops with an error otherwise

## Examples

``` r
validate_mass_props_and_unc_table(mp_tree_small, mp_table_small)
#> [1] TRUE
```
