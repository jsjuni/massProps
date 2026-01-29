# Roll up mass properties uncertainties without input validation

`rollup_mass_props_unc_fast()` performs the same operation as
[`rollup_mass_props_unc()`](https://jsjuni.github.io/massProps/reference/rollup_mass_props_unc.md)
but omits input validation. It is somewhat faster than
[`rollup_mass_props_unc()`](https://jsjuni.github.io/massProps/reference/rollup_mass_props_unc.md)
but should be used with caution and only under circumstances in which
the caller assumes responsibility for validity of input. Its behavior
when passed ill-formed input is unspecified.

## Usage

``` r
rollup_mass_props_unc_fast(tree, df)
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

The updated data frame

## Examples

``` r
rollup_mass_props_unc_fast(sawe_tree, sawe_table)
#>         id  mass       Cx      Cy      Cz     Ixx      Iyy      Izz     Ixy
#> 1   Widget 57.83 121.2000  0.0400 -0.1600 7258.90  8607.02 10453.40  834.44
#> 2 2nd Part 16.80  70.9000 -0.9500  0.4600   65.07  1124.65  1078.82   76.01
#> 3 Combined 74.63 109.8657 -0.1829 -0.0204 7341.73 42739.26 44547.27 1559.36
#>        Ixz      Iyz sigma_mass  sigma_Cx  sigma_Cy   sigma_Cz sigma_Ixx
#> 1 -1198.38 -1066.58    1.24160 0.2764000 0.2085000 0.06690000  386.9233
#> 2   202.83    13.62    1.73080 0.6234000 0.5173000 0.14050000   12.4687
#> 3 -1401.94 -1060.95    2.13008 0.9580009 0.1999846 0.06178391  387.4017
#>   sigma_Iyy sigma_Izz sigma_Ixy sigma_Ixz sigma_Iyz Ipoint POIconv
#> 1  171.4792  414.5547 1440.5402  344.6237  124.6860  FALSE       +
#> 2  109.1324  108.5481   55.8879  212.1241   11.5408  FALSE       +
#> 3 2787.8795 2813.9052 1488.0844  418.6026  125.3175  FALSE       +
```
