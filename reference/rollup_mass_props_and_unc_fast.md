# Roll up mass properties and uncertainties without input validation

`rollup_mass_props_and_unc_fast()` performs the same operation as
[`rollup_mass_props_and_unc()`](https://jsjuni.github.io/massProps/reference/rollup_mass_props_and_unc.md)
but omits input validation. It is somewhat faster than
[`rollup_mass_props_and_unc()`](https://jsjuni.github.io/massProps/reference/rollup_mass_props_and_unc.md)
but should be used with caution and only under circumstances in which
the caller assumes responsibility for validity of input. Its behavior
when passed ill-formed input is unspecified.

## Usage

``` r
rollup_mass_props_and_unc_fast(tree, df)
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
rollup_mass_props_and_unc_fast(sawe_tree, sawe_table)
#>         id  mass       Cx         Cy          Cz      Ixx      Iyy      Izz
#> 1   Widget 57.83 121.2000  0.0400000 -0.16000000 7258.900  8607.02 10453.40
#> 2 2nd Part 16.80  70.9000 -0.9500000  0.46000000   65.070  1124.65  1078.82
#> 3 Combined 74.63 109.8769 -0.1828594 -0.02043146 7341.733 42673.75 44482.05
#>        Ixy       Ixz       Iyz sigma_mass sigma_Cx  sigma_Cy   sigma_Cz
#> 1  834.440 -1198.380 -1066.580    1.24160  0.27640 0.2085000 0.06690000
#> 2   76.010   202.830    13.620    1.73080  0.62340 0.5173000 0.14050000
#> 3 1558.714 -1401.534 -1060.951    2.13008  0.95821 0.1999847 0.06178402
#>   sigma_Ixx sigma_Iyy sigma_Izz sigma_Ixy sigma_Ixz sigma_Iyz Ipoint POIconv
#> 1  386.9233  171.4792  414.5547 1440.5402  344.6237  124.6860  FALSE       +
#> 2   12.4687  109.1324  108.5481   55.8879  212.1241   11.5408  FALSE       +
#> 3  387.4017 2789.3133 2815.3260 1488.0948  418.6048  125.3175  FALSE       +
```
