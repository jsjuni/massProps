# Roll up radii of gyration uncertainties

`rollup_radii_of_gyration_unc()` adds calculated radii of gyration
uncertainties to a data frame of rolled-up mass properties and
uncertainties.

Radii of gyration uncertainties are calculated directly from moments of
inertia and mass and their uncertainties; they are not
recursively-defined. Radii of gyration uncertainties for composite
elements depend on uncertainties of their component elements.

## Usage

``` r
rollup_radii_of_gyration_unc(tree, df)
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

A data frame with the same columns as `df`, plus radii of gyration in
columns `sigma_kx`, `sigma_ky`, and `sigma_kz`.\`

## Examples

``` r
sawe_table_rollup <- rollup_mass_props(sawe_tree, sawe_table)
rollup_radii_of_gyration_unc(sawe_tree, add_radii_of_gyration(sawe_table_rollup))
#>         id  mass       Cx         Cy          Cz      Ixx      Iyy      Izz
#> 1   Widget 57.83 121.2000  0.0400000 -0.16000000 7258.900  8607.02 10453.40
#> 2 2nd Part 16.80  70.9000 -0.9500000  0.46000000   65.070  1124.65  1078.82
#> 3 Combined 74.63 109.8769 -0.1828594 -0.02043146 7341.733 42673.75 44482.05
#>        Ixy       Ixz       Iyz sigma_mass sigma_Cx sigma_Cy sigma_Cz sigma_Ixx
#> 1  834.440 -1198.380 -1066.580     1.2416   0.2764   0.2085   0.0669  386.9233
#> 2   76.010   202.830    13.620     1.7308   0.6234   0.5173   0.1405   12.4687
#> 3 1558.714 -1401.534 -1060.951     2.1301   0.9591   0.2000   0.0618  387.4017
#>   sigma_Iyy sigma_Izz sigma_Ixy sigma_Ixz sigma_Iyz Ipoint POIconv        kx
#> 1  171.4792  414.5547 1440.5402  344.6237  124.6860  FALSE       + 11.203631
#> 2  109.1324  108.5481   55.8879  212.1241   11.5408  FALSE       +  1.968048
#> 3 2794.5468 2820.5125 1488.1857  418.6320  125.3175  FALSE       +  9.918422
#>          ky        kz  sigma_kx  sigma_ky  sigma_kz
#> 1 12.199719 13.444733 0.3219068 0.1786630 0.3031529
#> 2  8.181898  8.013456 0.2140836 0.5789819 0.5769935
#> 3 23.912428 24.413817 0.2971254 0.5505729 0.5422391
```
