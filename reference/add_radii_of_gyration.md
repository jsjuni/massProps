# Add radii of gyration

`add_radii_of_gyration()` adds calculated radii of gyration to a data
frame of rolled-up mass properties.

Radii of gyration are calculated directly from moments of inertia and
mass; they are not recursively-defined, and do not require a rollup
method.

## Usage

``` r
add_radii_of_gyration(df)
```

## Arguments

- df:

  A data frame with (at least) these columns: `id`, `mass`, `Cx`, `Cy`,
  `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`.

## Value

A data frame with the same columns as `df`, plus radii of gyration in
columns `kx`, `ky`, and `kz`.\`

## Examples

``` r
test_table_rollup <- rollup_mass_props(test_tree, test_table)
add_radii_of_gyration(test_table_rollup)
#>     id parent mass Cx Cy Cz Ixx  Ixy   Ixz Iyy   Iyz Izz POIconv Ipoint
#> 1  A.1          21  0  0  0 144 -4.8 -24.8 144 -23.2 139       -  FALSE
#> 2  A.2    A.1    8  1  0  0  32 -0.4  -0.4  24   0.4  24       -  FALSE
#> 3  A.3    A.1    8 -1  0  0  32 -0.4  -0.4  24   0.4  24       -  FALSE
#> 4  C.1    A.1    5  0  0  0  80 -4.0 -24.0  80 -24.0  75       -  FALSE
#> 5  P.1    A.2    2  1  1  1   4 -0.1  -0.1   4   0.1   4       -  FALSE
#> 6  P.2    A.2    2  1  1 -1   4 -0.1  -0.1   4   0.1   4       -  FALSE
#> 7  P.3    A.2    2  1 -1  1   4 -0.1  -0.1   4   0.1   4       -  FALSE
#> 8  P.4    A.2    2  1 -1 -1   4 -0.1  -0.1   4   0.1   4       -  FALSE
#> 9  P.5    A.3    2 -1  1  1   4 -0.1  -0.1   4   0.1   4       -  FALSE
#> 10 P.6    A.3    2 -1  1 -1   4 -0.1  -0.1   4   0.1   4       -  FALSE
#> 11 P.7    A.3    2 -1 -1  1   4 -0.1  -0.1   4   0.1   4       -  FALSE
#> 12 P.8    A.3    2 -1 -1 -1   4 -0.1  -0.1   4   0.1   4       -  FALSE
#>          kx       ky       kz
#> 1  2.618615 2.618615 2.572751
#> 2  2.000000 1.732051 1.732051
#> 3  2.000000 1.732051 1.732051
#> 4  4.000000 4.000000 3.872983
#> 5  1.414214 1.414214 1.414214
#> 6  1.414214 1.414214 1.414214
#> 7  1.414214 1.414214 1.414214
#> 8  1.414214 1.414214 1.414214
#> 9  1.414214 1.414214 1.414214
#> 10 1.414214 1.414214 1.414214
#> 11 1.414214 1.414214 1.414214
#> 12 1.414214 1.414214 1.414214
```
