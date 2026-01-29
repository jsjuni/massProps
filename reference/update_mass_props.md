# Update mass properties

`update_mass_props()` updates mass properties for a specified target row
from specified source rows in a data frame.

## Usage

``` r
update_mass_props(df, target, sources, override = set_poi_conv_from_target)
```

## Arguments

- df:

  A data frame with (at least) these columns: `id`, `mass`, `Cx`, `Cy`,
  `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`.

- target:

  The `id` value of the target row.

- sources:

  List of `id` values of the of the source rows.

- override:

  An override function, called as override(df, target, value). The
  default override sets the POI sign convention of a computed aggregate
  to the `POIconv` column of the target row in the data frame.

## Value

The updated data frame.

## Examples

``` r
leaves <- names(igraph::neighbors(test_tree, "A.3", mode = "in"))
update_mass_props(test_table, "A.3", leaves)
#>     id parent mass Cx Cy Cz Ixx  Ixy   Ixz Iyy   Iyz Izz POIconv Ipoint
#> 1  A.1          NA NA NA NA  NA   NA    NA  NA    NA  NA       -  FALSE
#> 2  A.2    A.1   NA NA NA NA  NA   NA    NA  NA    NA  NA       -  FALSE
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
```
