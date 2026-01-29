# Roll up mass properties without input validation

`rollup_mass_props_fast()` performs the same operation as
[`rollup_mass_props()`](https://jsjuni.github.io/massProps/reference/rollup_mass_props.md)
but omits input validation. It is somewhat faster than
[`rollup_mass_props()`](https://jsjuni.github.io/massProps/reference/rollup_mass_props.md)
but should be used with caution and only under circumstances in which
the caller assumes responsibility for validity of input. Its behavior
when passed ill-formed input is unspecified.

## Usage

``` r
rollup_mass_props_fast(tree, df)
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
rollup_mass_props_fast(test_tree, test_table)
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
```
