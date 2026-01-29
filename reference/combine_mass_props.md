# Combine mass properties

`combine_mass_props()` calculates the mass properties of an aggregate
from a list of constituent mass properties.

## Usage

``` r
combine_mass_props(mpl)
```

## Arguments

- mpl:

  A list of mass properties lists, each of which contains the following
  named elements:

  - `mass` Numeric mass.

  - `center_mass` Numeric 3-vector center of mass.

  - `point` Logical indicating point mass. The inertia of point masses
    is excluded from calculations.

  - `inertia` Numeric 3x3 matrix inertia tensor.

## Value

Combined mass properties list with the same named elements.

## Details

See vignette("massProps", package = "massProps") for details on the
algorithms employed.

## Examples

``` r
leaves <- names(igraph::neighbors(test_tree, "A.3", mode = "in"))
mpl <- Map(f = function(id) get_mass_props(test_table, id), leaves)
combine_mass_props(mpl)
#> $mass
#> [1] 8
#> 
#> $center_mass
#>  x  y  z 
#> -1  0  0 
#> 
#> $inertia
#>      x    y    z
#> x 32.0 -0.4 -0.4
#> y -0.4 24.0  0.4
#> z -0.4  0.4 24.0
#> 
#> $point
#> [1] FALSE
#> 
```
