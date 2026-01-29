# Get mass properties for a row in a data frame

`get_mass_props()` creates a mass properties list from a selected row in
a data frame.

## Usage

``` r
get_mass_props(df, id)
```

## Arguments

- df:

  A data frame with (at least) these columns: `id`, `mass`, `Cx`, `Cy`,
  `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`.

- id:

  The `id` value of the desired row.

## Value

A list with the following named elements:

- `mass` Numeric mass.

- `center_mass` Numeric 3-vector center of mass.

- `point` Logical indicating point mass. The inertia of point masses is
  excluded from calculations.

- `inertia` Numeric 3x3 matrix inertia tensor. The signs of the
  off-diagonal elements of the inertia tensor are determined by
  `POIconv`. For example, the \\xy\\ element of the inertia tensor is
  `Ixy` if `POIconv` is "-"; it is -`Ixy` if `POIconv` is "+".

## Examples

``` r
get_mass_props(mp_table, "C.1.2.2.3.1.2.3")
#> $mass
#> [1] 0.1213345
#> 
#> $center_mass
#>        x        y        z 
#> 70.54848 81.54389 44.52262 
#> 
#> $inertia
#>              x             y            z
#> x  0.143256682 -0.0123144484 0.0012773920
#> y -0.012314448  0.1458692409 0.0003485651
#> z  0.001277392  0.0003485651 0.1309228273
#> 
#> $point
#> [1] FALSE
#> 
```
