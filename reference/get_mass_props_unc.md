# Get mass properties uncertainties for a row in a data frame

`get_mass_props_unc()` creates a mass properties uncertainties list from
a selected row in a data frame.

## Usage

``` r
get_mass_props_unc(df, id)
```

## Arguments

- df:

  A data frame with (at least) these columns: `id`, `sigma_mass`,
  `sigma_Cx`, `sigma_Cy`, `sigma_Cz`, `sigma_Ixx`, `sigma_Iyy`,
  `sigma_Izz`, `sigma_Ixy`, `sigma_Ixz`, `sigma_Iyz`.

- id:

  The `id` value of the desired row.

## Value

A list with the following named elements:

- `sigma_mass` Numeric mass uncertainty.

- `sigma_center_mass` Numeric 3-vector center of mass uncertainties.

- `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.

## Examples

``` r
get_mass_props_unc(mp_table, "C.1.2.2.3.1.2.3")
#> $sigma_mass
#> [1] 0.2892205
#> 
#> $sigma_center_mass
#>        x        y        z 
#> 28.83321 29.16371 29.59459 
#> 
#> $sigma_inertia
#>            x          y          z
#> x 0.37222944 0.02191471 0.02146982
#> y 0.02191471 0.36942277 0.02242524
#> z 0.02146982 0.02242524 0.37416478
#> 
```
