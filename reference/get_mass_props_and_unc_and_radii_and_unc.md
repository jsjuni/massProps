# Get mass properties and uncertainties and radii of gyration and uncertainties

`get_mass_props_and_unc_and_radii_and_unc()` creates a mass properties
and uncertainties and radii of gyration and uncertainties list from a
selected row in a data frame.

## Usage

``` r
get_mass_props_and_unc_and_radii_and_unc(df, id)
```

## Arguments

- df:

  A data frame with (at least) these columns: `id`, `mass`, `Cx`, `Cy`,
  `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
  `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`,`sigma_Ixy`,
  `sigma_Ixz`, `sigma_Iyz`, `kx`, `ky`, `kz`, `sigma_kx`, `sigma_ky`,
  `sigma_kz`.

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

- `sigma_mass` Numeric mass uncertainty.

- `sigma_center_mass` Numeric 3-vector center of mass uncertainties.

- `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.

- `radii_gyration` Numeric 3-vector radii of gyration.

- `sigma_radii_gyration` Numeric 3-vector radii of gyration
  uncertainties.

## Examples

``` r
mp_table_small_rollup <- rollup_mass_props_and_unc(mp_tree_small, mp_table_small)
radii_and_unc_table <- rollup_radii_of_gyration_unc(
                          mp_tree_small, add_radii_of_gyration(mp_table_small_rollup))
get_mass_props_and_unc_and_radii_and_unc(radii_and_unc_table, "C.1")
#> $mass
#> [1] 635.6602
#> 
#> $center_mass
#>          x          y          z 
#>  0.3318148  1.3661463 -0.6631918 
#> 
#> $inertia
#>             x          y           z
#> x 4250838.375  -36939.71   -5703.635
#> y  -36939.709 4135081.32  -41942.779
#> z   -5703.635  -41942.78 4387869.582
#> 
#> $point
#> [1] FALSE
#> 
#> $sigma_mass
#> [1] 10.29479
#> 
#> $sigma_center_mass
#>        x        y        z 
#> 1.381855 1.407531 1.394688 
#> 
#> $sigma_inertia
#>           x         y         z
#> x 138446.95  64821.46  63906.77
#> y  64821.46 137009.71  64024.22
#> z  63906.77  64024.22 139073.03
#> 
#> $radii_gyration
#>        x        y        z 
#> 81.77580 80.65467 83.08342 
#> 
#> $sigma_radii_gyration
#>        x        y        z 
#> 1.487076 1.486767 1.478160 
#> 
```
