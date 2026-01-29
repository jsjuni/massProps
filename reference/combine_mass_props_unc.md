# Combine mass properties uncertainties

`combine_mass_prop_unc()` calculates the mass properties uncertainties
of an aggregate from the mass properties and uncertainties of its
constituents and the mass properties of the aggregate.

## Usage

``` r
combine_mass_props_unc(mpl, amp)
```

## Arguments

- mpl:

  A list of mass properties and uncertainties lists, each of which
  contains the following named elements:

  - `mass` Numeric mass.

  - `center_mass` Numeric 3-vector center of mass.

  - `point` Logical indicating point mass. The inertia of point masses
    is excluded from calculations.

  - `inertia` Numeric 3x3 matrix inertia tensor.

  - `sigma_mass` mass uncertainty

  - `sigma_center_mass` center of mass uncertainty (3-dimensional
    numeric)

  - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)

- amp:

  A named list of mass properties for the aggregate containing the
  following named elements:

  - `mass` Numeric mass.

  - `center_mass` Numeric 3-vector center of mass.

  - `point` Logical indicating point mass. The inertia of point masses
    is excluded from calculations.

  - `inertia` Numeric 3x3 matrix inertia tensor.

## Value

The mass properties and uncertainties of the aggregate. A list with the
same elements as members of `mpl`.

## Details

See vignette("massProps", package = "massProps") for details on the
algorithms employed.

## Examples

``` r
leaves <- names(igraph::neighbors(sawe_tree, "Combined", mode = "in"))
mpl <- Map(f = function(id) get_mass_props_and_unc(sawe_table, id), leaves)
combine_mass_props_unc(mpl, amp = get_mass_props(sawe_table, "Combined"))
#> $mass
#> [1] 74.63
#> 
#> $center_mass
#>        x        y        z 
#> 109.8657  -0.1829  -0.0204 
#> 
#> $inertia
#>          x        y        z
#> x  7341.73 -1559.36  1401.94
#> y -1559.36 42739.26  1060.95
#> z  1401.94  1060.95 44547.27
#> 
#> $point
#> [1] FALSE
#> 
#> $sigma_mass
#> [1] 2.13008
#> 
#> $sigma_center_mass
#>          x          y          z 
#> 0.95800093 0.19998463 0.06178391 
#> 
#> $sigma_inertia
#>           x         y         z
#> x  387.4017 1488.0844  418.6026
#> y 1488.0844 2787.8795  125.3175
#> z  418.6026  125.3175 2813.9052
#> 
```
