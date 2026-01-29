# Combine mass properties and uncertainties

`combine_mass_props_and_unc()` is a convenience wrapper that
concatenates the results of
[`combine_mass_props()`](https://jsjuni.github.io/massProps/reference/combine_mass_props.md)
and
[`combine_mass_props_unc()`](https://jsjuni.github.io/massProps/reference/combine_mass_props_unc.md).

## Usage

``` r
combine_mass_props_and_unc(mpl)
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

## Value

Combined mass properties list with the same named elements.

## Examples

``` r
leaves <- names(igraph::neighbors(sawe_tree, "Combined", mode = "in"))
mpl <- Map(f = function(id) get_mass_props_and_unc(sawe_table, id), leaves)
combine_mass_props_and_unc(mpl)
#> $mass
#> [1] 74.63
#> 
#> $center_mass
#>            x            y            z 
#> 109.87693957  -0.18285944  -0.02043146 
#> 
#> $inertia
#>           x         y         z
#> x  7341.733 -1558.714  1401.534
#> y -1558.714 42673.747  1060.951
#> z  1401.534  1060.951 44482.052
#> 
#> $point
#> [1] FALSE
#> 
#> $sigma_mass
#> [1] 2.13008
#> 
#> $sigma_center_mass
#>          x          y          z 
#> 0.95821004 0.19998470 0.06178402 
#> 
#> $sigma_inertia
#>           x         y         z
#> x  387.4017 1488.0948  418.6048
#> y 1488.0948 2789.3133  125.3175
#> z  418.6048  125.3175 2815.3260
#> 
```
