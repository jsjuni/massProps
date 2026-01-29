# Set POI convention for mass properties list to match a target item

`set_poi_conv_from_target()` sets the products of inertia sign
convention for a mass properties list to that of a target item in a mass
properties table. This convention determines how products of inertia are
saved to the data frame.

The signature `of set_poi_conv_from_target()` is such that it can be
passed as an `override` argument to
[`update_mass_props()`](https://jsjuni.github.io/massProps/reference/update_mass_props.md)
and
[`update_mass_props_and_unc()`](https://jsjuni.github.io/massProps/reference/update_mass_props_and_unc.md),
thus ensuring that all calculated POI values follow the negative
integral convention of the target item to which they are written.

## Usage

``` r
set_poi_conv_from_target(df, target, mp)
```

## Arguments

- df:

  A data frame with columns `id` and `POIconv`.

- target:

  The `id` value of the target row.

- mp:

  A mass properties list.

## Value

The mass properties list with the named element `poi_conv` set to the
`POIconv` column of the target row in the data frame.

## Examples

``` r
set_poi_conv_from_target(mp_table, "C.1.2.2.3.2.1", get_mass_props(mp_table, "C.1.2.2.3.2.1.1"))
#> $mass
#> [1] 0.3413137
#> 
#> $center_mass
#>          x          y          z 
#>  34.274465 -44.982071   5.211377 
#> 
#> $inertia
#>               x             y             z
#> x  4.986894e-01 -4.436985e-02 -4.096337e-05
#> y -4.436985e-02  4.528114e-01 -4.510443e-06
#> z -4.096337e-05 -4.510443e-06  4.583186e-01
#> 
#> $point
#> [1] FALSE
#> 
#> $poi_conv
#> [1] "-"
#> 
```
