# Set POI sign convention for mass properties list to "-"

`set_poi_conv_minus()` sets the products of inertia sign convention for
a mass properties list to "-". This convention determines how products
of inertia are saved to a data set.

The signature of `set_poi_conv_minus()` is such that it can be passed as
an `override` argument to
[`update_mass_props()`](https://jsjuni.github.io/massProps/reference/update_mass_props.md)
and
[`update_mass_props_and_unc()`](https://jsjuni.github.io/massProps/reference/update_mass_props_and_unc.md),
thus ensuring that calculated POI values are saved using the negative
integral convention.

## Usage

``` r
set_poi_conv_minus(ds, target, mp)
```

## Arguments

- ds:

  Ignored.

- target:

  Ignored.

- mp:

  A mass properties list.

## Value

The mass properties list with the named element `poi_conv` set to "-"

## Examples

``` r
set_poi_conv_minus(NULL, NULL, get_mass_props(mp_table, "C.1.2.2.3.2.1.1"))
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
