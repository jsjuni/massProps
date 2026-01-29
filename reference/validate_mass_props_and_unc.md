# Validate mass properties and uncertainties

`validate_mass_props_and_unc()` is a convenience wrapper that calculates
the logical conjunction of
[`validate_mass_props()`](https://jsjuni.github.io/massProps/reference/validate_mass_props.md)
and
[`validate_mass_props_unc()`](https://jsjuni.github.io/massProps/reference/validate_mass_props_unc.md).

## Usage

``` r
validate_mass_props_and_unc(mpu)
```

## Arguments

- mpu:

  Mass properties and uncertainties list containing the following named
  elements

  - `mass` mass (numeric)

  - `center_mass` center of mass (3-dimensional numeric)

  - `inertia` Inertia tensor (3x3 numeric matrix)

  - `point` Logical indicating point mass, i.e., negligible inertia

  - `sigma_mass` mass uncertainty

  - `sigma_center_mass` center of mass uncertainty (3-dimensional
    numeric)

  - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)

## Value

TRUE if valid, stops otherwise

## Examples

``` r
mpu <- get_mass_props_and_unc(sawe_table, "Widget")
validate_mass_props_and_unc(mpu)
#> [1] TRUE
```
