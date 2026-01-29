# Validate mass properties uncertainties

`validate_mass_props_unc()` ensures that a mass properties and
uncertainties list satisfies the following constraints:

- `sigma_mass` is non-missing and non-negative

- `sigma_center_mass` is a 3-vector of non-missing non-negative values

- if `point` is FALSE, the `sigma_inertia` contains no missing or
  negative values

## Usage

``` r
validate_mass_props_unc(mp)
```

## Arguments

- mp:

  Mass properties and uncertainties list containing the following named
  elements

  - `point` Logical indicating point mass, i.e., negligible inertia

  - `sigma_mass` mass uncertainty

  - `sigma_center_mass` center of mass uncertainty (3-dimensional
    numeric)

  - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)

## Value

TRUE if valid, stops otherwise

## Examples

``` r
mp <- get_mass_props_and_unc(sawe_table, "Widget")
validate_mass_props_unc(mp)
#> [1] TRUE
```
