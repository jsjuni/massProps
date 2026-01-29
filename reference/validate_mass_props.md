# Validate mass properties

`validate_mass_props()` ensures that a mass properties list satisfies
the following constraints:

- `mass` is non-missing and positive

- `center_mass` is a 3-vector of non-missing numeric values

- `point` is TRUE or FALSE

- if `point` is FALSE:

  - `inertia` is positive definite

  - eigenvalues \\\\\lambda_1, \lambda_2, \lambda_3\\\\ of `inertia`
    satisfy the triangle inequalities:

    - \\\lambda_1 \< \lambda_2 + \lambda_3\\

    - \\\lambda_2 \< \lambda_1 + \lambda_3\\

    - \\\lambda_3 \< \lambda_1 + \lambda_2\\

## Usage

``` r
validate_mass_props(mp)
```

## Arguments

- mp:

  Mass properties list containing the following named elements

  - `mass` Numeric mass.

  - `center_mass` Numeric 3-vector center of mass.

  - `point` Logical indicating point mass. The inertia of point masses
    is excluded from calculations.

  - `inertia` Numeric 3x3 matrix inertia tensor.

## Value

TRUE if valid, stops otherwise

## Examples

``` r
mp <- get_mass_props(test_table, "C.1")
validate_mass_props(mp)
#> [1] TRUE
```
