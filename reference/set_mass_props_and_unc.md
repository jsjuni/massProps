# Set mass properties and uncertainties for a row in a data frame

`set_mass_props_and_unc()` is a convenience wrapper that combines the
results of
[`set_mass_props()`](https://jsjuni.github.io/massProps/reference/set_mass_props.md)
and
[`set_mass_props_unc()`](https://jsjuni.github.io/massProps/reference/set_mass_props_unc.md).

## Usage

``` r
set_mass_props_and_unc(df, id, mpu)
```

## Arguments

- df:

  A data frame with an `id` column.

- id:

  The `id` value of the desired row.

- mpu:

  A list containing the following named elements:

  - `mass` Numeric mass.

  - `center_mass` Numeric 3-vector center of mass.

  - `point` Logical indicating point mass. The inertia of point masses
    is excluded from calculations.

  - `poi_conv` Enumeration c("+", "-") indicating sign convention for
    products of inertia.

  - `inertia` Numeric 3x3 matrix inertia tensor. The signs of the
    products of inertia are determined by `POIconv`. For example, `Ixy`
    is the \\xy\\ element of the inertia tensor if `POIconv` is "-"; it
    is the additive inverse of that value if `POIconv` is "+".

  - `sigma_mass` Numeric mass uncertainty.

  - `sigma_center_mass` Numeric 3-vector center of mass uncertainties.

  - `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.

## Value

The updated data frame.

## Examples

``` r
mpu <- c(get_mass_props_and_unc(sawe_table, "Widget"), poi_conv = "+")
set_mass_props_and_unc(sawe_table, "Combined", mpu)
#>         id  mass    Cx    Cy    Cz     Ixx     Iyy      Izz    Ixy      Ixz
#> 1   Widget 57.83 121.2  0.04 -0.16 7258.90 8607.02 10453.40 834.44 -1198.38
#> 2 2nd Part 16.80  70.9 -0.95  0.46   65.07 1124.65  1078.82  76.01   202.83
#> 3 Combined 57.83 121.2  0.04 -0.16 7258.90 8607.02 10453.40 834.44 -1198.38
#>        Iyz sigma_mass sigma_Cx sigma_Cy sigma_Cz sigma_Ixx sigma_Iyy sigma_Izz
#> 1 -1066.58     1.2416   0.2764   0.2085   0.0669  386.9233  171.4792  414.5547
#> 2    13.62     1.7308   0.6234   0.5173   0.1405   12.4687  109.1324  108.5481
#> 3 -1066.58     1.2416   0.2764   0.2085   0.0669  386.9233  171.4792  414.5547
#>   sigma_Ixy sigma_Ixz sigma_Iyz Ipoint POIconv
#> 1 1440.5402  344.6237  124.6860  FALSE       +
#> 2   55.8879  212.1241   11.5408  FALSE       +
#> 3 1440.5402  344.6237  124.6860  FALSE       +
```
