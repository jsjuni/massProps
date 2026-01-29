# Set mass properties uncertainties for a row in a data frame

`set_mass_props_unc()` sets mass properties uncertainties for a selected
row in a data frame with an `id` column.

## Usage

``` r
set_mass_props_unc(df, id, mpu)
```

## Arguments

- df:

  A data frame with an `id` column.

- id:

  The `id` value of the desired row.

- mpu:

  A list with the following named elements:

  - `sigma_mass` Numeric mass uncertainty.

  - `sigma_center_mass` Numeric 3-vector center of mass uncertainties.

  - `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.

## Value

The updated data frame.

## Examples

``` r
set_mass_props_unc(sawe_table, "Combined", get_mass_props_unc(sawe_table, "Widget"))
#>         id  mass       Cx      Cy      Cz     Ixx      Iyy      Izz     Ixy
#> 1   Widget 57.83 121.2000  0.0400 -0.1600 7258.90  8607.02 10453.40  834.44
#> 2 2nd Part 16.80  70.9000 -0.9500  0.4600   65.07  1124.65  1078.82   76.01
#> 3 Combined 74.63 109.8657 -0.1829 -0.0204 7341.73 42739.26 44547.27 1559.36
#>        Ixz      Iyz sigma_mass sigma_Cx sigma_Cy sigma_Cz sigma_Ixx sigma_Iyy
#> 1 -1198.38 -1066.58     1.2416   0.2764   0.2085   0.0669  386.9233  171.4792
#> 2   202.83    13.62     1.7308   0.6234   0.5173   0.1405   12.4687  109.1324
#> 3 -1401.94 -1060.95     1.2416   0.2764   0.2085   0.0669  386.9233  171.4792
#>   sigma_Izz sigma_Ixy sigma_Ixz sigma_Iyz Ipoint POIconv
#> 1  414.5547 1440.5402  344.6237  124.6860  FALSE       +
#> 2  108.5481   55.8879  212.1241   11.5408  FALSE       +
#> 3  414.5547 1440.5402  344.6237  124.6860  FALSE       +
```
