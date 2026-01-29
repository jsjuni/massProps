# Update mass properties and uncertainties

`update_mass_props_and_unc()` updates mass properties and uncertainties
for a specified target row from specified source rows in a data frame.

## Usage

``` r
update_mass_props_and_unc(
  df,
  target,
  sources,
  override = set_poi_conv_from_target
)
```

## Arguments

- df:

  A data frame with (at least) these columns: `id`, `mass`, `Cx`, `Cy`,
  `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
  `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`, `sigma_Ixx`,
  `sigma_Iyy`, `sigma_Izz`, `sigma_Ixy`, `sigma_Ixz`, `sigma_Iyz`.

- target:

  The `id` value of the target row.

- sources:

  List of `id` values of the of the source rows.

- override:

  An override function, called as override(df, target, value). The
  default override sets the POI sign convention of a computed aggregate
  to the `POIconv` column of the target row in the data frame.

## Value

The updated data frame.

## Examples

``` r
leaves <- list("Widget", "2nd Part")
update_mass_props_and_unc(sawe_table, "Combined", leaves)
#>         id  mass       Cx         Cy          Cz      Ixx      Iyy      Izz
#> 1   Widget 57.83 121.2000  0.0400000 -0.16000000 7258.900  8607.02 10453.40
#> 2 2nd Part 16.80  70.9000 -0.9500000  0.46000000   65.070  1124.65  1078.82
#> 3 Combined 74.63 109.8769 -0.1828594 -0.02043146 7341.733 42673.75 44482.05
#>        Ixy       Ixz       Iyz sigma_mass sigma_Cx  sigma_Cy   sigma_Cz
#> 1  834.440 -1198.380 -1066.580    1.24160  0.27640 0.2085000 0.06690000
#> 2   76.010   202.830    13.620    1.73080  0.62340 0.5173000 0.14050000
#> 3 1558.714 -1401.534 -1060.951    2.13008  0.95821 0.1999847 0.06178402
#>   sigma_Ixx sigma_Iyy sigma_Izz sigma_Ixy sigma_Ixz sigma_Iyz Ipoint POIconv
#> 1  386.9233  171.4792  414.5547 1440.5402  344.6237  124.6860  FALSE       +
#> 2   12.4687  109.1324  108.5481   55.8879  212.1241   11.5408  FALSE       +
#> 3  387.4017 2789.3133 2815.3260 1488.0948  418.6048  125.3175  FALSE       +
```
