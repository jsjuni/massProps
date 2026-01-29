# Set radii of gyration uncertainties for a row in a data frame

`set_radii_of_gyration_unc()` sets radii of gyration uncertainties for a
selected row in a data frame with an `id` column.

## Usage

``` r
set_radii_of_gyration_unc(df, id, rgu)
```

## Arguments

- df:

  A data frame with an `id` column.

- id:

  The `id` value of the desired row.

- rgu:

  A list with the following named elements:

  - `sigma_radii_gyration` Numeric 3x3 matrix radii of gyration
    uncertainties.

## Value

The updated data frame.

## Examples

``` r
rgul <- list(sigma_radii_gyration = c(x = 1, y = 2, z = 3))
set_radii_of_gyration_unc(mp_table, "C.1", rgul)[1:5, ]
#>        id      name POIconv mass Cx Cy Cz Ixx Iyy Izz Ixy Ixz Iyz Ipoint
#> 1     C.1  System 1       -   NA NA NA NA  NA  NA  NA  NA  NA  NA     NA
#> 2   C.1.1 Segment 1       -   NA NA NA NA  NA  NA  NA  NA  NA  NA     NA
#> 3   C.1.2 Segment 2       -   NA NA NA NA  NA  NA  NA  NA  NA  NA     NA
#> 4 C.1.1.1 Element 1       -   NA NA NA NA  NA  NA  NA  NA  NA  NA     NA
#> 5 C.1.1.2 Element 2       -   NA NA NA NA  NA  NA  NA  NA  NA  NA     NA
#>   sigma_mass sigma_Cx sigma_Cy sigma_Cz sigma_Ixx sigma_Iyy sigma_Izz sigma_Ixy
#> 1         NA       NA       NA       NA        NA        NA        NA        NA
#> 2         NA       NA       NA       NA        NA        NA        NA        NA
#> 3         NA       NA       NA       NA        NA        NA        NA        NA
#> 4         NA       NA       NA       NA        NA        NA        NA        NA
#> 5         NA       NA       NA       NA        NA        NA        NA        NA
#>   sigma_Ixz sigma_Iyz sigma_kx sigma_ky sigma_kz
#> 1        NA        NA        1        2        3
#> 2        NA        NA       NA       NA       NA
#> 3        NA        NA       NA       NA       NA
#> 4        NA        NA       NA       NA       NA
#> 5        NA        NA       NA       NA       NA
```
