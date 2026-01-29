# Set mass properties for a row in a data frame

`set_mass_props()` sets mass properties for a specified row in a data
frame.

## Usage

``` r
set_mass_props(df, id, mp)
```

## Arguments

- df:

  A data frame with an `id` column.

- id:

  The `id` value of the desired row.

- mp:

  A list with the following named elements:

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

## Value

The updated data frame with columns `id`, `mass`, `Cx`, `Cy`, `Cz`,
`Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`.

## Examples

``` r
df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
mp <- get_mass_props(mp_table, "C.1.2.2.3.2.1.1")
mp$poi_conv = "+"
set_mass_props(df, "C.1.2.2.3.2.1.1", mp)
#>                id      mass       Cx        Cy       Cz       Ixx       Iyy
#> 1 C.1.2.2.3.1.2.3        NA       NA        NA       NA        NA        NA
#> 2 C.1.2.2.3.2.1.1 0.3413137 34.27446 -44.98207 5.211377 0.4986894 0.4528114
#>         Izz        Ixy          Ixz          Iyz POIconv Ipoint
#> 1        NA         NA           NA           NA    <NA>     NA
#> 2 0.4583186 0.04436985 4.096337e-05 4.510443e-06       +  FALSE
```
