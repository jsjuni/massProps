#' Add radii of gyration
#'
#' @description
#' `add_radii_of_gyration()` adds calculated radii of gyration to a data frame
#' of rolled-up mass properties.
#'
#' Radii of gyration are calculated directly from moments of inertia and mass;
#' they are not recursively-defined, and do not require a rollup method.
#'
#' @inheritParams get_mass_props
#'
#' @returns A data frame with the same columns as `df`, plus
#' radii of gyration in columns `kx`, `ky`, and `kz`.`
#'
#' @export
#'
#' @examples
#' test_table_rollup <- rollup_mass_props(test_tree, test_table)
#' add_radii_of_gyration(test_table_rollup)
add_radii_of_gyration <- function(df) {
  Reduce(
    f = function(d, i) {
      rg <- get_mass_props(d, i)
      rg$radii_gyration <- Reduce(
        f = function(v, d) {
          v[d] = sqrt(rg$inertia[d, d] / rg$mass)
          v
        },
        x = c("x", "y", "z"),
        init = c()
      )
      set_radii_of_gyration(d, i, rg)
    },
    x = df_get_ids(df),
    init = df
  )
}

#' Add radii of gyration uncertainties
#'
#' @description
#' `add_radii_of_gyration_unc()` adds calculated radii of gyration uncertainties to a data frame
#' of rolled-up mass properties and uncertainties.
#'
#' Radii of gyration uncertainties are calculated directly from moments of inertia and mass
#' and their uncertainties;
#' they are not recursively-defined, and do not require a rollup method.
#'
#' @inheritParams get_mass_props_and_unc
#'
#' @returns A data frame with the same columns as `df`, plus
#' radii of gyration in columns `sigma_kx`, `sigma_ky`, and `sigma_kz`.`
#'
#' @export
#'
#' @examples
#' sawe_table_rollup <- rollup_mass_props(sawe_tree, sawe_table)
#' add_radii_of_gyration_unc(add_radii_of_gyration(sawe_table_rollup))
add_radii_of_gyration_unc <- function(df) {
  Reduce(
    f = function(d, i) {
      rgu <- get_mass_props_and_unc(d, i)
      rgu$sigma_radii_gyration <- Reduce(
        f = function(v, d) {
          v[d] = sqrt(rgu$sigma_inertia[d, d]^2 / (rgu$mass * rgu$inertia[d, d]) + (rgu$inertia[d, d] * rgu$sigma_mass^2) / rgu$mass^3) / 2
          v
        },
        x = c("x", "y", "z"),
        init = c()
      )
      set_radii_of_gyration_unc(d, i, rgu)
    },
    x = df_get_ids(df),
    init = df
  )
}
#' Get mass properties and uncertainties and radii of gyration
#'
#' @description
#' `get_mass_props_and_unc_and_radii()` creates a mass properties and uncertainties
#' and radii of gyration list from a selected row in a data frame.
#'
#' @inheritParams get_mass_props
#' @param df A data frame with (at least) these columns: `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
#'   `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`,`sigma_Ixy`, `sigma_Ixz`,
#'   `sigma_Iyz`, `kx`, `ky`, `kz`.
#'
#' @returns A list with the following named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `inertia` Numeric 3x3 matrix inertia tensor. The signs of the off-diagonal elements of the inertia tensor
#' are determined by `POIconv`. For example, the \eqn{xy} element of the inertia
#' tensor is `Ixy` if `POIconv` is "-"; it is -`Ixy` if `POIconv` is "+".
#' - `sigma_mass` Numeric mass uncertainty.
#' - `sigma_center_mass` Numeric 3-vector center of mass uncertainties.
#' - `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.
#' - `radii_gyration` Numeric 3-vector radii of gyration.
#'
#' @export
#'
#' @examples
#' mp_table_rollup <- rollup_mass_props_and_unc(mp_tree, mp_table)
#' radii_table <- add_radii_of_gyration(mp_table_rollup)
#' get_mass_props_and_unc_and_radii(radii_table, "C.1")
get_mass_props_and_unc_and_radii <- function(df, id) {
  l <- get_mass_props_and_unc(df, id)
  l$radii_gyration = sapply(c(x = "kx", y = "ky", z = "kz"), FUN=function(p) df_get_by_id(df, id, p))
  l
}

#' Get mass properties and uncertainties and radii of gyration and uncertainties
#'
#' @description
#' `get_mass_props_and_unc_and_radii_and_unc()` creates a mass properties and uncertainties
#' and radii of gyration and uncertainties list from a selected row in a data frame.
#'
#' @inheritParams get_mass_props
#' @param df A data frame with (at least) these columns: `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
#'   `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`,`sigma_Ixy`, `sigma_Ixz`,
#'   `sigma_Iyz`, `kx`, `ky`, `kz`, `sigma_kx`, `sigma_ky`, `sigma_kz`.
#'
#' @returns A list with the following named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `inertia` Numeric 3x3 matrix inertia tensor. The signs of the off-diagonal elements of the inertia tensor
#' are determined by `POIconv`. For example, the \eqn{xy} element of the inertia
#' tensor is `Ixy` if `POIconv` is "-"; it is -`Ixy` if `POIconv` is "+".
#' - `sigma_mass` Numeric mass uncertainty.
#' - `sigma_center_mass` Numeric 3-vector center of mass uncertainties.
#' - `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.
#' - `radii_gyration` Numeric 3-vector radii of gyration.
#' - `sigma_radii_gyration` Numeric 3-vector radii of gyration uncertainties.
#'
#' @export
#'
#' @examples
#'
get_mass_props_and_unc_and_radii_and_unc <- function(df, id) {
  l <- get_mass_props_and_unc_and_radii(df, id)
  l$sigma_radii_gyration = sapply(c(x = "sigma_kx", y = "sigma_ky", z = "sigma_kz"), FUN=function(p) df_get_by_id(df, id, p))
  l
}

#' Set radii of gyration for a row in a data frame
#'
#' `set_radii_of_gyration()` sets radii of gyration for a
#' selected row in a data frame with an `id` column.
#'
#' @inheritParams set_mass_props
#' @param rg
#' A list with the following named elements:
#' - `radii_gyration` Numeric 3x3 matrix radii of gyration.
#'
#' @returns The updated data frame.
#'
#' @examples
set_radii_of_gyration <- function(df, id, rg) {
  values <- list(
    kx = rg$radii_gyration["x"],
    ky = rg$radii_gyration["y"],
    kz = rg$radii_gyration["z"]
  )
  Reduce(
    f = function(d, n) df_set_by_id(d, id, n, values[[n]]),
    x = names(values),
    init = df
  )
}

#' Set radii of gyration uncertainties for a row in a data frame
#'
#' `set_radii_of_gyration_unc()` sets radii of gyration uncertainties for a
#' selected row in a data frame with an `id` column.
#'
#' @inheritParams set_mass_props
#' @param rgu
#' A list with the following named elements:
#' - `sigma_radii_gyration` Numeric 3x3 matrix radii of gyration uncertainties.
#'
#' @returns The updated data frame.
#'
#' @examples
set_radii_of_gyration_unc <- function(df, id, rgu) {
  values <- list(
    sigma_kx = rgu$sigma_radii_gyration["x"],
    sigma_ky = rgu$sigma_radii_gyration["y"],
    sigma_kz = rgu$sigma_radii_gyration["z"]
  )
  Reduce(
    f = function(d, n) df_set_by_id(d, id, n, values[[n]]),
    x = names(values),
    init = df
  )
}
