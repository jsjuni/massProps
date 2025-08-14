#' Add radii of gyration
#'
#' @description
#' `add_radii_of_gyration()` adds calculated radii of gyration to a data set
#' of rolled-up mass properties.
#'
#' Radii of gyration are calculated directly from moments of inertia and mass;
#' they are not recursively-defined, and do not require a rollup method.
#'
#' @inheritParams get_mass_props
#' @param get A function to get a mass property list for a specified item in a data set, called as get(ds, id). Default: get_mass_props().
#' @param get_ids A function to get ids from a data set, called as get_ids(ds). Default: df_get_ids().
#' @param set_radii A function to set the radii of gyration value for a specified item in data set,
#'  called as set_radii(ds, id, value).
#'
#' @returns A data set with the same columns as `ds`, plus
#' radii of gyration in properties `kx`, `ky`, and `kz`.`
#'
#' @export
#'
#' @examples
#' test_table_rollup <- rollup_mass_props(test_tree, test_table)
#' add_radii_of_gyration(test_table_rollup)
#'
add_radii_of_gyration <- function(ds, get = get_mass_props, get_ids = df_get_ids, set_radii = set_radii_of_gyration) {
  Reduce(
    f = function(d, i) {
      rg <- get(d, i)
      rg$radii_gyration <- sqrt(diag(rg$inertia) / rg$mass)
      set_radii(d, i, rg)
    },
    x = get_ids(ds),
    init = ds
  )
}

#' Roll up radii of gyration uncertainties
#'
#' @description
#' `rollup_radii_of_gyration_unc()` adds calculated radii of gyration uncertainties to a data set
#' of rolled-up mass properties and uncertainties.
#'
#' Radii of gyration uncertainties are calculated directly from moments of inertia and mass
#' and their uncertainties; they are not recursively-defined. Radii of gyration uncertainties
#' for composite elements depend on uncertainties of their component elements.
#'
#' @inheritParams rollup_mass_props_and_unc
#' @param get_mpu A function to get mass properties and uncertainties for an item in a data set, called as
#'   get_mpu(ds, id). Default: get_mass_props_and_unc().
#' @param set_radii_unc A function to set radii of gyration uncertainties for an item in a data set, called
#'   as set_radii_unc(ds, target, mp). Default: set_radii_of_gyration_unc().
#'
#' @returns A data set with the same properties as `ds`, plus
#' radii of gyration properties `sigma_kx`, `sigma_ky`, and `sigma_kz`.`
#'
#' @export
#'
#' @examples
#' sawe_table_rollup <- rollup_mass_props(sawe_tree, sawe_table)
#' rollup_radii_of_gyration_unc(sawe_tree, add_radii_of_gyration(sawe_table_rollup))
#'
rollup_radii_of_gyration_unc <- function(tree, ds,
                                         get_mpu = get_mass_props_and_unc,
                                         set_radii_unc = set_radii_of_gyration_unc,
                                         validate_ds = validate_mass_props_and_unc_table
                                         ) {
  rollup(
    tree,
    ds,
    update = function(ds, target, sources) {
      amp <- get_mpu(ds, target)
      I <- diag(amp$inertia)
      sigma_I <- diag(amp$sigma_inertia)
      amp$sigma_radii_gyration <- sqrt(
        sigma_I^2 / (amp$mass * I) + (I * amp$sigma_mass^2) / amp$mass^3 -
          2 / amp$mass^2 * Reduce(
            `+`,
            Map(
              f = function(s) {
                mp <- get_mpu(ds, s)
                d2 <- (mp$center_mass - amp$center_mass)^2
                mp$sigma_mass^2 * (sum(d2) - d2)
              },
              sources
            ),
            init = c(0, 0, 0)
          )
      ) / 2
      set_radii_unc(ds, target, amp)
    },
    validate_ds
  )
}

#' Get mass properties and uncertainties and radii of gyration
#'
#' @description
#' `get_mass_props_and_unc_and_radii()` creates a mass properties and uncertainties
#' and radii of gyration list from a selected item in a data set
#'
#' @inheritParams get_mass_props
#' @param ds A data set with (at least) these properties: `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
#'   `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`,`sigma_Ixy`, `sigma_Ixz`,
#'   `sigma_Iyz`, `kx`, `ky`, `kz`.
#' @param get_mpu A function to get mass properties and uncertainties for a specified item in a data set, called as
#' get_mpu(ds, id). Default: get_mass_props_and_unc().
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
#' mp_table_small_rollup <- rollup_mass_props_and_unc(mp_tree_small, mp_table_small)
#' radii_table_small <- add_radii_of_gyration(mp_table_small_rollup)
#' get_mass_props_and_unc_and_radii(radii_table_small, "C.1")
#'
get_mass_props_and_unc_and_radii <- function(ds, id, get_mpu = get_mass_props_and_unc, get_by_id = df_get_by_id) {
  l <- get_mpu(ds, id)
  l$radii_gyration = sapply(c(x = "kx", y = "ky", z = "kz"), FUN=function(p) get_by_id(ds, id, p))
  l
}

#' Get mass properties and uncertainties and radii of gyration and uncertainties
#'
#' @description
#' `get_mass_props_and_unc_and_radii_and_unc()` creates a mass properties and uncertainties
#' and radii of gyration and uncertainties list from a selected row in a data set
#'
#' @inheritParams get_mass_props
#' @inheritParams get_mass_props_and_unc_and_radii
#' @param ds A data set with (at least) these properties: `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
#'   `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`,`sigma_Ixy`, `sigma_Ixz`,
#'   `sigma_Iyz`, `kx`, `ky`, `kz`, `sigma_kx`, `sigma_ky`, `sigma_kz`.
#' @param get_mpur A function to get mass properties and uncertainties and radii of gyration and uncertainties
#'   for an item in a data set, called as
#'   get_mpur(ds, id). Default: get_mass_props_and_unc_and_radii().
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
#' mp_table_small_rollup <- rollup_mass_props_and_unc(mp_tree_small, mp_table_small)
#' radii_and_unc_table <- rollup_radii_of_gyration_unc(
#'                           mp_tree_small, add_radii_of_gyration(mp_table_small_rollup))
#' get_mass_props_and_unc_and_radii_and_unc(radii_and_unc_table, "C.1")
#'
get_mass_props_and_unc_and_radii_and_unc <- function(ds, id, get_mpur = get_mass_props_and_unc_and_radii, get_by_id = df_get_by_id) {
  l <- get_mpur(ds, id)
  l$sigma_radii_gyration = sapply(c(x = "sigma_kx", y = "sigma_ky", z = "sigma_kz"), FUN=function(p) get_by_id(ds, id, p))
  l
}

#' Set radii of gyration for a row in a data frame
#'
#' `set_radii_of_gyration()` sets radii of gyration for a
#' selected item in a data set with an `id` property
#'
#' @inheritParams set_mass_props
#' @param rg
#' A list with the following named elements:
#' - `radii_gyration` Numeric 3x3 matrix radii of gyration.
#'
#' @returns The updated data set
#'
#' @export
#'
#' @examples
#' rgl <- list(radii_gyration = c(x = 1, y = 2, z = 3))
#' set_radii_of_gyration(mp_table, "C.1", rgl)[1:5, ]
set_radii_of_gyration <- function(ds, id, rg, set_by_id = df_set_by_id) {
  values <- list(
    kx = rg$radii_gyration["x"],
    ky = rg$radii_gyration["y"],
    kz = rg$radii_gyration["z"]
  )
  Reduce(
    f = function(d, n) set_by_id(d, id, n, values[[n]]),
    x = names(values),
    init = ds
  )
}

#' Set radii of gyration uncertainties for an item in a data set
#'
#' `set_radii_of_gyration_unc()` sets radii of gyration uncertainties for a
#' selected item in a data set with an `id` property.
#'
#' @inheritParams set_radii_of_gyration
#' @param rgu
#' A list with the following named elements:
#' - `sigma_radii_gyration` Numeric 3x3 matrix radii of gyration uncertainties.
#'
#' @returns The updated data set
#'
#' @export
#'
#' @examples
#' rgul <- list(sigma_radii_gyration = c(x = 1, y = 2, z = 3))
#' set_radii_of_gyration_unc(mp_table, "C.1", rgul)[1:5, ]
#'
set_radii_of_gyration_unc <- function(ds, id, rgu, set_by_id = df_set_by_id) {
  values <- list(
    sigma_kx = rgu$sigma_radii_gyration["x"],
    sigma_ky = rgu$sigma_radii_gyration["y"],
    sigma_kz = rgu$sigma_radii_gyration["z"]
  )
  Reduce(
    f = function(d, n) set_by_id(d, id, n, values[[n]]),
    x = names(values),
    init = ds
  )
}
