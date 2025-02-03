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
#' radii of gyration in columns `kx1, `ky`, and `kz`.`
#'
#' @export
#'
#' @examples
#' test_table_rollup <- rollup_mass_props(test_tree, test_table)
#' add_radii_of_gyration(test_table_rollup)
add_radii_of_gyration <- function(df) {
  moment <- c("Ixx", "Iyy", "Izz")
  names(moment) <- c("kx", "ky", "kz")
  Reduce(
    f = function(d, i) Reduce(
      f = function(dd, r) df_set_by_id(dd, i, r, sqrt(df_get_by_id(dd, i, moment[r]) / df_get_by_id(dd, i, "mass"))),
      x = names(moment),
      init = d
    ),
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

#' Title
#'
#' @param df
#' @param id
#' @param mp
#'
#' @returns
#' @export
#'
#' @examples
set_radii_of_gyration_unc <- function(df, id, mp) {
  values <- list(
    sigma_kx = mp$sigma_k["x"],
    sigma_ky = mp$sigma_k["y"],
    sigma_kz = mp$sigma_k["z"]
  )
  Reduce(
    f = function(d, n) df_set_by_id(d, id, n, values[[n]]),
    x = names(values),
    init = df
  )
}

#' Title
#'
#' @param mpl
#' @param amp
#'
#' @returns
#' @export
#'
#' @examples
combine_radii_of_gyration_unc <- function(mpl, amp) {
  ak2 <- amp$k^2
  amp$sigma_k <- sqrt(Reduce(`+`, Map(
    f = function(v) {
      k2 <- v$k^2

      d <- v$center_mass - amp$center_mass
      d2 <- d^2

      p <- 2 * v$mass * d * v$sigma_center_mass

      q <- d2 - sum(d2)

      m1 <- (k2 - ak2 - q) * v$sigma_mass
      m2 <- c(p$y, p$x, p$x)
      m3 <- c(p$z, p$z, p$y)
      m4 <- 2 * v$mass * v$k * v$sigma_k

      m1^2 + m2^2 + m3^2 + m4^2
    }
  ) / (2 * amp$k * amp$mass)))
}

#' Title
#'
#' @param df
#' @param target
#' @param sources
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
update_radii_of_gyration_unc <- function(df, target, sources, ...) {
  update_prop(
    df,
    target = target,
    sources = sources,
    set = get_mass_props_and_unc_and_radii,
    get = set_radii_of_gyration_unc,
    combine = function(l) { combine_radii_of_gyration_unc(l, amp = get_mass_props_and_unc_and_radii(df, target))},
    ...
  )
}
