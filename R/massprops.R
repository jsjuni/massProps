#' Get mass properties for an item in a data set
#'
#' `get_mass_props()` creates a mass properties list from a selected item in a data set
#'
#' @param ds A data set with (at least) these properties: `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`.
#' @param id The `id` value of the desired item
#' @param get_by_id A function to get the value of a property from the selected item,
#'   called as get_by_id(ds, id, property); default df_get_by_id()
#'
#' @returns A list with the following named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `inertia` Numeric 3x3 matrix inertia tensor. The signs of the off-diagonal elements of the inertia tensor
#' are determined by `POIconv`. For example, the \eqn{xy} element of the inertia
#' tensor is `Ixy` if `POIconv` is "-"; it is -`Ixy` if `POIconv` is "+".
#'
#' @export
#'
#' @examples
#' get_mass_props(mp_table, "C.1.2.2.3.1.2.3")
#'
get_mass_props <- function(ds, id, get_by_id = df_get_by_id) {
  poi_conv <- get_by_id(ds, id, "POIconv")
  list(
    mass = get_by_id(ds, id, "mass"),
    center_mass = sapply(c(x = "Cx", y = "Cy", z = "Cz"), FUN=function(p) get_by_id(ds, id, p)),
    inertia = {
      xyz <- list("x", "y", "z")
      it <- matrix(data = rep.int(0, 9), nrow = 3, dimnames = list(xyz, xyz))
      it["x", "x"] <- get_by_id(ds, id, "Ixx")
      it["y", "y"] <- get_by_id(ds, id, "Iyy")
      it["z", "z"] <- get_by_id(ds, id, "Izz")
      poi_factor <- if (poi_conv == '-') 1 else -1
      it["x", "y"] <- it["y", "x"] <- poi_factor * get_by_id(ds, id, "Ixy")
      it["x", "z"] <- it["z", "x"] <- poi_factor * get_by_id(ds, id, "Ixz")
      it["y", "z"] <- it["z", "y"] <- poi_factor * get_by_id(ds, id, "Iyz")
      it
    },
    point = get_by_id(ds, id, "Ipoint")
  )
}

#' Get mass properties uncertainties for an item in a data set
#'
#' `get_mass_props_unc()` creates a mass properties uncertainties list from a selected item in a data set.
#'
#' @inheritParams get_mass_props
#' @param ds A data set  with (at least) these properties: `id`, `sigma_mass`,
#'   `sigma_Cx`, `sigma_Cy`, `sigma_Cz`, `sigma_Ixx`, `sigma_Iyy`, `sigma_Izz`,
#'   `sigma_Ixy`, `sigma_Ixz`, `sigma_Iyz`.
#'
#' @returns A list with the following named elements:
#' - `sigma_mass` Numeric mass uncertainty.
#' - `sigma_center_mass` Numeric 3-vector center of mass uncertainties.
#' - `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.

#' @export
#'
#' @examples
#' get_mass_props_unc(mp_table, "C.1.2.2.3.1.2.3")
#'
get_mass_props_unc <- function(ds, id, get_by_id = df_get_by_id) {
  list(
    sigma_mass = get_by_id(ds, id, "sigma_mass"),
    sigma_center_mass = sapply(c(x = "sigma_Cx", y = "sigma_Cy", z = "sigma_Cz"), FUN=function(p) get_by_id(ds, id, p)),
    sigma_inertia = {
      xyz <- list("x", "y", "z")
      sit <- matrix(data = rep.int(0, 9), nrow = 3, dimnames = list(xyz, xyz))
      sit["x", "x"] <- get_by_id(ds, id, "sigma_Ixx")
      sit["y", "y"] <- get_by_id(ds, id, "sigma_Iyy")
      sit["z", "z"] <- get_by_id(ds, id, "sigma_Izz")
      sit["x", "y"] <- sit["y", "x"] <- get_by_id(ds, id, "sigma_Ixy")
      sit["x", "z"] <- sit["z", "x"] <- get_by_id(ds, id, "sigma_Ixz")
      sit["y", "z"] <- sit["z", "y"] <- get_by_id(ds, id, "sigma_Iyz")
      sit
    }
  )
}

#' Get mass properties and uncertainties for an item in a data set
#'
#' @description
#' `get_mass_props_and_unc()` is a convenience wrapper that combines the results of
#' `get_mass_props()` and `get_mass_props_unc()`.
#'
#' @inheritParams get_mass_props
#' @param ds A data set with (at least) these properties: `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
#'   `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`,`sigma_Ixy`, `sigma_Ixz`,
#'   `sigma_Iyz`.
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
#'
#' @export
#'
#' @examples
#' get_mass_props_and_unc(mp_table, "C.1.2.2.3.1.2.3")
#'
get_mass_props_and_unc <- function(ds, id, get_by_id = df_get_by_id) {
  c(get_mass_props(ds, id, get_by_id), get_mass_props_unc(ds, id, get_by_id))
}

#' Set mass properties for an item in a data set
#'
#' `set_mass_props()` sets mass properties for a specified item in a data set
#'
#' @inheritParams get_mass_props
#' @param ds A data set with an `id` property.
#' @param mp
#' A list with the following named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `poi_conv` Enumeration c("+", "-") indicating sign convention for products of inertia.
#' - `inertia` Numeric 3x3 matrix inertia tensor. The signs of the products of inertia
#' are determined by `POIconv`. For example, `Ixy` is the \eqn{xy} element of the inertia
#' tensor if `POIconv` is "-"; it is the additive inverse of that value if `POIconv` is "+".
#' @param set_by_id A function to set the value of a property for the selected item,
#'   called as set_by_id(ds, id, property, value); default df_set_by_id()
#'
#' @returns The updated data set with properties `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`.
#'
#' @export
#'
#' @examples
#' df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
#' mp <- get_mass_props(mp_table, "C.1.2.2.3.2.1.1")
#' mp$poi_conv = "+"
#' set_mass_props(df, "C.1.2.2.3.2.1.1", mp)
#'
set_mass_props <- function(ds, id, mp, set_by_id = df_set_by_id) {
  m <- mp$inertia
  tryCatch(
    poi_factor <- if (mp$poi_conv == "-") 1 else if (mp$poi_conv == "+") -1 else stop(),
    error = function(e) stop("invalid sign convention")
  )
  values <- list(
    mass = mp$mass,

    Cx = mp$center_mass["x"],
    Cy = mp$center_mass["y"],
    Cz = mp$center_mass["z"],

    Ixx = m["x", "x"],
    Iyy = m["y", "y"],
    Izz = m["z", "z"],

    Ixy = poi_factor * (m["x", "y"] + m["y", "x"]) / 2.0,
    Ixz = poi_factor * (m["x", "z"] + m["z", "x"]) / 2.0,
    Iyz = poi_factor * (m["y", "z"] + m["z", "y"]) / 2.0,

    POIconv = mp$poi_conv,
    Ipoint = mp$point
  )
  Reduce(
    f = function(d, n) set_by_id(d, id, n, values[[n]]),
    x = names(values),
    init = ds
  )
}

#' Set mass properties uncertainties for an item in a data set
#'
#' `set_mass_props_unc()` sets mass properties uncertainties for a
#' selected item in a data set
#'
#' @inheritParams set_mass_props
#' @param mpu
#' A list with the following named elements:
#' - `sigma_mass` Numeric mass uncertainty.
#' - `sigma_center_mass` Numeric 3-vector center of mass uncertainties.
#' - `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.
#'
#' @returns The updated data set.
#'
#' @export
#'
#' @examples
#' set_mass_props_unc(sawe_table, "Combined", get_mass_props_unc(sawe_table, "Widget"))
#'
set_mass_props_unc <- function(ds, id, mpu, set_by_id = df_set_by_id) {
  values <- list(
    sigma_mass = mpu$sigma_mass,

    sigma_Cx = mpu$sigma_center_mass["x"],
    sigma_Cy = mpu$sigma_center_mass["y"],
    sigma_Cz = mpu$sigma_center_mass["z"],

    sigma_Ixx = mpu$sigma_inertia["x", "x"],
    sigma_Iyy = mpu$sigma_inertia["y", "y"],
    sigma_Izz = mpu$sigma_inertia["z", "z"],

    sigma_Ixy = (mpu$sigma_inertia["x", "y"] + mpu$sigma_inertia["y", "x"]) / 2.0,
    sigma_Ixz = (mpu$sigma_inertia["x", "z"] + mpu$sigma_inertia["z", "x"]) / 2.0,
    sigma_Iyz = (mpu$sigma_inertia["y", "z"] + mpu$sigma_inertia["z", "y"]) / 2.0
  )
  Reduce(
    f = function(d, n) set_by_id(d, id, n, values[[n]]),
    x = names(values),
    init = ds
  )
}

#' Set mass properties and uncertainties for an item in a data set
#'
#' @description
#' `set_mass_props_and_unc()` is a convenience wrapper that combines the results of
#' `set_mass_props()` and `set_mass_props_unc()`.
#'
#' @inheritParams set_mass_props
#' @param mpu A list containing the following named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `poi_conv` Enumeration c("+", "-") indicating sign convention for products of inertia.
#' - `inertia` Numeric 3x3 matrix inertia tensor. The signs of the products of inertia
#' are determined by `POIconv`. For example, `Ixy` is the \eqn{xy} element of the inertia
#' tensor if `POIconv` is "-"; it is the additive inverse of that value if `POIconv` is "+".
#' - `sigma_mass` Numeric mass uncertainty.
#' - `sigma_center_mass` Numeric 3-vector center of mass uncertainties.
#' - `sigma_inertia` Numeric 3x3 matrix inertia tensor uncertainties.
#'
#' @returns The updated data set.
#'
#' @export
#'
#' @examples
#' mpu <- c(get_mass_props_and_unc(sawe_table, "Widget"), poi_conv = "+")
#' set_mass_props_and_unc(sawe_table, "Combined", mpu)
#'
set_mass_props_and_unc <- function(ds, id, mpu, set_by_id = df_set_by_id) {
  set_mass_props_unc(set_mass_props(ds, id, mpu, set_by_id), id, mpu, set_by_id)
}

#' Set POI sign convention for mass properties list to "+"
#'
#' @description
#' `set_poi_conv_plus()` sets the products of inertia sign convention for a
#' mass properties list to "+". This convention determines how products of inertia are
#' saved to a data set.
#'
#' The signature of `set_poi_conv_plus()` is such that it can be passed as an `override` argument
#' to `update_mass_props()` and `update_mass_props_and_unc()`, thus ensuring
#' that calculated POI values are saved using the positive integral convention.
#'
#' @param ds Ignored.
#' @param target Ignored.
#' @param mp A mass properties list.
#'
#' @returns The input mass properties list with the named element `poi_conv` set to "+"
#'
#' @export
#'
#' @examples
#' set_poi_conv_plus(NULL, NULL, get_mass_props(mp_table, "C.1.2.2.3.2.1.1"))
#'
set_poi_conv_plus <- function(ds, target, mp) {
  mp$poi_conv <- "+"
  mp
}

#' Set POI sign convention for mass properties list to "-"
#'
#' @description
#' `set_poi_conv_minus()` sets the products of inertia sign convention for a
#' mass properties list to "-". This convention determines how products of inertia are
#' saved to a data set.
#'
#' The signature of `set_poi_conv_minus()` is such that it can be passed as an `override` argument
#' to `update_mass_props()` and `update_mass_props_and_unc()`, thus ensuring
#' that calculated POI values are saved using the negative integral convention.
#'
#' @inheritParams set_poi_conv_plus
#'
#' @returns The mass properties list with the named element `poi_conv` set to "-"
#'
#' @export
#'
#' @examples
#' set_poi_conv_minus(NULL, NULL, get_mass_props(mp_table, "C.1.2.2.3.2.1.1"))
#'
set_poi_conv_minus <- function(ds, target, mp) {
  mp$poi_conv <- "-"
  mp
}

#' Set POI convention for mass properties list to match a target item
#'
#' @description
#' `set_poi_conv_from_target()` sets the products of inertia sign convention for a
#' mass properties list to that of a target item in a mass properties data set. This convention
#' determines how products of inertia are saved to the data set.
#'
#' The signature `of set_poi_conv_from_target()` is such that it can be passed as an `override` argument
#' to `update_mass_props()` and `update_mass_props_and_unc()`, thus ensuring
#' that all calculated POI values follow the negative integral convention of the target item to which they are written.
#'
#' @inheritParams get_mass_props
#' @inheritParams set_poi_conv_plus
#' @param ds A data set with properties `id` and `POIconv`.
#' @param target The `id` value of the target item
#'
#' @return The mass properties list with the named element `poi_conv` set to the
#'   `POIconv` property of the target item in the data set
#'
#' @export
#'
#' @examples
#' set_poi_conv_from_target(mp_table, "C.1.2.2.3.2.1", get_mass_props(mp_table, "C.1.2.2.3.2.1.1"))
#'
set_poi_conv_from_target <- function(ds, target, mp, get_by_id = df_get_by_id) {
  mp$poi_conv <- get_by_id(ds, target, "POIconv")
  mp
}

#' Combine mass properties
#'
#' @description
#' `combine_mass_props()` calculates the mass properties of an aggregate from
#' a list of constituent mass properties.
#'
#' @details
#' See vignette("massProps", package = "massProps") for details on the algorithms
#' employed.
#'
#' @param mpl A list of mass properties lists, each of which contains the
#' following named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `inertia` Numeric 3x3 matrix inertia tensor.
#'
#' @returns Combined mass properties list with the same named elements.
#'
#' @export
#'
#' @examples
#' leaves <- names(igraph::neighbors(test_tree, "A.3", mode = "in"))
#' mpl <- Map(f = function(id) get_mass_props(test_table, id), leaves)
#' combine_mass_props(mpl)
#'
combine_mass_props <- function(mpl) {

  amp <- list()

  # sum of masses

  amp$mass <- Reduce(`+`, Map(f = function(mp) mp$mass, mpl))

  # mass-weighted sum of centers of mass

  amp$center_mass <- Reduce(`+`, Map(f = function(mp) mp$mass * mp$center_mass, mpl)) / amp$mass

  # inertia tensor

  amp$inertia <- Reduce(`+`, Map(
    f  = function(mp) {
      d <- amp$center_mass - mp$center_mass
      Q <- outer(d, d)
      M <- Q - sum(diag(Q)) * diag(3)
      if (mp$point) -mp$mass * M else mp$inertia - mp$mass * M
    },
    mpl
  ))

  # aggregate is a point mass iff all parts are point masses at the same center

  amp$point = Reduce(f = `&&`,
         Map(f = function(mp) mp$point && isTRUE(all.equal(mp$center_mass, amp$center_mass)), mpl)
  )

  amp
}

#' Combine mass properties uncertainties
#'
#' @description
#' `combine_mass_prop_unc()` calculates the mass properties uncertainties of an aggregate from
#' the mass properties and uncertainties of its constituents and the mass properties of the aggregate.
#'
#' @details
#' See vignette("massProps", package = "massProps") for details on the algorithms
#' employed.
#'
#' @param mpl A list of mass properties and uncertainties lists, each of which contains
#' the following named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `inertia` Numeric 3x3 matrix inertia tensor.
#' - `sigma_mass` mass uncertainty
#' - `sigma_center_mass` center of mass uncertainty (3-dimensional numeric)
#' - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)
#' @param amp A named list of mass properties for the aggregate containing the following
#' named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `inertia` Numeric 3x3 matrix inertia tensor.
##'
#' @returns The mass properties and uncertainties of the aggregate. A list with the same elements as
#' members of `mpl`.
#'
#' @export
#'
#' @examples
#' leaves <- names(igraph::neighbors(sawe_tree, "Combined", mode = "in"))
#' mpl <- Map(f = function(id) get_mass_props_and_unc(sawe_table, id), leaves)
#' combine_mass_props_unc(mpl, amp = get_mass_props(sawe_table, "Combined"))
#'
combine_mass_props_unc <- function(mpl, amp) {

  # mass uncertainty

  amp$sigma_mass = sqrt(Reduce(`+`, Map(f = function(mp) mp$sigma_mass^2, mpl)))

  # center of mass uncertainty

  amp$sigma_center_mass = sqrt(Reduce(`+`, Map(
    f = function(mp) {
      (mp$mass * mp$sigma_center_mass)^2 +
        (mp$sigma_mass * (mp$center_mass - amp$center_mass))^2
    },
    mpl
  ))) / amp$mass

  # inertia tensor uncertainty

  amp$sigma_inertia = sqrt(Reduce(`+`, Map(
    f = function(mp) {

      d <- mp$center_mass - amp$center_mass

      P <- outer(d, mp$sigma_center_mass)
      p <- diag(P)

      Q <- outer(d, d)

      M1 <-   P  - diag(p - 2 * p[c("y", "x", "x")])
      M2 <- t(P) - diag(p - 2 * p[c("z", "z", "y")])
      M3 <-   Q  - sum(diag(Q)) * diag(3)
      M4 <- mp$mass^2 * (M1^2 + M2^2) + (mp$sigma_mass * M3)^2

      if (mp$point) M4 else mp$sigma_inertia^2 + M4
    },
    mpl
  )))

  # result

  amp
}

#' Combine mass properties and uncertainties
#'
#' @description
#' `combine_mass_props_and_unc()` is a convenience wrapper that concatenates the
#' results of `combine_mass_props()` and `combine_mass_props_unc()`.
#'
#' @param mpl A list of mass properties and uncertainties lists, each of which contains
#' the following named elements:
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `inertia` Numeric 3x3 matrix inertia tensor.
#' - `sigma_mass` mass uncertainty
#' - `sigma_center_mass` center of mass uncertainty (3-dimensional numeric)
#' - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)
#'
#' @returns Combined mass properties list with the same named elements.
#'
#' @export
#'
#' @examples
#' leaves <- names(igraph::neighbors(sawe_tree, "Combined", mode = "in"))
#' mpl <- Map(f = function(id) get_mass_props_and_unc(sawe_table, id), leaves)
#' combine_mass_props_and_unc(mpl)
#'
combine_mass_props_and_unc <- function(mpl) {
  combine_mass_props_unc(mpl, amp = combine_mass_props(mpl))
}

#' Update mass properties
#'
#' `update_mass_props()` updates mass properties for a specified target item from
#' specified source items in a data set.
#'
#' @param ds A data set  with (at least) these properties: `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`.
#' @param target The `id` value of the target item.
#' @param sources List of `id` values of the of the source items.
#' @param set A function to set the mass properties of the target item, called as set(ds, target, mpl). Default set_mass_props().
#' @param get A function to get the mass properties of a source item, called as get(ds, source). Default get_mass_props().
#' @param combine A function to combine a list of mass property sets, called as combine(mpl). Default combine_mass_props().
#' @param override An override function, called as override(ds, target, value). Default set_poi_conv_from_target().
#'
#' @return The updated data set.
#'
#' @export
#'
#' @examples
#' leaves <- names(igraph::neighbors(test_tree, "A.3", mode = "in"))
#' update_mass_props(test_table, "A.3", leaves)
#'
update_mass_props <- function(ds, target, sources,
                              set = set_mass_props,
                              get = get_mass_props,
                              combine = combine_mass_props,
                              override = set_poi_conv_from_target) {
  update_prop(
    ds,
    target = target,
    sources = sources,
    set,
    get,
    combine,
    override = override
  )
}

#'  Update mass properties uncertainties
#'
#' @description
#' `update_mass_props_unc()` updates mass properties uncertainties
#' for a specified target row from
#' specified source items in a data set
#' with (at least) these columns: `id`, `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`,
#' `sigma_Ixx`, `sigma_Iyy`, `sigma_Izz`, `sigma_Ixy`, `sigma_Ixz`, `sigma_Iyz`.
#'
#' @inheritParams update_mass_props
#' @param df A data set  with (at least) these properties: `id`, `mass`, `Cx`,
#'   `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz`, `POIconv`, `Ipoint`,
#'   `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`, `sigma_Ixx`, `sigma_Iyy`,
#'   `sigma_Izz`, `sigma_Ixy`, `sigma_Ixz`, `sigma_Iyz`.
#'
#' @returns The updated data set.
#'
#' @export
#'
#' @examples
#' leaves <- names(igraph::neighbors(sawe_tree, "Combined", mode = "in"))
#' update_mass_props_unc(sawe_table, "Combined", leaves)
#'
update_mass_props_unc <- function(df, target, sources,
                                  set = set_mass_props_unc,
                                  get = get_mass_props_and_unc,
                                  combine = function(l) { combine_mass_props_unc(l, amp = get_mass_props(df, target))},
                                  override = set_poi_conv_from_target) {
  update_prop(
    df,
    target = target,
    sources = sources,
    set,
    get,
    combine,
    override = override
  )
}

#' Update mass properties and uncertainties
#'
#' @description
#' `update_mass_props_and_unc()` updates mass properties and uncertainties
#' for a specified target item from
#' specified source items in a data set
#'
#' @inheritParams update_mass_props_unc
#'
#' @return The updated data set.
#'
#' @export
#'
#' @examples
#' leaves <- list("Widget", "2nd Part")
#' update_mass_props_and_unc(sawe_table, "Combined", leaves)
#'
update_mass_props_and_unc <- function(df, target, sources,
                                      set = set_mass_props_and_unc,
                                      get = get_mass_props_and_unc,
                                      combine = combine_mass_props_and_unc,
                                      override = set_poi_conv_from_target) {
  update_prop(
    df,
    target = target,
    sources = sources,
    set,
    get,
    combine,
    override = override
  )
}

#' Validate mass properties
#'
#' @description
#' `validate_mass_props()` ensures that a mass properties list satisfies the following
#' constraints:
#' - `mass` is non-missing and positive
#' - `center_mass` is a 3-vector of non-missing numeric values
#' - `point` is TRUE or FALSE
#' - if `point` is FALSE:
#'   - `inertia` is positive definite
#'   - eigenvalues \eqn{\{\lambda_1, \lambda_2, \lambda_3\}} of  `inertia` satisfy the triangle inequalities:
#'     - \eqn{\lambda_1 < \lambda_2 + \lambda_3}
#'     - \eqn{\lambda_2 < \lambda_1 + \lambda_3}
#'     - \eqn{\lambda_3 < \lambda_1 + \lambda_2}
#'
#' @param mp Mass properties list containing the following named elements
#' - `mass` Numeric mass.
#' - `center_mass` Numeric 3-vector center of mass.
#' - `point` Logical indicating point mass. The inertia of point masses is excluded from calculations.
#' - `inertia` Numeric 3x3 matrix inertia tensor.
#'
#' @returns TRUE if valid, stops otherwise
#'
#' @export
#'
#' @examples
#' mp <- get_mass_props(test_table, "C.1")
#' validate_mass_props(mp)
#'
validate_mass_props <- function(mp) {

  # ensure mass is numeric and positive.

  if (is.null(mp$mass) || is.na(mp$mass)) stop("mass missing")
  if (!is.numeric(mp$mass)) stop("mass non-numeric")
  if (mp$mass <= 0.) stop("mass non-positive")

  # ensure center of mass is numeric.

  if (is.null(mp$center_mass)) stop("center of mass missing")
  if (length(mp$center_mass) != 3) stop("center of mass not a 3-vector")
  if (any(is.na(mp$center_mass))) stop("center of mass element missing")
  if (any(!is.numeric(mp$center_mass))) stop("center of mass element non-numeric")

  # ensure inertia tensor point mass indicator is logical

  if (is.null(mp$point)) stop("point mass indicator missing")
  if (is.na(mp$point) || !is.logical(mp$point)) stop("point mass indicator non-logical")

  if (!mp$point) {

    # ensure inertia tensor elements for non-point-masses are numeric.

    if (is.null(mp$inertia)) stop("inertia tensor missing")
    if (!isTRUE(all.equal(dim(mp$inertia), c(3, 3)))) stop("inertia tensor not a 3x3 matrix")
    if (any(is.na(mp$inertia))) stop("inertia tensor element missing")
    if (!is.numeric(mp$inertia)) stop("inertia tensor element non-numeric")

    # ensure inertia tensor is positive definite.

    ev <- eigen(mp$inertia, symmetric=TRUE, only.values=TRUE)$values
    if (any(ev <= 0.)) stop("inertia tensor not positive definite")

    # ensure principal moments obey triangle inequalities

    if (any(c(
      ev[1] >= ev[2] + ev[3],
      ev[2] >= ev[1] + ev[3],
      ev[3] >= ev[1] + ev[2]
    ))) stop("inertia tensor violates triangle inequalities")

  }

  TRUE
}

#' Validate mass properties uncertainties
#'
#' @description
#' `validate_mass_props_unc()` ensures that a mass properties and uncertainties
#' list satisfies the following constraints:
#' - `sigma_mass` is non-missing and non-negative
#' - `sigma_center_mass` is a 3-vector of non-missing non-negative values
#' - if `point` is FALSE, the `sigma_inertia` contains no missing or negative values
#'
#' @param mp Mass properties and uncertainties list containing the following named elements
#' - `point` Logical indicating point mass, i.e., negligible inertia
#' - `sigma_mass` mass uncertainty
#' - `sigma_center_mass` center of mass uncertainty (3-dimensional numeric)
#' - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)
#'
#' @returns TRUE if valid, stops otherwise
#'
#' @export
#'
#' @examples
#' mp <- get_mass_props_and_unc(sawe_table, "Widget")
#' validate_mass_props_unc(mp)
#'
validate_mass_props_unc <- function(mp) {

  # ensure mass uncertainty is numeric and positive.

  if (is.null(mp$sigma_mass) || is.na(mp$sigma_mass)) stop("mass uncertainty missing")
  if (!is.numeric(mp$sigma_mass)) stop("mass uncertainty non-numeric")
  if (mp$sigma_mass < 0.) stop("mass uncertainty negative")

  # ensure center of mass uncertainties are numeric and non-negative.

  if (is.null(mp$sigma_center_mass)) stop("center of mass uncertainty missing")
  if (length(mp$sigma_center_mass) != 3) stop("center of mass uncertainty not a 3-vector")
  if (any(is.na(mp$sigma_center_mass))) stop("center of mass uncertainty element missing")
  if (any(!is.numeric(mp$sigma_center_mass))) stop("center of mass uncertainty element non-numeric")
  if (any(mp$sigma_center_mass < 0.0)) stop("center of mass uncertainty element negative")

  if (!mp$point) {

    # ensure inertia tensor uncertainty elements for non-point-masses are numeric.

    if (is.null(mp$sigma_inertia)) stop("inertia tensor uncertainty missing")
    if (!isTRUE(all.equal(dim(mp$sigma_inertia), c(3, 3)))) stop("inertia tensor uncertainty not a 3x3 matrix")
    if (any(is.na(mp$sigma_inertia))) stop("inertia tensor uncertainty element missing")
    if (!is.numeric(mp$sigma_inertia)) stop("inertia tensor uncertainty element non-numeric")
    if (any(mp$sigma_inertia < 0.0)) stop("inertia tensor uncertainty element negative")

  }

  TRUE
}

#' Validate mass properties and uncertainties
#'
#' @description
#' `validate_mass_props_and_unc()` is a convenience wrapper that calculates the logical
#' conjunction of `validate_mass_props()` and `validate_mass_props_unc()`.
#'
#' @param mpu Mass properties and uncertainties list containing the following named elements
#' - `mass` mass (numeric)
#' - `center_mass` center of mass (3-dimensional numeric)
#' - `inertia` Inertia tensor (3x3 numeric matrix)
#' - `point` Logical indicating point mass, i.e., negligible inertia
#' - `sigma_mass` mass uncertainty
#' - `sigma_center_mass` center of mass uncertainty (3-dimensional numeric)
#' - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)
#'
#' @returns TRUE if valid, stops otherwise
#'
#' @export
#'
#' @examples
#' mpu <- get_mass_props_and_unc(sawe_table, "Widget")
#' validate_mass_props_and_unc(mpu)
#'
validate_mass_props_and_unc <- function(mpu) {
  validate_mass_props(mpu) && validate_mass_props_unc(mpu)
}

#' Validate a mass properties table
#'
#' @description `validate_mass_props_table()` checks that the names of vertices
#'   in a tree and the `id` values of a data set are identical. It further
#'   applies the checks of `validate_mass_props()` to every item of the data
#'   set corresponding to a leaf vertex of the tree.
#'
#' @inheritParams update_mass_props
#' @inheritParams rollup_mass_props
#'
#' @description
#' `validate_mass_props_table()` ensures that the `id` column of the table and the vertices
#' of the tree contain the same identifiers, and that the mass properties of every leaf element
#' of the table are valid.
#'
#' @returns TRUE if valid, stops with an error otherwise
#'
#' @export
#'
#' @examples
#' validate_mass_props_table(mp_tree_small, mp_table_small)
validate_mass_props_table <- function(tree, ds) {
  validate_ds(tree, ds, df_get_ids, get_mass_props, validate_mass_props)
}

#' Validate a mass properties and uncertainties table
#'
#' @inheritParams update_mass_props_and_unc
#' @inheritParams validate_mass_props_table
#'
#' @description `validate_mass_props_and_unc()` calls
#' `validate_mass_props_table()` and further applies the checks of
#' `validate_mass_props_and_unc()` to every item of the data set corresponding
#' to a leaf vertex of the tree.
#'
#' @returns TRUE if valid, stops with an error otherwise
#'
#' @export
#'
#' @examples
#' validate_mass_props_and_unc_table(mp_tree_small, mp_table_small)
validate_mass_props_and_unc_table <- function(tree, ds) {
  validate_ds(tree, ds, df_get_ids, get_mass_props_and_unc, validate_mass_props_and_unc)
}

#' Roll up mass properties
#'
#' @description
#' 'rollup_mass_props()' rolls up mass properties in a data set such that the mass properties of each
#' non-leaf vertex item is the aggregation of those of its child items.
#'
#' @inheritParams update_mass_props
#' @param tree An 'igraph' tree whose vertices are named as the values of the `id`
#'   column of `ds` and whose directed edges point from child id to parent id.
#' @param validate_ds A validator for the tree and table, default `validate_mass_props_table()`
#' @param ... Other parameters passed to `rollupTree::rollup()`
#'
#' @returns The updated data set.
#'
#' @export
#'
#' @examples
#' rollup_mass_props(mp_tree_small, mp_table_small)
#'
rollup_mass_props <- function(tree, ds, validate_ds = validate_mass_props_table, ...) {
  rollup(tree, ds, update_mass_props, validate_ds, ...)
}

#' Roll up mass properties uncertainties
#'
#' @description
#' `rollup_mass_props_unc()` rolls up mass properties uncertainties in a data set such that the uncertainties of each
#' non-leaf vertex item is the aggregation of the mass properties and uncertainties of its child items.
#'
#' The difference between `rollup_mass_props_unc()` and `rollup_mass_props_and_unc()` is that `rollup_mass_props_unc()`
#' expects the mass properties in its input to have been rolled up, whereas `rollup_mass_props_and_unc()` performs
#' the mass properties rollup itself.
#'
#' @inheritParams rollup_mass_props
#' @inheritParams update_mass_props_unc
#' @param validate_ds A validator for the tree and table, default `validate_mass_props_and_unc_table()`
#'
#' @returns The updated data set.
#'
#' @export
#'
#' @examples
#' mp_ru <- rollup_mass_props(mp_tree_small, mp_table_small)
#' rollup_mass_props_unc(mp_tree_small, mp_ru)

rollup_mass_props_unc <- function(tree, df, validate_ds = validate_mass_props_and_unc_table, ...) {
  rollup(tree, df, update_mass_props_unc, validate_ds, ...)
}

#' Roll up mass properties and uncertainties
#'
#' @description
#' 'rollup_mass_props_and_unc()' rolls up mass properties in a data set
#' with (at least) these columns: `id`, `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
#' `Ixz`, `Iyz`, `POIconv`, `Ipoint`, `sigma_mass`, `sigma_Cx`, `sigma_Cy`, `sigma_Cz`,
#' `sigma_Ixx`, `sigma_Iyy`, `sigma_Izz`, `sigma_Ixy`, `sigma_Ixz`, `sigma_Iyz`.
#'
#' The difference between `rollup_mass_props_unc()` and `rollup_mass_props_and_unc()` is that `rollup_mass_props_unc()`
#' expects the mass properties in its input to have been rolled up, whereas `rollup_mass_props_and_unc()` performs
#' the mass properties rollup itself.
#'
#' @inheritParams rollup_mass_props
#' @inheritParams update_mass_props_unc
#' @param validate_ds A validator for the tree and table, default `validate_mass_props_and_unc_table()`
#'
#' @returns The updated data set.
#' @export
#'
#' @examples
#' rollup_mass_props_and_unc(mp_tree_small, mp_table_small)
rollup_mass_props_and_unc <- function(tree, ds, validate_ds = validate_mass_props_and_unc_table, ...) {
  rollup(tree, ds, update_mass_props_and_unc, validate_ds, ...)
}

#' Roll up mass properties without input validation
#'
#' @description
#' `rollup_mass_props_fast()` performs the same operation as `rollup_mass_props()`
#' but omits input validation. It is somewhat faster than  `rollup_mass_props()` but should
#' be used with caution and only under circumstances in which the caller assumes
#' responsibility for validity of input. Its behavior when passed ill-formed input is unspecified.
#'
#' @inheritParams rollup_mass_props
#'
#' @returns The updated data set.
#'
#' @export
#'
#' @examples
#' rollup_mass_props_fast(test_tree, test_table)
#'
rollup_mass_props_fast <- function(tree, ds) {
  rollup_mass_props(tree, ds, validate_ds = function(t, d) TRUE, validate_tree = function(t) NA)
}


#' Roll up mass properties uncertainties without input validation
#'
#' @description
#' `rollup_mass_props_unc_fast()` performs the same operation as `rollup_mass_props_unc()`
#' but omits input validation. It is somewhat faster than  `rollup_mass_props_unc()` but should
#' be used with caution and only under circumstances in which the caller assumes
#' responsibility for validity of input. Its behavior when passed ill-formed input is unspecified.
#'
#' @inheritParams rollup_mass_props
#' @inheritParams update_mass_props_unc
#'
#' @returns The updated data set.
#'
#' @export
#'
#' @examples
#' rollup_mass_props_unc_fast(sawe_tree, sawe_table)
#'
rollup_mass_props_unc_fast <- function(tree, ds) {
  rollup_mass_props_unc(tree, ds, validate_ds = function(t, d) TRUE, validate_tree = function(t) NA)
}

#' Roll up mass properties and uncertainties without input validation
#'
#' @description
#' `rollup_mass_props_and_unc_fast()` performs the same operation as `rollup_mass_props_and_unc()`
#' but omits input validation. It is somewhat faster than `rollup_mass_props_and_unc()` but should
#' be used with caution and only under circumstances in which the caller assumes
#' responsibility for validity of input. Its behavior when passed ill-formed input is unspecified.
#'
#' @inheritParams rollup_mass_props
#' @inheritParams update_mass_props_unc
#'
#' @returns The updated data set.
#'
#' @export
#'
#' @examples
#' rollup_mass_props_and_unc_fast(sawe_tree, sawe_table)
#'
rollup_mass_props_and_unc_fast <- function(tree, ds) {
  rollup_mass_props_and_unc(tree, ds, validate_ds = function(t, d) TRUE, validate_tree = function(t) NA)
}
