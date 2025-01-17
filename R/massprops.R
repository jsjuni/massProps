#' Get mass properties for a row in a data frame
#'
#' `get_mass_props()` gets mass properties for a specified row in a data frame
#' with (at least) these columns: `id`, `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
#' `Ixz`, `Iyz`, `POIconv`, `Ipoint`.
#'
#' @param df A data frame
#' @param id ID value of the desired row
#'
#' @return
#' A list with the following named elements:
#' - `mass` mass (numeric)
#' - `center_mass` center of mass (3-dimensional numeric)
#' - `inertia` Inertia tensor (3x3 numeric matrix)
#' - `point` Logical indicating point mass, i.e., negligible inertia
#' @export
#'
#' @examples
#' get_mass_props(mp_table, "C.1.2.2.3.1.2.3")
get_mass_props <- function(df, id) {
  poi_conv <- df_get_by_id(df, id, "POIconv")
  list(
    mass = df_get_by_id(df, id, "mass"),
    center_mass = sapply(c(x = "Cx", y = "Cy", z = "Cz"), FUN=function(p) df_get_by_id(df, id, p)),
    inertia = {
      xyz <- list("x", "y", "z")
      it <- matrix(data = rep.int(0, 9), nrow = 3, dimnames = list(xyz, xyz))
      it["x", "x"] <- df_get_by_id(df, id, "Ixx")
      it["y", "y"] <- df_get_by_id(df, id, "Iyy")
      it["z", "z"] <- df_get_by_id(df, id, "Izz")
      poi_factor <- if (poi_conv == '-') 1 else -1
      it["x", "y"] <- it["y", "x"] <- poi_factor * df_get_by_id(df, id, "Ixy")
      it["x", "z"] <- it["z", "x"] <- poi_factor * df_get_by_id(df, id, "Ixz")
      it["y", "z"] <- it["z", "y"] <- poi_factor * df_get_by_id(df, id, "Iyz")
      it
    },
    point = df_get_by_id(df, id, "Ipoint")
  )
}

#' Get mass properties and uncertainties for a row in a data frame
#'
#' `get_mass_props_and_unc()` gets mass properties with uncertainties for a specified row in a data frame
#' with (at least) these columns: `id`, `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
#' `Ixz`, `Iyz`, `POIconv`, `Ipoint`, `σ_mass`, `σ_Cx`, `σ_Cy`, `σ_Cz`, `σ_Ixx`, `σ_Iyy`, `σ_Izz`, `σ_Ixy`, `σ_Ixz`, `σ_Iyz`.
#'
#' @param df A data frame
#' @param id ID value of the desired row
#'
#' @return
#' A list with the following named elements:
#' - `mass` mass (numeric)
#' - `center_mass` center of mass (3-dimensional numeric)
#' - `inertia` Inertia tensor (3x3 numeric matrix)
#' - `point` Logical indicating point mass, i.e., negligible inertia
#' - `sigma_mass` mass uncertainty
#' - `sigma_center_mass` center of mass uncertainty (3-dimensional numeric)
#' - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)
#' @export
#'
#' @examples
#' get_mass_props_and_unc(mp_table, "C.1.2.2.3.1.2.3")
get_mass_props_and_unc <- function(df, id) {
  r <- get_mass_props(df, id)
  r$sigma_mass <- df_get_by_id(df, id, "\u03c3_mass")
  r$sigma_center_mass <- sapply(c(x = "\u03c3_Cx", y = "\u03c3_Cy", z = "\u03c3_Cz"), FUN=function(p) df_get_by_id(df, id, p))
  r$sigma_inertia <- {
    xyz <- list("x", "y", "z")
    sigma_inertia <- matrix(data = rep.int(0, 9), nrow = 3, dimnames = list(xyz, xyz))
    sigma_inertia["x", "x"] <- df_get_by_id(df, id, "\u03c3_Ixx")
    sigma_inertia["y", "y"] <- df_get_by_id(df, id, "\u03c3_Iyy")
    sigma_inertia["z", "z"] <- df_get_by_id(df, id, "\u03c3_Izz")
    sigma_inertia["x", "y"] <- sigma_inertia["y", "x"] <- df_get_by_id(df, id, "\u03c3_Ixy")
    sigma_inertia["x", "z"] <- sigma_inertia["z", "x"] <- df_get_by_id(df, id, "\u03c3_Ixz")
    sigma_inertia["y", "z"] <- sigma_inertia["z", "y"] <- df_get_by_id(df, id, "\u03c3_Iyz")
    sigma_inertia
  }
  r
}

#' Set mass properties for a row in a data frame
#'
#' `set_mass_props()` sets mass properties for a specified row in a data frame with
#' an `id` column.
#'
#' @param df A data frame
#' @param id ID value of the desired row
#' @param v
#' #' A list with the following named elements:
#' - `mass` mass (numeric)
#' - `center_mass` center of mass (3-dimensional numeric)
#' - `inertia` Inertia tensor (3x3 numeric matrix)
#' - `point` Logical indicating point mass, i.e., negligible inertia
#' - `poi_conv` Enumeration c("+", "-") indicating sign convention for products of inertia
#'
#' @return The updated data frame
#' @export
#'
#' @examples
#' df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
#' v <- get_mass_props(mp_table, "C.1.2.2.3.2.1.1")
#' v$poi_conv = "+"
#' df <- set_mass_props(df, "C.1.2.2.3.2.1.1", v)
#' get_mass_props(df, "C.1.2.2.3.2.1.1")
set_mass_props <- function(df, id, v) {
  m <- v$inertia
  poi_factor <- if (v$poi_conv == "-") 1 else -1
  df |> df_set_by_id(id, "mass", v$mass) |>

    df_set_by_id(id, "Cx", v$center_mass[1]) |>
    df_set_by_id(id, "Cy", v$center_mass[2]) |>
    df_set_by_id(id, "Cz", v$center_mass[3]) |>

    df_set_by_id(id, "Ixx", m["x", "x"]) |>
    df_set_by_id(id, "Iyy", m["y", "y"]) |>
    df_set_by_id(id, "Izz", m["z", "z"]) |>
    df_set_by_id(id, "Ixy", poi_factor * (m["x", "y"] + m["y", "x"]) / 2.0) |>
    df_set_by_id(id, "Ixz", poi_factor * (m["x", "z"] + m["z", "x"]) / 2.0) |>
    df_set_by_id(id, "Iyz", poi_factor * (m["y", "z"] + m["z", "y"]) / 2.0) |>

    df_set_by_id(id, "POIconv", v$poi_conv) |>
    df_set_by_id(id, "Ipoint", v$point)
}

#' Set mass properties and uncertainties for a row in a data frame
#'
#' `set_mass_props_and_unc()` sets mass properties and uncertainties for a
#' specified row in a data frame with an `id` column.
#'
#' @param df A data frame
#' @param id ID value of the desired row
#' @param v
#' #' A list with the following named elements:
#' - `mass` mass (numeric)
#' - `center_mass` center of mass (3-dimensional numeric)
#' - `inertia` Inertia tensor (3x3 numeric matrix)
#' - `point` Logical indicating point mass, i.e., negligible inertia
#' - `poi_conv` Enumeration c("+", "-") indicating sign convention for products of inertia
#' - `sigma_mass` mass uncertainty (numeric)
#' - `sigma_center_mass` center of mass uncertainty (3-dimensional numeric)
#' - `sigma_inertia` Inertia tensor uncertainty (3x3 numeric matrix)
#'
#' @return The updated data frame
#' @export
#'
#' @examples
#' df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
#' v <- get_mass_props_and_unc(mp_table, "C.1.2.2.3.2.1.1")
#' v$poi_conv = "+"
#' df <- set_mass_props_and_unc(df, "C.1.2.2.3.2.1.1", v)
#' get_mass_props_and_unc(df, "C.1.2.2.3.2.1.1")
set_mass_props_and_unc <- function(df, id, v) {
  df |> set_mass_props(id, v) |>

    df_set_by_id(id, "\u03c3_mass", v$sigma_mass) |>

    df_set_by_id(id, "\u03c3_Cx", v$sigma_center_mass[1]) |>
    df_set_by_id(id, "\u03c3_Cy", v$sigma_center_mass[2]) |>
    df_set_by_id(id, "\u03c3_Cz", v$sigma_center_mass[3]) |>

    df_set_by_id(id, "\u03c3_Ixx", v$sigma_inertia["x", "x"]) |>
    df_set_by_id(id, "\u03c3_Iyy", v$sigma_inertia["y", "y"]) |>
    df_set_by_id(id, "\u03c3_Izz", v$sigma_inertia["z", "z"]) |>
    df_set_by_id(id, "\u03c3_Ixy", v$sigma_inertia["x", "y"]) |>
    df_set_by_id(id, "\u03c3_Ixz", v$sigma_inertia["x", "z"]) |>
    df_set_by_id(id, "\u03c3_Iyz", v$sigma_inertia["y", "z"])
}

#' Combine mass properties
#'
#' @param vl List of mass properties lists
#'
#' @return Combined mass properties list
#' @export
#'
#' @examples
#' leaves <- test_table[which(!is.na(test_table$mass)), "id"]
#' vl <- Map(f = function(id) get_mass_props(test_table, id), leaves)
#' combine_mass_props(vl)

combine_mass_props <- function(vl) {

  r <- list()

  # sum of masses

  r$mass <- Reduce(`+`, Map(f = function(v) v$mass, vl))

  # mass-weighted sum of centers of mass

  r$center_mass <- Reduce(`+`, Map(f = function(v) v$mass * v$center_mass, vl)) / r$mass

  # parallel axis theorem
  # https://en.wikipedia.org/wiki/Parallel_axis_theorem#Moment_of_inertia_matrix
  # d_ss2 is [d]^2 computed using the identities given
  r$inertia <- Reduce(`+`, Map(
    f  = function(v) {
      d <- r$center_mass - v$center_mass
      d_ss2 <- outer(d, d) - sum(d^2) * diag(3)
      if (v$point) -v$mass * d_ss2 else v$inertia - v$mass * d_ss2
    },
    vl
  ))

  # aggregate is a point mass iff all parts are point masses at the same center

  r$point = Reduce(f = `&&`,
         Map(f = function(v) v$point && isTRUE(all.equal(v$center_mass, r$center_mass)), vl)
  )

  r
}

#' Combine mass properties and uncertainties
#'
#' @param vl List of mass properties and uncertainties lists
#'
#' @return Combined mass properties and uncertainties
#' @export
#'
#' @examples
#' vl <- Map(f = function(id) get_mass_props_and_unc(sawe_table, id), list("Widget", "2nd Part"))
#' combine_mass_props_and_unc(vl)
combine_mass_props_and_unc <- function(vl) {

  r <- combine_mass_props(vl)

  # mass uncertainty

  r$sigma_mass = sqrt(Reduce(`+`, Map(f = function(v) v$sigma_mass^2, vl)))

  # center of mass uncertainty

  r$sigma_center_mass = sqrt(Reduce(`+`, Map(
    f = function(v) {
      (v$mass * v$sigma_center_mass)^2 +
        (v$sigma_mass * (v$center_mass - r$center_mass))^2
    },
    vl
  ))) / r$mass

  # inertia tensor uncertainty

  r$sigma_inertia = sqrt(Reduce(`+`, Map(
    f = function(v) {

      d <- r$center_mass - v$center_mass

      P <- outer(d, v$sigma_center_mass)
      p <- as.list(diag(P))
      diag_1 <- diag(c(p$x - 2 * p$y, p$y - 2 * p$x, p$z - 2 * p$x))
      diag_2 <- diag(c(p$x - 2 * p$z, p$y - 2 * p$z, p$z - 2 * p$y))

      Q <- outer(d, d)
      diag_3 <- sum(diag(Q)) * diag(3)

      v$sigma_inertia^2 +
        v$mass^2 * ((P - diag_1)^2 + (t(P) - diag_2)^2) +
        (v$sigma_mass * (Q - diag_3))^2
    },
    vl
  )))

  # result

  r
}

#' Set POI convention for mass properties object to "+"
#'
#' @description
#' `set_poi_conv_plus()` sets the products of inertia convention for a
#' mass properties object to "+". This does not affect the values of the
#' object, but it determines how products of inertia are saved to a
#' data set.
#'
#' The signature of `set_poi_conv_plus()` is such that it can be passed as an `override` argument
#' to `update_mass_props()` and `update_mass_props_and_unc()`, thus ensuring
#' that all calculated POI values follow the positive integral convention.
#'
#' @param ds Ignored
#' @param target Ignored
#' @param v A mass properties object
#'
#' @return The mass properties object with the POI convention set to "+"
#' @export
#'
#' @examples
#' set_poi_conv_plus(NULL, NULL, get_mass_props(mp_table, "C.1.2.2.3.2.1.1"))
set_poi_conv_plus <- function(ds, target, v) {
  v$poi_conv <- "+"
  v
}

#' Set POI convention for mass properties object to "-"
#'
#' @description
#' `set_poi_conv_minus()` sets the products of inertia convention for a
#' mass properties object to "+". This does not affect the values of the
#' object, but it determines how products of inertia are saved to a
#' data set.
#'
#' The signature `of set_poi_conv_minus()` is such that it can be passed as an `override` argument
#' to `update_mass_props()` and `update_mass_props_and_unc()`, thus ensuring
#' that all calculated POI values follow the negative integral convention.
#'
#' @param ds Ignored
#' @param target Ignored
#' @param v A mass properties object
#'
#' @return The mass properties object with the POI convention set to "-"
#' @export
#'
#' @examples
#' set_poi_conv_minus(NULL, NULL, get_mass_props(mp_table, "C.1.2.2.3.2.1.1"))
set_poi_conv_minus <- function(ds, target, v) {
  v$poi_conv <- "-"
  v
}

#' Set POI convention for mass properties object to match the target item
#'
#' @description
#' `set_poi_conv_from_target()` sets the products of inertia convention for a
#' mass properties object to that of the target item in the mass properties table. This does not affect the values of
#' object, but it determines how products of inertia are saved to the
#' data frame.
#'
#' The signature `of set_poi_conv_from_target()` is such that it can be passed as an `override` argument
#' to `update_mass_props()` and `update_mass_props_and_unc()`, thus ensuring
#' that all calculated POI values follow the negative integral convention of the target item to which they are written.
#'
#' @param df A data frame with column `id`
#' @param target ID value for the target item
#' @param v A mass properties object
#'
#' @return The mass properties object with the POI convention set to that of the target item
#' @export
#'
#' @examples
#' set_poi_conv_from_target(mp_table, "C.1.2.2.3.2.1", get_mass_props(mp_table, "C.1.2.2.3.2.1.1"))
set_poi_conv_from_target <- function(df, target, v) {
  v$poi_conv <- df_get_by_id(df, target, "POIconv")
  v
}

#' Update mass properties
#'
#' `update_mass_props()` updates mass properties for a specified target row from
#' specified source rows in a data frame
#' with (at least) these columns: `id`, `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
#' `Ixz`, `Iyz`, `POIconv`, `Ipoint`.
#'
#' @param df A data frame
#' @param target ID of the target row
#' @param sources IDs of the source rows
#' @param override An override function, called as override(df, target, value)
#'
#' @return The updated data fram
#' @export
#'
#' @examples
#' leaves <- test_table[which(!is.na(test_table$mass)), "id"]
#' df <- update_mass_props(test_table, "A.1", leaves)
#' get_mass_props(df, "A.1")
update_mass_props <- function(df, target, sources, override = set_poi_conv_from_target) {
  update_prop(
    df,
    target = target,
    sources = sources,
    set = set_mass_props,
    get = get_mass_props,
    combine = combine_mass_props,
    override = override
  )
}

#' Update mass properties and uncertainties
#'
#' `update_mass_props_and_unc()` updates mass properties and uncertainties
#' for a specified target row from
#' specified source rows in a data frame
#' with (at least) these columns: `id`, `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
#' `Ixz`, `Iyz`, `POIconv`, `Ipoint`, `σ_mass`, `σ_Cx`, `σ_Cy`, `σ_Cz`, `σ_Ixx`, `σ_Iyy`, `σ_Izz`, `σ_Ixy`, `σ_Ixz`, `σ_Iyz`.
#'
#' @param df A data frame
#' @param target ID of the target row
#' @param sources IDs of the source rows
#' @param override An override function, called as override(df, target, value)
#'
#' @return The updated data frame
#' @export
#'
#' @examples
#' leaves <- list("Widget", "2nd Part")
#' df <- update_mass_props_and_unc(sawe_table, "Combined", leaves)
#' get_mass_props_and_unc(sawe_table, "Combined")
update_mass_props_and_unc <- function(df, target, sources, override = set_poi_conv_from_target) {
  update_prop(
    df,
    target = target,
    sources = sources,
    set = set_mass_props_and_unc,
    get = get_mass_props_and_unc,
    combine = combine_mass_props_and_unc,
    override = override
  )
}

#' Validate mass properties
#'
#' @description
#' `validate_mass_props()` ensures that a mass properties object satisfies the following
#' constraints:
#' - mass is non-missing and positive
#' - center of mass is a 3-vector of non-missing numeric values
#' - point mass indicator is TRUE or FALSE
#' - for every non-point mass:
#'   - the inertia tensor is positive definite
#'   - eigenvalues of the inertia tensor satisfy the triangle inequalities:
#'     - e1 < e2 + e3
#'     - e2 < e1 + e3
#'     - e3 < e1 + e2
#'
#' @param mp Mass properties object
#'
#' @return TRUE if valid, stops otherwise
#' @export
#'
#' @examples
#' mp <- get_mass_props(test_table, "C.1")
#' validate_mass_props(mp)
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

#' Validate mass properties and uncertainties
#'
#' @description
#' `validate_mass_props_and_unc()` performs the checks of `validate_mass_props()` and
#' ensures the following are true:
#' - mass uncertainty is non-missing and non-negative
#' - center of mass uncertainty is a 3-vector of non-missing non-negative values
#' - for non-point masses, the inertia tensor uncertainty is a 3x3 matrix of non-missing non-negative values
#'
#' @param mp Mass properties and uncertainties object
#'
#' @return TRUE if valid, stops otherwise
#' @export
#'
#' @examples
#' mp <- get_mass_props_and_unc(sawe_table, "Widget")
#' validate_mass_props_and_unc(mp)
validate_mass_props_and_unc <- function(mp) {

  validate_mass_props(mp)

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

#' Validate a mass properties table
#'
#' @param tree An `igraph` tree with edges from child to parent
#' @param df A data frame to validate
#'
#' @description
#' `validate_mass_props_table()` ensures that the `id` column of the table and the vertices
#' of the tree contain the same identifiers, and that the mass properties of every leaf element
#' of the table are valid.
#'
#' @return TRUE if valid, stops with an error otherwise
#' @export
#'
#' @examples
#' validate_mass_props_table(mp_tree, mp_table)
validate_mass_props_table <- function(tree, df) {
  validate_ds(tree, df, df_get_ids, get_mass_props, validate_mass_props)
}

#' Validate a mass properties and uncertainties table
#'
#' @param tree An `igraph` tree with edges from child to parent
#' @param df A data frame to validate
#'
#' @description
#' `validate_mass_props_and_unc()` calls
#' `validate_mass_props_table()` and further ensures that the mass properties uncertainties of every leaf element
#' of the table are valid.
#'
#' @return TRUE if valid, stops with an error otherwise
#' @export
#'
#' @examples
#' validate_mass_props_and_unc_table(mp_tree, mp_table)
validate_mass_props_and_unc_table <- function(tree, df) {
  validate_ds(tree, df, df_get_ids, get_mass_props_and_unc, validate_mass_props_and_unc)
}

#' Roll Up Mass Properties
#'
#' @description
#' 'rollup_mass_props()' rolls up mass properties in a data frame
#' with (at least) these columns: `id`, `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
#' `Ixz`, `Iyz`, `POIconv`, `Ipoint`.
#'
#' @param tree An igraph directed graph that is a valid single-rooted in-tree with edges from child vertex to parent vertex.
#' @param df A mass properties table
#' @param validate_df A validator for the tree and table, default `validate_mass_props_table()`
#' @param ... Other parameters passed `rollup()`
#'
#' @returns The updated data frame
#' @export
#'
#' @examples
#' rollup_mass_props(test_tree, test_table)
rollup_mass_props <- function(tree, df, validate_df = validate_mass_props_table, ...) {
  rollup(tree, df, update_mass_props, validate_df, ...)
}

#' Roll Up Mass Properties and Uncertainties
#'
#' @description
#' 'rollup_mass_props()' rolls up mass properties in a data frame
#' with (at least) these columns: `id`, `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
#' `Ixz`, `Iyz`, `POIconv`, `Ipoint`, `σ_mass`, `σ_Cx`, `σ_Cy`, `σ_Cz`, `σ_Ixx`, `σ_Iyy`, `σ_Izz`, `σ_Ixy`, `σ_Ixz`, `σ_Iyz`.
#'
#' @param tree An igraph directed graph that is a valid single-rooted in-tree with edges from child vertex to parent vertex.
#' @param df A mass properties and uncertainties table
#' @param validate_df A validator for the tree and table, default `validate_mass_props_and_unc_table()`
#' @param ... Other parameters passed `rollup()`
#'
#' @returns The updated data frame
#' @export
#'
#' @examples
#' rollup_mass_props_and_unc(sawe_tree, sawe_table)
rollup_mass_props_and_unc <- function(tree, df, validate_df = validate_mass_props_and_unc_table, ...) {
  rollup(tree, df, update_mass_props_and_unc, validate_df, ...)
}

#' Roll Up Mass Properties Without Input Validation
#'
#' @description
#' `rollup_mass_props_fast()` performs the same operation as `rollup_mass_props()`
#' but omits input validation. It is roughly 30% faster than  `rollup_mass_props()` but should
#' be used with caution and only under circumstances in which the caller assumes
#' responsibility for validity of input. Its behavior when passed ill-formed input is unspecified.
#'
#' @param tree tree passed to `rollup()`
#' @param df mass properties and uncertainties table passed to `rollup()`
#'
#' @returns The updated data frame
#' @export
#'
#' @examples
#' rollup_mass_props_fast(test_tree, test_table)
rollup_mass_props_fast <- function(tree, df) {
  rollup(tree, df, update_mass_props, validate_ds = function(t, d) TRUE, validate_tree = function(t) NA)
}

#' Roll Up Mass Properties And Uncertainties Without Input Validation
#'
#' @description
#' `rollup_mass_props_and_unc_fast()` performs the same operation as `rollup_mass_props_and_unc()`
#' but omits input validation. It is roughly 30% faster than  `rollup_mass_propss_and_unc()` but should
#' be used with caution and only under circumstances in which the caller assumes
#' responsibility for validity of input. Its behavior when passed ill-formed input is unspecified.
#'
#' @param tree tree passed to `rollup()`
#' @param df mass properties and uncertainties table passed to `rollup()`
#'
#' @returns The updated data frame
#' @export
#'
#' @examples
#' rollup_mass_props_and_unc_fast(sawe_tree, sawe_table)
rollup_mass_props_and_unc_fast <- function(tree, df) {
  rollup(tree, df, update_mass_props_and_unc, validate_ds = function(t, d) TRUE, validate_tree = function(t) NA)
}
