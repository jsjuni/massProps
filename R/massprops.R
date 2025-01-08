#' Get mass properties for a row in a data frame
#'
#' `df_get_mass_props()` gets mass properties for a specified row in a data frame
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
#' df_get_mass_props(mp_table, "C.1.2.2.3.1.2.3")
df_get_mass_props <- function(df, id) {
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
#' `df_get_mass_props_and_unc()` gets mass properties with uncertainties for a specified row in a data frame
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
#' - `σ_mass` mass uncertainty
#' - `σ_center_mass` center of mass uncertainty (3-dimensional numeric)
#' - `σ_inertia` Inertia tensor uncertainty (3x3 numeric matrix)
#' @export
#'
#' @examples
#' df_get_mass_props_and_unc(mp_table, "C.1.2.2.3.1.2.3")
df_get_mass_props_and_unc <- function(df, id) {
  r <- df_get_mass_props(df, id)
  r$σ_mass <- df_get_by_id(df, id, "σ_mass")
  r$σ_center_mass <- sapply(c(x = "σ_Cx", y = "σ_Cy", z = "σ_Cz"), FUN=function(p) df_get_by_id(df, id, p))
  r$σ_inertia <- {
    xyz <- list("x", "y", "z")
    σ_inertia <- matrix(data = rep.int(0, 9), nrow = 3, dimnames = list(xyz, xyz))
    σ_inertia["x", "x"] <- df_get_by_id(df, id, "σ_Ixx")
    σ_inertia["y", "y"] <- df_get_by_id(df, id, "σ_Iyy")
    σ_inertia["z", "z"] <- df_get_by_id(df, id, "σ_Izz")
    σ_inertia["x", "y"] <- σ_inertia["y", "x"] <- df_get_by_id(df, id, "σ_Ixy")
    σ_inertia["x", "z"] <- σ_inertia["z", "x"] <- df_get_by_id(df, id, "σ_Ixz")
    σ_inertia["y", "z"] <- σ_inertia["z", "y"] <- df_get_by_id(df, id, "σ_Iyz")
    σ_inertia
  }
  r
}

#' Set mass properties for a row in a data frame
#'
#' `df_set_mass_props()` sets mass properties for a specified row in a data frame with
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
#' v <- df_get_mass_props(mp_table, "C.1.2.2.3.2.1.1")
#' v$poi_conv = "+"
#' df_set_mass_props(df, "C.1.2.2.3.2.1.1", v)
df_set_mass_props <- function(df, id, v) {
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
#' `df_set_mass_props_and_unc()` sets mass properties and uncertainties for a
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
#' - `σ_mass` mass uncertainty (numeric)
#' - `σ_center_mass` center of mass uncertainty (3-dimensional numeric)
#' - `σ_inertia` Inertia tensor uncertainty (3x3 numeric matrix)
#'
#' @return The updated data frame
#' @export
#'
#' @examples
#' df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
#' v <- df_get_mass_props_and_unc(mp_table, "C.1.2.2.3.2.1.1")
#' v$poi_conv = "+"
#' df_set_mass_props_and_unc(df, "C.1.2.2.3.2.1.1", v)
df_set_mass_props_and_unc <- function(df, id, v) {
  df |> df_set_mass_props(id, v) |>

    df_set_by_id(id, "σ_mass", v$σ_mass) |>

    df_set_by_id(id, "σ_Cx", v$σ_center_mass[1]) |>
    df_set_by_id(id, "σ_Cy", v$σ_center_mass[2]) |>
    df_set_by_id(id, "σ_Cz", v$σ_center_mass[3]) |>

    df_set_by_id(id, "σ_Ixx", v$σ_inertia["x", "x"]) |>
    df_set_by_id(id, "σ_Iyy", v$σ_inertia["y", "y"]) |>
    df_set_by_id(id, "σ_Izz", v$σ_inertia["z", "z"]) |>
    df_set_by_id(id, "σ_Ixy", v$σ_inertia["x", "y"]) |>
    df_set_by_id(id, "σ_Ixz", v$σ_inertia["x", "z"]) |>
    df_set_by_id(id, "σ_Iyz", v$σ_inertia["y", "z"])
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
#' vl <- Map(f = function(id) df_get_mass_props(test_table, id), leaves)
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
  r$inertia <- Reduce(
    `+`,
    Map(
      f  = function(v) {
        d <- r$center_mass - v$center_mass
        ddt <- outer(d, d)
        d_ss2 <- ddt - sum(diag(ddt)) * diag(3)
        if (v$point) -v$mass * d_ss2 else v$inertia - v$mass * d_ss2
      },
      vl
    )
  )

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
#' vl <- Map(f = function(id) df_get_mass_props_and_unc(sawe_table, id), list("Widget", "2nd Part"))
#' combine_mass_props_and_unc(vl)
combine_mass_props_and_unc <- function(vl) {

  r <- combine_mass_props(vl)

  # mass uncertainty

  r$σ_mass = sqrt(Reduce(`+`, Map(f = function(v) v$σ_mass^2, vl)))

  # center of mass uncertainty

  r$σ_center_mass = sqrt(Reduce(`+`, Map(
    f = function(v) {
      (v$mass * v$σ_center_mass) ^ 2 +
        (v$σ_mass * (v$center_mass - r$center_mass)) ^ 2
    },
    vl
  ))) / r$mass

  # inertia tensor uncertainty

  r$σ_inertia = sqrt(Reduce(`+`, Map(
    f = function(v) {

      d <- r$center_mass - v$center_mass

      P <- outer(d, v$σ_center_mass)
      p <- diag(P)
      diag_1 <- diag(c(p['x'] + 2 * p['y'], p['y'] + 2 * p['x'], p['z'] + 2 * p['x']))
      diag_2 <- diag(c(p['x'] + 2 * p['z'], p['y'] + 2 * p['z'], p['z'] + 2 * p['y']))

      Q <- outer(d, d)
      diag_3 <- sum(diag(Q)) * diag(3)

      v$σ_inertia^2 + (v$mass * (P - diag_1))^2 + (v$mass * (t(P) - diag_2))^2 + (v$σ_mass * (Q - diag_3))^2
    },
    vl
  )))

  # result

  r
}
