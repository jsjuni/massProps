#' Get mass property set for a row in a data frame
#'
#' `df_get_mass_props()` gets a mass property set for a specified row in a data frame
#' with (at least) these columns: `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
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

#' Get mass property set and uncertainties for a row in a data frame
#'
#' `df_get_mass_props_and_unc()` gets a mass property set with uncertainties for a specified row in a data frame
#' with (at least) these columns: `mass`, `Cx`, `Cy`, `Cz`, `Ixx`, `Iyy`, `Izz`, `Ixy`,
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

