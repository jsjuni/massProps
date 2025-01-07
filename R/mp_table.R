#' Example Mass Properties Table
#' @format A data frame with columns:
#' \describe{
#' \item{id}{unique key}
#' \item{name}{character name}
#' \item{POIconv}{sign convention for products of inertia (one of c("+", "-"))}
#' \item{mass}{mass}
#' \item{Cx}{x component of center of mass}
#' \item{Cy}{y component of center of mass}
#' \item{Cz}{z component of center of mass}
#' \item{Ixx}{Ixx moment of inertia}
#' \item{Iyy}{Iyy moment of inertia}
#' \item{Izz}{Izz moment of inertia}
#' \item{Ixy}{Ixy product of inertia}
#' \item{Ixz}{Ixz product of inertia}
#' \item{Iyz}{Iyz product of inertia}
#' \item{Ipoint}{logical indicator to consider item a point mass}
#' \item{σ_mass}{mass uncertainty}
#' \item{σ_Cx}{x component of center of mass uncertainty}
#' \item{σ_Cy}{y component of center of mass uncertainty}
#' \item{σ_Cz}{z component of center of mass uncertainty}
#' \item{σ_Ixx}{Ixx moment of inertia uncertainty}
#' \item{σ_Iyy}{Iyy moment of inertia uncertainty}
#' \item{σ_Izz}{Izz moment of inertia uncertainty}
#' \item{σ_Ixy}{Ixy product of inertia uncertainty}
#' \item{σ_Ixz}{Ixz product of inertia uncertainty}
#' \item{σ_Iyz}{Iyz product of inertia uncertainty}
#' }
"mp_table"
