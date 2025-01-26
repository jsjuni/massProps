#' Example Mass Properties Table
#' @format A data frame with columns:
#' \describe{
#' \item{id}{unique key}
#' \item{name}{character name}
#' \item{POIconv}{sign convention for products of inertia (one of c("+", "-"))}
#' \item{mass}{mass}
#' \item{Cx}{\eqn{x}-component of center of mass}
#' \item{Cy}{\eqn{y}-component of center of mass}
#' \item{Cz}{\eqn{z}-component of center of mass}
#' \item{Ixx}{\eqn{I_{xx}} moment of inertia}
#' \item{Iyy}{\eqn{I_{yy}} moment of inertia}
#' \item{Izz}{\eqn{I_{zz}} moment of inertia}
#' \item{Ixy}{\eqn{I_{xy}} product of inertia}
#' \item{Ixz}{\eqn{I_{xz}} product of inertia}
#' \item{Iyz}{\eqn{I_{yz}} product of inertia}
#' \item{Ipoint}{logical indicator to consider item a point mass, i.e., with negligible inertia}
#' \item{sigma_mass}{mass uncertainty}
#' \item{sigma_Cx}{\eqn{x}-component of center of mass uncertainty}
#' \item{sigma_Cy}{\eqn{y}-component of center of mass uncertainty}
#' \item{sigma_Cz}{\eqn{z}-component of center of mass uncertainty}
#' \item{sigma_Ixx}{\eqn{I_{xx}} moment of inertia uncertainty}
#' \item{sigma_Iyy}{\eqn{I_{yy}} moment of inertia uncertainty}
#' \item{sigma_Izz}{\eqn{I_{zz}} moment of inertia uncertainty}
#' \item{sigma_Ixy}{\eqn{I_{xy}} product of inertia uncertainty}
#' \item{sigma_Ixz}{\eqn{I_{xz}} product of inertia uncertainty}
#' \item{sigma_Iyz}{\eqn{I_{yz}} product of inertia uncertainty}
#' }
"mp_table"
