#' Example Mass Properties Table
#' @format A data frame with columns:
#' \describe{
#' \item{id}{unique key}
#' \item{parent}{parent key}
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
#' \item{POIconv}{sign convention for products of inertia (one of c("+", "-"))}
#' \item{Ipoint}{logical indicator to consider item a point mass}
#' }
"test_table"
