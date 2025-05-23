% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/massprops.R
\name{get_mass_props_and_unc}
\alias{get_mass_props_and_unc}
\title{Get mass properties and uncertainties for a row in a data frame}
\usage{
get_mass_props_and_unc(df, id)
}
\arguments{
\item{df}{A data frame with (at least) these columns: \code{id}, \code{mass}, \code{Cx},
\code{Cy}, \code{Cz}, \code{Ixx}, \code{Iyy}, \code{Izz}, \code{Ixy}, \code{Ixz}, \code{Iyz}, \code{POIconv}, \code{Ipoint},
\code{sigma_mass}, \code{sigma_Cx}, \code{sigma_Cy}, \code{sigma_Cz},\code{sigma_Ixy}, \code{sigma_Ixz},
\code{sigma_Iyz}.}

\item{id}{The \code{id} value of the desired row.}
}
\value{
A list with the following named elements:
\itemize{
\item \code{mass} Numeric mass.
\item \code{center_mass} Numeric 3-vector center of mass.
\item \code{point} Logical indicating point mass. The inertia of point masses is excluded from calculations.
\item \code{inertia} Numeric 3x3 matrix inertia tensor. The signs of the off-diagonal elements of the inertia tensor
are determined by \code{POIconv}. For example, the \eqn{xy} element of the inertia
tensor is \code{Ixy} if \code{POIconv} is "-"; it is -\code{Ixy} if \code{POIconv} is "+".
\item \code{sigma_mass} Numeric mass uncertainty.
\item \code{sigma_center_mass} Numeric 3-vector center of mass uncertainties.
\item \code{sigma_inertia} Numeric 3x3 matrix inertia tensor uncertainties.
}
}
\description{
\code{get_mass_props_and_unc()} is a convenience wrapper that combines the results of
\code{get_mass_props()} and \code{get_mass_props_unc()}.
}
\examples{
get_mass_props_and_unc(mp_table, "C.1.2.2.3.1.2.3")

}
