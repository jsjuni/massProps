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

get_mass_props_and_unc_and_radii <- function(df, id) {
  l <- get_mass_props_and_unc(df, id)
  l$radii_gyration = sapply(c(x = "kx", y = "ky", z = "kz"), FUN=function(p) df_get_by_id(df, id, p))
  l
}

get_mass_props_and_unc_and_radii_and_unc <- function(df, id) {
  l <- get_mass_props_and_unc_and_radii(df, id)
  l$sigma_radii_gyration = sapply(c(x = "sigma_kx", y = "sigma_ky", z = "sigma_kz"), FUN=function(p) df_get_by_id(df, id, p))
  l
}

set_mass_props_radii_unc <- function(df, id, mp) {
  values <- list(
    sigma_kx = mp$sigma_k$x,
    sigma_ky = mp$sigma_k$y,
    sigma_kz = mp$sigma_k$z
  )
  Reduce(
    f = function(d, n) df_set_by_id(d, id, n, values[[n]]),
    x = names(values),
    init = df
  )
}

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
