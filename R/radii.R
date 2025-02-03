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
