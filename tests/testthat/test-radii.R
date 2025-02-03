test_that("add_radii_of_gyration() works", {
  in_df <- mp_table[which(!is.na(mp_table$mass))[1:20], ]
  out_df <- add_radii_of_gyration(in_df)
  in_df$kx <- sqrt(in_df$Ixx / in_df$mass)
  in_df$ky <- sqrt(in_df$Iyy / in_df$mass)
  in_df$kz <- sqrt(in_df$Izz / in_df$mass)

  expect_true(all.equal(out_df, in_df))
})

test_that("get_mass_props_and_unc_and_radii() works", {
  in_df <- mp_table[which(!is.na(mp_table$mass))[1:20], ]
  out_df <- add_radii_of_gyration(in_df)
  expected <- out_df[which(out_df$id == "C.1.1.1.1.1.1.1"), ]
  actual <- get_mass_props_and_unc_and_radii(out_df, "C.1.1.1.1.1.1.1")

  expect_equal(unname(actual$mass), unname(expected$mass))
  expect_equal(unname(actual$radii_gyration["x"]), unname(expected$kx))
  expect_equal(unname(actual$radii_gyration["y"]), unname(expected$ky))
  expect_equal(unname(actual$radii_gyration["z"]), unname(expected$kz))
})

test_that("set_radii_of_gyration_unc() works", {
  mp <- list(sigma_k = c(x = 31, y = 59, z = 83))
  actual <- set_radii_of_gyration_unc(mp_table, "C.1", mp)
  row <- which(actual$id == "C.1")

  expect_equal(unname(actual[row, "sigma_kx"]), unname(mp$sigma_k["x"]))
  expect_equal(unname(actual[row, "sigma_ky"]), unname(mp$sigma_k["y"]))
  expect_equal(unname(actual[row, "sigma_kz"]), unname(mp$sigma_k["z"]))
})
