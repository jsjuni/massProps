test_that("add_radii_of_gyration() works", {
  in_df <- mp_table[which(!is.na(mp_table$mass))[1:20], ]
  out_df <- add_radii_of_gyration(in_df)
  in_df$kx <- sqrt(in_df$Ixx / in_df$mass)
  in_df$ky <- sqrt(in_df$Iyy / in_df$mass)
  in_df$kz <- sqrt(in_df$Izz / in_df$mass)

  expect_true(all.equal(in_df, out_df))
})

test_that("get_mass_props_and_unc_and_radii() works", {
  in_df <- mp_table[which(!is.na(mp_table$mass))[1:20], ]
  out_df <- add_radii_of_gyration(in_df)
  expected <- out_df[which(out_df$id == "C.1.1.1.1.1.1.1"), ]
  actual <- get_mass_props_and_unc_and_radii(out_df, "C.1.1.1.1.1.1.1")

  expect_equal(unname(expected$mass), unname(actual$mass))
  expect_equal(unname(expected$kx), unname(actual$radii_gyration["x"]))
  expect_equal(unname(expected$ky), unname(actual$radii_gyration["y"]))
  expect_equal(unname(expected$kz), unname(actual$radii_gyration["z"]))
})
