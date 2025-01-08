xyz <- list("x", "y", "z")
expected <- list(
  '+' = list(
    mass = 0.121334510659333,
    center_mass = c(x = 70.5484760459512, y = 81.5438871271908, z = 44.5226242300123),
    inertia = matrix(data = c( 0.143256682060419,  -0.0123144483865307,   0.00127739195911864,
                              -0.0123144483865307,  0.14586924090344,     0.000348565076916697,
                               0.00127739195911864, 0.000348565076916697, 0.130922827321144), nrow = 3, byrow = 3, dimnames = list(xyz, xyz)
                     ),
    σ_mass = 0.289220546600936,
    σ_center_mass = c(x = 28.8332142065316, y = 29.1637070837215, z = 29.5945914710325),
    σ_inertia = matrix(data = c(0.372229441260767,  0.0219147131886038, 0.0214698151227243,
                                0.0219147131886038, 0.36942277459594,   0.0224252422361279,
                                0.0214698151227243, 0.0224252422361279, 0.374164784864648), nrow = 3, byrow = 3, dimnames = list(xyz, xyz)
                       )
  ),
  '-' = list(
    mass = 0.341313654366182,
    center_mass = c(x = 34.2744647525251, y = -44.9820708483458, z = 5.21137723699212),
    inertia = matrix(data = c( 0.498689387183853,    -0.0443698514655305,   -4.09633716939692e-05,
                               -0.0443698514655305,    0.452811377532981,    -4.51044268177994e-06,
                               -4.09633716939692e-05, -4.51044268177994e-06,  0.458318601936452), nrow = 3, byrow = 3, dimnames = list(xyz, xyz)
                     )
  )
)

test_that("df_get_mass_props() works for positive convention", {
  result <- df_get_mass_props(mp_table, "C.1.2.2.3.1.2.3")

  expect_equal(result$mass, expected$"+"$mass)
  expect_equal(result$center_mass, expected$"+"$center_mass)
  expect_equal(result$inertia, expected$"+"$inertia)
  expect_false(result$point)
})

test_that("df_get_mass_props() works for negative convention", {
  result <- df_get_mass_props(mp_table, "C.1.2.2.3.2.1.1")

  expect_equal(result$mass, expected$"-"$mass)
  expect_equal(result$center_mass, expected$"-"$center_mass)
  expect_equal(result$inertia, expected$"-"$inertia)
  expect_false(result$point)
})

test_that("df_get_mass_props_and_unc() works for positive convention", {
  result <- df_get_mass_props_and_unc(mp_table, "C.1.2.2.3.1.2.3")

  expect_equal(result$mass, expected$"+"$mass)
  expect_equal(result$center_mass, expected$"+"$center_mass)
  expect_equal(result$inertia, expected$"+"$inertia)
  expect_false(result$point)
  expect_equal(result$σ_mass, expected$"+"$σ_mass)
  expect_equal(result$σ_center_mass, expected$"+"$σ_center_mass)
  expect_equal(result$σ_inertia, expected$"+"$σ_inertia)
})

test_that("df_get_mass_props_and_unc() works for negative convention", {
  result <- df_get_mass_props_and_unc(mp_table, "C.1.2.2.3.2.1.1")

  expect_equal(result$mass, expected$"-"$mass)
  expect_equal(result$center_mass, expected$"-"$center_mass)
  expect_equal(result$inertia, expected$"-"$inertia)
  expect_false(result$point)
})

