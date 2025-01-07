test_that("df_get_by_id() works for positive convention", {
  result <- df_get_mass_props(mp_table, "C.1.2.2.3.1.2.3")
  xyz <- list("x", "y", "z")
  expected <- list(
    mass = 0.121334510659333,
    center_mass = c(x = 70.5484760459512, y = 81.5438871271908, z = 44.5226242300123),
    inertia = matrix(data = c( 0.143256682060419,  -0.0123144483865307,   0.00127739195911864,
                              -0.0123144483865307,  0.14586924090344,     0.000348565076916697,
                               0.00127739195911864, 0.000348565076916697, 0.130922827321144), nrow = 3, byrow = 3, dimnames = list(xyz, xyz))
  )

  expect_equal(result$mass, expected$mass)
  expect_equal(result$center_mass, expected$center_mass)
  expect_equal(result$inertia, expected$inertia)
  expect_false(result$point)
})

test_that("df_get_by_id() works for negative convention", {
  result <- df_get_mass_props(mp_table, "C.1.2.2.3.2.1.1")
  xyz <- list("x", "y", "z")
  expected <- list(
    mass = 0.341313654366182,
    center_mass = c(x = 34.2744647525251, y = -44.9820708483458, z = 5.21137723699212),
    inertia = matrix(data = c( 0.498689387183853,    -0.0443698514655305,   -4.09633716939692e-05,
                              -0.0443698514655305,    0.452811377532981,    -4.51044268177994e-06,
                              -4.09633716939692e-05, -4.51044268177994e-06,  0.458318601936452), nrow = 3, byrow = 3, dimnames = list(xyz, xyz))
  )

  expect_equal(result$mass, expected$mass)
  expect_equal(result$center_mass, expected$center_mass)
  expect_equal(result$inertia, expected$inertia)
  expect_false(result$point)
})
