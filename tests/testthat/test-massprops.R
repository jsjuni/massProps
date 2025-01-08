xyz <- list("x", "y", "z")
mp_set <- list(
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
                       ),
    point = FALSE
  ),
  '-' = list(
    mass = 0.341313654366182,
    center_mass = c(x = 34.2744647525251, y = -44.9820708483458, z = 5.21137723699212),
    inertia = matrix(data = c( 0.498689387183853,     -0.0443698514655305,   -4.09633716939692e-05,
                               -0.0443698514655305,    0.452811377532981,    -4.51044268177994e-06,
                               -4.09633716939692e-05, -4.51044268177994e-06,  0.458318601936452), nrow = 3, byrow = 3, dimnames = list(xyz, xyz)
                     ),
    σ_mass = 0.289220546600936,
    σ_center_mass = c(x = 28.8332142065316, y = 29.1637070837215, z = 29.5945914710325),
    σ_inertia = matrix(data = c(0.372229441260767,  0.0219147131886038, 0.0214698151227243,
                                0.0219147131886038, 0.36942277459594,   0.0224252422361279,
                                0.0214698151227243, 0.0224252422361279, 0.374164784864648), nrow = 3, byrow = 3, dimnames = list(xyz, xyz)
                      ),
    point = FALSE
  )
)

test_that("df_get_mass_props() works for positive convention", {
  result <- df_get_mass_props(mp_table, "C.1.2.2.3.1.2.3")

  expect_equal(result$mass, mp_set$"+"$mass)
  expect_equal(result$center_mass, mp_set$"+"$center_mass)
  expect_equal(result$inertia, mp_set$"+"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
})

test_that("df_get_mass_props() works for negative convention", {
  result <- df_get_mass_props(mp_table, "C.1.2.2.3.2.1.1")

  expect_equal(result$mass, mp_set$"-"$mass)
  expect_equal(result$center_mass, mp_set$"-"$center_mass)
  expect_equal(result$inertia, mp_set$"-"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
})

test_that("df_get_mass_props_and_unc() works for positive convention", {
  result <- df_get_mass_props_and_unc(mp_table, "C.1.2.2.3.1.2.3")

  expect_equal(result$mass, mp_set$"+"$mass)
  expect_equal(result$center_mass, mp_set$"+"$center_mass)
  expect_equal(result$inertia, mp_set$"+"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
  expect_equal(result$σ_mass, mp_set$"+"$σ_mass)
  expect_equal(result$σ_center_mass, mp_set$"+"$σ_center_mass)
  expect_equal(result$σ_inertia, mp_set$"+"$σ_inertia)
})

test_that("df_get_mass_props_and_unc() works for negative convention", {
  result <- df_get_mass_props_and_unc(mp_table, "C.1.2.2.3.2.1.1")

  expect_equal(result$mass, mp_set$"-"$mass)
  expect_equal(result$center_mass, mp_set$"-"$center_mass)
  expect_equal(result$inertia, mp_set$"-"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
  expect_equal(result$σ_mass, mp_set$"-"$σ_mass)
  expect_equal(result$σ_center_mass, mp_set$"-"$σ_center_mass)
  expect_equal(result$σ_inertia, mp_set$"-"$σ_inertia)
})

test_that("df_set_mass_props() works for positive convention", {
  input <- mp_set$"+"
  input$poi_conv = "+"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  result <- df_set_mass_props(df, "C.1.2.2.3.1.2.3", input)[1, ]

  expect_equal(result$mass, input$mass)
  expect_equal(result$Cx, input$center_mass[["x"]])
  expect_equal(result$Cy, input$center_mass[["y"]])
  expect_equal(result$Cz, input$center_mass[["z"]])
  it <- input$inertia
  expect_equal(result$Ixx, it["x", "x"])
  expect_equal(result$Iyy, it["y", "y"])
  expect_equal(result$Izz, it["z", "z"])
  expect_equal(result$Ixy, -it["x", "y"])
  expect_equal(result$Ixz, -it["x", "z"])
  expect_equal(result$Iyz, -it["y", "z"])
  expect_equal(result$POIconv, input$poi_conv)
  expect_equal(result$Ipoint, input$point)
})

test_that("df_set_mass_props() works for negative convention", {
  input <- mp_set$"-"
  input$poi_conv = "-"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  result <- df_set_mass_props(df, "C.1.2.2.3.2.1.1", input)[2, ]

  expect_equal(result$mass, input$mass)
  expect_equal(result$Cx, input$center_mass[["x"]])
  expect_equal(result$Cy, input$center_mass[["y"]])
  expect_equal(result$Cz, input$center_mass[["z"]])
  it <- input$inertia
  expect_equal(result$Ixx, it["x", "x"])
  expect_equal(result$Iyy, it["y", "y"])
  expect_equal(result$Izz, it["z", "z"])
  expect_equal(result$Ixy, it["x", "y"])
  expect_equal(result$Ixz, it["x", "z"])
  expect_equal(result$Iyz, it["y", "z"])
  expect_equal(result$POIconv, input$poi_conv)
  expect_equal(result$Ipoint, input$point)
})

test_that("df_set_mass_prop_and_unc() works for positive convention", {
  input <- mp_set$"+"
  input$poi_conv = "+"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  result <- df_set_mass_props_and_unc(df, "C.1.2.2.3.1.2.3", input)[1, ]

  expect_equal(result$mass, input$mass)
  expect_equal(result$Cx, input$center_mass[["x"]])
  expect_equal(result$Cy, input$center_mass[["y"]])
  expect_equal(result$Cz, input$center_mass[["z"]])
  it <- input$inertia
  expect_equal(result$Ixx, it["x", "x"])
  expect_equal(result$Iyy, it["y", "y"])
  expect_equal(result$Izz, it["z", "z"])
  expect_equal(result$Ixy, -it["x", "y"])
  expect_equal(result$Ixz, -it["x", "z"])
  expect_equal(result$Iyz, -it["y", "z"])
  expect_equal(result$POIconv, input$poi_conv)
  expect_equal(result$Ipoint, input$point)

  expect_equal(result$σ_mass, input$σ_mass)
  expect_equal(result$σ_Cx, input$σ_center_mass[["x"]])
  expect_equal(result$σ_Cy, input$σ_center_mass[["y"]])
  expect_equal(result$σ_Cz, input$σ_center_mass[["z"]])
  σ_it <- input$σ_inertia
  expect_equal(result$σ_Ixx, σ_it["x", "x"])
  expect_equal(result$σ_Iyy, σ_it["y", "y"])
  expect_equal(result$σ_Izz, σ_it["z", "z"])
  expect_equal(result$σ_Ixy, σ_it["x", "y"])
  expect_equal(result$σ_Ixz, σ_it["x", "z"])
  expect_equal(result$σ_Iyz, σ_it["y", "z"])

})

test_that("df_set_mass_props_and_unc() works for negative convention", {
  input <- mp_set$"-"
  input$poi_conv = "-"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  result <- df_set_mass_props_and_unc(df, "C.1.2.2.3.2.1.1", input)[2, ]

  expect_equal(result$mass, input$mass)
  expect_equal(result$Cx, input$center_mass[["x"]])
  expect_equal(result$Cy, input$center_mass[["y"]])
  expect_equal(result$Cz, input$center_mass[["z"]])
  it <- input$inertia
  expect_equal(result$Ixx, it["x", "x"])
  expect_equal(result$Iyy, it["y", "y"])
  expect_equal(result$Izz, it["z", "z"])
  expect_equal(result$Ixy, it["x", "y"])
  expect_equal(result$Ixz, it["x", "z"])
  expect_equal(result$Iyz, it["y", "z"])
  expect_equal(result$POIconv, input$poi_conv)
  expect_equal(result$Ipoint, input$point)

  expect_equal(result$σ_mass, input$σ_mass)
  expect_equal(result$σ_Cx, input$σ_center_mass[["x"]])
  expect_equal(result$σ_Cy, input$σ_center_mass[["y"]])
  expect_equal(result$σ_Cz, input$σ_center_mass[["z"]])
  σ_it <- input$σ_inertia
  expect_equal(result$σ_Ixx, σ_it["x", "x"])
  expect_equal(result$σ_Iyy, σ_it["y", "y"])
  expect_equal(result$σ_Izz, σ_it["z", "z"])
  expect_equal(result$σ_Ixy, σ_it["x", "y"])
  expect_equal(result$σ_Ixz, σ_it["x", "z"])
  expect_equal(result$σ_Iyz, σ_it["y", "z"])

})

test_that("combine_mass_props() works for non-point masses", {
  leaves <- test_table[which(!is.na(test_table$mass)), "id"]
  vl <- Map(f = function(id) df_get_mass_props(test_table, id), leaves)
  result <- combine_mass_props(vl)

  expect_equal(result$mass, 21)
  expect_equal(result$center_mass, c(x = 0, y = 0, z = 0))
  expect_equal(result$inertia,
               matrix(data = c(144, -4.8, -24.8, -4.8, 144, -23.2, -24.8, -23.2, 139), nrow = 3, byrow = TRUE, dimnames = list(xyz, xyz))
               )
  expect_false(result$point)
})

test_that("combine_mass_props() works for point masses", {
  leaves <- test_table[which(!is.na(test_table$mass)), "id"]
  vl <- Map(f = function(v) { v$point = TRUE; v },
            Map(f = function(id) df_get_mass_props(test_table, id), leaves))
  result <- combine_mass_props(vl)

  expect_equal(result$mass, 21)
  expect_equal(result$center_mass, c(x = 0, y = 0, z = 0))
  expect_equal(result$inertia,
               matrix(data = c(32, 0, 0, 0, 32, 0, 0, 0, 32), nrow = 3, byrow = TRUE, dimnames = list(xyz, xyz))
  )

  expect_false(result$point)
})

test_that("combine_mass_props() works for point masses at the origin", {
  leaves <- test_table[which(!is.na(test_table$mass)), "id"]
  vl <- Map(f = function(v) { v$center_mass = c(x = 0, y = 0, z = 0); v },
            Map(f = function(v) { v$point = TRUE; v },
                Map(f = function(id) df_get_mass_props(test_table, id), leaves)))
  result <- combine_mass_props(vl)

  expect_equal(result$mass, 21)
  expect_equal(result$center_mass, c(x = 0, y = 0, z = 0))

  expect_true(result$point)
})

test_that("combine_mass_props_and_unc() works", {
  expected = sawe_table[3, ]
  tol <- .002 # published numbers are not precise
  leaves <- list("Widget", "2nd Part")
  vl <- Map(f = function(id) df_get_mass_props_and_unc(sawe_table, id), leaves)
  result <- combine_mass_props_and_unc(vl)

  expect_equal(result$mass, expected$mass, tolerance = tol)
  expect_equal(result$center_mass, c(x = expected$Cx, y = expected$Cy, z = expected$Cz), tolerance = tol)
  expect_equal(result$inertia,
               matrix(data = c( expected$Ixx, -expected$Ixy, -expected$Ixz,
                               -expected$Ixy,  expected$Iyy, -expected$Iyz,
                               -expected$Ixz, -expected$Iyz,  expected$Izz), nrow = 3, byrow = TRUE, dimnames = list(xyz, xyz)),
               tolerance = tol
  )

  expect_equal(result$σ_mass, expected$σ_mass, tolerance = tol)
  expect_equal(result$σ_center_mass, c(x = expected$σ_Cx, y = expected$σ_Cy, z = expected$σ_Cz), tolerance = tol)
  expect_equal(result$σ_inertia,
               matrix(data = c(expected$σ_Ixx, expected$σ_Ixy, expected$σ_Ixz,
                               expected$σ_Ixy, expected$σ_Iyy, expected$σ_Iyz,
                               expected$σ_Ixz, expected$σ_Iyz, expected$σ_Izz), nrow = 3, byrow = TRUE, dimnames = list(xyz, xyz)),
               tolerance = tol
  )
})
