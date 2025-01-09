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

test_that("get_mass_props() works for positive convention", {
  result <- get_mass_props(mp_table, "C.1.2.2.3.1.2.3")

  expect_equal(result$mass, mp_set$"+"$mass)
  expect_equal(result$center_mass, mp_set$"+"$center_mass)
  expect_equal(result$inertia, mp_set$"+"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
})

test_that("get_mass_props() works for negative convention", {
  result <- get_mass_props(mp_table, "C.1.2.2.3.2.1.1")

  expect_equal(result$mass, mp_set$"-"$mass)
  expect_equal(result$center_mass, mp_set$"-"$center_mass)
  expect_equal(result$inertia, mp_set$"-"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
})

test_that("get_mass_props_and_unc() works for positive convention", {
  result <- get_mass_props_and_unc(mp_table, "C.1.2.2.3.1.2.3")

  expect_equal(result$mass, mp_set$"+"$mass)
  expect_equal(result$center_mass, mp_set$"+"$center_mass)
  expect_equal(result$inertia, mp_set$"+"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
  expect_equal(result$σ_mass, mp_set$"+"$σ_mass)
  expect_equal(result$σ_center_mass, mp_set$"+"$σ_center_mass)
  expect_equal(result$σ_inertia, mp_set$"+"$σ_inertia)
})

test_that("get_mass_props_and_unc() works for negative convention", {
  result <- get_mass_props_and_unc(mp_table, "C.1.2.2.3.2.1.1")

  expect_equal(result$mass, mp_set$"-"$mass)
  expect_equal(result$center_mass, mp_set$"-"$center_mass)
  expect_equal(result$inertia, mp_set$"-"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
  expect_equal(result$σ_mass, mp_set$"-"$σ_mass)
  expect_equal(result$σ_center_mass, mp_set$"-"$σ_center_mass)
  expect_equal(result$σ_inertia, mp_set$"-"$σ_inertia)
})

test_that("set_mass_props() works for positive convention", {
  input <- mp_set$"+"
  input$poi_conv = "+"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  result <- set_mass_props(df, "C.1.2.2.3.1.2.3", input)[1, ]

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

test_that("set_mass_props() works for negative convention", {
  input <- mp_set$"-"
  input$poi_conv = "-"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  result <- set_mass_props(df, "C.1.2.2.3.2.1.1", input)[2, ]

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

test_that("set_mass_prop_and_unc() works for positive convention", {
  input <- mp_set$"+"
  input$poi_conv = "+"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  result <- set_mass_props_and_unc(df, "C.1.2.2.3.1.2.3", input)[1, ]

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

test_that("set_mass_props_and_unc() works for negative convention", {
  input <- mp_set$"-"
  input$poi_conv = "-"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  result <- set_mass_props_and_unc(df, "C.1.2.2.3.2.1.1", input)[2, ]

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
  vl <- Map(f = function(id) get_mass_props(test_table, id), leaves)
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
            Map(f = function(id) get_mass_props(test_table, id), leaves))
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
                Map(f = function(id) get_mass_props(test_table, id), leaves)))
  result <- combine_mass_props(vl)

  expect_equal(result$mass, 21)
  expect_equal(result$center_mass, c(x = 0, y = 0, z = 0))

  expect_true(result$point)
})

test_that("combine_mass_props_and_unc() works", {
  expected = sawe_table[3, ]
  tol <- .002 # published numbers are not precise
  leaves <- list("Widget", "2nd Part")
  vl <- Map(f = function(id) get_mass_props_and_unc(sawe_table, id), leaves)
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

test_that("set_poi_conv_plus() works", {
  result <- set_poi_conv_plus(NULL, NULL, mp_set$"-") # either works
  expect_equal(result$poi_conv, "+")
})

test_that("set_poi_conv_minus() works", {
  result <- set_poi_conv_minus(NULL, NULL, mp_set$"+") # either works
  expect_equal(result$poi_conv, "-")
})

test_that("set_poi_from_target() works", {
  result_plus <- set_poi_conv_from_target(mp_table, "C.1.2.2.3.1.2.3", mp_set$"-") # either works
  result_minus <- set_poi_conv_from_target(mp_table, "C.1.2.2.3.2.1.1", mp_set$"+") # either works
  expect_equal(result_plus$poi_conv, "+")
  expect_equal(result_minus$poi_conv, "-")
})

test_that("update_mass_props() works", {
  leaves <- test_table[which(!is.na(test_table$mass)), "id"]
  df <- update_mass_props(test_table, "A.1", leaves)
  result <- df[which(df$id == "A.1"), ]

  expect_equal(result$mass, 21)

  expect_equal(result$Cx, 0)
  expect_equal(result$Cy, 0)
  expect_equal(result$Cz, 0)

  expect_equal(result$Ixx, 144)
  expect_equal(result$Iyy, 144)
  expect_equal(result$Izz, 139)

  expect_equal(result$Ixy, -4.8)
  expect_equal(result$Ixz, -24.8)
  expect_equal(result$Iyz, -23.2)

  expect_equal(result$POIconv, "-")
  expect_false(result$Ipoint)
})

test_that("update_mass_props_and_unc() works", {
  leaves <- list("Widget", "2nd Part")
  # Overwrite with wrong answers first
  df <-  update_mass_props_and_unc(sawe_table, "Combined", leaves[1]) |>
    update_mass_props_and_unc("Combined", leaves)
  combined <- which(df$id == "Combined")

  expect_true(isTRUE(all.equal(sawe_table[combined, ], df[combined, ], tolerance = .002)))
})

test_that("validate_mass_props() works", {
  valid <- mp_set$"+"

  expect_true(validate_mass_props(valid))

  null_mass <- valid
  null_mass$mass <- NULL
  expect_error(validate_mass_props(null_mass), "mass missing")

  na_mass <- valid
  na_mass$mass <- NA
  expect_error(validate_mass_props(na_mass), "mass missing")

  nn_mass <- valid
  nn_mass$mass <- "bad"
  expect_error(validate_mass_props(nn_mass), "mass non-numeric")

  np_mass <- valid
  np_mass$mass <- -1
  expect_error(validate_mass_props(np_mass), "mass non-positive")

  null_cm <- valid
  null_cm$center_mass <- NULL
  expect_error(validate_mass_props(null_cm), "center of mass missing")

  na_cm <- valid
  na_cm$center_mass <- NA
  expect_error(validate_mass_props(na_cm), "center of mass not a 3-vector")

  d2_cm <- valid
  d2_cm$center_mass <- c(1, 2)
  expect_error(validate_mass_props(d2_cm), "center of mass not a 3-vector")

  me_cm <- valid
  me_cm$center_mass <- c(1, NA, 2)
  expect_error(validate_mass_props(me_cm), "center of mass element missing")

  nn_cm <- valid
  nn_cm$center_mass <- c(1, "bad", 2)
  expect_error(validate_mass_props(nn_cm), "center of mass element non-numeric")

  null_point <- valid
  null_point$point <- NULL
  expect_error(validate_mass_props(null_point), "point mass indicator missing")

  na_point <- valid
  na_point$point <- NA
  expect_error(validate_mass_props(na_point), "point mass indicator non-logical")

  nl_point <- valid
  nl_point$point <- "bad"
  expect_error(validate_mass_props(nl_point), "point mass indicator non-logical")

  null_it <- valid
  null_it$inertia <- NULL
  expect_error(validate_mass_props(null_it), "inertia tensor missing")

  bd_it <- valid
  bd_it$inertia <- diag(2)
  expect_error(validate_mass_props(bd_it), "inertia tensor not a 3x3 matrix")

  mi_it <- valid
  mi_it$inertia <- diag(c(1, NA, 1))
  expect_error(validate_mass_props(mi_it), "inertia tensor element missing")

  bd_it <- valid
  bd_it$inertia <- matrix(c(1, "bad", 1, "bad", 1, 1, 1, 1, 1), nrow = 3)
  expect_error(validate_mass_props(bd_it), "inertia tensor element non-numeric")

  id_it <- valid
  id_it$inertia <- diag(c(1, -1, 1))
  expect_error(validate_mass_props(id_it), "inertia tensor not positive definite")

  tv_it <- valid
  tv_it$inertia <- diag(c(1, 3, 1))
  expect_error(validate_mass_props(tv_it), "inertia tensor violates triangle inequalities")
})
