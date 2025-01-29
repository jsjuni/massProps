xyz <- list("x", "y", "z")
mp_set <- list(
  '+' = list(
    mass = 0.121334510659333,
    center_mass = c(x = 70.5484760459512, y = 81.5438871271908, z = 44.5226242300123),
    inertia = matrix(data = c( 0.143256682060419,  -0.0123144483865307,   0.00127739195911864,
                              -0.0123144483865307,  0.14586924090344,     0.000348565076916697,
                               0.00127739195911864, 0.000348565076916697, 0.130922827321144), nrow = 3, byrow = 3, dimnames = list(xyz, xyz)
                     ),
    sigma_mass = 0.289220546600936,
    sigma_center_mass = c(x = 28.8332142065316, y = 29.1637070837215, z = 29.5945914710325),
    sigma_inertia = matrix(data = c(0.372229441260767,  0.0219147131886038, 0.0214698151227243,
                                    0.0219147131886038, 0.36942277459594,   0.0224252422361279,
                                    0.0214698151227243, 0.0224252422361279, 0.374164784864648), nrow = 3, byrow = 3, dimnames = list(xyz, xyz)
                       ),
    point = FALSE
  ),
  '-' = list(
    mass = 0.341313654366182,
    center_mass = c(x = 34.2744647525251, y = -44.9820708483458, z = 5.21137723699212),
    inertia = matrix(data = c( 0.498689387183853,     -0.0443698514655305,   -4.09633716939692e-05,
                              -0.0443698514655305,     0.452811377532981,    -4.51044268177994e-06,
                              -4.09633716939692e-05,  -4.51044268177994e-06,  0.458318601936452), nrow = 3, byrow = 3, dimnames = list(xyz, xyz)
                     ),
    sigma_mass = 0.289220546600936,
    sigma_center_mass = c(x = 28.8332142065316, y = 29.1637070837215, z = 29.5945914710325),
    sigma_inertia = matrix(data = c(0.372229441260767,  0.0219147131886038, 0.0214698151227243,
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
  expect_equal(result$sigma_mass, mp_set$"+"$sigma_mass)
  expect_equal(result$sigma_center_mass, mp_set$"+"$sigma_center_mass)
  expect_equal(result$sigma_inertia, mp_set$"+"$sigma_inertia)
})

test_that("get_mass_props_and_unc() works for negative convention", {
  result <- get_mass_props_and_unc(mp_table, "C.1.2.2.3.2.1.1")

  expect_equal(result$mass, mp_set$"-"$mass)
  expect_equal(result$center_mass, mp_set$"-"$center_mass)
  expect_equal(result$inertia, mp_set$"-"$inertia)
  expect_equal(result$point, mp_set$"+"$point)
  expect_equal(result$sigma_mass, mp_set$"-"$sigma_mass)
  expect_equal(result$sigma_center_mass, mp_set$"-"$sigma_center_mass)
  expect_equal(result$sigma_inertia, mp_set$"-"$sigma_inertia)
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

  expect_equal(result$"sigma_mass", input$sigma_mass)
  expect_equal(result$"sigma_Cx", input$sigma_center_mass[["x"]])
  expect_equal(result$"sigma_Cy", input$sigma_center_mass[["y"]])
  expect_equal(result$"sigma_Cz", input$sigma_center_mass[["z"]])
  sigma_it <- input$sigma_inertia
  expect_equal(result$"sigma_Ixx", sigma_it["x", "x"])
  expect_equal(result$"sigma_Iyy", sigma_it["y", "y"])
  expect_equal(result$"sigma_Izz", sigma_it["z", "z"])
  expect_equal(result$"sigma_Ixy", sigma_it["x", "y"])
  expect_equal(result$"sigma_Ixz", sigma_it["x", "z"])
  expect_equal(result$"sigma_Iyz", sigma_it["y", "z"])

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

  expect_equal(result$"sigma_mass", input$sigma_mass)
  expect_equal(result$"sigma_Cx", input$sigma_center_mass[["x"]])
  expect_equal(result$"sigma_Cy", input$sigma_center_mass[["y"]])
  expect_equal(result$"sigma_Cz", input$sigma_center_mass[["z"]])
  sigma_it <- input$sigma_inertia
  expect_equal(result$"sigma_Ixx", sigma_it["x", "x"])
  expect_equal(result$"sigma_Iyy", sigma_it["y", "y"])
  expect_equal(result$"sigma_Izz", sigma_it["z", "z"])
  expect_equal(result$"sigma_Ixy", sigma_it["x", "y"])
  expect_equal(result$"sigma_Ixz", sigma_it["x", "z"])
  expect_equal(result$"sigma_Iyz", sigma_it["y", "z"])

})

test_that("set_mass_props() properly diagnoses NA sign convention indicator", {
  input <- mp_set$"+"
  input$poi_conv = NA
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  expect_error(set_mass_props(df, "C.1.2.2.3.1.2.3", input), "invalid sign convention")
})

test_that("set_mass_props() properly diagnoses NULL sign convention indicator", {
  input <- mp_set$"+"
  input$poi_conv = NULL
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  expect_error(set_mass_props(df, "C.1.2.2.3.1.2.3", input), "invalid sign convention")
})

test_that("set_mass_props() properly diagnoses numeric sign convention indicator", {
  input <- mp_set$"+"
  input$poi_conv = 1.0
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  expect_error(set_mass_props(df, "C.1.2.2.3.1.2.3", input), "invalid sign convention")
})

test_that("set_mass_props() properly diagnoses invalid sign convention indicator", {
  input <- mp_set$"+"
  input$poi_conv = "plus"
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  expect_error(set_mass_props(df, "C.1.2.2.3.1.2.3", input), "invalid sign convention")
})

test_that("set_mass_props() properly diagnoses logical sign convention indicator", {
  input <- mp_set$"+"
  input$poi_conv = TRUE
  df <- data.frame(id = c("C.1.2.2.3.1.2.3", "C.1.2.2.3.2.1.1"))
  expect_error(set_mass_props(df, "C.1.2.2.3.1.2.3", input), "invalid sign convention")
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

test_that("combine_mass_props_unc() works for point masses", {
  leaves <- names(igraph::neighbors(sawe_tree, "Combined", mode = "in"))

  # non point masses

  np_vl <- Map(f = function(id) get_mass_props_and_unc(sawe_table, id), leaves)
  np_sisq <- Reduce(`+`, Map(f = function(x) x$sigma_inertia^2, np_vl))
  np_sigma_inertia <- combine_mass_props_unc(np_vl, amp = get_mass_props_and_unc(sawe_table, "Combined"))$sigma_inertia

  # point masses

  pm_vl <- Map(f = function(v) { v$point = TRUE; v },
            Map(f = function(id) get_mass_props_and_unc(sawe_table, id), leaves))
  pm_sigma_inertia <- combine_mass_props_unc(pm_vl, amp = get_mass_props_and_unc(sawe_table, "Combined"))$sigma_inertia

  expect_equal(np_sigma_inertia, sqrt(pm_sigma_inertia^2 + np_sisq))
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

  expect_equal(result$sigma_mass, expected$"sigma_mass", tolerance = tol)
  expect_equal(result$sigma_center_mass, c(x = expected$"sigma_Cx", y = expected$"sigma_Cy", z = expected$"sigma_Cz"), tolerance = tol)
  expect_equal(result$sigma_inertia,
               matrix(data = c(expected$"sigma_Ixx", expected$"sigma_Ixy", expected$"sigma_Ixz",
                               expected$"sigma_Ixy", expected$"sigma_Iyy", expected$"sigma_Iyz",
                               expected$"sigma_Ixz", expected$"sigma_Iyz", expected$"sigma_Izz"), nrow = 3, byrow = TRUE, dimnames = list(xyz, xyz)),
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

test_that("update_mass_props_unc() works", {
  leaves <- list("Widget", "2nd Part")
  # Overwrite with wrong answers first
  df <-  update_mass_props_unc(sawe_table, "Combined", leaves[1]) |>
    update_mass_props_unc("Combined", leaves)
  combined <- which(df$id == "Combined")

  expect_true(isTRUE(all.equal(sawe_table[combined, ], df[combined, ], tolerance = .003)))
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

  invalid <- valid
  invalid$mass <- NULL
  expect_error(validate_mass_props(invalid), "mass missing")

  invalid <- valid
  invalid$mass <- NA
  expect_error(validate_mass_props(invalid), "mass missing")

  invalid <- valid
  invalid$mass <- "bad"
  expect_error(validate_mass_props(invalid), "mass non-numeric")

  invalid <- valid
  invalid$mass <- -1
  expect_error(validate_mass_props(invalid), "mass non-positive")

  invalid <- valid
  invalid$center_mass <- NULL
  expect_error(validate_mass_props(invalid), "center of mass missing")

  invalid <- valid
  invalid$center_mass <- NA
  expect_error(validate_mass_props(invalid), "center of mass not a 3-vector")

  invalid <- valid
  invalid$center_mass <- c(1, 2)
  expect_error(validate_mass_props(invalid), "center of mass not a 3-vector")

  invalid <- valid
  invalid$center_mass <- c(1, NA, 2)
  expect_error(validate_mass_props(invalid), "center of mass element missing")

  invalid <- valid
  invalid$center_mass <- c(1, "bad", 2)
  expect_error(validate_mass_props(invalid), "center of mass element non-numeric")

  invalid <- valid
  invalid$point <- NULL
  expect_error(validate_mass_props(invalid), "point mass indicator missing")

  invalid <- valid
  invalid$point <- NA
  expect_error(validate_mass_props(invalid), "point mass indicator non-logical")

  invalid <- valid
  invalid$point <- "bad"
  expect_error(validate_mass_props(invalid), "point mass indicator non-logical")

  invalid <- valid
  invalid$inertia <- NULL
  expect_error(validate_mass_props(invalid), "inertia tensor missing")

  invalid <- valid
  invalid$inertia <- diag(2)
  expect_error(validate_mass_props(invalid), "inertia tensor not a 3x3 matrix")

  invalid <- valid
  invalid$inertia <- diag(c(1, NA, 1))
  expect_error(validate_mass_props(invalid), "inertia tensor element missing")

  invalid <- valid
  invalid$inertia <- matrix(c(1, "bad", 1, "bad", 1, 1, 1, 1, 1), nrow = 3)
  expect_error(validate_mass_props(invalid), "inertia tensor element non-numeric")

  invalid <- valid
  invalid$inertia <- diag(c(1, -1, 1))
  expect_error(validate_mass_props(invalid), "inertia tensor not positive definite")

  invalid <- valid
  invalid$inertia <- diag(c(1, 3, 1))
  expect_error(validate_mass_props(invalid), "inertia tensor violates triangle inequalities")
})

test_that("validate_mass_props_and_unc() works", {
  valid <- get_mass_props_and_unc(sawe_table, "Widget")

  expect_true(validate_mass_props_and_unc(valid))

  invalid <- valid
  invalid$sigma_mass <- NULL
  expect_error(validate_mass_props_and_unc(invalid), "mass uncertainty missing")

  invalid <- valid
  invalid$sigma_mass <- NA
  expect_error(validate_mass_props_and_unc(invalid), "mass uncertainty missing")

  invalid <- valid
  invalid$sigma_mass <- "bad"
  expect_error(validate_mass_props_and_unc(invalid), "mass uncertainty non-numeric")

  invalid <- valid
  invalid$sigma_mass <- -1
  expect_error(validate_mass_props_and_unc(invalid), "mass uncertainty negative")

  invalid <- valid
  invalid$sigma_center_mass <- NULL
  expect_error(validate_mass_props_and_unc(invalid), "center of mass uncertainty missing")

  invalid <- valid
  invalid$sigma_center_mass <- NA
  expect_error(validate_mass_props_and_unc(invalid), "center of mass uncertainty not a 3-vector")

  invalid <- valid
  invalid$sigma_center_mass <- c(1, 2)
  expect_error(validate_mass_props_and_unc(invalid), "center of mass uncertainty not a 3-vector")

  invalid <- valid
  invalid$sigma_center_mass <- c(1, NA, 2)
  expect_error(validate_mass_props_and_unc(invalid), "center of mass uncertainty element missing")

  invalid <- valid
  invalid$sigma_center_mass <- c(1, "bad", 2)
  expect_error(validate_mass_props_and_unc(invalid), "center of mass uncertainty element non-numeric")

  invalid <- valid
  invalid$sigma_center_mass <- c(1, -1, 2)
  expect_error(validate_mass_props_and_unc(invalid), "center of mass uncertainty element negative")

  invalid <- valid
  invalid$sigma_inertia <- NULL
  expect_error(validate_mass_props_and_unc(invalid), "inertia tensor uncertainty missing")

  invalid <- valid
  invalid$sigma_inertia <- diag(2)
  expect_error(validate_mass_props_and_unc(invalid), "inertia tensor uncertainty not a 3x3 matrix")

  invalid <- valid
  invalid$sigma_inertia <- diag(c(1, NA, 1))
  expect_error(validate_mass_props_and_unc(invalid), "inertia tensor uncertainty element missing")

  invalid <- valid
  invalid$sigma_inertia <- matrix(c(1, "bad", 1, "bad", 1, 1, 1, 1, 1), nrow = 3)
  expect_error(validate_mass_props_and_unc(invalid), "inertia tensor uncertainty element non-numeric")

  invalid <- valid
  invalid$sigma_inertia <-diag(c(1, -1, 1))
  expect_error(validate_mass_props_and_unc(invalid), "inertia tensor uncertainty element negative")
})

test_that("validate_mass_props_table() works", {
  expect_true(validate_mass_props_table(mp_tree, mp_table))

  bad_table <- df_set_by_id(mp_table, "C.1.2.3.5.3.5.3", "mass", -5)
  expect_error(validate_mass_props_table(mp_tree, bad_table), "mass non-positive")
})

test_that("validate_mass_props_and_unc_table() works", {
  expect_true(validate_mass_props_and_unc_table(sawe_tree, sawe_table))

  invalid <- df_set_by_id(sawe_table, "Widget", "sigma_mass", -5)
  expect_error(validate_mass_props_and_unc_table(sawe_tree, invalid), "mass uncertainty negative")
})

test_that("rollup_mass_props() works", {
  df <- rollup_mass_props(test_tree, test_table |> df_set_by_id("A.1", "mass", NA))
  result <- df[which(df$id == "A.1"), ]

  expect_equal(result$mass, 21)

  invalid_table <- test_table
  invalid_table$mass <- NA
  expect_error(rollup_mass_props(test_tree, invalid_table), "mass missing")
})

test_that("rollup_mass_props_unc() works", {
  df <- rollup_mass_props_unc(sawe_tree, sawe_table |> df_set_by_id("Combined", "sigma_mass", NA))
  result <- df[which(df$id == "Combined"), ]

  expect_equal(result$mass, 74.63, tolerance = .002)
  expect_equal(result$"sigma_mass", 2.1301, tolerance = .002)

  invalid_table <- sawe_table
  invalid_table$"sigma_mass" <- NA
  expect_error(rollup_mass_props_unc(sawe_tree, invalid_table), "mass uncertainty missing")
})

test_that("rollup_mass_props_and_unc() works", {
  df <-  rollup_mass_props_and_unc(sawe_tree, sawe_table |> df_set_by_id("Combined", "mass", NA) |>
    df_set_by_id("Combined", "sigma_mass", NA))
  result <- df[which(df$id == "Combined"), ]

  expect_equal(result$mass, 74.63, tolerance = .002)
  expect_equal(result$"sigma_mass", 2.1301, tolerance = .002)

  invalid_table <- sawe_table
  invalid_table$"sigma_mass" <- NA
  expect_error(rollup_mass_props_and_unc(sawe_tree, invalid_table), "mass uncertainty missing")
})

test_that("rollup_mass_props_fast() works", {
  df <- rollup_mass_props_fast(test_tree, test_table |> df_set_by_id("A.1", "mass", NA))
  result <- df[which(df$id == "A.1"), ]

  expect_equal(result$mass, 21)

  invalid_table <- test_table
  invalid_table$mass <- NA
  expect_error(rollup_mass_props(test_tree, invalid_table), "mass missing")
})

test_that("rollup_mass_props_unc_fast() works", {
  df <- rollup_mass_props_unc_fast(sawe_tree, sawe_table |> df_set_by_id("Combined", "sigma_mass", NA))
  result <- df[which(df$id == "Combined"), ]

  expect_equal(result$mass, 74.63, tolerance = .002)
  expect_equal(result$"sigma_mass", 2.1301, tolerance = .002)

  invalid_table <- sawe_table
  invalid_table$"sigma_mass" <- NA
  expect_error(rollup_mass_props_unc(sawe_tree, invalid_table), "mass uncertainty missing")
})

test_that("rollup_mass_props_and_unc_fast() works", {
  df <-  rollup_mass_props_and_unc_fast(sawe_tree, sawe_table |> df_set_by_id("Combined", "mass", NA) |>
                                     df_set_by_id("Combined", "sigma_mass", NA))
  result <- df[which(df$id == "Combined"), ]

  expect_equal(result$mass, 74.63, tolerance = .002)
  expect_equal(result$"sigma_mass", 2.1301, tolerance = .002)

  invalid_table <- sawe_table
  invalid_table$"sigma_mass" <- NA
  expect_error(rollup_mass_props_and_unc(sawe_tree, invalid_table), "mass uncertainty missing")
})

