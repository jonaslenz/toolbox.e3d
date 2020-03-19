library(testthat)
library(liberos)

context("test parameter determination")

test_that("skinfactor determination works properly", {
  expect_equal(determine.skin.runoff.E3D(Cl = 30, Si = 40, Sa = 30, Corg = 1.3, Bulk = 1300, Moist = 22, CumRunoff = 100, intensity = 0.5, plotwidth = 1, plotlength = 10, slope = 10, endmin = 30, ponding = TRUE), 0.0002283277)
  expect_equal(determine.skin.runoff.E3D(Cl = 30, Si = 40, Sa = 30, Corg = 1.3, Bulk = 1300, Moist = 22, CumRunoff = 100, intensity = 0.5, plotwidth = 1, plotlength = 10, slope = 10, endmin = 30), 0.0002488979)
})

# test_that("resistance to erosion determination works properly", {
#   expect_equal
# })
