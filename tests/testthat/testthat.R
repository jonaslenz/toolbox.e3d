library(testthat)
library(liberos)

context("test parameter determination")

test_that("skinfactor determination works properly", {
  expect_equal(determine.skin.runoff.E3D(Cl = 30, Si = 40, Sa = 30, Corg = 1.3, Bulk = 1300, Moist = 22, CumRunoff = 100, intensity = 0.5, plotwidth = 1, plotlength = 10, slope = 10, endmin = 30, ponding = TRUE), 0.0002283277)
  expect_equal(determine.skin.runoff.E3D(Cl = 30, Si = 40, Sa = 30, Corg = 1.3, Bulk = 1300, Moist = 22, CumRunoff = 100, intensity = 0.5, plotwidth = 1, plotlength = 10, slope = 10, endmin = 30), 0.0002488979)
})

test_that("resistance to erosion determination works properly", {
  expect_equal(determine.eros.cumsed.E3D(FCl=5,MCl=10,CCl=15, FSi=10,MSi=20,CSi=10, FSa=15,MSa=10,CSa=5, Corg = 1.3, Bulk = 1300, Moist = 22, Skin = 0.005,Roughness=0.05, Cover = 20, Soilloss = 1, intensity = 0.5, plotwidth = 1, plotlength = 10, slope = 10, endmin = 30, ponding = FALSE), 1.267132e-05)
})
