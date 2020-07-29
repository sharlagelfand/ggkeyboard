library(vdiffr)
context("ggkeyboard")

test_that("Default looks as expected", {
  p <- ggkeyboard()
  expect_doppelganger("default", p)
})

test_that("Colours change as expected", {
  colours <- ggkeyboard(palette = keyboard_palette("serika"))
  expect_doppelganger("colours", colours)
})

test_that("iso layout works", {
  iso <- ggkeyboard(tkl_iso, layout = "iso")
  expect_doppelganger("iso", iso)
})
