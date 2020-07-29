library(vdiffr)
context("ggkeyboard")

test_that("Default looks as expected", {
  p <- ggkeyboard()
  expect_doppelganger("default", p)
})

test_that("Colours change as expected", {
  colours <- ggkeyboard(
    keyboard_colour = "#51504A", modifier_colour = "#ffce00", accent_colour = "#454A49",
    alphanum_colour = "#EDEDD8", arrow_colour = "#454A49", font_family = "Courier",
    background_colour = "lightgrey", light_colour = "#8aff2b"
  )
  expect_doppelganger("colours", colours)
})

test_that("iso layout works", {
  iso <- ggkeyboard(tkl_iso, layout = "iso")
  expect_doppelganger("iso", iso)
})
