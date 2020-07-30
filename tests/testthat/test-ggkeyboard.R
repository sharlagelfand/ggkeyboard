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
  iso <- ggkeyboard(tkl, layout = "iso")
  expect_doppelganger("iso", iso)
})

test_that("full and 60% layouts work, ansi and iso", {
  p <- ggkeyboard(full)
  expect_doppelganger("full", p)

  p <- ggkeyboard(full, layout = "iso")
  expect_doppelganger("full-iso", p)

  p <- ggkeyboard(sixty_percent)
  expect_doppelganger("sixty-percent", p)

  p <- ggkeyboard(sixty_percent, layout = "iso")
  expect_doppelganger("sixty-percent-iso", p)
})
