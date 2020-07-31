#' Keyboard Measurements
#'
#' Measurement options for \code{\link{ggkeyboard}}.
#'
#' There are the following options:
#' * key_height: Height of keys.
#' * key_width: Base width of keys.
#' * height_gap: Height gap between rows of keys.
#' * width_gap: Width gap between keys in the same row.
#' * segment_size: Size of segments used to draw arrows.
#' * arrow_size; Size of arrow head.
#'
#' @param name Measurement options name.
#'
#' @export
keyboard_measurements <- function(name = "default") {
  name <- match.arg(name)

  switch(name,
    default = c(
      key_height = 15 / 15.5,
      key_width = 1,
      height_gap = 2 / 15.5,
      width_gap = 2 / 15.5,
      segment_size = 0.25,
      arrow_size = 0.03
    )
  )
}
