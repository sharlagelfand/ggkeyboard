#' Highlight keys
#'
#' Draw attention to keys in a \code{ggkeyboard}.
#'
#' @param ggkeyboard An input keyboard from \link{\code{ggkeyboard}}.
#' @param keys Keys to highlight. Key names are available from the \code{key} column of the data passed to \code{ggkeyboard}.
#' @param colour Highlight outline colour.
#' @param fill Highlight fill colour.
#' @param size Highlight outline signs.
#' @param ... Additional options, passed to \code{geom_rect}.
#'
#' @return
#' @export
#'
#' @examples
#' ggkeyboard(tkl) %>%
#'   highlight_keys(c("Alt Left", "Shift Left", "M"))
highlight_keys <- function(ggkeyboard, keys, colour = "yellow", fill = NA, size = 1, ...) {
  keyboard <- purrr::map(p$layers, "data") %>%
    purrr::keep(~ "key" %in% names(.x)) %>%
    dplyr::bind_rows()

  key_data <- keyboard %>%
    dplyr::filter(key %in% !!keys) %>%
    dplyr::distinct(key, x_start, x_end, y_start, y_end)

  ggkeyboard +
    ggplot2::geom_rect(
      data = key_data,
      ggplot2::aes(
        xmin = x_start, xmax = x_end,
        ymin = y_start, ymax = y_end
      ),
      colour = colour,
      fill = fill,
      size = size,
      ...
    )
}
