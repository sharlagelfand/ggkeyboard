#' Plot a keyboard using ggplot2
#'
#' @param data Keyboard data. A data frame with the key name, what row of the keyboard it is in, and key width. Defaults to \code{tkl}.
#' @param key_height Height of keys. Defaults to 15 / 15.5.
#' @param key_width Base width of keys. Defaults to 1.
#' @param height_gap Height gap between rows of keys. Defaults to 2 / 15.5.
#' @param width_gap Width gap between keys in the same row. Defaults to 2 / 15.5.
#' @param font_size Base font size. Defaults to 3.
#' @param segment_size Size of segments used to draw arrows. Defaults to 0.25.
#' @param arrow_size Size of arrow head. Defaults to 0.03 (npc).
#' @param font_family Font used. Defaults to "Avenir Next". See the \code{extrafont} package for using fonts.
#' @param palette Colour palette. Defaults to \code{keyboard_palette("pastel")}. To use a custom palette, create a vector with the names described in \code{\link{keyboard_palette}}\code{()}.
#' @param adjust_text_colour Whether to lighten the text colour on dark keys. Defaults to TRUE.
#' @param layout Keyboard layout - one of "ansi" or "iso". Defaults to "ansi".
#'
#' @importFrom dplyr mutate group_by bind_rows tibble case_when ungroup rowwise row_number filter
#' @importFrom stringr str_detect
#' @importFrom prismatic clr_lighten clr_darken
#' @importFrom ggplot2 ggplot aes theme_void theme element_rect margin geom_text scale_fill_identity scale_colour_identity scale_size_identity geom_segment arrow unit coord_equal geom_point geom_rect geom_segment annotate
#' @importFrom ggforce geom_ellipse
#' @importFrom grDevices col2rgb
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggkeyboard()
#'
#' ggkeyboard(tkl,
#'   keyboard_colour = "#51504A", modifier_colour = "#ffce00",
#'   accent_colour = "#454A49", alphanum_colour = "#EDEDD8", arrow_colour = "#454A49",
#'   font_family = "Courier", background_colour = "lightgrey", light_colour = "#8aff2b"
#' )
#' }
ggkeyboard <- function(data = tkl, key_height = 15 / 15.5, key_width = 1, height_gap = 2 / 15.5, width_gap = 2 / 15.5, font_size = 3, segment_size = 0.25, arrow_size = 0.03, font_family = "Avenir Next", palette = keyboard_palette("pastel"), adjust_text_colour = TRUE, layout = c("ansi", "iso")) {

  layout <- match.arg(layout)

  keyboard <- construct_keyboard(data = data, key_height = key_height, key_width = key_width, height_gap = height_gap, width_gap = width_gap, font_size = font_size, palette = palette, adjust_text_colour = adjust_text_colour, layout = layout)

  keyboard_full <- construct_keyboard_outline(keyboard, keyboard_colour = palette[["keyboard"]])

  construct_plot(keyboard, keyboard_full, key_height = key_height, key_width = key_width, height_gap = height_gap, font_size = font_size, segment_size = segment_size, arrow_size = arrow_size, font_family = font_family, palette = palette, adjust_text_colour = adjust_text_colour, layout = layout)
}

construct_keyboard <- function(data = tkl, key_height = 15 / 15.5, key_width = 1, height_gap = 2 / 15.5, width_gap = 2 / 15.5, font_size = 3, palette = keyboard_palette("pastel"), adjust_text_colour = TRUE, layout = c("ansi", "iso")) {

  layout <- match.arg(layout)

  palette_df <- tibble::enframe(palette, name = "key_type", value = "fill") %>%
    tidyr::unnest(cols = c(fill))

  keyboard <- data %>%
    mutate(
      width = key_width * width,
      width = width + width_gap * (width - key_width)
    ) %>%
    group_by(row) %>%
    mutate(
      gap = ifelse(number == 1, 0, width_gap),
      x_start = cumsum(width) - width + cumsum(gap),
      x_mid = x_start + width / 2,
      x_end = x_start + width
    ) %>%
    ungroup() %>%
    mutate(
      y_start = (height_gap * ifelse(row == 6, 2, 1) + key_height) * (row - 1),
      y_mid = y_start + key_height / 2,
      y_end = y_start + key_height,
      size = font_size * case_when(
        str_detect(key, "^[:alnum:]$") ~ 1.75,
        TRUE ~ 1
      )
    ) %>%
    dplyr::left_join(palette_df, by = "key_type") %>%
    rowwise() %>%
    mutate(
      text_colour = ifelse(is_dark(fill) & adjust_text_colour, clr_lighten(palette[["text"]], 0.5), palette[["text"]])
    ) %>%
    ungroup()

  keyboard[["colour"]] <- unclass(clr_darken(keyboard[["fill"]], 0.1))

  keyboard %>%
    mutate(colour = ifelse(is.na(key), NA_character_, colour))
}

construct_keyboard_outline <- function(keyboard, keyboard_colour = keyboard_palette("pastel")[["keyboard"]]) {
  tibble(
    x_start = min(keyboard[["x_start"]]),
    x_end = max(keyboard[["x_end"]]),
    y_start = min(keyboard[["y_start"]]),
    y_end = max(keyboard[["y_end"]])
  ) %>%
    mutate(
      x_mid = (x_end - x_start) / 2,
      y_mid = (y_end - y_start) / 2,
      fill = keyboard_colour
    )
}

construct_plot <- function(keyboard, keyboard_full, key_height = 15 / 15.5, key_width = 1, height_gap = 2 / 15.5, font_size = 3, segment_size = 0.25, arrow_size = 0.03, font_family = "Avenir Next", palette = palette, adjust_text_colour = TRUE, layout = c("ansi", "iso")) {
  layout <- match.arg(layout)

  p <- ggplot() +
    geom_ellipse(data = keyboard_full, aes(x0 = x_mid, y0 = y_mid, a = x_mid * 1.05, b = y_mid * 1.1, angle = 0, m1 = 100, fill = fill, colour = clr_darken(fill, 0.10)), size = 1) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = palette[["background"]], colour = palette[["background"]]),
      plot.margin = margin(10, 10, 10, 10)
    )

  # Add rows ----

  for (i in unique(keyboard[["row"]])) {
    row_data <- keyboard %>%
      filter(row == i)

    p <- p +
      geom_ellipse(data = row_data, aes(x0 = x_mid, y0 = y_mid, a = width / 2, b = key_height / 2, angle = 0, m1 = 10, fill = fill, colour = colour), size = 1) +
      geom_text(data = row_data %>%
        filter(!is.na(key_label)), aes(x = x_start + width / 2, y = (y_start + y_end) / 2, label = key_label, size = size, colour = text_colour), family = font_family, lineheight = 0.9)
  }

  # Add arrows/images/etc ----

  arrows <- keyboard %>%
    filter(key %in% c("Up", "Down", "Left", "Right")) %>%
    split(.$key)

  backspace <- keyboard %>%
    filter(key == "Backspace")

  shift <- keyboard %>%
    filter(str_detect(key, "Shift"))

  circles <- keyboard %>%
    filter(key %in% c("Ins", "Home", "PgUp"))

  windows <- keyboard %>%
    filter(key == "Win")

  p <- p +
    # Backspace
    geom_segment(data = backspace, aes(x = (x_end + x_mid) / 2, xend = (x_start + x_mid) / 2, y = y_mid, yend = y_mid, colour = palette[["text"]]), arrow = arrow(length = unit(0.02, "npc")), size = segment_size) +
    # Arrows
    geom_segment(data = arrows[["Down"]], aes(x = x_mid, xend = x_mid, y = (y_end + y_mid) / 2, yend = (y_start + y_mid) / 2, colour = palette[["text"]]), arrow = arrow(length = unit(arrow_size, "npc")), size = segment_size) +
    geom_segment(data = arrows[["Up"]], aes(x = x_mid, xend = x_mid, yend = (y_end + y_mid) / 2, y = (y_start + y_mid) / 2, colour = palette[["text"]]), arrow = arrow(length = unit(arrow_size, "npc")), size = segment_size) +
    geom_segment(data = arrows[["Left"]], aes(x = (x_end + x_mid) / 2, xend = (x_start + x_mid) / 2, y = y_mid, yend = y_mid, colour = palette[["text"]]), arrow = arrow(length = unit(arrow_size, "npc")), size = segment_size) +
    geom_segment(data = arrows[["Right"]], aes(xend = (x_end + x_mid) / 2, x = (x_start + x_mid) / 2, y = y_mid, yend = y_mid, colour = palette[["text"]]), arrow = arrow(length = unit(arrow_size, "npc")), size = segment_size) +
    # Shift arrows
    geom_segment(data = shift, aes(x = x_mid - key_width * 0.1, xend = x_mid - key_width * 0.1, y = (y_start + y_mid) / 2, yend = y_mid, colour = palette[["text"]]), size = segment_size) +
    geom_segment(data = shift, aes(x = x_mid + key_width * 0.1, xend = x_mid + key_width * 0.1, y = (y_start + y_mid) / 2, yend = y_mid, colour = palette[["text"]]), size = segment_size) +
    geom_segment(data = shift, aes(x = x_mid - key_width * 0.25, xend = x_mid, y = y_mid, yend = (y_end + y_mid) / 2, colour = palette[["text"]]), size = segment_size) +
    geom_segment(data = shift, aes(x = x_mid, xend = x_mid + key_width * 0.25, yend = y_mid, y = (y_end + y_mid) / 2, colour = palette[["text"]]), size = segment_size) +
    geom_segment(data = shift, aes(x = x_mid + key_width * 0.1, xend = x_mid + key_width * 0.25, yend = y_mid, y = y_mid, colour = palette[["text"]]), size = segment_size) +
    geom_segment(data = shift, aes(x = x_mid - key_width * 0.25, xend = x_mid - key_width * 0.1, yend = y_mid, y = y_mid, colour = palette[["text"]]), size = segment_size) +
    # Circles
    geom_point(data = circles, aes(x = x_mid, y = y_end + height_gap * 3), size = 2.5, colour = palette[["light"]], alpha = 0.75) +
    # Windows keys
    geom_text(data = windows, aes(x = x_mid, y = y_mid, colour = text_colour), label = "\u{263A}", size = font_size * 2) +
    coord_equal() +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_size_identity()

  if (layout == "iso") {
    enter <- keyboard %>%
      filter(key == "Enter")

    enter <- tibble(
      xmin = enter[1, ][["x_start"]],
      xmax = enter[1, ][["x_end"]],
      ymin = enter[1, ][["y_end"]],
      ymax = enter[2, ][["y_start"]],
      colour = unique(enter[["colour"]]),
      fill = unique(enter[["fill"]]),
      text_colour = unique(enter[["text_colour"]])
    )

    p <- p +
      geom_rect(data = enter, aes(xmin = xmin, xmax = xmax, ymin = ymin * 0.95, ymax = ymax * 1.05, colour = fill, fill = fill), size = 1) +
      geom_segment(data = enter, aes(x = xmin, xend = xmin, y = ymin * 0.90, yend = ymax * 1.01, colour = colour), size = 1) +
      geom_segment(data = enter, aes(x = xmax, xend = xmax, y = ymin * 0.90, yend = ymax * 1.1, colour = colour), size = 1) +
      annotate("text", x = (enter[["xmin"]] + enter[["xmax"]]) / 2, y = (enter[["ymin"]] + enter[["ymax"]]) / 2, label = "Enter", family = font_family, colour = enter[["text_colour"]])
  }

  p
}

is_dark <- function(colour) {
  (sum(col2rgb(colour) * c(299, 587, 114)) / 1000 < 123)
}
