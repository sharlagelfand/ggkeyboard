#' Plot a keyboard using ggplot2
#'
#' @param keyboard Keyboard data. A data frame with the key name, what row of the keyboard it is in, and key width. Defaults to \code{\link{tkl}} (a tenkeyless layout). Other available keyboards are a full keyboard (\code{\link{full}}), 60% keyboard (\code{\link{sixty_percent}}), and a basic mac keyboard (\code{\link{mac}}).
#' @param key_height Height of keys. Defaults to 15 / 15.5.
#' @param key_width Base width of keys. Defaults to 1.
#' @param height_gap Height gap between rows of keys. Defaults to 2 / 15.5.
#' @param width_gap Width gap between keys in the same row. Defaults to 2 / 15.5.
#' @param font_size Base font size. Defaults to 3.
#' @param segment_size Size of segments used to draw arrows. Defaults to 0.25.
#' @param arrow_size Size of arrow head. Defaults to 0.03 (npc).
#' @param font_family Font used. Defaults to "Avenir Next". See the \code{extrafont} package for using fonts.
#' @param palette Colour palette. Defaults to \code{keyboard_palette("pastel")}. To use a custom palette, create a vector with the names described in \code{\link{keyboard_palette}}.
#' @param adjust_text_colour Whether to lighten the text colour on dark keys. Defaults to TRUE.
#' @param layout Keyboard layout - one of "ansi" or "iso". Defaults to "ansi".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggkeyboard()
#'
#' ggkeyboard(sixty_percent, palette = keyboard_palette("cyberpunk"))
#' }
ggkeyboard <- function(keyboard = tkl, key_height = 15 / 15.5, key_width = 1, height_gap = 2 / 15.5, width_gap = 2 / 15.5, font_size = 3, segment_size = 0.25, arrow_size = 0.03, font_family = "Avenir Next", palette = keyboard_palette("pastel"), adjust_text_colour = TRUE, layout = c("ansi", "iso")) {

  layout <- match.arg(layout)

  keyboard_layout <- dplyr::case_when(
    any(keyboard[["layout"]] == "full") ~ "full",
    any(keyboard[["layout"]] == "tkl") ~ "tkl",
    any(keyboard[["layout"]] == "mac") ~ "mac",
    all(keyboard[["layout"]] == "60%") ~ "60%"
  )


  if (layout == "iso") {
    keyboard <- convert_to_iso(keyboard, keyboard_layout)
  }

  keyboard <- construct_keyboard(keyboard = keyboard, key_height = key_height, key_width = key_width, height_gap = height_gap, width_gap = width_gap, font_size = font_size, palette = palette, adjust_text_colour = adjust_text_colour, layout = layout, keyboard_layout = keyboard_layout)

  keyboard_full <- construct_keyboard_outline(keyboard, keyboard_colour = palette[["keyboard"]])

  construct_plot(keyboard, keyboard_full, key_height = key_height, key_width = key_width, height_gap = height_gap, font_size = font_size, segment_size = segment_size, arrow_size = arrow_size, font_family = font_family, palette = palette, adjust_text_colour = adjust_text_colour, layout = layout, keyboard_layout = keyboard_layout)
}

construct_keyboard <- function(keyboard = tkl, key_height = 15 / 15.5, key_width = 1, height_gap = 2 / 15.5, width_gap = 2 / 15.5, font_size = 3, palette = keyboard_palette("pastel"), adjust_text_colour = TRUE, layout = c("ansi", "iso"), keyboard_layout = "tkl") {

  layout <- match.arg(layout)

  palette_df <- tibble::enframe(palette, name = "key_type", value = "fill")

  keyboard <- keyboard %>%
    dplyr::mutate(
      width = key_width * width,
      width = width + width_gap * (width - key_width),
      height = key_height * height,
      height = height + height_gap * (height - key_height)
    ) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(
      gap = ifelse(number == 1, 0, width_gap),
      x_start = cumsum(width) - width + cumsum(gap),
      x_mid = x_start + width / 2,
      x_end = x_start + width
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      y_start = (height_gap * ifelse(row == 6 & keyboard_layout != "mac", 2, 1) + key_height) * (row - 1),
      y_mid = y_start + height / 2,
      y_end = y_start + height,
      size = font_size * dplyr::case_when(
        stringr::str_detect(key, "^[:alnum:]$") & key_type == "alphanumeric" ~ 1.75,
        TRUE ~ 1
      )
    ) %>%
    dplyr::left_join(palette_df, by = "key_type") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      text_colour = ifelse(is_dark(fill) & adjust_text_colour, prismatic::clr_lighten(palette[["text"]], 0.5), palette[["text"]])
    ) %>%
    dplyr::ungroup()

  keyboard[["colour"]] <- unclass(prismatic::clr_darken(keyboard[["fill"]], 0.1))

  keyboard %>%
    dplyr::mutate(colour = ifelse(is.na(key), NA_character_, colour))
}

construct_keyboard_outline <- function(keyboard, keyboard_colour = keyboard_palette("pastel")[["keyboard"]]) {
  dplyr::tibble(
    x_start = min(keyboard[["x_start"]]),
    x_end = max(keyboard[["x_end"]]),
    y_start = min(keyboard[["y_start"]]),
    y_end = max(keyboard[["y_end"]])
  ) %>%
    dplyr::mutate(
      x_mid = (x_end - x_start) / 2,
      y_mid = (y_end - y_start) / 2,
      fill = keyboard_colour
    )
}

construct_plot <- function(keyboard, keyboard_full, key_height = 15 / 15.5, key_width = 1, height_gap = 2 / 15.5, font_size = 3, segment_size = 0.25, arrow_size = 0.03, font_family = "Avenir Next", palette = palette, adjust_text_colour = TRUE, layout = c("ansi", "iso"), keyboard_layout) {

  layout <- match.arg(layout)

  p <- ggplot2::ggplot() +
    ggforce::geom_ellipse(data = keyboard_full, ggplot2::aes(x0 = x_mid, y0 = y_mid, a = x_mid * 1.05, b = y_mid * 1.1, angle = 0, m1 = 100, fill = fill, colour = prismatic::clr_darken(fill, 0.10)), size = 1) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = palette[["background"]], colour = palette[["background"]]),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  # Add rows ----

  for (i in unique(keyboard[["row"]])) {
    row_data <- keyboard %>%
      dplyr::filter(row == i)

    p <- p +
      ggforce::geom_ellipse(data = row_data, ggplot2::aes(x0 = x_mid, y0 = y_mid, a = width / 2, b = height / 2, angle = 0, m1 = 10, fill = fill, colour = colour), size = 1) +
      ggplot2::geom_text(data = row_data %>%
        dplyr::filter(!is.na(key_label)), ggplot2::aes(x = x_start + width / 2, y = (y_start + y_end) / 2, label = key_label, size = size, colour = text_colour), family = font_family, lineheight = 0.9)
  }

  # Add arrows if present in layout, and power button for mac
  if (keyboard_layout %in% c("tkl", "full", "mac")) {
    arrows <- keyboard %>%
      dplyr::filter(key %in% c("Up", "Down", "Left", "Right", "UpDown"))

    arrow_colour <- ifelse(is_dark(unique(arrows[["fill"]])) & adjust_text_colour, prismatic::clr_lighten(palette[["text"]], 0.5), palette[["text"]])

    arrows <- arrows %>%
      split(.$key)

    if (keyboard_layout == "mac") {

      p <- p +
        ggplot2::geom_segment(data = arrows[["Left"]], ggplot2::aes(x = x_end, xend = x_start + 0.375*width, y = y_mid, yend = y_mid, colour = NA_character_), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc"), type = "closed"), size = segment_size, arrow.fill = arrow_colour, alpha = 0.80) +
        ggplot2::geom_segment(data = arrows[["Right"]], ggplot2::aes(xend = x_end - 0.375*width, x = x_start, y = y_mid, yend = y_mid, colour = NA_character_), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc"), type = "closed"), size = segment_size, arrow.fill = arrow_colour, alpha = 0.80) +
        ggplot2::geom_segment(data = arrows[["UpDown"]], ggplot2::aes(x = x_start, xend = x_end, y = y_mid, yend = y_mid, colour = colour), size = 1) +
        ggplot2::geom_segment(data = arrows[["UpDown"]], ggplot2::aes(x = x_mid, xend = x_mid, yend = y_start + (0.375/3)*height, y = y_mid, colour = NA_character_), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc"), type = "closed"), size = segment_size, arrow.fill = arrow_colour, alpha = 0.80) +
        ggplot2::geom_segment(data = arrows[["UpDown"]], ggplot2::aes(x = x_mid, xend = x_mid, yend = y_end - (0.375/3)*height, y = y_mid, colour = NA_character_), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc"), type = "closed"), size = segment_size, arrow.fill = arrow_colour, alpha = 0.80)

      power <- keyboard %>%
        dplyr::filter(key == "Power")

      power_colour <- ifelse(is_dark(unique(power[["fill"]])) & adjust_text_colour, prismatic::clr_lighten(palette[["text"]], 0.5), palette[["text"]])

      p <- p +
        ggplot2::geom_segment(data = power,  ggplot2::aes(x = x_mid, xend = x_mid, yend = y_end - (0.375)*height, y = y_mid, colour = NA_character_), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc"), type = "closed", angle = 50), size = segment_size, arrow.fill = arrow_colour, alpha = 0.80) +
        ggplot2::geom_segment(data = power,  ggplot2::aes(x = x_mid - 0.175*width, xend = x_mid + 0.175*width, yend = y_mid - (0.375/4)*height, y = y_mid - (0.375/4)*height, colour = arrow_colour), size = segment_size*2, alpha = 0.80)

    } else {

    p <- p +
      ggplot2::geom_segment(data = arrows[["Down"]], ggplot2::aes(x = x_mid, xend = x_mid, y = (y_end + y_mid) / 2, yend = (y_start + y_mid) / 2, colour = arrow_colour), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc")), size = segment_size) +
      ggplot2::geom_segment(data = arrows[["Up"]], ggplot2::aes(x = x_mid, xend = x_mid, yend = (y_end + y_mid) / 2, y = (y_start + y_mid) / 2, colour = arrow_colour), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc")), size = segment_size) +
      ggplot2::geom_segment(data = arrows[["Left"]], ggplot2::aes(x = (x_end + x_mid) / 2, xend = (x_start + x_mid) / 2, y = y_mid, yend = y_mid, colour = arrow_colour), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc")), size = segment_size) +
      ggplot2::geom_segment(data = arrows[["Right"]], ggplot2::aes(xend = (x_end + x_mid) / 2, x = (x_start + x_mid) / 2, y = y_mid, yend = y_mid, colour = arrow_colour), arrow = ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc")), size = segment_size)
    }
  }

  # Draw on backspace/shift buttons
  backspace <- keyboard %>%
    dplyr::filter(key == "Backspace")

  backspace_colour <- ifelse(is_dark(unique(backspace[["fill"]])) & adjust_text_colour, prismatic::clr_lighten(palette[["text"]], 0.5), palette[["text"]])

  shift <- keyboard %>%
    dplyr::filter(stringr::str_detect(key, "Shift"))

  shift_colour <- ifelse(is_dark(unique(shift[["fill"]])) & adjust_text_colour, prismatic::clr_lighten(palette[["text"]], 0.5), palette[["text"]])

  p <- p +
    # Backspace
    ggplot2::geom_segment(data = backspace, ggplot2::aes(x = (x_end + x_mid) / 2, xend = (x_start + x_mid) / 2, y = y_mid, yend = y_mid, colour = backspace_colour), arrow = ggplot2::arrow(length = ggplot2::unit(0.02, "npc")), size = segment_size) +
    # Shift arrows
    ggplot2::geom_segment(data = shift, ggplot2::aes(x = x_mid - key_width * 0.1, xend = x_mid - key_width * 0.1, y = (y_start + y_mid) / 2, yend = y_mid, colour = shift_colour), size = segment_size) +
    ggplot2::geom_segment(data = shift, ggplot2::aes(x = x_mid + key_width * 0.1, xend = x_mid + key_width * 0.1, y = (y_start + y_mid) / 2, yend = y_mid, colour = shift_colour), size = segment_size) +
    ggplot2::geom_segment(data = shift, ggplot2::aes(x = x_mid - key_width * 0.25, xend = x_mid, y = y_mid, yend = (y_end + y_mid) / 2, colour = shift_colour), size = segment_size) +
    ggplot2::geom_segment(data = shift, ggplot2::aes(x = x_mid, xend = x_mid + key_width * 0.25, yend = y_mid, y = (y_end + y_mid) / 2, colour = shift_colour), size = segment_size) +
    ggplot2::geom_segment(data = shift, ggplot2::aes(x = x_mid + key_width * 0.1, xend = x_mid + key_width * 0.25, yend = y_mid, y = y_mid, colour = shift_colour), size = segment_size) +
    ggplot2::geom_segment(data = shift, ggplot2::aes(x = x_mid - key_width * 0.25, xend = x_mid - key_width * 0.1, yend = y_mid, y = y_mid, colour = shift_colour), size = segment_size)

    # Draw on lights - above Ins, Home, PgUp if tkl, and above numpad if full
    if (keyboard_layout == "tkl") {

      lights <- keyboard %>%
        dplyr::filter(key %in% c("Ins", "Home", "PgUp"))

      p <- p +
        ggplot2::geom_point(data = lights, ggplot2::aes(x = x_mid, y = y_end + height_gap * 3), size = 2.5, colour = palette[["light"]], alpha = 0.75)
    } else if (keyboard_layout == "full") {

      numpad_x <- keyboard %>%
        dplyr::filter(layout == "full", !is.na(key)) %>%
        dplyr::distinct(x_start, x_end)

      lights_x <- seq(from = min(numpad_x[["x_start"]]), to = max(numpad_x[["x_end"]]), length.out = 5)
      lights_x <- lights_x[c(2:4)]

      lights_y <- keyboard %>%
        dplyr::filter(row == 6) %>%
        dplyr::pull(y_mid) %>%
        unique()

      lights <- dplyr::tibble(x = lights_x,
                       y = lights_y)
      p <- p +
        ggplot2::geom_point(data = lights, ggplot2::aes(x = x, y = y), size = 2.5, colour = palette[["light"]], alpha = 0.75)
    }

  # Add symbols in Win key
  windows <- keyboard %>%
    dplyr::filter(key == "Win")

  p <- p +
    ggplot2::geom_text(data = windows, ggplot2::aes(x = x_mid, y = y_mid, colour = text_colour), label = "\u{263A}", size = font_size * 2)

  # Final layout aspects
  p <- p +
    ggplot2::coord_equal() +
    ggplot2::scale_size_identity()

  if (layout == "iso") {
    enter <- keyboard %>%
      dplyr::filter(key == "Enter" & layout == "60%")

    enter <- dplyr::tibble(
      xmin = enter[1, ][["x_start"]],
      xmax = enter[1, ][["x_end"]],
      xmid_top = enter[2, ][["x_mid"]],
      ymin = enter[1, ][["y_end"]],
      ymax = enter[2, ][["y_start"]],
      ymid_top = enter[2, ][["y_mid"]],
      colour = unique(enter[["colour"]]),
      fill = unique(enter[["fill"]]),
      text_colour = unique(enter[["text_colour"]])
    )

    p <- p +
      ggplot2::geom_rect(data = enter, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin * 0.95, ymax = ymax * 1.05, colour = fill, fill = fill), size = 1) +
      ggplot2::geom_segment(data = enter, ggplot2::aes(x = xmin, xend = xmin, y = ymin * 0.90, yend = ymax * 1.01, colour = colour), size = 1) +
      ggplot2::geom_segment(data = enter, ggplot2::aes(x = xmax, xend = xmax, y = ymin * 0.90, yend = ymax * 1.1, colour = colour), size = 1) +
      ggplot2::annotate("text", x = ifelse(keyboard_layout == "mac", enter[["xmid_top"]], (enter[["xmin"]] + enter[["xmax"]]) / 2), y = ifelse(keyboard_layout == "mac", enter[["ymid_top"]], (enter[["ymin"]] + enter[["ymax"]]) / 2), label = "Enter", family = font_family, colour = enter[["text_colour"]])
  }

  p
}

is_dark <- function(colour) {
  (sum(grDevices::col2rgb(colour) * c(299, 587, 114)) / 1000 < 123)
}

convert_to_iso <- function(keyboard, keyboard_layout) {

  # Change existing keys
  keyboard_iso <- keyboard %>%
    dplyr::mutate(
      key = dplyr::case_when(
        row == 5 & number == 1 ~ "±\n§",
        row == 4 & number == 14 ~ "Enter",
        TRUE ~ key
      ),
      key_label = dplyr::case_when(
        key %in% c("±\n§", "Enter") ~ key,
        TRUE ~ key_label
      ),
      key_type = dplyr::case_when(
        key == "±\n§" ~ "alphanumeric",
        key == "Enter" ~ "modifier",
        TRUE ~ key_type
      ),
      width = dplyr::case_when(
        key == "Shift" & row == 2 ~ 1.25,
        TRUE ~ width
      ),
      number = dplyr::case_when(
        row == 2 & number > 1 ~ number + 1L,
        TRUE ~ number
      )
    )

  # Remove right shift key so that long enter can be in its place
  iso_key_remove <- keyboard_iso %>%
    dplyr::filter(row == 3 & number %in% 13)

  keyboard_iso <- keyboard_iso %>%
    dplyr::anti_join(iso_key_remove, by = c("row", "number"))

  # Add new keys
  iso_key_add <- dplyr::tibble(
    key = c("|\n\\", "Enter", "~\n`"),
    row = c(3, 3, 2),
    width = c(1, ifelse(keyboard_layout == "mac", 0.75, 1.25), 1),
    height = c(1, 1, 1),
    number = c(13, 14, 2),
    key_type = c("alphanumeric", "modifier", "alphanumeric"),
    layout = rep("60%", 3)
  ) %>%
    dplyr::mutate(key_label = key)

  # Combine keyboard
  keyboard_iso %>%
    dplyr::bind_rows(iso_key_add) %>%
    dplyr::arrange(row, number) %>%
    dplyr::mutate(key_label = dplyr::case_when(
      key == "Enter" & layout == "60%" ~ NA_character_,
      TRUE ~ key_label
    ))
}
