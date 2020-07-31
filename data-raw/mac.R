library(readr)
library(dplyr)
library(ggkeyboard)

mac_addons <- read_csv(here::here("data-raw", "mac_addons.csv"))

mac <- sixty_percent %>%
  filter(!(row == 1 & number %in% 7:8)) %>%
  mutate(
    width = case_when(
      row == 1 & number == 5 ~ 1.25,
      row == 1 & key != "Spacebar" ~ 1,
      key == "|\n\\" ~ 1,
      key == "Enter" ~ 1.75,
      key == "Backspace" ~ 1.5,
      key == "Spacebar" ~ 5,
      key == "Shift2" ~ 2.25,
      TRUE ~ width
    ),
    number = case_when(
      row == 1 & number >= 4 ~ number + 1,
      TRUE ~ number
    )
  ) %>%
  bind_rows(mac_addons) %>%
  mutate(
    key = case_when(
      row == 1 & number == 1 ~ "Fn",
      row == 1 & number == 2 ~ "Ctrl",
      row == 1 & number == 3 ~ "\u{2325}",
      row == 1 & number == 4 ~ "\u{2318}",
      row == 1 & number == 6 ~ "\u{2318}",
      row == 1 & number == 7 ~ "\u{2325}",
      row == 5 & key == "Esc" ~ "~\n`",
      TRUE ~ key
    ),
    key_label = case_when(
      row == 5 & key == "~\n`" ~ "~\n`",
      row == 1 & !key %in% c("Spacebar", "Left", "UpDown", "Right") ~ key,
      TRUE ~ key_label
    )
  ) %>%
  arrange(row, number)

usethis::use_data(mac, overwrite = TRUE)
