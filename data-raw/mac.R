library(readr)
library(dplyr)
library(ggkeyboard)

mac_addons <- read_csv(here::here("data-raw", "mac_addons.csv")) %>%
  mutate(key_label = str_replace(key_label, "&nbsp;", "\n"))

mac <- sixty_percent %>%
  filter(row != 1) %>%
  filter(!(row == 5 & key == "Esc")) %>%
  bind_rows(mac_addons) %>%
  mutate(
    width = case_when(
      key == "|\\" ~ 1,
      key == "Enter" ~ 1.75,
      key == "Backspace" ~ 1.5,
      key == "Shift Right" ~ 2.25,
      TRUE ~ width
    ),
    key_label = case_when(
      key %in% c("Option Left", "Option Right") ~ "\u{2325}\nOption",
      key %in% c("Command Left", "Command Right") ~ "\u{2318}\nCommand",
      TRUE ~ key_label
    )) %>%
  arrange(row, number)

usethis::use_data(mac, overwrite = TRUE)
