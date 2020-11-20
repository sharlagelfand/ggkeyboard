library(dplyr)
library(ggkeyboard)
library(readr)
library(stringr)

sixty_percent_dvorak <- read_csv(here::here("data-raw", "sixty_percent_dvorak.csv")) %>%
  mutate(key_label = str_replace(key_label, "&nbsp;", "\n"))

tkl_addons <- read_csv(here::here("data-raw", "tkl_addons.csv")) %>%
  mutate(key_label = str_replace(key_label, "&nbsp;", "\n"))

tkl_dvorak <- sixty_percent_dvorak %>%
  filter(!(row == 5 & number == 1)) %>%
  bind_rows(tkl_addons) %>%
  arrange(row, number)

usethis::use_data(tkl_dvorak, overwrite = TRUE)
