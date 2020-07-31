library(dplyr)
library(ggkeyboard)
library(readr)
library(stringr)

tkl_addons <- read_csv(here::here("data-raw", "tkl_addons.csv")) %>%
  mutate(key_label = str_replace(key_label, "&nbsp;", "\n"))

tkl <- sixty_percent %>%
  filter(!(row == 5 & number == 1)) %>%
  bind_rows(tkl_addons) %>%
  arrange(row, number)

usethis::use_data(tkl, overwrite = TRUE)
