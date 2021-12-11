library(dplyr)
library(stringr)
library(ggkeyboard)
library(readr)

numpad <- read_csv(here::here("data-raw", "numpad.csv")) %>%
  mutate(key_label = str_replace(key_label, "&nbsp;", "\n"))

full <- tkl %>%
  bind_rows(numpad) %>%
  arrange(row, number) %>%
  mutate(layout = "full")

usethis::use_data(full, overwrite = TRUE)
