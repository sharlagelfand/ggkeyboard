library(dplyr)
library(stringr)
library(ggkeyboard)
library(readr)

numpad <- read_csv(here::here("data-raw", "numpad.csv")) %>%
  mutate(across(c(key, key_label), ~ str_replace(.x, "&nbsp;", "\n")))

full <- tkl %>%
  bind_rows(numpad) %>%
  arrange(row, number)

usethis::use_data(full, overwrite = TRUE)
