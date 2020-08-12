library(dplyr)
library(stringr)
library(readr)

steno <- read_csv(here::here("data-raw", "steno.csv")) %>%
  mutate(key_label = str_replace(key_label, "&nbsp;", "\n"))

usethis::use_data(steno, overwrite = TRUE)
