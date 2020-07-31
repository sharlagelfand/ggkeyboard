library(dplyr)
library(stringr)
library(readr)

sixty_percent <- read_csv(here::here("data-raw", "sixty_percent.csv")) %>%
  mutate(key_label = str_replace(key_label, "&nbsp;", "\n"))

usethis::use_data(sixty_percent, overwrite = TRUE)
