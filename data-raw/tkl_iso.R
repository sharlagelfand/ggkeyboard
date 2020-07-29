library(dplyr)

tkl_iso <- ggkeyboard::tkl %>%
  mutate(key = case_when(row == 5 & number == 1 ~ "±\n§",
                         row == 4 & number == 14 ~ "Enter",
                         TRUE ~ key))

iso_key_remove <- tkl_iso %>%
  filter(row == 3 & number %in% 13)

tkl_iso <- tkl_iso %>%
  anti_join(iso_key_remove, by = c("row", "number"))

add_index <- tkl_iso %>%
  filter(row == 3) %>%
  pull(number) %>%
  max()

iso_key_add <- tibble(
  key = c("|\n\\", "Enter"),
  row = 3,
  width = c(1, 1.25),
  number = c(add_index + 1, add_index + 2)
)

tkl_iso <- tkl_iso %>%
  bind_rows(iso_key_add) %>%
  arrange(row, number)

usethis::use_data(tkl_iso, overwrite = TRUE)
