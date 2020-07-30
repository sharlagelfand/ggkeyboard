library(dplyr)

tkl_iso <- ggkeyboard::tkl %>%
  mutate(key = case_when(
    row == 5 & number == 1 ~ "±\n§",
    row == 4 & number == 14 ~ "Enter",
    TRUE ~ key
  ),
  key_label = case_when(
    key %in% c("±\n§", "Enter") ~ key,
    TRUE ~ key_label
  ),
  key_type = case_when(
    key == "±\n§" ~ "alphanumeric",
    key == "Enter" ~ "modifier",
    TRUE ~ key_type
  ),
  width = case_when(
    key == "Shift" & row == 2 ~ 1.25,
    TRUE ~ width),
  number = case_when(
    row == 2 & number > 1 ~ number + 1L,
    TRUE ~ number
  ))

iso_key_remove <- tkl_iso %>%
  filter(row == 3 & number %in% 13)

tkl_iso <- tkl_iso %>%
  anti_join(iso_key_remove, by = c("row", "number"))

add_index <- tkl_iso %>%
  filter(row == 3) %>%
  pull(number) %>%
  max()

iso_key_add <- tibble(
  key = c("|\n\\", "Enter", "~\n`"),
  row = c(3, 3, 2),
  width = c(1, 1.25, 1),
  number = c(add_index + 1, add_index + 2, 2),
  key_type = c("alphanumeric", "modifier", "alphanumeric")
) %>%
  mutate(key_label = key)

tkl_iso <- tkl_iso %>%
  bind_rows(iso_key_add) %>%
  arrange(row, number) %>%
  mutate(key_label = case_when(
    key == "Enter" ~ NA_character_,
    TRUE ~ key_label
  ))

usethis::use_data(tkl_iso, overwrite = TRUE)
