library(dplyr)

r6 <- tibble(
  key = c("Esc", "NA1", paste0("F", 1:4), NA, paste0("F", 5:8), NA, paste0("F", 9:12), "NA2", "Print", "Scroll", "Pause"),
  row = 6
) %>%
  mutate(
    width = case_when(
      key == "NA1" ~ 1,
      key == "NA2" ~ 0.25,
      is.na(key) ~ 0.5,
      TRUE ~ 1
    ),
    key = case_when(
      key %in% c("NA1", "NA2") ~ NA_character_,
      TRUE ~ key
    )
  )

r5 <- tibble(
  key = c("~\n`", 1:9, 0, "_\n-", "+\n=", "Backspace", NA, "Ins", "Home", "PgUp"),
  row = 5
) %>%
  mutate(width = case_when(
    key == "Backspace" ~ 2,
    is.na(key) ~ 0.25,
    TRUE ~ 1
  ))

r4 <- tibble(
  key = c("Tab", "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", "[\n{", "]\n}", "|\n\\", NA, "Del", "End", "PgDn"),
  row = 4
) %>%
  mutate(width = case_when(
    key %in% c("Tab", "|\n\\") ~ 1.5,
    is.na(key) ~ 0.25,
    TRUE ~ 1
  ))

r3 <- tibble(
  key = c("Caps", "A", "S", "D", "F", "G", "H", "J", "K", "L", ":\n;", "\"\n'", "Enter"),
  row = 3
) %>%
  mutate(width = case_when(
    key == "Caps" ~ 1.75,
    key == "Enter" ~ 2.25,
    TRUE ~ 1
  ))

r2 <- tibble(
  key = c("Shift", "Z", "X", "C", "V", "B", "N", "M", "<\n,", ">\n.", "?\n/", "Shift2", NA, "Up"),
  row = 2
) %>%
  mutate(width = case_when(
    key == "Shift" ~ 2.25,
    key == "Shift2" ~ 2.75,
    is.na(key) ~ 1.25,
    TRUE ~ 1
  ))

r1 <- tibble(
  key = c("Ctrl", "Cmd", "Alt", "Spacebar", "Alt", "??", "Fn", "Ctrl", NA, "Left", "Down", "Right"),
  row = 1
) %>%
  mutate(width = case_when(
    key == "Spacebar" ~ 6.25,
    is.na(key) ~ 0.25,
    key %in% c("Left", "Down", "Right") ~ 1,
    TRUE ~ 1.25
  ))

# Combine into list for function ----

tkl <- bind_rows(
  r1 = r1,
  r2 = r2,
  r3 = r3,
  r4 = r4,
  r5 = r5,
  r6 = r6
)

tkl <- bind_rows(tkl) %>%
  group_by(row) %>%
  mutate(number = row_number()) %>%
  ungroup()

usethis::use_data(tkl, overwrite = TRUE)
