test_that("key names are unique", {

  key_counts <- sixty_percent %>%
    dplyr::filter(!is.na(key)) %>%
    dplyr::count(key)

  expect_true(all(key_counts[["n"]]) == 1)

  keys <- tkl %>%
    dplyr::filter(!is.na(key)) %>%
    dplyr::count(key)

  expect_true(all(key_counts[["n"]]) == 1)

  keys <- full %>%
    dplyr::filter(!is.na(key)) %>%
    dplyr::count(key)

  expect_true(all(key_counts[["n"]]) == 1)

  keys <- mac %>%
    dplyr::filter(!is.na(key)) %>%
    dplyr::count(key)

  expect_true(all(key_counts[["n"]]) == 1)

  key_counts <- tkl_dvorak %>%
    dplyr::filter(!is.na(key)) %>%
    dplyr::count(key)

  expect_true(all(key_counts[["n"]]) == 1)

})

test_that("no key names contain &nbsp; or \n", {

  key_newline <- sixty_percent %>%
    dplyr::filter(stringr::str_detect(key, "nbsp|\n"))

  expect_equal(nrow(key_newline), 0)

  key_newline <- tkl %>%
    dplyr::filter(stringr::str_detect(key, "nbsp|\n"))

  expect_equal(nrow(key_newline), 0)

  key_newline <- full %>%
    dplyr::filter(stringr::str_detect(key, "nbsp|\n"))

  expect_equal(nrow(key_newline), 0)

  key_newline <- mac %>%
    dplyr::filter(stringr::str_detect(key, "nbsp|\n"))

  expect_equal(nrow(key_newline), 0)

  key_newline <- tkl_dvorak %>%
    dplyr::filter(stringr::str_detect(key, "nbsp|\n"))

  expect_equal(nrow(key_newline), 0)

})
