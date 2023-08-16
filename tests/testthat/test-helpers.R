test_that("we can return data", {
  googlesheets4::gs4_deauth()

  actual <- get_data("https://docs.google.com/spreadsheets/d/17ou0qWgzfs4Hzpc0b437R5SLE-9PE5heBaitTn5zWMA/edit?usp=sharing") |>
    colnames()

  expect_equal(actual, c("item", "value"))
})
