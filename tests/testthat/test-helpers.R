test_that("we can return total operating spend and total income", {
  googlesheets4::gs4_deauth()
  googlesheets4::gs4_user()

  test_data <- tibble::tibble(
    item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
    type = c(rep("operating_spend", 3), rep("income", 2)),
    value = c(100,250,50,1000,1200)
  )

  expect_equal(get_total(test_data, "operating_spend"), 400)
  expect_equal(get_total(test_data, "income"), 2200)
})
