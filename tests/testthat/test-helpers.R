test_that("we can return total operating spend and total income", {
  test_data <- tibble::tibble(
    item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
    type = c(rep("operating_spend", 3), rep("income", 2)),
    value = c(100,250,50,1000,1200)
  )

  expect_equal(get_total(test_data, "operating_spend"), 400)
  expect_equal(get_total(test_data, "income"), 2200)
})


test_that("we can return total net income", {
  test_data <- tibble::tibble(
    item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
    type = c(rep("operating_spend", 3), rep("income", 2)),
    value = c(100,250,50,1000,1200)
  )

  expect_equal(get_net_income(test_data), 1800)
})


test_that("a monthly value is returned as a fortnightly value", {
  expect_equal(get_by_fortnight(900), 415.38462)
})
