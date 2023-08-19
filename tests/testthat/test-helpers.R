test_that("we can return total operating spend and total income, monthly or fortnightly", {
  test_data <- tibble::tibble(
    item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
    type = c(rep("operating_spend", 3), rep("income", 2)),
    value = c(100,250,50,1000,1200)
  )

  expect_equal(get_total(test_data, "operating_spend"), 400)
  expect_equal(get_total(test_data, "operating_spend", "fortnightly"), 184.615385)
  expect_equal(get_total(test_data, "income"), 2200)
  expect_equal(get_total(test_data, "income", "fortnightly"), 1015.38462)
})


test_that("we can return total net income, monthly or fortnightly", {
  test_data <- tibble::tibble(
    item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
    type = c(rep("operating_spend", 3), rep("income", 2)),
    value = c(100,250,50,1000,1200)
  )

  expect_equal(get_net_income(test_data), 1800)
  expect_equal(get_net_income(test_data, "fortnightly"), 830.76923)
})


test_that("a monthly or annual value is returned as a fortnightly value", {
  expect_equal(get_by_fortnight(900), 415.38462)
  expect_equal(get_by_fortnight(900, "annually"), 34.615385)
})


test_that("total savings allowance is correctly returned and returned as either a monthly or fortnightly value", {
  test_data <- tibble::tibble(
    item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
    type = c(rep("operating_spend", 3), rep("income", 2)),
    value = c(100,250,50,1000,1200)
  )

  net_income <- get_net_income(test_data)

  expect_equal(get_savings_allowance(net_income), -200)
  expect_equal(get_savings_allowance(net_income, period = "fortnightly"), -92.307692)
})
