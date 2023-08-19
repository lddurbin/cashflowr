#' Get Total By Budget Type
#'
#' @param data A tibble with three columns: item, type, and value.
#' @param type The budget type, either "operating_spend" or "income".
#' @param period Should the values be calculated as monthly or fortnightly?
#'   Default is monthly.
#' @param value_period Is this being converted from "monthly" (the default) or
#'   "annually"?
#'
#' @return A numeric value.
#'
#' @export
#'
#' @examples
#' test_data <- tibble::tibble(
#'   item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
#'   type = c(rep("operating_spend", 3), rep("income", 2)),
#'   value = c(100,250,50,1000,1200)
#' )
#'
#' # Sum operating spend:
#' get_total(test_data, type = "operating_spend")
get_total <- function(data, type, period = "monthly", value_period = "monthly") {
  total <- value <- NULL

  total_spend <- data |>
    dplyr::with_groups(type, summarise, total = sum(value)) |>
    filter(type == {{type}}) |>
    pull(total)

  if(period == "fortnightly") {
    total_spend <- get_by_fortnight(total_spend, value_period = value_period)
  }

  return(total_spend)
}


#' Calculate Value As Fortnightly
#'
#' @param value A numeric value.
#' @param value_period Is this being converted from "monthly" (the default) or
#'   "annually"?
#'
#' @return A numeric value.
#'
#' @noRd
get_by_fortnight <- function(value, value_period = "monthly") {
  if(value_period == "monthly") {
    by_fortnight <- (value*12)/26
  }

  if(value_period == "annually") {
    by_fortnight <- value/26
  }


  return(by_fortnight)
}


#' Calculate Income After Operating Spend
#'
#' @param data A tibble with three columns: item, type, and value.
#' @param period Should the values be calculated as monthly or fortnightly?
#'   Default is monthly.
#' @param value_period Is this being converted from "monthly" (the default) or
#'   "annually"?
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' test_data <- tibble::tibble(
#'   item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
#'   type = c(rep("operating_spend", 3), rep("income", 2)),
#'   value = c(100,250,50,1000,1200)
#' )
#'
#' #Total net income:
#' get_net_income(test_data)
get_net_income <- function(data, period = "monthly", value_period = "monthly") {
  total_income <- get_total(data, "income")
  total_operating_spend <- get_total(data, "operating_spend")

  net_income <- total_income - total_operating_spend

  if(period == "fortnightly") {
    net_income <- get_by_fortnight(net_income, value_period = value_period)
  }

  return(net_income)
}


#' Calculate Savings Allowance
#'
#' @param net_income Numeric value that represents total net income from all
#'   sources.
#' @param disposable_income_allowance Numeric value that represents disposable
#'   income allowance, default is 1000. Multiplied by 2 in the calculation to
#'   assume that this budget is for a couple.
#' @param period Should the values be calculated as monthly or fortnightly?
#'   Default is monthly.
#' @param value_period Is this being converted from "monthly" (the default) or
#'   "annually"?
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' test_data <- tibble::tibble(
#'   item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
#'   type = c(rep("operating_spend", 3), rep("income", 2)),
#'   value = c(100,250,50,1000,1200)
#' )
#'
#' get_savings_allowance(get_net_income(test_data))
get_savings_allowance <- function(net_income, disposable_income_allowance = 1000,
                                  period = "monthly", value_period = "monthly") {
  savings_allowance <- net_income - (disposable_income_allowance*2)

  if(period == "fortnightly") {
    savings_allowance <- get_by_fortnight(savings_allowance, value_period = value_period)
  }

  return(savings_allowance)
}


#' Calculate Individual Contribution
#'
#' @param income A numeric value expressing the income for this individual.
#' @param dispoable_allowance A numeric value expressing the amount set aside as
#'   disposable income.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' test_data <- tibble::tibble(
#'   item = c("Item 1", "Item 2", "Item 3", "Item 1", "Item 2"),
#'   type = c(rep("operating_spend", 3), rep("income", 2)),
#'   value = c(100,250,50,1000,1200)
#' )
#'
#' income <- test_data |>
#' filter(type == "income" & item == "Item 1") |>
#' pull(value)
#'
#' get_contribution(income, 250)
get_contribution <- function(income, dispoable_allowance) {
  contribution <- income - dispoable_allowance

  return(contribution)
}
