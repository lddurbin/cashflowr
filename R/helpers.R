#' Get Total By Budget Type
#'
#' @param data A tibble with three columns: item, type, and value.
#' @param type The budget type, either "operating_spend" or "income".
#' @param period Should the values be calculated as monthly or fortnightly?
#'   Default is monthly.
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
get_total <- function(data, type, period = "monthly") {
  total <- value <- NULL

  total_spend <- data |>
    dplyr::with_groups(type, summarise, total = sum(value)) |>
    filter(type == {{type}}) |>
    pull(total)

  if(period == "fortnightly") {
    total_spend <- get_by_fortnight(total_spend)
  }

  return(total_spend)
}


#' Calculate Value As Fortnightly
#'
#' @param value A numeric value.
#'
#' @return A numeric value.
#'
#' @noRd
get_by_fortnight <- function(value) {
  by_fortnight <- (value*12)/26

  return(by_fortnight)
}


#' Calculate Income After Operating Spend
#'
#' @param data A tibble with three columns: item, type, and value.
#' @param period Should the values be calculated as monthly or fortnightly?
#'   Default is monthly.
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
get_net_income <- function(data, period = "monthly") {
  total_income <- get_total(data, "income")
  total_operating_spend <- get_total(data, "operating_spend")

  net_income <- total_income - total_operating_spend

  if(period == "fortnightly") {
    net_income <- get_by_fortnight(net_income)
  }

  return(net_income)
}
