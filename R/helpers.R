#' Get Total By Budget Type
#'
#' @param data A tibble with three columns: item, type, and value.
#' @param type The budget type, either "operating_spend" or "income".
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
get_total <- function(data, type) {
  total <- value <- NULL

  total_spend <- data |>
    dplyr::with_groups(type, summarise, total = sum(value)) |>
    filter(type == {{type}}) |>
    pull(total)

  return(total_spend)
}

