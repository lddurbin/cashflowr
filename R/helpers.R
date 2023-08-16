#' Get Budget Data From Google Sheets
#'
#' @param sheet_link A link to the Google Sheet where your budget is stored.
#'   There should be two columns: the first with the budget item, the second
#'   with the budget value.
#'
#' @return A tibble.
#'
#' @noRd
get_data <- function(sheet_link) {
  data <- googlesheets4::read_sheet(sheet_link)

  return(data)
}
