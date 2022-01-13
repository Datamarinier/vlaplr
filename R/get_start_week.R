#' Test function
#'
#' Toy example to init empty package structure. Returns the date on which the week started.
#'
#' @param x Get the startdate of that week (Monday).
#' @return
#' @export
#' @examples
#' get_start_week(Sys.Date())

get_start_week <- function(x){

  aweek::get_date(week = lubridate::epiweek(lubridate::date(x)),
                  year = lubridate::year(lubridate::date(x)),
                  day = 1,
                  start = 1 )

}
#test
