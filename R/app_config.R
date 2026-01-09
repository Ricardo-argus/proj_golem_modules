#' System dependencies
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "proj_golem")
}


#' @param ... character vectors, specifying subdirectory and file(s)
#' within your app.
#'
#' @noRd
app_config <- function(...) {
  config::get(
    value = ...,
    file = app_sys("golem-config.yml")
  )
}
