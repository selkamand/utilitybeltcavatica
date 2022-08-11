#' Assert object is a sevenbridges Project
#'
#' @param object object that should be a sevenbridges Project object
#'
assert_is_project <- function(object) {
  assertthat::assert_that(
    class(object) == "Project",
    msg = paste0("Expected a sevenbridges 'Project' object. Instead we got a [", class(object), "] object")
  )
}

#' Assert object is a sevenbridges Project
#'
#' @param object object that should be a sevenbridges Project object
#'
assert_is_file <- function(object) {
  assertthat::assert_that(
    class(object) == "Files",
    msg = paste0("Expected a sevenbridges 'Project' object. Instead we got a [", class(object), "] object")
  )
}
