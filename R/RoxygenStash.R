#' Roxygen Cache
#'
#' Place to stash roxygen descriptions for common paramaters used accross package
#'
#' @param project a sevenbridges 'Project' object. See readme for instructions on how to create this object
#' @param app_id  unique app id: <project_id>/<short_app_id>/<version_number>. Can find this by running cavatica_app_list_all, cavatica_app_search, or using web platform (ID is everything after # in URL. May still need to add /<version_number>) (string)
#' @param file_id id of file. Can be found using api OR using the wep portal (see number after #) (string)
#' @param file a sevenbridges file object. E.g. that prduced by cavatica_file_from_filepath
#' @param auth authorisation object used to interface with cavatica api. Retrieved by running cavatica_api_connect()
#' @param status one of: c("all", "queued", "draft", "running", "completed", "aborted", "failed")
standard_description_function <- function(project, app_id, file_id, file, auth, status){

}
