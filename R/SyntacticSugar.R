#' List all tasks
#'
#'
#' @param project a sevenbridges 'Project' object. See readme for instructions on how to create this object
#' @param status only return tasks with the chosen status
#'
#' @return TaskList with full task list
#' @export
cavatica_tasks_list_all <- function(project, status = c("all", "queued", "draft", "running", "completed", "aborted", "failed")){
  assert_is_project(project)
  project$task(complete=TRUE, status=status)
}

