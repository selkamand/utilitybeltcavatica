#' List all tasks
#'
#' @inheritParams standard_description_function
#' @param status only return tasks with the chosen status
#'
#' @return TaskList with full task list
#' @export
cavatica_tasks_list_all <- function(project, status = c("all", "queued", "draft", "running", "completed", "aborted", "failed")){
  assert_is_project(project)
  project$task(complete=TRUE, status=status)
}


#' Create Task
#'
#' @inheritParams standard_description_function
#' @param name  name of task
#' @param description description of task
#' @param app_id id of app/workflow to run
#' @param inputs list describing input values. See [cavatica_app_inputs()] for a list of input paramaters
#' @param use_interruptible_instances use spot instances? (boolean)
#' @param execution_settings control the instance type and maximum number of parallel instances (list, e.g. \code{list(instance_type = "c4.2xlarge;ebs-gp2;2000", max_parallel_instances = 1, use_memoization = false, use_elastic_disk = false)})
#' @param batch list describing how to batch task. Construct value passed to function using [sevenbridges::batch()]. For example: \code{batch = batch(input = "fastq", criteria = c("metadata.sample_id","metadata.noexist_id"))}
#' @return sevenbridges Task object

#' @export
cavatica_tasks_create <- function(project, name, app_id, inputs = NULL, batch = NULL, description = NULL, use_interruptible_instances = TRUE, execution_settings = NULL){
  assert_is_project(project)
  project$task_add(
    project = project,
    name = name,
    description = description,
    batch = batch,
    app = app_id,
    inputs = inputs,
    input_check = TRUE,use_interruptible_instances = use_interruptible_instances,
    execution_settings = execution_settings
  )
}


#' List all apps in project
#'
#' @inheritParams standard_description_function
#'
#' @return list of all apps in project(Sevenbridges AppList Object)
#' @export
cavatica_app_list_all <- function(project){
  project$app(complete = TRUE)
}

#' Non-exact app searching by name
#'
#' Non-exact app searching by name. Typically used to find an apps ID for  `cavatica_app_get`
#'
#' @inheritParams standard_description_function
#' @param name Name of app. Does not need to be complete or exact.
#'
#' @return Applists
#' @export
#'
cavatica_app_search <- function(project, name){
  project$app(complete = TRUE, name = name)
}


#' Get Specific App
#'
#' Get sevenbridges App object using its unique ID
#'
#' @inheritParams cavatica_tasks_list_all
#'
#' @return If on app_id is supplied, returns a sevenbridges App object. If given multiple ids, will return a list of sevenbridges App objects
#' @export
#'
cavatica_app_get <- function(project, app_id){
  assertthat::assert_that(is.character(app_id))

  applist = purrr::map(app_id, .f = function(id) { project$app(id=id) })

  if(length(applist) == 1){
    return(applist[[1]])
  }
  else
    return(applist)
}

#' Cavatica App Describe Inputs
#'
#' @inheritParams standard_description_function
#'
#' @return named character vector. Names are inputID, value describes expected input type
#' @export
#'
cavatica_app_inputs <- function(project, app_id){
  app = cavatica_app_get(project, app_id)
  app$input_type()
}



