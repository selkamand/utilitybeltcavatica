
# Tasks -------------------------------------------------------------------


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

#' Cavatica Tasks
#'
#' @inheritParams standard_description_function
#' @param pattern pattern of name e.g. 'prefix*'
#' @return tasklist
#' @export
#'
cavatica_tasks_search <- function(project, pattern, status = c("all", "queued", "draft", "running", "completed", "aborted", "failed")){
  assert_is_project(project)
  status = rlang::arg_match(status)
  project$task(name = pattern, complete = TRUE, status = status)
}

#' Cavatica Tasks
#'
#' Find tasks with name starting with 'prefix'
#'
#' @inheritParams cavatica_tasks_search
#' @param prefix prefix of task name (string)
#' @return tasklist
#' @export
#'
cavatica_tasks_search_by_prefix <- function(project, prefix, status = c("all", "queued", "draft", "running", "completed", "aborted", "failed")){
  cavatica_tasks_search(project = project, pattern = paste0(prefix, "*"), status = status)
}


#' Cavatica Tasks
#'
#' delete task / tasks.
#'
#' @param tasks task / tasklist / list of task objects to delete
#'
#' @return run for its side effects
#' @export
#'
cavatica_tasks_delete <- function(tasks){
  sapply(tasks, FUN = function(x) {x$delete()})
  return(invisible(NULL))
}

#' Cavatica Tasks
#'
#' Run cavatica tasks
#'
#' @param tasks task / tasklist / list of task objects to delete
#'
#' @return run for its side effects
#' @export
#'
cavatica_tasks_run <- function(tasks){
  sapply(tasks, FUN = function(x) {x$run()})
  return(invisible(NULL))
}

#' Download cavatica task outputs
#'
#' @param tasks task / tasklist / list of task objects to delete
#' @param output_dir directory to download outputfiles to. Can supply one folder for all task downloads or 1 folder for each task
#'
#' @return run for its side effects
#' @export
#'
cavatica_tasks_download <- function(tasks, output_dir){
  assertthat::assert_that(all(cavatica_tasks_is_completed(tasks)), msg = "All tasks must be completed to download the results")

  if(length(output_dir) != 1){
   assertthat::assert_that(length(output_dir) ==  length(tasks))
  }

  sapply(tasks, function(task){
    message("Downloading Task: ", task$name)
    task$download(destfile=output_dir)
    })
}

cavatica_tasks_is_completed <- function(tasks){
  sapply(tasks, function(task){ task$status == "COMPLETED"})
}
# App ---------------------------------------------------------------------

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
#' @inheritParams standard_description_function
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


# Projects ----------------------------------------------------------------


#' Cavatica Get Project
#'
#' @param project_id project id (project_owner/project_name). If unsure,  (string)
#' @inheritParams standard_description_function
#'
#' @return cavatica project
#' @export
#'
cavatica_project_get <- function(project_id, status = "all", auth = cavatica_api_connect()){
  auth$project(id = project_id)
}

#' List all projects
#'
#' @param project_id project id (project_owner/project_name). If unsure,  (string)
#' @inheritParams standard_description_function
#'
#' @return sevenbridges ProjectList object
#' @export
cavatica_project_list <- function(auth = cavatica_api_connect(), project_id){
  auth$project(id = project_id)
}



# Api ---------------------------------------------------------------------


#' Cavatica Api Rate Limit
#'
#' Find the api rate limit
#'
#' @inheritParams standard_description_function
#'
#' @return cavatica api rate limit (string)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' auth = cavatica_api_connect()
#' cavatica_api_rate_limit(auth)
#' }
cavatica_api_rate_limit <- function(auth){
  auth$rate_limit()
}

#' Connect to API
#'
#' Connect to the cavatica api and return the Auth object
#'
#' @param from where to source cavatica credentials. defualt is to look for a file (~/.sevenbridges/credentials). See README for details on how to create this file.
#' @param profile_name assuming credentials are coming from file, which profile should be used (text within square brackets within ~/.sevenbridges/credentials file are different profiles) (string)
#'
#' @return sevenbridges api auth object
#' @export
cavatica_api_connect <- function(from = "file", profile_name = "default"){
  api = sevenbridges::Auth(from = from, profile_name = profile_name, type = "cavatica")
  return(api)
}


