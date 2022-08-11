
#' Task Scheduler
#'
#' @param project cavatica project object
#' @param task_list a list of cavatica 'Task' objects
#' @param seconds_between_job_starts how long to wait after spawning a task to spawn the next one. Useful to stagger input phases.
#' @param max_number_of_concurrent_task Maximum number of tasks that can be running concurrently. If more than this number of tasks are running, scheduler will check every 5 minutes until enough finish before spawning any new runs
#'
#' @return NULL - run for its side effects
#' @export
cavatica_tasks_schedule <- function(project, task_list, seconds_between_job_starts, max_number_of_concurrent_task){
  #browser()
  if(class(task_list) == "TaskList"){
    task_list <- as.list(task_list)
  }
  assertthat::assert_that(is.list(task_list), msg = "task_list must be a list")
  assertthat::assert_that(length(task_list) > 0, msg = "task_list is empty")
  for (element in task_list) {assertthat::assert_that(class(element) == "Task", msg = "Not all elements in task_list belong to the Task Class")}

  next_task_to_spawn = 1
  while (TRUE){
    running_tasks=project$task(status="running", complete=TRUE)
    if (is.null(running_tasks)) running_tasks <- 0
    number_of_running_tasks <- length(running_tasks)

    while(number_of_running_tasks >= max_number_of_concurrent_task){
      #browser()
      message("Number of running tasks [", number_of_running_tasks, "] is at or above the maximum [",max_number_of_concurrent_task,"]. Waiting 5 minutes then checking again")
      Sys.sleep(time = 60*5)
      running_tasks=project$task(status="running", complete=TRUE)
      if (is.null(running_tasks)) running_tasks <- 0
      number_of_running_tasks <- length(running_tasks)
    }

    message("Running Task ",next_task_to_spawn," of ", length(task_list))

    tryCatch(
      expr = {
        task_list[[next_task_to_spawn]]$run()
      },
      error = function(err){
        message(err)
        message("\n\nresuming in spite of error")
      },
      warning = function(warn){
        message(warning)
        message("\n\nresuming in spite of warning")
      }
    )

    next_task_to_spawn = next_task_to_spawn + 1

    if(next_task_to_spawn > length(task_list)){
      message("All tasks in task list have been spawned")
      break()
    }

    message("Waiting ", seconds_between_job_starts, " seconds before starting next job")
    Sys.sleep(time = seconds_between_job_starts)
  }
}
