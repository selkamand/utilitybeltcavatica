
# list_files_recursively <- function(folder){
# }


#' File and FileList Operations
#'
#' @param project a cavatica project object
#' @param filepaths the filepaths to files you want retrieve (character)
#'
#' @return A Files object or a list of Files Objects.
cavatica_file_from_filepath2 <- function(project, filepaths){
  assertthat::assert_that(is.character(filepaths))
  filepaths <- sub(pattern = "\\/$", replacement = "", x = filepaths)
  filepaths <- gsub(pattern = "(\\/)+", replacement = "/", x = filepaths)
  filepaths <- sub(pattern = "^\\/", replacement = "", x = filepaths)

  # Split path into each element
  splitpath <- strsplit(x = filepaths, split = "/", fixed = TRUE)
  files <- sapply(splitpath, FUN = function(path){ get_path_from_splitpath(project = project, splitpath = path)})
  if(length(files) == 1) { files <- files[[1]]}
  return(files)
}

#' Cavatica File From Filepath
#'
#' returns file_id from filepaths
#'
#' @inheritParams  cavatica_file_from_filepath
#'
#' @return character filepaths
#' @export
#'
cavatica_file_id_from_filepath <- function(project, filepaths){
  files = cavatica_file_from_filepath(project, filepaths)
  if (length(files) == 1) files <- list(files)

  vapply(
      X = files,
      FUN.VALUE = "",
      FUN = function(x){
        if(!isS4(x)) return(NA_character_)
        return(x[["id"]])
      })
}

#' File and FileList Operations
#'
#' @inheritParams standard_description_function
#' @param splitpath a single vector where each element in a folder/file in a filepath
#' @param file_object_only return only the file object - or a list where element 1 is file object and element 2 is a dataframe of all cached filepaths stepped through on the way to finding the file (boolean)
#' @return File at the end of the filepath (sevenbridges Files class object) OR NA if file doesnt exist
#'
get_path_from_splitpath <- function(project, splitpath, file_object_only = FALSE){
  current_folder <- project$file(name=splitpath[1], exact=TRUE, complete=TRUE)

  if(length(current_folder) == 0 ) { message("Could not find: ", splitpath[1]); return(NA)}

  path_record <- c(splitpath[1])
  folder_record <- list(current_folder)

  if (current_folder$type == "file" | length(splitpath) == 1){
    record_df = dplyr::tibble(path = path_record, file = folder_record)
    if(file_object_only) return(current_folder) else return(list("FileObject" = current_folder, "Record" = record_df))
  }

  i=0
  for (filename in splitpath[-1]){
    i=i+1
    current_folder_contents <- current_folder$list_folder_contents(complete=TRUE)

      next_file_or_folder <- get_files_from_filelist_by_name(
        filelist = current_folder_contents,
        filenames = filename
      )

      if(!isS4(next_file_or_folder) && is.na(next_file_or_folder)){
        message(paste0("Could not find the file: ", paste0(splitpath, collapse = "/")))
        return(NA)
        }

      if(next_file_or_folder$type == "file") {
        current_folder <- next_file_or_folder
        path_record <- c(path_record,paste0(splitpath[1:min(i+1, length(splitpath))], collapse = "/"))
        folder_record[[length(folder_record)+1]] <- current_folder
        record_df = dplyr::tibble(path = path_record, file = folder_record)
        if(file_object_only) return(next_file_or_folder) else return(list("FileObject" = current_folder, "Record" = record_df))
      }
      else{
        current_folder <- next_file_or_folder
        path_record <- c(path_record,paste0(splitpath[1:min(i+1, length(splitpath))], collapse = "/"))
        folder_record[[length(folder_record)+1]] <- current_folder
      }
  }

  #This code gets run only if last element of splitlist was a folder
  record_df = dplyr::tibble(path = path_record, file = folder_record)
  if(file_object_only) return(next_file_or_folder) else return(list("FileObject" = current_folder, "Record" = record_df))
}

#' File and FileList Operations
#'
#' @inheritParams standard_description_function
#' @param splitpath a single vector where each element in a folder/file in a filepath
#' @param parent_folder folder to act as 'root' folder from which splitpath is relative to (Sevenbridges File Object)
#' @param file_object_only return only the file object - or a list where element 1 is file object and element 2 is a dataframe of all cached filepaths stepped through on the way to finding the file (boolean)
#' @return File at the end of the filepath (sevenbridges Files class object) OR NA if file doesnt exist
#'
get_path_from_splitpath_with_explicit_parent_folder <- function(project, splitpath, parent_folder, file_object_only = FALSE){

  current_folder <- parent_folder

  if(length(current_folder) == 0) { message("Could not find: ", splitpath[1]); return(NA)}

  path_prefix = cavatica_file_path_from_file(project = project, file = parent_folder)

  path_record <- paste0(path_prefix)
  folder_record <- list(current_folder)

  if (current_folder$type == "file"){
    record_df = dplyr::tibble(path = path_record, file = folder_record)
    if(file_object_only) return(current_folder) else return(list("FileObject" = current_folder, "Record" = record_df))
  }

  i=0
  for (filename in splitpath){
    i=i+1
    current_folder_contents <- current_folder$list_folder_contents(complete=TRUE)

    next_file_or_folder <- get_files_from_filelist_by_name(
      filelist = current_folder_contents,
      filenames = filename
    )

    if(!isS4(next_file_or_folder) && is.na(next_file_or_folder)){
      message(paste0("Could not find the file: ", paste0(splitpath, collapse = "/")))
      return(NA)
    }

    if(next_file_or_folder$type == "file") {
      current_folder <- next_file_or_folder

      path_record <- c(path_record,paste0(path_prefix,"/", paste0(splitpath[1:min((i), length(splitpath))], collapse = "/")))
      folder_record[[length(folder_record)+1]] <- current_folder
      record_df = dplyr::tibble(path = path_record, file = folder_record)
      if(file_object_only) return(next_file_or_folder) else return(list("FileObject" = current_folder, "Record" = record_df))
    }
    else{
      current_folder <- next_file_or_folder
      path_record <- c(path_record,paste0(path_prefix, "/", paste0(splitpath[1:min((i), length(splitpath))], collapse = "/")))
      folder_record[[length(folder_record)+1]] <- current_folder
    }
  }

  #This code gets run only if last element of splitlist was a folder
  record_df = dplyr::tibble(path = path_record, file = folder_record)
  if(file_object_only) return(next_file_or_folder) else return(list("FileObject" = current_folder, "Record" = record_df))
}

#' cavatica_file_path_from_file
#'
#' @inheritParams standard_description_function
#'
#' @return path of file (string)
#' @export
#'
cavatica_file_path_from_file <- function(project, file){
  assert_is_file(file)

  path = character(0)
  currentfile = file
  while(currentfile$id != project$root_folder){
    path = c(path, currentfile$name)
    currentfile = currentfile$get_parent_folder()
  }
  return(paste0(rev(path), collapse = "/"))
}

#' File and FileList Operations
#'
#' @param filelist a sevenbridges FilesList class object
#' @param filenames a vector of names
#'
#' @return a sevenbridges Files object OR a FileList Object (if youve supplied multiple filenames) OR NA if names don't exists in the filelist
#' @export
#'
get_files_from_filelist_by_name <- function(filelist, filenames){
  assertthat::assert_that(is.character(filenames), msg = paste0("Filename must be a character, not a ", class(filenames)))
  query_results <- filelist %>%
    as.list %>%
    vapply(FUN.VALUE = TRUE, FUN = function(x) x$name %in% filenames) %>%
    which()

  if(length(query_results) == 0) return(NA)
  else if (length(query_results) == 1) return(filelist[[query_results]])
  else return(filelist[query_results])
}


recursive_file_list <- function(project, folder){
  #folder_contents = list()
  current_folder = folder
  purrr::map(as.list(current_folder$list_folder_contents(complete = TRUE)), ~ if(.x$type == "folder") {recursive_file_list(project, .x)})
}

#' Get File from ID
#'
#' @inheritParams standard_description_function
#'
#' @return sevenbridges File Object (or FileList if multiple IDs are supplied)
#' @export
cavatica_file_from_file_id <- function(project, file_id){
  assert_is_project(project)
  project$file(id=file_id)
}




#' File and FileList Operations
#'
#' Finds files from filepaths: designed to be vectorised and efficient with respect to api calls
#' How it works is if you parse multiple filepaths with shared folders in their history,
#' this fuction will remembar paths to shared folders so we don't have to re-lookup the same folders over and over.
#'
#' This is usually more efficient than spending forever building a cache which has to be updated whenever new files are added to the volume.
#'
#'
#' @param project a cavatica project object
#' @param filepaths the filepaths to files you want retrieve (character)
#' @param return_ids return file ids instead of file objects (boolean)
#' @return A Files object or a list of Files Objects.
#' @export
#'
cavatica_file_from_filepath <- function(project, filepaths, return_ids = FALSE){
  assertthat::assert_that(is.character(filepaths))
  filepaths = filepaths_fix(filepaths)

  known_paths_df = dplyr::tibble(path = character(0), file = list())

  # Split path into each element
  splitpath <- strsplit(x = filepaths, split = "/", fixed = TRUE)

  for (i in seq_along(splitpath)){
    current_splitpath=splitpath[[i]]

    paths = purrr::map_chr(seq_along(current_splitpath), .f = function(n) {paste0(current_splitpath[1:n], collapse = "/")})

    message(rep("=", times=40))
    message("Finding File: [", filepaths[i], "]")
    message(rep("=", times=40))

    # If some part of the path has already been recorded
    if(any(paths %in% known_paths_df$path)){
      longest_path_matched_already_known_index = max(which(paths %in% known_paths_df$path))
      longest_path_matched_already_known_path = paths[longest_path_matched_already_known_index]
      longest_path_matched_already_known_file_object = known_paths_df$file[match(longest_path_matched_already_known_path, known_paths_df$path)][[1]]

      message("Utilising previously described path: ", longest_path_matched_already_known_path)

      remaining_path_steps = current_splitpath[(longest_path_matched_already_known_index+1):max((longest_path_matched_already_known_index+1), length(current_splitpath))]

      if(all(is.na(remaining_path_steps))){
        message("Full path described. Pulling file from known db.") # Don't need to do anyhting in this section since its already in the db we filter for samples of interest at the end

      }
      else{

        res = get_path_from_splitpath_with_explicit_parent_folder(project = project, splitpath = remaining_path_steps, parent_folder = longest_path_matched_already_known_file_object)
        if(all(!is.na(res))) known_paths_df = rbind(known_paths_df, res[[2]])
      }
    }

    #If no part of the path has been previously recorded
    else{
      message("No part of this path has been previously cached ... finding from scratch")
      res = get_path_from_splitpath(project = project, splitpath = current_splitpath) # returning the same folders
      if(all(!is.na(res))) known_paths_df = rbind(known_paths_df, res[[2]])
    }
    message("\n")
  }

  known_paths_df[["id"]] = cavatica_file_id_from_files(files = known_paths_df$file)

  filepaths_of_interest_only = dplyr::left_join(
    x = dplyr::tibble(filepaths = filepaths),
    y= dplyr::distinct(known_paths_df, path, .keep_all = TRUE),
    by = list(x="filepaths", y= "path")
  )

  filepaths_not_found = dplyr::filter(filepaths_of_interest_only, is.null(unlist(file)))[["filepaths"]]

  #browser()
  if(length(filepaths_not_found) == 0){
    message("======================\nSuccess!\n======================")
    message("[\u2714] all files were found on cavatica!")
    message("======================")
  }
  else{
    message("======================\nMissing Files\n======================")
    message("Failed to find the following files: \n")
    message(paste0("[", seq_along(filepaths_not_found), "] ", filepaths_not_found, collapse = "\n"))
    message("======================")
  }


  if(return_ids){
    return(filepaths_of_interest_only[["id"]])
  }
  else
    return(filepaths_of_interest_only[["file"]])


}

#' Files to file IDs
#'
#' @param files sevenbridges file objects
#'
#' @return character vector describing IDs
#' @export
#'
cavatica_file_id_from_files <- function(files){
  vapply(
    X = files,
    FUN.VALUE = "",
    FUN = function(x){
      assert_is_file(x)
      if(!isS4(x)) return(NA_character_)
      return(x[["id"]])
    })
}


filepaths_fix <- function(filepaths){
  filepaths <- sub(pattern = "\\/$", replacement = "", x = filepaths)
  filepaths <- gsub(pattern = "(\\/)+", replacement = "/", x = filepaths)
  filepaths <- sub(pattern = "^\\/", replacement = "", x = filepaths)
  return(filepaths)
}

#' Convert download link to filename
#'
#' @param link cavatica file download link
#'
#' @return file names (character)
#' @export
#'
cavatica_download_link_to_filenames <- function(link){
    link = sub(x= link, pattern = ".*?filename%3D%22", replacement = "")
    link = sub(x= link, pattern = "%.*$", replacement = "")
    return(link)
}
