
# list_files_recursively <- function(folder){
# }


#' File and FileList Operations
#'
#' @param project a cavatica project object
#' @param filepaths the filepaths to files you want retrieve (character)
#'
#' @return A Files object or a list of Files Objects.
#' @export
#'
find_file_using_filepath <- function(project, filepaths){
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

#'
#' @inherit find_file_using_filepath
#'
#' @return character fileids
#' @export
#'
find_fileid_using_filepath <- function(project, filepaths){
  find_file_using_filepath(project, filepaths) %>%
    vapply(FUN.VALUE = "", FUN = function(x){x[["id"]]})
}

#' File and FileList Operations
#'
#' @param project cavatica project
#' @param splitpath a single vector where each element in a folder/file in a filepath
#'
#' @return File at the end of the filepath (sevenbridges Files class object) OR NA if file doesnt exist
#'
get_path_from_splitpath <- function(project, splitpath){
  #browser()
  current_folder <- project$file(name=splitpath[1], exact=TRUE)

  if(length(current_folder) == 0 ) { message("Could not find: ", splitpath[1]); return(NA)}

  if (current_folder$type == "file" | length(splitpath) == 1){
    return(current_folder)
  }

  for (filename in splitpath[-1]){

    current_folder_contents <- current_folder$list_folder_contents()

      next_file_or_folder <- get_files_from_filelist_by_name(
        filelist = current_folder_contents,
        filename = filename
      )

      if(!isS4(next_file_or_folder) && is.na(next_file_or_folder)){
        message(paste0("Could not find any file named: ", filename, " in the folder ", current_folder$name))
        return(NA)
        }

      if(next_file_or_folder$type == "file") return(next_file_or_folder)
      else
        current_folder <- next_file_or_folder
  }
  return(next_file_or_folder)
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
