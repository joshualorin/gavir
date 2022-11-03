
#' Set root for file paths
#'
#' This function just sets the root directory for filepaths. It simplifies data-loading and dynamically pulls in the username to the filepath.
#'
#' @return a vector with the filepath root.
#' @export
#'
#' @examples
#' set_root()
#' # can also save as root object to re-use when loading multiple data files
#' root <- set_root()

set_root <- function(){

  os <- Sys.info()["sysname"]
  if(os == "Darwin"){user <- Sys.info()["user"]}else{Sys.getenv("USERNAME")}
  root_mac <- file.path("/Users", user, "Library", "CloudStorage", "OneDrive-SharedLibraries-Gavi", "Measurement, Evaluation and Learning - Documents")
  root_windows <- file.path("C:", "Users", user, "Gavi", "Measurement, Evaluation and Learning - CPMM")
  if(os == "Darwin"){root_mac}else{root_windows}
}



#' Load in the groupings file
#'
#' This function loads in the country groupings file into your current environment
#'
#' @param version_date a string in the format of "yyyy-mm", corresponding to the version of the country groupings file you wish to load.
#'
#' @return a data frame of country groupings
#' @export
#'
#' @examples
#' get_groupings("2022-07")
#' # can also save as object to re-use
#' groupings <- get_groupings("2022-07")

get_groupings <- function(version_date){

  root <- set_root()
  folders <- list.files(file.path(root, "CPMM", "Datasets", "Country Groupings"), pattern = "^20")
  max_date <- max(as.Date(paste(folders,"-01",sep="")))

  path <- file.path(root, "CPMM", "Datasets", "Country Groupings", version_date, paste0("country_groupings_", version_date, ".rds"))
  msg <- paste0("Reading in groupings file dated: ", version_date)
  error_msg1 <- c("Oops! Must set a version date!")
  error_msg2 <- paste0("Oops! A groupings file dated ", version_date," does not exist. Date must be one of the following:")
  error_msg3 <- stringr::str_wrap(paste(folders, collapse=" "),1)
  error_msg4 <- paste0("Interesting! The groupings file you have selected is not the most recent. If this was unintentional, you can update to the most recent file, which is ", max_date)


  if(is.null(version_date)){
    message(error_msg1)
  }else if(!version_date %in% folders){
    message(error_msg2)
    message(error_msg3)
  }else if(as.Date(paste(version_date,"-01",sep="")) != max_date){
    message(msg)
    message(error_msg4)
    readRDS(path)
  }else{
    message(msg)
    readRDS(path)
  }

}
