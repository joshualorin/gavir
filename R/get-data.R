
#' Set root for file paths
#'
#' This function just sets the root directory for filepaths. It simplifies data-loading and dynamically pulls in the username to the filepath.
#'
#' @param datasets set to TRUE if root should be the CPMM datasets folder, otherwise leave as FALSE to have root at CPMM folder.
#'
#' @return a vector with the filepath root.
#' @export
#'
#' @examples
#' set_root()
#' # can also save as root object to re-use when loading multiple data files
#' root <- set_root()

set_root <- function(datasets = F){

  os <- Sys.info()["sysname"]
  if(os == "Darwin"){user <- Sys.info()["user"]}else{Sys.getenv("USERNAME")}
  root_mac <- file.path("/Users", user, "Library", "CloudStorage", "OneDrive-SharedLibraries-Gavi", "Measurement, Evaluation and Learning - Documents")
  root_windows <- file.path("C:", "Users", user, "Gavi", "Measurement, Evaluation and Learning - CPMM")
  if(os == "Darwin"){base_root <- root_mac}else{base_root <- root_windows}
  root_datasets <- file.path(base_root, "CPMM", "Datasets")
  if(datasets == T){root_datasets}else{base_root}
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
#'

get_groupings <- function(version_date){

  root <- set_root()
  folders <- list.files(file.path(root, "CPMM", "Datasets", "Country Groupings"), pattern = "^20")
  max_date <- max(as.Date(paste(folders,"-01",sep="")))
  format_date <- format(max_date, "%Y-%m")

  path <- file.path(root, "CPMM", "Datasets", "Country Groupings", version_date, paste0("country_groupings_", version_date, ".rds"))
  msg <- paste0("Reading in groupings file dated: ", version_date)
  error_msg1 <- c("Oops! Must set a version date!")
  error_msg2 <- paste0("Oops! A groupings file dated ", version_date," does not exist. Date must be one of the following:")
  error_msg3 <- stringr::str_wrap(paste(folders, collapse=" "),1)
  error_msg4 <- paste0("Interesting! The groupings file you have selected is not the most recent. If this was unintentional, you can update to the most recent file, which is ", format_date)


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


#' Retrieve WUENIC data
#'
#' Retrieves WUENIC data from the shared drive. NOTE: Only works for 2021 or later versions.
#'
#' @param version_date A string in the format of "yyyy-mm", corresponding to the version of the WUENIC file you wish to load.
#' @param format A string used to determine which format of WUENIC file to load. Options are: "normal", "old_new", and "to_share".
#'
#' @return a WUENIC data frame
#' @export
#'
#' @examples
#'
#' get_wuenic("2022-07")
#' get_wuenic("2021-07", "old_new")

get_wuenic <- function(version_date, format = "normal"){

  root <- set_root()
  version_year <- stringr::str_sub(version_date, end = 4)

  if(format == "normal"){format <- "_"}else{format <- paste0("_", format, "_")}

  path <- file.path(root, "CPMM", "Datasets", "Coverage", "WUENIC", version_year, "outputs", paste0("wuenic", format, version_date, ".rds"))
  msg <- paste0("Reading in WUENIC file dated: ", version_date)

  message(msg)
  readRDS(path)

}

#' Retreive dates of JRF files
#'
#' This is a helper function to look at dates for the JRF files.
#'
#' @param source A string used to determine which format of JRF file to load. Options are: "admin" (default), "official", "system.", and "subnational".
#' @param latest Use TRUE if you only want latest file date.
#' @param guids Use TRUE if you want a file with GUIDs. Only relevant for subnational data.
#'
#' @return A vector of dates.
#' @export
#'
#' @examples
#'
#' get_jrf_dates()
#' get_jrf_dates("official")
#' get_jrf_dates("subnational", guids = TRUE)
#'
#' @importFrom rlang .data

get_jrf_dates <- function(source = "admin", latest = FALSE, guids = FALSE){

  root <- set_root()
  folders <- list.files(file.path(root, "CPMM", "Datasets", "Coverage", "JRF", stringr::str_to_title(source)), pattern = "^20")
  path_years <- folders[5:length(folders)]
  paths <- file.path(root, "CPMM", "Datasets", "Coverage", "JRF", stringr::str_to_title(source), path_years, "outputs")

  files <- unlist(lapply(paths, list.files))
  stringr::str_length(files)

  if(source == "admin"){
    name_length <- 24
  }else if(source == "official"){
    name_length <- 27
  }else if(source == "system"){
    name_length <- 21
  }else if(source == "subnational" & guids == F){
    name_length <- 30
  }else if(source == "subnational" & guids == T){
    name_length <- 36
  }

  dates <- dplyr::as_tibble(files) %>%
    dplyr::filter(stringr::str_detect(.data$value, ".rds")) %>%
    dplyr::filter(stringr::str_length(.data$value) == name_length) %>%
    dplyr::mutate(value = stringr::str_sub(.data$value, -14,-5)) %>%
    dplyr::pull()

  if(latest == T){max(dates)}else{dates}


}



#' Retrieve JRF data
#'
#' Retrieves JRF data from the shared drive. NOTE: Only works for 2021 or later versions.
#'
#' @param version_date A string in the format of "yyyy-mm-dd", corresponding to the version of the JRF file you wish to load.
#' @param source A string used to determine which format of JRF file to load. Options are: "admin" (default), "official", "system.", and "subnational".
#' @param guids Use TRUE if you want a file with GUIDs. Only relevant for subnational data.
#'
#' @return a JRF data frame.
#' @export
#'
#' @examples
#'
#' get_jrf("2022-08-03")
#' get_jrf("2022-09-02", "subnational", guids = TRUE)
#' # to dynamically always pull most recent file
#' get_jrf(get_jrf_dates("official", latest = TRUE), "official")

get_jrf <- function(version_date, source = "admin", guids = FALSE){

  root <- set_root()
  version_year <- stringr::str_sub(version_date, end = 4)

  if(source=="admin"){
    filename <- paste0("jrf_admin_", version_date, ".rds")
  }else if(source == "official"){
    filename <- paste0("official_jrf_", version_date, ".rds")
  }else if(source == "system"){
    filename <- paste0("system_", version_date, ".rds")
  }else if(source == "subnational"){
    filename <- paste0("jrf_subnational_", version_date, ".rds")
  }

  if(guids==TRUE){filename <- paste0("jrf_subnational_GUIDS_", version_date, ".rds")}

  path <- file.path(root, "CPMM", "Datasets", "Coverage", "JRF", stringr::str_to_title(source), version_year, "outputs", filename)
  msg <- paste0("Reading in JRF", stringr::str_to_title(source), "file dated: ", version_date)

  message(msg)
  readRDS(path)

}

