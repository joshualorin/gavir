
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
  if(os == "Darwin"){user <- Sys.info()["user"]}else{user <- Sys.getenv("USERNAME")}
  root_mac <- file.path("/Users", user, "Library", "CloudStorage", "OneDrive-SharedLibraries-Gavi", "Measurement, Evaluation and Learning - Documents")
  root_windows <- file.path("C:", "Users", user, "Gavi", "Measurement, Evaluation and Learning - Documents")
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
  msg <- paste0("Reading in JRF ", stringr::str_to_title(source), " file dated: ", version_date)

  message(msg)
  readRDS(path)

}


#' Retrieve Survey data
#'
#' Retrieves Survey data from the shared drive. NOTE: Only works for 2021 or later versions.
#'
#' @param version_date A string in the format of "yyyy-mm", corresponding to the version of the survey file you wish to load.
#' @param source A string used to determine which survey file to load. Options are: "national" and "equity".
#'
#' @return a survey data frame.
#' @export
#'
#' @examples
#'
#' get_survey("2022-08")
#' get_survey("2022-08", source = "equity")
#'

get_survey <- function(version_date, source = "national"){

  root <- set_root()

  if(source=="national"){
    filename <- paste0("survey_long_", version_date, ".rds")
  }else if(source == "equity"){
    filename <- paste0("survey_equity_", version_date, ".rds")
  }

  path <- file.path(root, "CPMM", "Datasets", "Coverage", "Survey", "Database", version_date, "outputs", filename)
  msg <- paste0("Reading in ", stringr::str_to_title(source), " Survey file dated: ", version_date)

  message(msg)
  readRDS(path)

}

#' Retrieve IHME data
#'
#' Retrieves IHME data from the shared drive.
#'
#' @param version_date A string in the format of "yyyy-mm", corresponding to the version of the IHME file you wish to load.
#' @param source A string used to determine which IHME file to load. Options are: "national", "subnational", "raster", and "citations".
#' @param aggregation A string used to determine which aggregation to use when loading subnational data. Options are: "polio" or "gadm"
#' @param vaccine A string used to detemine which vaccine to use when loading raster or citation data. Use standard vaccine format (e.g. "dtp1")
#'
#' @return a data frame or raster object.
#' @export
#'
#' @examples
#'
#' #defaults load subnational file aggregated to polio shapes
#' get_ihme("2022-12")
#'
#' get_ihme("2022-11", "national")
#' get_ihme("2022-12", "raster", vaccine = "dtp1")
#' get_ihme("2022-12", "citations", vaccine = "dtp1")
#'
get_ihme <- function(version_date, source = "subnational", aggregation = "polio", vaccine = NULL){

 if(is.null(vaccine) & source %in% c("raster", "citations")){stop("Uh oh! You must add in vaccine in the vaccine argument")}

  root <- file.path(set_root(datasets = T), "Coverage", "IHME")
  vaccine <- stringr::str_replace(vaccine, "dtp", "dpt")
  vaccine_no_dose <- stringr::str_sub(vaccine, end = 3)

  if(source=="subnational"){
    path <- file.path(root, "Subnational", version_date, paste0(aggregation, " aggregations"), "outputs", paste0("ihme_subnational_coverage_", aggregation, "_", version_date, ".csv"))
  }else if(source == "national"){
    path <- file.path(root, "National", version_date, "outputs", paste0("ihme_national_coverage_", version_date, ".csv"))
  }else if(source == "raster"){
    path <- file.path(root, "Subnational" , version_date, "rasters", paste0(vaccine, "_cov_mean_raked_2000_2021.tif"))
  }else if(source == "citations"){
    path <- file.path(root, "Subnational", version_date, "citations", paste0(vaccine_no_dose, "_coverage_estimates_survey_citations.csv"))
  }

  msg1 <- paste0("Reading in ", stringr::str_to_title(source), " IHME file dated ", version_date, " and aggregated to ", aggregation, " shapes")
  msg2  <- paste0("Reading in ", stringr::str_to_title(source), " IHME file dated: ", version_date)
  msg3  <- paste0("Reading in ", stringr::str_to_title(vaccine),  " ", stringr::str_to_title(source), " IHME file dated: ", version_date)
  msg4  <- paste0("Reading in ", stringr::str_to_title(vaccine),  " Survey ", stringr::str_to_title(source), " file dated: ", version_date)

  if(source == "subnational"){
    message(msg1)
  }else if(source == "national"){
      message(msg2)
  }else if(source == "raster"){
    message(msg4)
  }else if(source == "citations"){
    message(msg4)
  }

  if(source!="raster"){readr::read_csv(path)}else{raster::brick(path)}

}



#' Retrieve Shape data
#'
#' Retrieves Shape data from the shared drive.
#'
#' @param version_date A string in the format of "yyyy-mm", corresponding to the version of the shape file you wish to load.
#' @param source A string used to determine which shape file to load. Options are: "polio" and "gadm".
#' @param layer A string used to determine which layer to load. Options are: "admin0", "admin1", and "admin2"
#' @param latest Defaults to TRUE. If loading polio shapes, this ensures you only use the latest shape version for each country
#' for a given version_date. If set to FALSE, you will load the file which has all shape file versions available for a given version date.
#'
#' @return A sf object
#' @export
#'
#' @examples
#'
#' # Since defaults are set to polio, admin2, and latest shape version, you can just provide version_date:
#' get_shapes("2022-03")
#'
#' # Note for GADM, no version date needed
#' get_shapes(source = "gadm")
#'
#' # can also add arguments to override defaults
#' get_shapes("2022-03", layer = "admin1", latest = FALSE)
#'


get_shapes <- function(version_date = NULL, source = "polio", layer = "admin2", latest = T){

  if(is.null(version_date) & source == "polio"){stop("Uh oh! You must add in a date in the version_date argument")}

  root <- set_root(datasets = T)

  if(source=="polio" & latest == T){
    path <- file.path(root, "Shapes", "Polio", version_date, "outputs", paste0("polio_shapes_latest_", version_date, ".gpkg"))
  }else if(source == "polio" & latest == F){
    path <- file.path(root, "Shapes", "Polio", version_date, "outputs", paste0("polio_shapes_", version_date, ".gpkg"))
  }else if(source == "gadm"){
    path <- file.path(root, "Shapes", "GADM", "gadm_shapes.gpkg")
  }

  msg1 <- paste0("Reading in ", stringr::str_to_title(source), " ", layer, " shape file dated: ", version_date)
  msg2 <- paste0("Reading in ", stringr::str_to_upper(source), " ", layer, " shape file")
  if(source == "polio"){message(msg1)}else{message(msg2)}
  sf::read_sf(path, layer = layer)

}











