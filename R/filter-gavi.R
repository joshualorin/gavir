utils::globalVariables(c("groupings"))

#' Filter by gavi groups
#'
#' This function takes a data frame, adds the grouping file and filters it by the specified group and filter values. It then removes the filter group.
#'
#' @param data A data frame. Required to have a column labeled iso3 with iso3 country codes.
#' @param group A group which is in groupings file.
#' @param group_value A value or vector to filter the data by. Must correspond to values found in the group variable.
#' @param drop defaults to TRUE, which drops the filter variable. Use FALSE if you wish to keep the filter varible.
#'
#' @return A filtered data frame.
#' @export
#'
#' @examples
#'
#' groupings <- get_groupings("2022-11")
#'
#' wuenic %>%
#' filter_gavi_group(gavi57, 1)
#'
#' wuenic %>%
#' filter_gavi_group(who_region, c("AFRO", "EMRO"))
#'
  #' @importFrom rlang .data

filter_gavi_group <- function(data, group, group_value, drop = TRUE){

  error <- tryCatch(if(exists("groupings")==F)stop(""), error=function(e){cat("Uh oh! You're missing the groupings file! Add it using get_groupings() ")})

  df1 <- tryCatch(expr = {data %>%
    dplyr::left_join(dplyr::select(groupings, .data$iso3, {{group}}), by = "iso3") %>%
    dplyr::filter({{group}} %in% group_value)}, error = function(e){cat("")})

  df2 <- df1 %>% dplyr::select(-{{group}})
  df <- if(drop == T){df2}else{df1}

  if(!is.null(error)){return(error)}else{return(df)}

}


#' Add Gavi groups
#'
#' This function takes a data frame and adds the specified Gavi groups to it.
#'
#'
#' @param data A data frame. Required to have a column labeled iso3 with iso3 country codes.
#' @param ... A group or groups which is in groupings file.
#'
#' @return A data frame with the added groups
#' @export
#'
#' @examples
#'
#' wuenic %>%
#' add_gavi_group(country, who_region)


add_gavi_group <- function(data, ...){

  error <- tryCatch(if(exists("groupings")==F)stop(""), error=function(e){cat("Uh oh! You're missing the groupings file! Add it using get_groupings() ")})

  df <- tryCatch(expr = {data %>%
      dplyr::left_join(dplyr::select(groupings, .data$iso3, ...), by = "iso3")},
      error = function(e){cat("")})

  if(!is.null(error)){return(error)}else{return(df)}

}


#' Add iso3 code to data
#'
#' This function adds the iso3 code to a data frame, using the country synonyms file to match on country.
#'
#' @param data A data frame. Required to have a column labeled country.
#'
#' @return A data frame with iso3 code.
#' @export
#'
#' @examples
#'
#' data.frame(country = c("DRC", "Congo", "Malawi"), value = 1) %>%
#' add_iso3()

add_iso3 <- function(data){

  root <- set_root()
  path <- file.path(root, "CPMM", "Datasets", "Country Groupings", "synonyms", "country_synonyms.xlsx")
  syn <- readxl::read_excel(path) %>% dplyr::select(iso3, country)

  if("country" %in% names(data) == F){
    message("Oops! You need a variable called 'country' in your data frame.")
  }else if("iso3" %in% names(data) == T){
    message("Oops! You already have a variable called 'iso3'")}
  else{

  df <- data %>%
    dplyr::left_join(syn, by = "country") %>%
    dplyr::select(iso3, tidyr::everything())}

  nas <- df %>% dplyr::filter(is.na(iso3))
  clist <- paste(unique(nas$country), collapse = ", ")
  if(nrow(nas)==0){
    message("Success! All rows matched with an iso3 code.")
    return(df)
  }else{
    message(paste0("Uh oh. Not all countries matched with an iso3 code. Here are the countries with no iso3: ", clist, ". Add these to the synonyms file with the correct iso3."))
  }

}


