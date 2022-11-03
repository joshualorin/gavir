utils::globalVariables(c("groupings"))

#' Filter by gavi groups
#'
#' This function takes a data frame, adds the grouping file and filters it by the specified group and filter values. It then removes the filter group.
#'
#' @param data A data frame. Required to have a column labeled iso3 with iso3 country codes.
#' @param group A group which is in groupings file.
#' @param group_value A value or vector to filter the data by. Must correspond to values found in the group variable.
#'
#' @return A filtered data frame.
#' @export
#'
#' @examples
#'
#' wuenic %>%
#' filter_gavi_group(gavi57, 1)
#'
#' wuenic %>%
#' filter_gavi_group(who_region, c("AFRO", "EMRO"))
#'
#' @importFrom rlang .data

filter_gavi_group <- function(data, group, group_value){


  tryCatch(if(exists("groupings")==F)stop("Uh oh! "), error=function(e){cat("ERROR:",conditionMessage(e))})

  df <-  tryCatch(data %>%
                    dplyr::left_join(dplyr::select(groupings, .data$iso3, {{group}}), by = "iso3") %>%
                    dplyr::filter({{group}} %in% group_value) %>%
                    dplyr::select(-{{group}}), error = function(e){"No groupings file!!! Country Groupings file must be loaded in environment and called 'groupings'"})

  return(df)


}

