#' Gavi ggplot2 theme
#'
#' A simple theme based on [hrbrthemes::theme_ipsum()] with slight tweaks.
#'
#' @export
#'
theme_gavi <- function(){

  hrbrthemes::theme_ipsum(axis_title_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.2,0.2,0.2,0,2), "cm"))

}



#' Gavi flextable theme
#'
#' A theme which applies some pretty defaults to a flextable.
#'
#' @param ft A flextable object.
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(flextable)
#' library(officer)
#'
#' wuenic %>%
#'   filter(year == 2020 & iso3 %in% c("COD", "ETH", "IND", "PAK") & vaccine == "dtp3") %>%
#'   select(iso3, coverage) %>%
#'   flextable() %>%
#'   add_header_lines("DTP3 coverage in high impact countries") %>%
#'   add_footer_lines("Source: WUENIC, July 2021") %>%
#'   theme_gavi_table()
#'
theme_gavi_table <-  function(ft){

  fp_border <- NULL
  ft <- ft
  ncols <- flextable::ncol_keys(ft)

  ft %>%
    flextable::theme_alafoli() %>%                                       # Convert to simple theme as base
    flextable::fontsize(i = 1, size = 14, part = "header") %>%           # Make the header text larger
    flextable::bold(i = 1, part = "header") %>%                          # Make header text bold
    flextable::color(part = "header", color = "black") %>%               # Make header text color black (instead of dark grey)
    flextable::hline_bottom(part = "header",
                            border = officer::fp_border(width = 2)) %>%           # Add a bottom border to the header
    flextable::line_spacing(part = "header", space = .9) %>%             # Shrink white space between header and body
    flextable::padding(part = "header",                                  # Remove padding in header, needed for when output to ppt
                       padding.top = 0,
                       padding.bottom = 0) %>%
    flextable::align(j = 1, align = "left", part = "all") %>%            # Left align first column
    flextable::align(j = 2:ncols, align = "center", part = "all") %>%    # Center align all the rest
    flextable::hline(part = "body",
                     border = officer::fp_border(color = "grey", width = 1)) %>%  # Add border at bottom
    flextable::color(part = "footer", color = "#666666") %>%             # Modify footer color to be light grey
    flextable::fontsize(part = "footer", size = 8) %>%                   # Make the footer text smaller
    flextable::padding(part = "footer",                                  # Remove padding in footer, needed for when output to ppt
                       padding.top = 0,
                       padding.bottom = 0)


}
