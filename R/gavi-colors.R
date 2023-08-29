# first make small table with all the gavi colors in it

gavi_cols <- c(
  `org blue`         = "#005CB9",
  `org light blue`   = "#00A1DF",
  `org green`        = "#83BD00",
  `org grey`         = "#878786",
  `org purple`       = "#A51890",
  `org pink`         = "#CE0F69",
  `org dark green`   = "#3E9B6E",
  `org red`          = "#E24A3F",
  `org orange`       = "#FA7650",
  `org yellow`       = "#F8A623",
  `org light yellow` = "#FED141",
  `map blue`         = "#3D649D",
  `map green`        = "#1a9641",
  `map turquoise`    = "#0097a9",
  `map yellow`       = "#FFE291",
  `map white`        = "#f7f7f7",
  `map orange`       = "#D86018",
  `map red`          = "#B03027",
  `map blues1`       = "#C4E3D9",
  `map blues2`       = "#385A83",
  `map reds1`        = "#FFFFCC",
  `map reds2`        = "#800026",
  `map greens1`      = "#edf8e9",
  `map greens2`      = "#006d2c",
  `blue`             = "#1A3C58",
  `light blue`       = "#4DA9E3",
  `purple`           = "#784AA2",
  `green`            = "#73AA8A",
  `pink`             = "#B57E95",
  `yellow`           = "#F4DE7D",
  `orange`           = "#DB9C51",
  `red`              = "#DB655C",
  `brown`            = "#BF9E73",
  `turquoise`        = "#60D0D2",
  `cholera`          = "#A25A3A",
  `hepb`             = "#D50032",
  `hpv`              = "#F59BBB",
  `ipv`              = "#CEDC00",
  `je`               = "#B288B9",
  `mcv`              = "#0097A9",
  `mr`               = "#005A70",
  `mena`             = "#009639",
  `penta`            = "#653279",
  `pcv`              = "#0033A0",
  `rota`             = "#D86018",
  `yf`               = "#EAAA00",
  `tcv`              = "#9B5638",
  `malaria`          = "#D44237",
  `ebola`            = "#D54D6B",
  `core`             = "#D86018",
  `high impact`      = "#653279",
  `fragile`          = "#009639",
  `never gavi mics`  = "#005cb9",
  `non gavi`         = "#00A1DF",
  `post transition`  = "#cedc00",
  `afro`             = "#D86018",
  `emro`             = "#653279",
  `euro`             = "#ce0f69",
  `paho`             = "#009639",
  `searo`            = "#005cb9",
  `wpro`             = "#cedc00",
  `hic`              = "#d86018",
  `umic`             = "#eaaa00",
  `lmic`             = "#95d600",
  `lic`              = "#005cb9")

#then make initial function
#' Gavi colors
#'
#' @param ... a list of gavi color labels
#'
#' @return a character vector with color names as labels and hex numbers as values
#' @export
#'
#' @examples
#'# Example one - use to get hex number for gavi color
#' gavi_colors("blue")
#'
#'# Example two - use with a geom call to provide color
#' library(dplyr)
#' library(ggplot2)
#' wuenic %>%
#'   filter(iso3 == "AFG" & vaccine == "dtp3") %>%
#'   ggplot(aes(year, coverage)) +
#'   geom_line(color = gavi_colors("penta"))
#'
#'# Example three - pair with `as_vector()` to manually set colors
#' wuenic %>%
#'   filter(iso3 == "AFG" & vaccine %in% c("dtp3", "mcv1")) %>%
#'   ggplot(aes(year,coverage, color = vaccine)) +
#'   geom_line() +
#'   geom_point() +
#'   scale_color_manual(values = as.vector(gavi_colors("penta", "mcv")))


gavi_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (gavi_cols)

  gavi_cols[cols]
}

# make a list of all the color palettes
gavi_pal <- list(
  `all` = gavi_colors(),
  `standard` = gavi_colors("org purple", "org blue", "org light blue", "org dark green", "org green", "org light yellow", "org yellow", "org orange", "org red"),
  `millennial` = gavi_colors("blue", "light blue", "pink", "green", "yellow", "orange", "purple", "red", "turquoise"),
  `main` = gavi_colors("org blue", "org light blue", "org green", "org grey"),
  `strategy` = gavi_colors("org purple", "org pink", "org green", "org light blue"),
  `vaccine` = gavi_colors("cholera", "ebola", "hepb", "hpv", "ipv", "je", "malaria", "mcv", "mr", "mena", "penta", "pcv", "rota", "tcv", "yf"),
  `who_region` = gavi_colors("afro", "emro", "euro", "paho", "searo", "wpro"),
  `gavi_segment`= gavi_colors("core", "fragile", "high impact", "never gavi mics", "non gavi", "post transition"),
  `world_bank` = gavi_colors("hic", "umic", "lmic", "lic"),
  `red_green` = gavi_colors("hepb", "mena"),
  `traffic` = gavi_colors("hepb", "yf", "mena"),
  `misc1` = gavi_colors("penta", "pcv", "mena", "rota", "yf"),
  `misc2` = gavi_colors("penta", "yf"),
  `map_blue_red` = gavi_colors("map blue", "map yellow", "map red"),
  `map_blue_red2` = gavi_colors("map blue", "map white", "map red"),
  `map_orange_turquoise` = gavi_colors("map orange", "map white", "map turquoise"),
  `map_green_red` = gavi_colors("map green", "map yellow", "map red"),
  `map_blues` = gavi_colors("map blues1", "map blues2"),
  `map_reds` = gavi_colors("map reds1", "map reds2"),
  `map_greens` = gavi_colors("map greens1", "map greens2"))

# for use inside the main functions, so will not be exported (i.e. available to use for package users)
# two different functions, depending on if discrete or continuous
palette_continuous <- function(palette = "all", reverse = FALSE, ...) {
  pal <- gavi_pal[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

palette_discrete <- function(palette = "all", reverse = FALSE){
  pal <- gavi_pal[[palette]]

  if (reverse) pal <- rev(pal)

  scales::manual_pal(pal)}


#' Plot Gavi color scales
#'
#' Helper function to assist in choosing a Gavi color or palette.
#'
#' @param pal_name defaults to all colors. Can choose from a list of available palettes:
#' all, standard, main, strategy, vaccine, blues, purples, greens, greens3, bluegreen, redgreen, traffic, misc1, misc2, map, map blues
#'
#' @export
#'
#' @examples
#' plot_gavi_colors()
#' plot_gavi_colors("vaccine")
#'
plot_gavi_colors <- function(pal_name = "all"){

  desc <- n <- color <- label <- value <-  hex <- NULL
  df_cols <- gavi_pal[[pal_name]] %>%
    tibble::enframe(name = "color", value = "hex") %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    dplyr::mutate(value = 1) %>%
    dplyr::mutate(label = .5) %>%
    dplyr::mutate(label_text = paste0(color, " (", hex, ")")) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::mutate(color = factor(color, levels = color))


  p <- ggplot2::ggplot(df_cols, ggplot2::aes(color, value, fill = color)) +
    ggplot2::geom_bar(stat = "identity", width = 1, position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_text(ggplot2::aes(y = label), label = df_cols$label_text, color = "white") +
    ggplot2::scale_fill_manual(values = rev(as.vector(gavi_pal[[pal_name]]))) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "none",
          axis.ticks = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank())

  return(p)

}




#' Color scales based on Gavi color palette
#'
#' @param palette A palette. Defaults to standard palette, but other palettes also available. Full list: all, standard, millennial, main, strategy, vaccine, who_region, gavi_segment, world_bank, red_green, traffic, misc1, misc2, map_blue_red, map_blue_red2, map_orange_turquoise, map_green_red, map_blues, map_reds, map_greens.
#' @param discrete Defaults to TRUE, keep if data is discrete. If continuous, used FALSE.
#' @param reverse Reverses order of palette.
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()] or [ggplot2::scale_fill_gradientn()], used respectively when discrete is TRUE or FALSE
#'
#' @export
#'
#' @examples
#'# Example one - applied to discrete data
#' library(dplyr)
#' library(ggplot2)
#' wuenic %>%
#'  filter(iso3 == "AFG" & vaccine %in% c("dtp1", "dtp3")) %>%
#'  ggplot(aes(year, coverage, color = vaccine)) +
#'  geom_line() +
#'  scale_color_gavi()
#'
#'# Example two - with different palette, and continuous data
#' wuenic %>%
#'  filter(vaccine == "dtp3" & !is.na(coverage)) %>%
#'  ggplot(aes(year, coverage, color = coverage)) +
#'  geom_point() +
#'  scale_color_gavi(palette = "traffic", discrete = FALSE)
#'
scale_color_gavi <- function(palette = "standard", discrete = TRUE, reverse = FALSE, ...) {
  if (discrete) {
    pal <- palette_discrete(palette = palette, reverse = reverse)
    ggplot2::discrete_scale("colour", paste0("gavi_", palette), palette = pal, ...)
  }
  else {
    pal <- palette_continuous(palette = palette, reverse = reverse)
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill scales based on Gavi color palette
#'
#' @param palette A palette. Defaults to standard palette, but other palettes also available. Full list: all, standard, millennial, main, strategy, vaccine, who_region, gavi_segment, world_bank, red_green, traffic, misc1, misc2, map_blue_red, map_blue_red2, map_orange_turquoise, map_green_red, map_blues, map_reds, map_greens.
#' @param discrete Defaults to TRUE, keep if data is discrete. If continuous, used FALSE.
#' @param reverse Reverses order of palette.
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()] or [ggplot2::scale_fill_gradientn()], used respectively when discrete is TRUE or FALSE
#'
#' @export
#'
#' @examples
#'# Example one - use main palette as output has four colors needed
#'library(dplyr)
#'library(ggplot2)
#'wuenic %>%
#'  filter(iso3 == "AFG" & year == 2021) %>%
#'  ggplot(aes(vaccine, coverage, fill = vaccine)) +
#'  geom_bar(stat = "identity") +
#'  scale_fill_gavi(palette = "main", reverse = TRUE)
#'
#'# Example two - the "misc1" palette has five colors, so palette takes first four shades
#' wuenic %>%
#'  filter(iso3 == "AFG" & year == 2021) %>%
#'  ggplot(aes(vaccine, coverage, fill = vaccine)) +
#'  geom_bar(stat = "identity") +
#'  scale_fill_gavi(palette = "misc1", reverse = TRUE)

scale_fill_gavi <- function(palette = "standard", discrete = TRUE, reverse = FALSE, ...) {
  if (discrete) {
    pal <- palette_discrete(palette = palette, reverse = reverse)
    ggplot2::discrete_scale("fill", paste0("gavi_", palette), palette = pal, ...)
  }
  else {
    pal <- palette_continuous(palette = palette, reverse = reverse)
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}





