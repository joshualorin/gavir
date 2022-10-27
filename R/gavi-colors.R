# first make small table with all the gavi colors in it

gavi_cols <- c(
  `blue`       = "#005CB9",
  `light blue` = "#00A1DF",
  `green`      = "#95D600",
  `grey`       = "#878786",
  `purple`     = "#A51890",
  `pink`       = "#CE0F69",
  `cholera`    = "#AF5C37",
  `hepb`       = "#D50032",
  `hpv`        = "#F59BBB",
  `ipv`        = "#CEDC00",
  `ipv1`        = "#CEDC00",
  `ipv2`        = "#CEDC00",
  `je`         = "#B288B9",
  `mcv`        = "#0097A9",
  `mr`         = "#005A70",
  `mena`       = "#009639",
  `penta`      = "#653279",
  `pcv`        = "#0033A0",
  `rota`       = "#D86018",
  `yf`         = "#EAAA00",
  `yfv`         = "#EAAA00")

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
  `main` = gavi_colors("blue", "light blue", "green", "grey"),
  `strategy` = gavi_colors("purple", "pink", "green", "light blue"),
  `vaccine` = gavi_colors("cholera", "hepb", "hpv", "ipv", "je", "mcv", "mr", "mena", "penta", "pcv", "rota", "yf"),
  `blues` = gavi_colors("blue", "light blue"),
  `purples` = gavi_colors("purple", "pink"),
  `greens` = gavi_colors("mena", "mr"),
  `greens3` = gavi_colors("mena", "mr", "mcv"),
  `bluegreen` = gavi_colors("light blue", "green"),
  `redgreen` = gavi_colors("hepb", "mena"),
  `traffic` = gavi_colors("hepb", "yf", "mena"),
  `misc1` = gavi_colors("penta", "pcv", "mena", "rota", "yf"),
  `misc2` = gavi_colors("penta", "yf"))

# for use inside the main functions, so will not be exported (i.e. available to use for package users)
gavi_palette <- function(palette = "all", reverse = FALSE, ...) {
  pal <- gavi_pal[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}



#' Color scales based on Gavi color palette
#'
#' @param palette A palette. Defaults to all 21 Gavi colors, but other versions can be used. Full list: all, main, strategy, vaccine, blues, purples, greens, greens3, bluegreen, redgreen, traffic, misc1, misc2.
#' @param discrete Defaults to TRUE, keep if data is discrete. If continuous, used FALSE.
#' @param reverse Reverses order of palette.
#' @param ... Additional arguments passed to `discrete_scale()` or `scale_fill_gradientn()`, used respectively when discrete is TRUE or FALSE
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
scale_color_gavi <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- gavi_palette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("gavi_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill scales based on Gavi color palette
#'
#' @param palette A palette. Defaults to all 21 Gavi colors, but other versions can be used. Full list: all, main, strategy, vaccine, blues, purples, greens, greens3, bluegreen, redgreen, traffic, misc1, misc2.
#' @param discrete Defaults to TRUE, keep if data is discrete. If continuous, used FALSE.
#' @param reverse Reverses order of palette.
#' @param ... Additional arguments passed to `discrete_scale()` or `scale_fill_gradientn()`, used respectively when discrete is TRUE or FALSE
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
#'# Example two - the "greens" palette has only two colors, so palette interpolates shades
#' wuenic %>%
#'  filter(iso3 == "AFG" & year == 2021) %>%
#'  ggplot(aes(vaccine, coverage, fill = vaccine)) +
#'  geom_bar(stat = "identity") +
#'  scale_fill_gavi(palette = "greens", reverse = TRUE)

scale_fill_gavi <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- gavi_palette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("gavi_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}



