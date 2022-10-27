#' Gavi colors
#'
#' @param ... a list of gavi color labels
#'
#' @return a character vector with color names as labels and hex numbers as values
#' @export
#'
gavi_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (gavi_cols)

  gavi_cols[cols]
}


#' Gavi palette
#'
#' @param palette A name of a Gavi palette (eg. "all").
#' @param reverse Reverses order of the palette
#' @param ... I don't know.
#'
#' @return A character vector with all colors in the named pallette, with names as labels and hex numbers as values.
#' @export
#'
#' @importFrom  grDevices colorRampPalette
gavi_palette <- function(palette = "all", reverse = FALSE, ...) {
  pal <- gavi_pal[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

