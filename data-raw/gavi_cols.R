## code to prepare `gavi_cols` dataset goes here

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

usethis::use_data(gavi_cols, overwrite = TRUE)
