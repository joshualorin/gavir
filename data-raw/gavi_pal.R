## code to prepare `gavi_pal` dataset goes here
# define various color paletes
# uses names found in the gavi


user <-  Sys.getenv("USERNAME")
os <- Sys.info()["sysname"]
user <- Sys.info()["user"]
root <- file.path("/Users", user, "Library", "CloudStorage", "OneDrive-SharedLibraries-Gavi", "Measurement, Evaluation and Learning - Documents")
source(file.path(root, "CPMM", "Misc", "R Functions", "functions.R"))

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


usethis::use_data(gavi_pal, overwrite = TRUE)
