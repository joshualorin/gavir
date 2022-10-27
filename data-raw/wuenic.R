## code to prepare `wuenic` dataset goes here

library(tidyverse)

user <-  Sys.getenv("USERNAME")
os <- Sys.info()["sysname"]
user <- Sys.info()["user"]
root <- file.path("/Users", user, "Library", "CloudStorage", "OneDrive-SharedLibraries-Gavi", "Measurement, Evaluation and Learning - Documents")

wuenic_in <- read_rds(file.path(root, "CPMM", "Datasets", "Coverage", "WUENIC", "2022", "outputs", "wuenic_2022-07.rds"))[[1]]
wuenic <- wuenic_in %>% filter(vaccine %in% c("dtp1", "dtp3", "mcv1", "pcv3")) %>% select(iso3, year, vaccine, target, nvax, coverage)

usethis::use_data(wuenic, overwrite = TRUE)
