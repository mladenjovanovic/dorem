## code to prepare `injury-data` dataset

injury_data <- read.csv("data-raw/injury-data.csv", header = TRUE)

usethis::use_data(injury_data, overwrite = TRUE)
