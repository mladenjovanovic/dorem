## code to prepare `bike_score` dataset

bike_score <- read.csv("data-raw/bike-score.csv", header = TRUE)

usethis::use_data(bike_score, overwrite = TRUE)
