## code to prepare `strength_training` dataset

strength_training <- read.csv("strength_training.csv", header = TRUE)

usethis::use_data(strength_training, overwrite = TRUE)
