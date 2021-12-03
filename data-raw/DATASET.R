## code to prepare `DATASET` dataset goes here

games <- read.csv("misc/games.csv", header = T)

usethis::use_data(games, overwrite = TRUE)
