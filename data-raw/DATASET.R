## code to prepare `DATASET` dataset goes here

games <- read.csv("misc/games.csv", header = T)
split_games <- strsplit(games$moves, split = " ")
loaded_data <- load_data(split_games)

indexes <- sapply(X=loaded_data, FUN = is.null)
games_clean <- rlist::list.remove(loaded_data, indexes)

usethis::use_data(games_clean, overwrite = TRUE, compress = "bzip2")
