## code to prepare `DATASET` dataset goes here

games <- read.csv("misc/games.csv", header = T)
split_games <- strsplit(games$moves, split = " ")

load_data <- function(split_games, num_games = length(split_games),
                      estimator = 100) {
  games <- vector("list", length = num_games)
  start <- Sys.time()
  for (i in 1:num_games) {
    chss <- Chess$new()
    moves_command <- "chss"
    for (j in 1:length(split_games[[i]])) {
      new_str <- paste0("$move(\"", split_games[[i]][j],"\")")
      moves_command <- paste0(moves_command, new_str)
    }
    tryCatch({
      s <- eval(parse(text = moves_command))
      pgn_data <- s$pgn()
      hist <- s$history(verbose = T)
      games[[i]] <- c(pgn_data, hist)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    progress(i, num_games)
    if (i == round(num_games/estimator)) {
      timer = Sys.time()
      predicted_time <- (timer - start)*(num_games/estimator)
      print(c("The estimated time until completion in minutes is...", (predicted_time/60)))
      print(predicted_time)
    }
  }
  end <- Sys.time()
  print(end - start)
  return(games)
}

loaded_data <- load_data(split_games)

indexes <- sapply(X=loaded_data, FUN = is.null)
filtered_data <- list.remove(loaded_data, indexes)

# save(filtered_data, file = "filtered_data.Rdata")

games_clean <- filtered_data

usethis::use_data(games_clean, overwrite = TRUE)
