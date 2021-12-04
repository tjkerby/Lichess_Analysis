library(rchess)
library(svMisc)

games <- read.csv("misc/games.csv", header = T)
split_games <- strsplit(games$moves, split = " ")
split_games <- split_games[1:8250]
# split_games <- split_games[-8251]

load_data <- function(split_games, num_games = length(split_games), estimator = 100) {
  games <- vector("list", length = num_games)
  start <- Sys.time()
  for (i in 1:num_games) {
    chss <- Chess$new()
    moves_command <- "chss"
    for (j in 1:length(split_games[[i]])) {
      new_str <- paste0("$move(\"", split_games[[i]][j],"\")")
      moves_command <- paste0(moves_command, new_str)
    }
    s <- eval(parse(text = moves_command))
    pgn_data <- s$pgn()
    hist <- s$history(verbose = T)
    games[[i]] <- c(pgn_data, hist)
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

save(loaded_data, file = "loaded_data.Rdata")

a <- loaded_data[[1]][[1]]
chss <- Chess$new()
chss$load_pgn(a)
chss$history(verbose=T)







game_1 <- loaded_data[[2]]
game_1$ascii() # prints the board
game_1$get('a1') # gets the piece in the position
game_1$game_over()
game_1$summary()
game_1$history_detail()
a <- game_1$history(verbose = T)
class(a)

game_1$moves(verbose = T)

a <- game_1$fen()

chss <- Chess$new()
chss$load(a)
# usethis::use_data(loaded_data, overwrite = TRUE)

chss <- Chess$new()
chss$move("e4")$move("d6")$move("Bc4")$move("f5")$move("exf5")$move("Nf6")$move("d3")$move("d5")$move("Bb3")$move("Bxf5")$move("Nf3")$move("Nc6")$move("Ng5")$move("h6")$move("Nf3")$move("g5")$move("O-O")$move("d4")$move("c3")$move("e5")$move("cxd4")$move("exd4")
chss$plot()
a <- chss$history(verbose = T)
tail(a)
chss$move("Re1+")

split_games[[8251]]
