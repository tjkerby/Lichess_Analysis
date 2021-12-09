##### Load Libraries #####
library(rchess)
library(svMisc)
library(hash)
library(rlist)

##### Load Data #####
games <- read.csv("misc/games.csv", header = T)
split_games <- strsplit(games$moves, split = " ")

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

# loaded_data <- load_data(split_games)

### Figure out where games are failing ###
# indexes <- sapply(X=loaded_data, FUN = is.null)
# sum(indexes) # The number of missing observations
# filtered_data <- list.remove(loaded_data, indexes)
# sum(sapply(X=filtered_data, FUN = is.null))
#
# save(filtered_data, file = "filtered_data.Rdata")

load("filtered_data.Rdata")

##### Where is the King Checkmated? #####
### Did the game end in checkmate? ###
checkmate <- function(game) {
  grepl("#", game$san[length(game$san)], fixed = T)
}

### Get position of the king at the end of the game ###
king_pos <- function(game) {
  for (i in length(game$san):1) {
    if (game$piece[i] == "k") {
      return(game$to[i])
    }
  }
  if (game$color[length(game$san)] == "b") {
    return("e1")
  } else {
    return("e8")
  }
}

### Store a board to count where the king is when checkmated ###
initialize_board <- function() {
  board <- hash()
  board_ind <- hash()
  board_rows <- c("a", "b", "c", "d", "e", "f", "g", "h")
  board_cols <- c("1", "2", "3", "4", "5", "6", "7", "8")
  i <- 1
  for (letter in board_rows) {
    j <- 1
    for (number in board_cols) {
      board[[paste(letter, number, sep = "")]] <- 0
      board_ind [[paste(letter, number, sep = "")]] <- c(i, j)
      j <- j + 1
    }
    i <- i + 1
  }
  return(list('board' = board, 'board_ind' = board_ind))
}


##### Run on data #####
board_obj <- initialize_board()
board <- board_obj$board
board_ind <- board_obj$board_ind

for (i in 1:length(filtered_data)) {
  if (checkmate(filtered_data[[i]])) {
    k_pos <- king_pos(filtered_data[[i]])
    board[[k_pos]] <- board[[k_pos]] + 1
  }
}

##### Plot count data #####
df <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in keys(board)) {
  idx <- board_ind[[i]]
  df <- rbind(df, c(idx[1], idx[2], board[[i]]))
}
colnames(df) <- c("X", "Y", "CHECKMATE")
library(ggplot2)
ggplot(df, aes(X, Y)) + geom_tile(aes(fill = CHECKMATE)) + scale_fill_gradient(low = "white", high = "red")

##### Look when the king is checked #####
get_check_index <- function(game) {
  game[[1]] <- gsub("\n", " ", game[[1]])
  moves <- strsplit(game[[1]], " ")
  moves <- moves[[1]]
  check_indexes <- c()
  offset <- 0
  for (i in 1:length(moves)) {
    if (grepl(".", moves[i], fixed=T) == F) {
      if (grepl("+", moves[i], fixed=T)) {
        check_indexes <- c(check_indexes, i-offset)
        print(moves[i])
      }
    } else {
      offset <- offset + 1
    }
  }
  return(check_indexes)
}

##### Example of recreating an rchess object from a pgn #####
a <- loaded_data[[1]][[1]]
chss <- Chess$new()
chss$load_pgn(a)
chss$history(verbose=T)

