##### Load Libraries #####
library(rchess)
library(svMisc)
library(hash)
library(rlist)
library(ggplot2)

##### Load Data #####
games <- read.csv("misc/games.csv", header = T)
split_games <- strsplit(games$moves, split = " ")

load_data <- function(split_games, lichess_games, num_games = length(split_games), estimator = 100) {
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
      games[[i]] <- list("pgn" = pgn_data, "history" = hist, "rated" = lichess_games$rated[i],
                         "winner" = lichess_games$winner[i], "white_rating" = lichess_games$white_rating[i],
                         "black_rating" = lichess_games$black_rating[i], "victory_type" = lichess_games$victory_status[i])
      # games[[i]] <- c(pgn_data, hist)#, games[i,]$rated, games[i,]$winner, games[i,]$white_rating, games[i,]$black_rating, games[i,]$victory_status)
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

# loaded_data <- load_data(split_games, games)
# save(loaded_data, file = "loaded_data.Rdata")
load("loaded_data.Rdata")

### Figure out where games are failing ###
indexes <- sapply(X=loaded_data, FUN = is.null)
sum(indexes) # The number of missing observations
filtered_data <- list.remove(loaded_data, indexes)
sum(sapply(X=filtered_data, FUN = is.null))

# save(filtered_data, file = "filtered_data.Rdata")

load("filtered_data.Rdata")

##### FUNCTIONS FOR PIECES #####
### KING ###
checkmate <- function(game, color = "W") {
  if (grepl("#", game$san[length(game$san)], fixed = T)) {
    if (game$color[length(game$san) - 1] == color) {
      return(TRUE)
    }
  }
  return(FALSE)
}

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

### Other pieces ###
piece_death <- function(game, piece = 'r', color = "w", position = 'l') {
  if (length(game) < 9) {
    return(FALSE)
  }
  initial_pos <- NULL
  if (piece == 'r') {
    if (color == 'w') {
      if (position == 'l') {initial_pos <- 'a1'} else {initial_pos <- 'h1'}
    } else {
      if (position == 'l') {initial_pos <- 'a8'} else {initial_pos <- 'h8'}
    }
  } else if (piece == 'b') {
    if (color == 'w') {
      if (position == 'l') {initial_pos <- 'c1'} else {initial_pos <- 'f1'}
    } else {
      if (position == 'l') {initial_pos <- 'c8'} else {initial_pos <- 'f8'}
    }
  } else if (piece == 'n') {
    if (color == 'w') {
      if (position == 'l') {initial_pos <- 'b1'} else {initial_pos <- 'g1'}
    } else {
      if (position == 'l') {initial_pos <- 'b8'} else {initial_pos <- 'g8'}
    }
  } else if (piece == 'q') {
    if (color == 'w') {initial_pos <- 'd1'} else {initial_pos <- 'd8'}
  }
  cur_pos <- initial_pos
  for (i in 1:length(game$san)) {
    if (game$from[i] == cur_pos) {
      cur_pos <- game$to[i]
    } else if (game$to[i] == cur_pos) {
      return(TRUE)
    }
  }
  return(FALSE)
}

piece_pos <- function(game, piece = 'r', color = "w", position = 'l') {
  initial_pos <- NULL
  if (piece == 'r') {
    if (color == 'w') {
      if (position == 'l') {initial_pos <- 'a1'} else {initial_pos <- 'h1'}
    } else {
      if (position == 'l') {initial_pos <- 'a8'} else {initial_pos <- 'h8'}
    }
  } else if (piece == 'b') {
    if (color == 'w') {
      if (position == 'l') {initial_pos <- 'c1'} else {initial_pos <- 'f1'}
    } else {
      if (position == 'l') {initial_pos <- 'c8'} else {initial_pos <- 'f8'}
    }
  } else if (piece == 'n') {
    if (color == 'w') {
      if (position == 'l') {initial_pos <- 'b1'} else {initial_pos <- 'g1'}
    } else {
      if (position == 'l') {initial_pos <- 'b8'} else {initial_pos <- 'g8'}
    }
  } else if (piece == 'q') {
    if (color == 'w') {initial_pos <- 'd1'} else {initial_pos <- 'd8'}
  }
  cur_pos <- initial_pos
  for (i in 1:length(game$san)) {
    if (game$from[i] == cur_pos) {
      cur_pos <- game$to[i]
    } else if (game$to[i] == cur_pos) {
      return(cur_pos)
    }
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

plot_event <- function(data, event, color = 'w', position = 'l', transformation = 'none') {
  # Initialize a blank board
  board_obj <- initialize_board()
  board <- board_obj$board
  board_ind <- board_obj$board_ind

  # Fill board with data
  if (event == "k") {
    for (i in 1:length(data)) {
      if (checkmate(data[[i]]$history, color)) {
        k_pos <- king_pos(data[[i]]$history)
        board[[k_pos]] <- board[[k_pos]] + 1
      }
    }
  } else {
    for (i in 1:length(data)) {
      if (piece_death(data[[i]]$history, piece = event, color = color, position = position) ) {
        piece_pos <- piece_pos(data[[i]]$history, piece = event, color = color, position = position)
        board[[piece_pos]] <- board[[piece_pos]] + 1
      }
    }
  }

  # Prep the data for plotting
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  for (i in keys(board)) {
    idx <- board_ind[[i]]
    value <- board[[i]]
    if (transformation == 'none') {
      trans_value = value
    } else if (transformation == 'sqrt') {
      trans_value = value ^.5
    } else if (transformation == 'log') {
      trans_value = log(1 + value)
    }
    df <- rbind(df, c(idx[1], idx[2], trans_value))
  }
  colnames(df) <- c("X", "Y", "event")


  # Plot the data
  ggplot(df, aes(X, Y)) + geom_tile(aes(fill = event)) + scale_fill_gradient(low = "white", high = "red")
}

plot_event(filtered_data, "b", 'w', 'r', 'none')

filtered_data[[2]]$winner

##### Example of recreating an rchess object from a pgn #####
a <- loaded_data[[1]][[1]]
chss <- Chess$new()
chss$load_pgn(a)
chss$history(verbose=T)
chss$plot()

