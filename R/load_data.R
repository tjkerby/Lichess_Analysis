#' Load Data
#' @name load_data
#' @description Takes the data from the lichess dataset on kaggle and converts
#'   them into rchess objects and then extracts desired information in a
#'   memory efficient manner
#' @param split_games The lichess dataset moves for each game split by spaces
#' @param lichess_games the lichess dataset
#' @param num_games the number of games to be processed
#' @param estimator the number of iterations used to predict the time until
#'   completion.
#' @return A list of games where each game is a list that includes the pgn,
#'   the history, and other traits for subsetting.
#' @import rchess
load_data <- function(split_games, lichess_games,
                      num_games = length(split_games), estimator = 100) {
  games <- vector("list", length = num_games)
  start <- Sys.time()
  for (i in 1:num_games) {
    chss <- Chess$new()
    moves_command <- ifelse(chss$in_checkmate(), "", "chss")
    l <- length(split_games[[i]])
    for (j in seq_len(l)) {
      new_str <- paste0("$move(\"", split_games[[i]][j], "\")")
      moves_command <- paste0(moves_command, new_str)
    }
    tryCatch({
        s <- eval(parse(text = moves_command))
        pgn_data <- s$pgn()
        hist <- s$history(verbose = T)
        games[[i]] <- list(
          "pgn" = pgn_data, "history" = hist,
          "rated" = lichess_games$rated[i],
          "winner" = lichess_games$winner[i],
          "white_rating" = lichess_games$white_rating[i],
          "black_rating" = lichess_games$black_rating[i],
          "victory_type" = lichess_games$victory_status[i]
        )

      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
    progress(i, num_games)
    if (i == round(num_games / estimator)) {
      timer <- Sys.time()
      predicted_time <- (timer - start) * (num_games / estimator)
      print(c("The estimated time until completion in minutes is...",
              (predicted_time / 60)))
      print(predicted_time)
    }
  }
  end <- Sys.time()
  print(end - start)
  return(games)
}
