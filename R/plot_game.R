#' Plot Chess Game
#' @name plot_game
#' @description Takes a chess game number associated with the games dataset
#'   and a time delay between moves and generates the sequence of chess moves
#'   that make up that specific game.
#' @param game_num The row number of the game dataset for plotting.
#' @param delay The time in seconds between each plot of a new chess move.
#' @return A sequence of chess moves characterizing the desired game.
#' @examples
#' plot_game(game_num = 1, delay = 0)
#' @export


library(rchess)

plot_game <- function(game_num = 1, delay = 0.5){
  v <- strsplit(games[game_num, ]$moves, split = " ")
  chss <- Chess$new()
  l <- length(v[[1]])

  for(i in seq_len(l)){
    s <- eval(parse(text = paste0("chss", "$move(\"", v[[1]][i], "\")")))
    print(plot(chss))
    Sys.sleep(time = delay)
  }
}

plot_game(game_num = 1, delay = 0.5)
