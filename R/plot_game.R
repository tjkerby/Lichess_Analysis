#' Plot Chess Game
#' @name plot_game
#' @description Takes a chess game number associated with the games dataset
#'   and a time delay between moves and generates the sequence of chess moves
#'   that make up that specific game.
#' @param game_num The row number of the game dataset for plotting.
#' @param delay The time in seconds between each plot of a new chess move.
#' @return A sequence of chess moves characterizing the desired game.
#' @import rchess
#' @examples
#' plot_game(game_num = 1, delay = 0)
#' @export

plot_game <- function(game_num = 1, delay = 0.5) {
  g <- chess.analytics::games_clean[[game_num]][[1]]
  chss <- Chess$new()
  chss$load_pgn(g)
  ch <- chss$history(verbose = T)
  v <- ch$san
  chss <- Chess$new()

  l <- length(v)

  for (i in seq_len(l)) {
    eval(parse(text = paste0("chss", "$move(\"", v[i], "\")")))
    print(plot(chss))
    Sys.sleep(time = delay)
  }
}
