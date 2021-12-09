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
    #par(mar = c(0,0,0,0))
    #layout(matrix(c(1, 1, 1, 2, 1, 1, 1, 3), nrow = 2, byrow = TRUE))
    print(plot(chss))
    Sys.sleep(time = delay)
  }
}

plot_game(game_num = 8258, delay = 0.5)
games$moves[8258]
nrow(games)

chss <- Chess$new()
par(mar = c(0,0,0,0))
par(mfrow = c(1, 2))
layout(matrix(c(1, 1, 1, 2,
                1, 1, 1, 3), nrow = 2, byrow = TRUE))
p <- plot(chss)
p
plot(hist(iris$Sepal.Length))
plot(hist(iris$Sepal.Length))

image(p)
