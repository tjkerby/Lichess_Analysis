#' Plot Chess Score
#' @name plot_score
#' @description Takes a chess game number associated with the games dataset
#'   and generates a plot of the score differential across moves.
#' @param game_num The row number of the game dataset for scoring.
#' @return A plot of chess scores across moves for the desired game.
#' @import rchess
#' @import ggplot2
#' @examples
#' plot_score(game_num = 100)
#' @export

plot_score <- function(game_num = 1) {
  g <- chess.analytics::games_clean[[game_num]][[1]]
  chss <- Chess$new()
  chss$load_pgn(g)
  ch <- chss$history(verbose = T)
  v <- ch$san
  chss <- Chess$new()
  l <- length(v)

  for (i in seq_len(l)) {
    eval(parse(text = paste0("chss", "$move(\"", v[i], "\")")))
  }

  score <- data.frame(move = seq_len(l), white = 39, black = 39)
  score$diff <- score$white - score$black


  check <- grep("x", v)

  if (length(check) == 0) {
    df <- score
  }

  if (length(check) > 0) {
    his <- chss$history_detail()

    h1 <- subset(his, !is.na(his$number_move_capture))

    h2 <- cbind(h1, t(as.data.frame(strsplit(h1$piece, split = " "))))
    rownames(h2) <- rownames(h1)
    colnames(h2)[9:10] <- c("id", "name")

    h2$color <- ifelse(h2$id %in%
      c(paste0(
        rep(letters[1:8], 2),
        rep(1:2, each = 8)
      ), "White"),
    "White", "Black"
    )

    h2$val <- ifelse(h2$name == "Pawn", 1,
      ifelse(h2$name == "Rook", 5,
        ifelse(h2$name == "Knight", 3,
          ifelse(h2$name == "Bishop", 3,
            ifelse(h2$name == "Queen", 9, 0)
          )
        )
      )
    )

    df <- merge(score, h2, by.x = "move", by.y = "number_move_capture", all = T)

    df$color <- ifelse(is.na(df$color), 0, df$color)

    for (i in 1:(nrow(df) - 1)) {
      df[i + 1, 2] <- ifelse(df[i + 1, 14] == "White",
                             df[i, 2] - df[i + 1, 15], df[i, 2])
      df[i + 1, 3] <- ifelse(df[i + 1, 14] == "Black",
                             df[i, 3] - df[i + 1, 15], df[i, 3])
    }

    df$diff <- df$white - df$black
  }

  if (max(abs(df$diff)) == 0) {
    ggplot(data = df, aes(x = move, y = diff, group = 1)) +
      geom_line() +
      geom_abline(intercept = 0, slope = 0) +
      ylim(-1, 1) +
      ylab("Black Lead          White Lead")
  } else {
    ggplot(data = df, aes(x = move, y = diff, group = 1)) +
      geom_line() +
      geom_abline(intercept = 0, slope = 0) +
      ylim(-max(abs(df$diff)), max(abs(df$diff))) +
      ylab("Black Lead          White Lead")
  }
}
