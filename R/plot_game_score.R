#' Plot Chess Game with Score
#' @name plot_game_score
#' @description Takes a chess game number associated with the games dataset
#'   and a time delay between moves and generates the sequence of chess moves
#'   that make up that specific game. Above the game board a scoreboard is
#'   provided to track the number points each player has as the game progresses.
#' @param game_num The row number of the game dataset for plotting.
#' @param delay The time in seconds between each plot of a new chess move.
#' @return A sequence of chess moves characterizing the desired game, with a
#'   scoreboard displayed above it.
#' @import rchess
#' @import manipulateWidget
#' @examples
#' plot_game_score(game_num = 1, delay = 0)
#' @export

plot_game_score <- function(game_num = 1, delay = 0.5) {
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
      df[i + 1, 2] <- ifelse(df[i + 1, 13] == "White",
        df[i, 2] - df[i + 1, 14], df[i, 2]
      )
      df[i + 1, 3] <- ifelse(df[i + 1, 13] == "Black",
        df[i, 3] - df[i + 1, 14], df[i, 3]
      )
    }
  }

  df <- df[, 1:3]

  chss <- Chess$new()

  for (i in seq_len(l)) {
    eval(parse(text = paste0("chss", "$move(\"", v[i], "\")")))

    print(combineWidgets(plot(chss),
      title = paste0(
        "White: ", df[i, 2],
        ", Black: ", df[i, 3]
      )
    ))
    Sys.sleep(time = delay)
  }
}
