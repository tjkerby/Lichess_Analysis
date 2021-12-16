#' Piece captured heatmap
#' @name plot_event
#' @description This function takes a list of games and plots a heatmap of
#'   where a specific piece is captured on the board.
#' @param data The list of games to look for the event
#' @param event The name of the piece that is captured. "k" = checkmate,
#'   "q" = queen, "r" = rook, "b" = bishop, "n" = knight
#' @param color The color of the piece that is captured. "w" = white,
#'   "b" = black
#' @param position The of the piece when there are two for a color. For
#'   instance the left rook. "l" = left, "r" = right
#' @param transformation How should the counts of the heatmap be transformed.
#'   "none" = count, "sqrt" = count^.5, "log" = log(count + 1)
#' @return A plot of chess scores across moves for the desired game.
#' @import rchess
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradient
#' @importFrom hash keys
#' @importFrom rlist list.remove
#' @examples
#' plot_event(games_clean, "k", "w", "l")
#' @export
plot_event <- function(data, event, color = "w", position = "l",
                       transformation = "none") {
  # Initialize a blank board
  board_obj <- initialize_board()
  board <- board_obj$board
  board_ind <- board_obj$board_ind

  # Fill board with data
  if (event == "k") {
    l <- length(data)
    for (i in seq_len(l)) {
      if (checkmate(data[[i]]$history, color)) {
        k_pos <- king_pos(data[[i]]$history)
        board[[k_pos]] <- board[[k_pos]] + 1
      }
    }
  } else {
    l <- length(data)
    for (i in seq_len(l)) {
      if (piece_death(data[[i]]$history,
        piece = event,
        color = color, position = position
      )) {
        piece_pos <- piece_pos(data[[i]]$history,
          piece = event, color = color,
          position = position
        )
        board[[piece_pos]] <- board[[piece_pos]] + 1
      }
    }
  }

  # Prep the data for plotting
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  for (i in hash::keys(board)) {
    idx <- board_ind[[i]]
    value <- board[[i]]
    if (transformation == "none") {
      trans_value <- value
    } else if (transformation == "sqrt") {
      trans_value <- value^.5
    } else if (transformation == "log") {
      trans_value <- log(1 + value)
    }
    df <- rbind(df, c(idx[1], idx[2], trans_value))
  }
  colnames(df) <- c("X", "Y", "event")


  # Plot the data
  ggplot(df, aes(X, Y)) +
    geom_tile(aes(fill = event)) +
    scale_fill_gradient(low = "white", high = "red")
}
