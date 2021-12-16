#### HELPER FUNCTIONS #####
#' Checkmate
#'
#' This function helps to find the games that ended in a checkmate
#'
#' @param game information about the game
#' @param color a string argument that takes a letter of 'W' for White
#'    and 'B' for black.
#' @return A boolean logic of TRUE or FALSE

### KING ###
checkmate <- function(game, color = "W") {
  if (grepl("#", game$san[length(game$san)], fixed = T)) {
    if (game$color[length(game$san) - 1] == color) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' King Position
#'
#' This function helps to find the kings position
#'
#' @param game information about the game
#'
#' @return A boolean logic of TRUE or FALSE
#'
king_pos <- function(game) {
  l <- length(game$san)
  for (i in l:1) {
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
#' Helper function to find death of a piece in a chess game
#'
#' This function takes the users input of game, piece, color, and position and
#'   finds where that piece died in a game.
#'
#' @param game information about the game
#' @param piece a lower case character input of one of the following:
#'   'r' for rook, 'n' for knight, 'b' for bishop.
#' @param color a string argument that takes a letter of 'W' for White
#'    and 'B' for black.
#' @param position a character of 'l' for left and 'r' for right side
#'
#' @return A boolean logic of TRUE or FALSE

piece_death <- function(game, piece = "r", color = "w", position = "l") {
  if (length(game) < 9) {
    return(FALSE)
  }

  initial_pos <- NULL
  if (piece == "r") {
    initial_pos <- ifelse(color == "w", ifelse(position == "l", "a1", "h1"),
                          ifelse(position == "l", "a8", "h8"))
  } else if (piece == "b") {
    initial_pos <- ifelse(color == "w", ifelse(position == "l", "c1", "f1"),
                          ifelse(position == "l", "c8", "f8"))
  } else if (piece == "n") {
    initial_pos <- ifelse(color == "w", ifelse(position == "l", "b1", "g1"),
                          ifelse(position == "l", "b8", "g8"))
  } else if (piece == "q") {
    initial_pos <- ifelse(color == "w", "d1", "d8")
  }
  cur_pos <- initial_pos
  l1 <- length(game$san)
  for (i in seq_len(l1)) {
    if (game$from[i] == cur_pos) {
      cur_pos <- game$to[i]
    } else if (game$to[i] == cur_pos) {
      return(TRUE)
    }
  }
  return(FALSE)
}

### Other pieces ###
#' Helper function to find death of a piece in a chess game
#'
#' This function takes the users input of game, piece, color, and position and
#'   finds where that piece died in a game.
#'
#' @param game information about the game
#' @param piece a lower case character input of one of the following:
#'   'r' for rook, 'n' for knight, 'b' for bishop.
#' @param color a string argument that takes a letter of 'W' for White
#'    and 'B' for black.
#' @param position a character of 'l' for left and 'r' for right side
#'
#' @return A boolean logic of TRUE or FALSE
piece_pos <- function(game, piece = "r", color = "w", position = "l") {
  initial_pos <- NULL
  if (piece == "r") {
    initial_pos <- ifelse(color == "w", ifelse(position == "l", "a1", "h1"),
                          ifelse(position == "l", "a8", "h8"))
  } else if (piece == "b") {
    initial_pos <- ifelse(color == "w", ifelse(position == "l", "c1", "f1"),
                          ifelse(position == "l", "c8", "f8"))
  } else if (piece == "n") {
    initial_pos <- ifelse(color == "w", ifelse(position == "l", "b1", "g1"),
                          ifelse(position == "l", "b8", "g8"))
  } else if (piece == "q") {
    initial_pos <- ifelse(color == "w", "d1", "d8")
  }
  cur_pos <- initial_pos
  l1 <- length(game$san)
  for (i in seq_len(l1)) {
    if (game$from[i] == cur_pos) {
      cur_pos <- game$to[i]
    } else if (game$to[i] == cur_pos) {
      return(cur_pos)
    }
  }
}

### Other pieces ###
#' Helper function to store a board when the king is checkmated
#'
#' This function initializes a board and stores information about what locations
#'   pieces are killed at.
#'
#' @importFrom hash hash
#'
#' @return A list with the board and board indices
#'
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
      board_ind[[paste(letter, number, sep = "")]] <- c(i, j)
      j <- j + 1
    }
    i <- i + 1
  }
  return(list("board" = board, "board_ind" = board_ind))
}
