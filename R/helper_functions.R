#### HELPER FUNCTIONS #####
#' Checkmate
#'
#' This function helps to find the games that involved a checkmate of the
#'
#' @param game information about the game
#' @param color a string argument that takes a letter of 'W' for White
#'    and 'B' for black.
#' @return A boolean logic of TRUE or FALSE
#'
#' @import rchess

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
#' This function helps to find the games that involved a checkmate of the
#'
#' @param game information about the game
#' @param color a string argument that takes a letter of 'W' for White
#'    and 'B' for black.
#' @return A boolean logic of TRUE or FALSE
#'
#' @import rchess

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
#' Helper function of
#'
#' This function helps to find the games that involved a checkmate of the
#'   and a time delay between moves and generates the sequence of chess moves
#'   that make up that specific game. Above the game board a scoreboard is
#'   provided to track the number points each player has as the game progresses.
#'
#' @param game information about the game
#' @param color a string argument that takes a letter of 'W' for White
#'    and 'B' for black.
#' @return A boolean logic of TRUE or FALSE
#'
#' @import rchess

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
