#' Subset Chess Data
#' @name subset_games
#' @description Takes a chess dataset stored as a list object and feature values
#'   to subset on. This then performs operations on that list comparable to
#'   baseR's subset command for dataframes. The result is the original chess
#'   list, but missing any observations that fail the given conditions.
#'
#' @param data A chess dataset stored as a list object.
#' @param rated A chess subset feature indicating whether a game impacted
#'   player ratings. Possible values are "all", "TRUE", or "FALSE".
#' @param winner A chess subset feature indicating which player won the game.
#'   Possible values are "all", "white", "black" or "draw".
#' @param white_rating A chess subset feature indicating the range of rating
#'   scores for the white player. It should be a vector of a lower bound and
#'   upper bound of ratings. For example: c(1500, 2000).
#' @param black_rating A chess subset feature indicating the range of rating
#'   scores for the black player. It should be a vector of a lower bound and
#'   upper bound of ratings. For example: c(1500, 2000).
#' @param victory_type A chess subset feature indicating how the game ended.
#'   Possible values are "all", "outoftime", "resign", "mate", or "draw".
#'
#' @return A chess dataset list with games meeting each of the specified
#'   conditions.
#'
#' @examples
#' \dontrun{
#' newlist <- subset_games(games_clean,
#'   rated = "TRUE", winner = "white",
#'   white_rating = c(1500, 3000),
#'   black_rating = c(1500, 3000)
#' )
#' newlist
#' }
#' @export

subset_games <- function(data = filtered_data, rated = "all", winner = "all",
                         white_rating = c(0, 3000), black_rating = c(0, 3000),
                         victory_type = "all") {
  l <- length(data)
  p <- vector(length = l)

  for (i in seq_len(l)) {
    data[[i]]$check <- 0
    rcheck <- ifelse(rated != "all", data[[i]]$rated == rated, T)
    wcheck <- ifelse(winner != "all", data[[i]]$winner == winner, T)
    vcheck <- ifelse(victory_type != "all",
      data[[i]]$victory_type == victory_type, T
    )

    w1check <- data[[i]]$white_rating >= white_rating[1]
    w2check <- data[[i]]$white_rating <= white_rating[2]
    b1check <- data[[i]]$black_rating >= black_rating[1]
    b2check <- data[[i]]$black_rating <= black_rating[2]

    p[i] <- rcheck * wcheck * vcheck * w1check * w2check * b1check * b2check
  }

  new_data <- Filter(function(x) p == 1, data)
  new_data <- new_data[!sapply(new_data, is.null)]
  new_data
}
