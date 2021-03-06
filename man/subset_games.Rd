% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_games.R
\name{subset_games}
\alias{subset_games}
\title{Subset Chess Data}
\usage{
subset_games(
  data = filtered_data,
  rated = "all",
  winner = "all",
  white_rating = c(0, 3000),
  black_rating = c(0, 3000),
  victory_type = "all"
)
}
\arguments{
\item{data}{A chess dataset stored as a list object.}

\item{rated}{A chess subset feature indicating whether a game impacted
player ratings. Possible values are "all", "TRUE", or "FALSE".}

\item{winner}{A chess subset feature indicating which player won the game.
Possible values are "all", "white", "black" or "draw".}

\item{white_rating}{A chess subset feature indicating the range of rating
scores for the white player. It should be a vector of a lower bound and
upper bound of ratings. For example: c(1500, 2000).}

\item{black_rating}{A chess subset feature indicating the range of rating
scores for the black player. It should be a vector of a lower bound and
upper bound of ratings. For example: c(1500, 2000).}

\item{victory_type}{A chess subset feature indicating how the game ended.
Possible values are "all", "outoftime", "resign", "mate", or "draw".}
}
\value{
A chess dataset list with games meeting each of the specified
  conditions.
}
\description{
Takes a chess dataset stored as a list object and feature values
  to subset on. This then performs operations on that list comparable to
  baseR's subset command for dataframes. The result is the original chess
  list, but missing any observations that fail the given conditions.
}
\examples{
\dontrun{
newlist <- subset_games(games_clean,
  rated = "TRUE", winner = "white",
  white_rating = c(1500, 3000),
  black_rating = c(1500, 3000)
)
newlist
}
}
