% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_game.R
\name{plot_game}
\alias{plot_game}
\title{Plot Chess Game}
\usage{
plot_game(game_num = 1, delay = 0.5)
}
\arguments{
\item{game_num}{The row number of the game dataset for plotting.}

\item{delay}{The time in seconds between each plot of a new chess move.}
}
\value{
A sequence of chess moves characterizing the desired game.
}
\description{
Takes a chess game number associated with the games dataset
  and a time delay between moves and generates the sequence of chess moves
  that make up that specific game.
}
\examples{
plot_game(game_num = 1, delay = 0)
}
