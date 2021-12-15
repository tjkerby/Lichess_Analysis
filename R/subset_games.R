

gc <- chess.analytics::games_clean

load("data-raw/filtered_data.Rdata")
filtered_data[[3]]

g <- chess.analytics::games
summary(g$victory_status)
g$rated
library(dplyr)
wt <- g %>% filter(rated == "TRUE", winner == "white", white_rating >= 1500)

subset_games <- function(data = filtered_data, rated = "all", winner = "all",
                         white_rating = c(0, 3000), black_rating = c(0, 3000),
                         victory_type = "all") {

  p <- vector(length = length(data))

  for(i in 1:length(data)){
    data[[i]]$check <- 0
    rcheck <- ifelse(rated != "all", data[[i]]$rated == rated, T)
    wcheck <- ifelse(winner != "all", data[[i]]$winner == winner, T)
    vcheck <- ifelse(victory_type != "all",
                     data[[i]]$victory_type == victory_type, T)

    w1check <- data[[i]]$white_rating >= white_rating[1]
    w2check <- data[[i]]$white_rating <= white_rating[2]
    b1check <- data[[i]]$black_rating >= black_rating[1]
    b2check <- data[[i]]$black_rating <= black_rating[2]

    p[i] <- rcheck*wcheck*vcheck*w1check*w2check*b1check*b2check
  }

  new_data <- Filter(function(x) p == 1, data)
  new_data <- new_data[!sapply(new_data, is.null)]
  new_data
}

newlist <- subset_games(filtered_data, rated = "TRUE", winner = "white",
                        white_rating = c(1500, 3000))

library(dplyr)
g <- chess.analytics::games
wt <- g %>% filter(rated == "TRUE",
                   winner == "white",
                   white_rating >= 1500)

newlist[[1]]$winner

