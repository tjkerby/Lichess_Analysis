library(rchess)

setwd("C:/Users/kelvy/OneDrive/KelvynsFiles/Masters Program/Stat Comp/ChessProject")

games <- read.csv("misc/games.csv", header = T)
games$moves[4]
v <- strsplit(games$moves, split = " ")
v
v[[4]]
v[[4]][13] == "O-O"

chss <- Chess$new()
paste0("chss",
       "$move(", m, ")")
s <- eval(parse(text = "2+3"))
k <- "d4"
s <- eval(parse(text = paste0("chss", "$move(", d4, ")")))
s

v[[1]]

seq_len(v[[1]])

chss <- Chess$new()
l <- length(v[[1]])
for(i in seq_len(l)){
  s <- eval(parse(text = paste0("chss", "$move(\"", v[[1]][i], "\")")))
  print(plot(chss))
  Sys.sleep(time = 0.5)
}


#detach("package:rchess", unload = T)
#rchess::chessboardjs()
#data("chesswc")
#library(dplyr)
#count(chesswc, event)
#data("chessopenings")
#head(chessopenings)
chss <- Chess$new()
class(chss)
plot(chss)
chss$move("e4")#$move("e5")
plot(chss)
chss$move("e5")
plot(chss)
chss
chss$history()
chss$move("Nf3")
plot(chss)
chss$move("Nf6")
plot(chss)
chss
chss$move("Bc4")
plot(chss)
chss$move("Bc5")
plot(chss)
chss$move("O-O")
plot(chss)
chss$move("0-0")
chss$move("O-O")
plot(chss)
chss$move("Nxe5")
plot(chss)
chss$move("Nxe4")
plot(chss)
s <- chss$history_detail()
chss$square_color("a1")
chss$move("a4")
plot(chss)
chss$move("h5")
plot(chss)
chss$move("a5")
plot(chss)
chss$move("h4")
plot(chss)
chss$move("g4")
plot(chss)
chss$move("hxg3")
plot(chss)
chss$move("hxg3")
plot(chss)
chss$move("Nxg3")
plot(chss)
chss$move("Re1")
plot(chss)
chss$move("Qh4")
plot(chss)
chss$move("Bxf7")
chss$move("Bxf7+")
plot(chss)
chss$move("Rxf7")
plot(chss)
chss$move("Nxf7")
plot(chss)
chss$move("Qh1++")
chss$move("Qh1#")
plot(chss)
chss$in_checkmate()
chss

chss$square_color("a1")
chss$get("e4")
chss$get("h1")
chss$history()
s <- chss$history_detail()
