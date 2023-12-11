source(here::here("common functions.R"))

day <- 10

# Data input
input <- get_input(day)

input <- c("-L|F7","7S-7|","L|7||","-L-J|","L|-JF")

map <- str_split(input,"", simplify = T) %>%
  rbind(rep(".",length(.[1,])), ., rep(".",length(.[1,]))) %>%
  cbind(rep(".",length(.[,1])), ., rep(".",length(.[,1])))

distance <- matrix(rep(-1, length(map)), nrow = length(map[,1]))

# which(map == "S", arr.ind = TRUE)

confirm_neighbour <- function(start_pos, end_pos){
  
  case_when(
    start_pos[1,1] - 1 == end_pos[1,1] ~ # going up
      (map[start_pos] %in% c("S","|","L","J") & map[end_pos] %in% c("S","|","7","F")),
    start_pos[1,1] + 1 == end_pos[1,1] ~ # going down
      (map[start_pos] %in% c("S","|","7","F") & map[end_pos] %in% c("S","|","L","J")),
    start_pos[1,2] - 1 == end_pos[1,2] ~ # going left
      (map[start_pos] %in% c("S","-","7","J") & map[end_pos] %in% c("S","-","L","F")),
    start_pos[1,2] + 1 == end_pos[1,2] ~ # going right
      (map[start_pos] %in% c("S","-","L","F") & map[end_pos] %in% c("S","-","7","J"))
  )
}

update_distance <- function(start_pos, end_pos){
  start_pos <- start_pos %>% matrix(nrow = 1)
  end_pos   <- end_pos %>% matrix(nrow = 1)
  
  if(confirm_neighbour(start_pos, end_pos) == TRUE){
    if(distance[end_pos] == -1){
      distance[end_pos] <<- distance[start_pos] + 1
    }
  }
  return(NULL)
}

confirm_neighbour(start_pos = matrix(c(4,3), nrow = 1),
                  end_pos = matrix(c(5,3), nrow = 1))

map

distance[which(map == "S", arr.ind = TRUE)] <- 0

i <- 0

while(which(distance == i) %>% length() > 0){
  pipes <- which(distance == i, arr.ind = TRUE)
  for (row in 1:length(pipes[,1])){
    update_distance(pipes[row,], pipes[row,] + c( 1, 0))
    update_distance(pipes[row,], pipes[row,] + c(-1, 0))
    update_distance(pipes[row,], pipes[row,] + c( 0, 1))
    update_distance(pipes[row,], pipes[row,] + c( 0,-1))
  }
  
  i <- i + 1 
  if(i %% 100 == 0){print(i)}
}

# Pt 1
(ans_1 <- max(distance))