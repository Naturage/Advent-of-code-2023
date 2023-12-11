source(here::here("common functions.R"))

day <- 10

# Data input
input <- get_input(day)

input_clean <- str_split(input,"", simplify = T)
input_rows <- nrow(input_clean)
input_cols <- nrow(input_clean)

map <- matrix(".", 
              nrow = input_rows * 2 + 1,
              ncol = input_cols * 2 + 1)

for (i in 1:input_rows){
  for (j in 1:input_cols){
    map[2*i,2*j] <- input_clean[i,j]
  }
}

distance <- matrix(rep(-1, length(map)), nrow = length(map[,1]))

# Pt 1

confirm_neighbour_p1 <- function(start_pos, end_pos){
  
  tmp <- end_pos - start_pos %>% as.numeric
  
  case_when(
    # going up
    tmp[1] == -2 ~ (map[start_pos] %in% c("S","|","L","J") & map[end_pos] %in% c("S","|","7","F")),
    # going down
    tmp[1] ==  2 ~ (map[start_pos] %in% c("S","|","7","F") & map[end_pos] %in% c("S","|","L","J")),
    # going left
    tmp[2] == -2 ~ (map[start_pos] %in% c("S","-","7","J") & map[end_pos] %in% c("S","-","L","F")),
    # going right
    tmp[2] ==  2 ~ (map[start_pos] %in% c("S","-","L","F") & map[end_pos] %in% c("S","-","7","J"))
  )
}

update_distance_p1 <- function(start_pos, end_pos){
  
  # edge case
  if (end_pos[1] %in% 1:length(map[,1]) & end_pos[2] %in% 1:length(map[1,])){
    
    start_pos <- start_pos %>% matrix(nrow = 1)
    end_pos   <- end_pos %>% matrix(nrow = 1)
    
    if(distance[end_pos] %in% c(-1, i+1)){
      if(confirm_neighbour_p1(start_pos, end_pos) == TRUE){
        
        distance[end_pos] <<- distance[start_pos] + 1
        
        mid_tile <- ((start_pos + end_pos) / 2) %>% matrix(nrow = 1)
        map[mid_tile] <<- "+"
        distance[mid_tile] <<- distance[start_pos] + 0.5
      }
    }
  }
  return(NULL)
}


distance[which(map == "S", arr.ind = TRUE)] <- 0

i <- 0

while(max(distance) == i){
  pipes <- which(distance == i, arr.ind = TRUE)
  for (row in 1:length(pipes[,1])){
    update_distance_p1(pipes[row,], pipes[row,] + c( 2, 0))
    update_distance_p1(pipes[row,], pipes[row,] + c(-2, 0))
    update_distance_p1(pipes[row,], pipes[row,] + c( 0, 2))
    update_distance_p1(pipes[row,], pipes[row,] + c( 0,-2))
  }
  
  i <- i + 1 
}

(ans_1 <- max(distance))


# Pt 2
# we use negative numbers just to figure out the loop's still going.
confirm_neighbour_p2 <- function(start_pos, end_pos){
 map[start_pos] %in% c("0",".") & map[end_pos] %in% c("0",".")
}

update_distance_p2 <- function(start_pos, end_pos){
  
  if (end_pos[1] %in% 1:length(map[,1]) & end_pos[2] %in% 1:length(map[1,])){
    start_pos <- start_pos %>% matrix(nrow = 1)
    end_pos   <- end_pos %>% matrix(nrow = 1)
    
    if(distance[end_pos] %in% c(-1, i-1)){
      if(confirm_neighbour_p2(start_pos, end_pos) == TRUE){
        distance[end_pos] <<- distance[start_pos] - 1
        map[end_pos] <<- "0"
      }
    }
  }
  return(NULL)
}

map[which(distance == -1)] <- "."

i <- -2
distance[1,1] <- -2
map[1,1] <- "0"

while(which(distance == i) %>% length() > 0){
  voids <- which(distance == i, arr.ind = TRUE)
  for (row in 1:length(voids[,1])){
    update_distance_p2(voids[row,], voids[row,] + c( 1, 0))
    update_distance_p2(voids[row,], voids[row,] + c(-1, 0))
    update_distance_p2(voids[row,], voids[row,] + c( 0, 1))
    update_distance_p2(voids[row,], voids[row,] + c( 0,-1))
  }
  
  i <- i - 1 
}

(ans_2 <- which(map[seq(2, 2*input_rows, by = 2), 
                    seq(2, 2*input_cols, by = 2)] == ".") %>% length())
