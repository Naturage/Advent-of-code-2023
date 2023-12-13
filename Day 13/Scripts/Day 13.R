source(here::here("common functions.R"))

day <- 13

# Data input
input <- get_input(day)

input_clean <- input %>% 
  split(cumsum(. == "") + 1) %>%
  lapply(function(map){
    map %>% .[. != ""] %>% str_split("", simplify = T)
  })

find_mirror <- function(map, threshold){
  ans <- 0
  
  for (i in 1:(nrow(map) - 1)){
    rows_to_mirror <- min(i, nrow(map) - i)
    start <- i + 1 - rows_to_mirror
    end <- i + rows_to_mirror
    if ((map[c(i:start),] == map[c((i+1):end),]) %>% .[. == FALSE] %>% length() %>% {. == threshold}){
      ans <- ans + i * 100
    }
  }
  for (j in 1:(ncol(map) - 1)){
    cols_to_mirror <- min(j, ncol(map) - j)
    start <- j + 1 - cols_to_mirror
    end <- j + cols_to_mirror
    if ((map[,c(j:start)] == map[,c((j+1):end)]) %>% .[. == FALSE] %>% length() %>% {. == threshold}){
      ans <- ans + j
    }
  }
  return(ans)
}

# Pt 1
(ans_1 <- sapply(input_clean, find_mirror, threshold = 0) %>% sum())

# Pt 2
(ans_2 <- sapply(input_clean, find_mirror, threshold = 1) %>% sum())