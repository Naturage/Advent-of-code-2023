source(here::here("common functions.R"))

day <- 9

# Data input
input <- get_input(day)

input_clean <- input %>%
  lapply(function(line){str_split(line, " ", simplify = T) %>% sapply(as.numeric)})

# Note that the answer of each line is just sum of last elements, so don't bother saving the whole thing.
predict_next <- function(line){
  ans <- 0
  while(!all(line == 0)){
    ans <- ans + line[length(line)]
    
    line <- line[2:length(line)] - line[1:(length(line) - 1)]
  }
  return(ans)
}

# Pt 1
(ans_1 <- input_clean %>% sapply(predict_next) %>% sum())

# Pt 2
# reverse the strings.
(ans_2 <- input_clean %>% lapply(rev) %>% sapply(predict_next) %>% sum())