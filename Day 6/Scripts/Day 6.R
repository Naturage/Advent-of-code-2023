source(here::here("common functions.R"))

day <- 6

# Data input
input <- get_input(day)
input_clean <- input %>% str_split(" +") %>% 
  lapply(function(line){line[2:length(line)] %>% as.numeric})

num_options <- function(time, distance){
  1:time %>% {. * (time - .)} %>% {. > distance} %>% sum()
}

# Pt 1
(ans_1 <- mapply(num_options, input_clean[[1]], input_clean[[2]]) %>% prod())

# Pt 2
(ans_2 <- num_options(time     = paste0(input_clean[[1]], collapse = "") %>% as.numeric,
                      distance = paste0(input_clean[[2]], collapse = "") %>% as.numeric))