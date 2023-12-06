source(here::here("common functions.R"))

day <- 6

# Data input
input <- get_input(day)

input_clean <- input %>% str_split(" +") %>% lapply(function(line){line[2:length(line)]})

times <- input_clean[[1]] %>% as.numeric
distances <- input_clean[[2]] %>% as.numeric

# Pt 1
(ans_1 <- sapply(1:length(times), function(race){
  1:times[race] %>% {. * (times[race]- .)} %>% .[. > distances[race]] %>% length()
}) %>%
  prod)

# Pt 2

time_single <- paste0(times, collapse = "") %>% as.numeric
distance_single <- paste0(distances, collapse = "") %>% as.numeric

1:time_single %>% {. * (time_single - .)} %>% .[. > distance_single] %>% length()