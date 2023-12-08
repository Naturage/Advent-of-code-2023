source(here::here("common functions.R"))

day <- 8

# Data input
input <- get_input(day)
  
directions <- input[1] %>% 
  str_split("") %>% 
  unlist

map <- input[3:length(input)] %>% 
  str_remove_all("= \\(|,|\\)") %>% 
  str_split(" ") %>% 
  sapply(function(line){
    out <- list(list("L" = line[2], "R" = line[3]))
    names(out) <- line[1]
    return(out)
  })

# Pt 1

position <- "AAA"
steps <- 0
dir <- 0

while (position != "ZZZ"){
  steps <- steps + 1
  dir <- dir + 1
  if (dir > length(directions)){
    dir <- 1
  }
  position <- map[[position]][[directions[dir]]]
}
  
(ans_1 <- steps)

# Pt 2
starts <- names(map) %>% .[str_detect(., "[A-Z]{2}A")]

# The part you can only find out by trying via p1 code: each one reaches --Z only at the end of directions, and goes back to start 
# on the step after for a perfect loop. There was a LOT of potential to ruin us here (moving through multiple Z's, 
# out of sync with directions so you don't loop nicely, etc), but as it is, all you need is LCM.

# for proof:
# for (start_pos in starts){
#   position <- start_pos
#   steps <- 0
#   dir <- 0
#   
#   while (steps < 100000){
#     steps <- steps + 1
#     dir <- dir + 1
#     if (dir > length(directions)){
#       dir <- 1
#     }
#     position <- map[[position]][[directions[dir]]]
#     if(str_detect(position,"[A-Z]{2}Z")){
#       print(paste(start_pos, steps, dir, position))
#     }
#   }
# }

out <- list()

for (start_pos in starts){
  position <- start_pos
  steps <- 0
  dir <- 0
  
  while (!str_detect(position,"[A-Z]{2}Z")){
    steps <- steps + 1
    dir <- dir + 1
    if (dir > length(directions)){
      dir <- 1
    }
    position <- map[[position]][[directions[dir]]]
  }
  out[[start_pos]] <- steps
}

# Made two quick functions for GCD and LCM (as not part of base R), see in common functions.
options(scipen = 999)
(ans_2 <- out %>% unlist %>% Reduce(lcm_math, .))
