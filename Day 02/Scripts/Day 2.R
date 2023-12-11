source(here::here("common functions.R"))

day <- 2

# Data input
input <- get_input(day)

input_clean <- input %>% 
  lapply(function(line){
    line %>% 
      str_split("(: )|(, )|(; )") %>% 
      unlist %>% 
      tibble(game = .[1], cubes = .)
}) %>% 
  bind_rows %>% 
  filter(!grepl("Game",cubes)) %>%
  mutate(game   = str_extract(game, "\\d+") %>% as.numeric,
         colour = str_extract(cubes, "blue|red|green"),
         cubes  = str_extract(cubes, "\\d+") %>% as.numeric)
  
max_cubes <- input_clean %>% 
  group_by(game) %>%
  summarise(blue  = max(cubes * (colour == "blue" )),
            red   = max(cubes * (colour == "red"  )),
            green = max(cubes * (colour == "green")))

# Pt 1

(ans_1 <- max_cubes %>%
  filter(blue <= 14 & red <= 12 & green <= 13) %>%
  summarise(ans = sum(game)) %>%
  pull(ans))

# Pt 2

(ans_2 <- max_cubes %>% summarise(ans = sum(blue * red * green)) %>% pull(ans))