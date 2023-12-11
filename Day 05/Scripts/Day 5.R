source(here::here("common functions.R"))

day <- 5

# Data input
input <- get_input(day)

input <- input %>% 
  split(cumsum(input == "")) %>%
  lapply(function(table){table[table != ""]})

maps <- input[2:length(input)] %>%
  lapply(function(map){
    title <- map[1] %>% 
      str_split("-| ", simplify = T)
    
    entries <- map[-1] %>% str_split(" ", simplify = T)
    
    out <- tibble(
      from = title[1],
      to = title[3],
      from_range_start = as.numeric(entries[,2]),
      from_range_end = as.numeric(entries[,2]) + as.numeric(entries[,3]) - 1,
      to_range_start = as.numeric(entries[,1]),
      )
    
    return(out)
  })

# Pt 1

seeds_1 <- input[[1]] %>% 
  str_remove("seeds: ") %>% 
  str_split(" ", simplify = T) %>% 
  as.numeric %>% 
  {tibble(ingredient = "seed", id = .)}

input_clean_1 <- c(list(`0` = seeds_1), maps)

traverse_maps_1 <- function(current_ingredient, map){
  
  to_impute <- map %>% distinct(to) %>% pull(to)
  
  out <- current_ingredient %>% 
    left_join(map, 
              by = join_by(between(id, from_range_start, from_range_end),
                           ingredient == from)) %>%
    # If not part of map ID stays, otherwise moves to next step
    mutate(id = case_when(
      is.na(from_range_start) ~ id,
      TRUE ~ id - from_range_start + to_range_start)) %>%
    # Clean up NA to lines
    mutate(to = coalesce(to, to_impute)) %>%
    select(ingredient = to,
           id)
  
  return(out)
}

mapped_through_1 <- Reduce(traverse_maps_1, input_clean_1)

(ans_1 <- mapped_through_1 %>% summarise(ans = min(id)) %>% pull(ans))

# Pt 2

seeds_2 <- input[[1]] %>% 
  str_remove("seeds: ") %>% 
  str_split(" ", simplify = T) %>% 
  as.numeric %>%
  matrix(ncol = 2, byrow = T) %>%
  {tibble(ingredient = "seed", 
          id_from = .[,1], 
          id_to = .[,1] + .[,2] - 1)}

input_clean_2 <- c(list(`0` = seeds_2), maps)

traverse_maps_2 <- function(current_ingredient, map){
  
  to_impute <- map %>% distinct(to) %>% pull(to)
  
  # Split our input seeds to chunks which fully fit within the mapped chunks.
  range_breakpoints <- map %>% 
    mutate(from_range_end = from_range_end + 1) %>%
    pivot_longer(cols = c(from_range_start, from_range_end)) %>%
    distinct(value) %>%
    arrange(value) %>%
    pull(value) %>%
    {tibble(value_min = c(-10^10, .),
            value_max = c(. - 1, 10^10))}

  
  current_ingredient <- current_ingredient %>%
    cross_join(range_breakpoints) %>%
    group_by(id_from, id_to) %>%
    mutate(id_from = case_when(
      id_from > value_max ~ NA,
      id_from < value_min ~ value_min,
      TRUE ~ id_from
    ),
    id_to = case_when(
      id_to < value_min ~ NA,
      id_to > value_max ~ value_max,
      TRUE ~ id_to
    )) %>%
    ungroup() %>%
    filter(!is.na(id_from) & !is.na(id_to)) %>%
    select(-value_min, -value_max)
  
  # And now we can reuse P1 reasonably
  moved_seeds <- current_ingredient %>% 
    left_join(map, 
              by = join_by(overlaps(id_from, id_to, from_range_start, from_range_end),
                           ingredient == from)) %>%
    # If not part of map ID stays, otherwise moves to next step
    mutate(id_from = case_when(
        is.na(from_range_start) ~ id_from,
        TRUE ~ id_from - from_range_start + to_range_start),
      id_to = case_when(
        is.na(from_range_start) ~ id_to,
        TRUE ~ id_to - from_range_start + to_range_start)) %>%
    # Clean up NA to lines
    mutate(to = coalesce(to, to_impute)) %>%
    select(ingredient = to, id_from, id_to)
  
  return(moved_seeds)
}

mapped_through_2 <- Reduce(traverse_maps_2, input_clean_2)

(ans_2 <- mapped_through_2 %>% summarise(ans = min(id_from)) %>% pull(ans))
