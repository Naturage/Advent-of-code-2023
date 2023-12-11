source(here::here("common functions.R"))

day <- 11

# Data input
input <- get_input(day)

input_clean <- str_split(input,"", simplify = T)

galaxies <- which(input_clean == "#", arr.ind = TRUE) %>%
  {tibble(row = .[,1], col = .[,2])} %>%
  mutate(galaxy_id = row_number())

row_width_1 <- tibble(row = 1:nrow(input_clean)) %>% 
  left_join(galaxies %>% count(row), by = "row") %>%
  mutate(width = case_when(
    is.na(n) ~ 2,
    TRUE ~ 1
  )) %>%
  select(-n)

row_width_2 <- row_width_1 %>% 
  mutate(width = if_else(width == 2, 1000000, 1))

col_width_1 <- tibble(col = 1:ncol(input_clean)) %>% 
  left_join(galaxies %>% count(col), by = "col") %>%
  mutate(width = case_when(
    is.na(n) ~ 2,
    TRUE ~ 1
  )) %>%
  select(-n)

col_width_2 <- col_width_1 %>% 
  mutate(width = if_else(width == 2, 1000000, 1))

find_ans <- function(row_width, col_width){
  # find galaxy pairs
  cross_join(
    galaxies %>% rename(galaxy_id_1 = galaxy_id, row_1 = row, col_1 = col),
    galaxies %>% rename(galaxy_id_2 = galaxy_id, row_2 = row, col_2 = col)
  ) %>%
    filter(galaxy_id_2 > galaxy_id_1) %>%
  # find horizontal distance
    mutate(row_min = pmin(row_1, row_2), row_max = pmax(row_1, row_2)) %>%
    inner_join(row_width, ., 
               by = join_by(between(row, row_min, row_max))) %>%
    summarise(distance_horz = sum(width) - 1, .by = c(galaxy_id_1, galaxy_id_2, col_1, col_2)) %>%
  # find vertical distance
    mutate(col_min = pmin(col_1, col_2), col_max = pmax(col_1, col_2)) %>%
    inner_join(col_width, ., 
               by = join_by(between(col, col_min, col_max))) %>%
    summarise(distance_vert = sum(width) - 1, .by = c(galaxy_id_1, galaxy_id_2, distance_horz)) %>%
    mutate(distance = distance_horz + distance_vert) %>%
    summarise(ans = sum(distance)) %>%
    pull(ans)
}

# Pt 1
(ans_1 <- find_ans(row_width_1, col_width_1))

# Pt 2
(ans_2 <- find_ans(row_width_2, col_width_2))