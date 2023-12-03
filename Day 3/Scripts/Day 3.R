source(here::here("common functions.R"))

day <- 3

# Data input
input <- get_input(day)

part_ids <- mapply(function(line, order){
  out <- tibble(
    number = str_match_all(line,"\\d+")   %>% unlist %>% as.numeric,
    row    = order,
    start  = str_locate_all(line, "\\d+") %>% .[[1]] %>% as.matrix %>% .[,"start"],
    end    = str_locate_all(line, "\\d+") %>% .[[1]] %>% as.matrix %>% .[,"end"]
  )
}, input, order = 1:length(input), SIMPLIFY = FALSE) %>%
  bind_rows()
  
symbols <- mapply(function(line, order){
  out <- tibble(
    sym_row    = order,
    sym_col    = str_locate_all(line, "[^\\d\\.]") %>% .[[1]] %>% as.matrix %>% .[,"start"],
    symbol     = str_match_all(line,"[^\\d\\.]")   %>% unlist,
  )
  return(out)
}, input, order = 1:length(input), SIMPLIFY = FALSE) %>%
  bind_rows()

symbol_neighbours <- symbols %>% 
  inner_join(part_ids %>% 
               mutate(from_col = start-1, to_col = end + 1,
                      from_row = row-1  , to_row = row + 1), 
             by = join_by(between(sym_col, from_col, to_col),
                          between(sym_row, from_row, to_row))) %>%
  select(-from_col, -to_col, -from_row, -to_row)

# Pt 1

(ans_1 <- symbol_neighbours %>%
  distinct(row,start,end,number) %>%
  summarise(ans = sum(number)) %>%
  pull(ans))

# Pt 2 

(ans_2 <- symbol_neighbours %>% 
  filter(symbol == "*") %>%
  group_by(sym_row, sym_col) %>%
  mutate(count_part = n()) %>%
  filter(count_part == 2) %>%
  summarise(gear_ratio = prod(number), .groups = "drop") %>%
  summarise(ans = sum(gear_ratio)) %>% 
  pull(ans))
