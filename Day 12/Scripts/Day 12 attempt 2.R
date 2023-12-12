source(here::here("common functions.R"))

day <- 12

# Data input
input <- get_input(day)

input_clean <- str_split(input," ")

# Function that does the dynamic programming
update_arrangements <- function(row, col, test_spring, test_record){
  
  arrangements[row, col] <<- 0
  next_string <- test_record[col]
  
  # if ends with possibly dot, add <# of options ignoring the first dot>.
  if(test_spring[row] %in% c("?",".")){
    arrangements[row, col] <<- arrangements[row, col] + arrangements[row + 1, col]
  }
  # if ends with possibly #...
  if (test_spring[row] %in% c("?","#")){
    # but is too short, do nothing.
    if (row + next_string > length(test_spring) + 1){
      
      # if it needs precisely the amt of characters and just barely fits - take off the next record and corresponding # of springs.
    } else if (row + next_string == length(test_spring) + 1){
      if (all(test_spring[row:(row - 1 + next_string)] %in% c("#","?"))){
        arrangements[row, col] <<- arrangements[row, col] + arrangements[length(test_spring) + 1, col + 1]
      }
      # otherwise if it fits, take off new next record and # of springs + one for the dot following it.
    } else {
      if ((all(test_spring[row:(row - 1 + next_string)] %in% c("#","?"))) &
          (test_spring[row + next_string] %in% c(".","?"))){
        arrangements[row, col] <<- arrangements[row, col] + arrangements[row + next_string + 1, col + 1]
      }
    }
  }
  return(NULL)
}

dynprog_line <- function(test_spring, test_record){
  
  # Set up a matrix
  # row marks (# - 1) of characters used up, col marks (# - 1) checks used up
  arrangements <<- matrix(rep(-1, (length(test_spring)+1) * (length(test_record) + 1)), nrow = (length(test_spring) + 1))
  
  # initialise: 
  # if we have two empty strings, 1, 
  # if we have requirements and no springs, 0, 
  # if we have no reqs and springs - 1 if no # present, else 0
  for (row in 1:length(test_spring)){
    arrangements[row, length(test_record) + 1] <<- if_else(all(test_spring[row:length(test_spring)] %in% c("?",".")), 1, 0)
  }
  arrangements[length(test_spring) + 1, ] <<- 0
  arrangements[length(test_spring) + 1, length(test_record) + 1] <<- 1
  
  # apply dynamic programming step above
  for (row in seq(length(test_spring), 1, by = -1)){
    for (col in seq(length(test_record), 1, by = -1)){
      update_arrangements(row, col, test_spring, test_record)
    }
  }
  
  return(arrangements[1,1])
}

# Pt 1
springs <- lapply(input_clean, function(line){
  line[1] %>% str_split("") %>% unlist
})

records <- lapply(input_clean, function(line){
  line[2] %>% str_split(",") %>% unlist %>% as.numeric
})

(ans_1 <- mapply(dynprog_line, springs, records) %>% sum())

# Pt 2

springs_2 <- lapply(springs, function(line){
  c(line,"?",line,"?",line,"?",line,"?", line)
})

records_2 <- lapply(records, function(line){
  rep(line,5)
})

(ans_2 <- mapply(dynprog_line, springs_2, records_2) %>% sum())
