source(here::here("common functions.R"))

day <- 12

# Data input
input <- get_input(day)

input_clean <- str_split(input," ")

test_spring <- springs[[2]]
test_record <- records[[2]]


update_arrangements <- function(rowno, colno, test_spring, test_record){
  
  print(paste(rowno, colno))
  
  arrangements[rowno, colno] <<- 0
  next_string <- test_record[colno]
  
  if(test_spring[rowno] %in% c("?",".")){
    arrangements[rowno, colno] <<- arrangements[rowno, colno] + arrangements[row + 1, col]
  }
  if (test_spring[rowno] %in% c("?","#")){
    if (rowno + next_string > length(test_spring) + 1){
      # do nothing
    } else if (rowno + next_string == length(test_spring) + 1){
      if (all(test_spring[rowno:(rowno - 1 + next_string)] %in% c("#","?"))){
        arrangements[rowno, colno] <<- arrangements[rowno, colno] + arrangements[length(test_spring) + 1, colno + 1]
      }
    } else {
      if (all(test_spring[row:(rowno - 1 + next_string)] %in% c("#","?"))){
        arrangements[rowno, colno] <<- arrangements[rowno, colno] + arrangements[row + next_string + 1, colno + 1]
      }
    }
  }
  return(NULL)
}


# Set up a matrix
# row marks (# - 1) of characters used up
arrangements <- matrix(rep(-1, (length(test_spring)+1) * (length(test_record) + 1)), nrow = (length(test_spring) + 1))

for (row in 1:length(test_spring)){
  arrangements[row, length(test_record) + 1] <- if_else(all(test_spring[row:length(test_spring)] %in% c("?",".")), 1, 0)
}
arrangements[length(test_spring) + 1, ] <- 0
arrangements[length(test_spring) + 1, length(test_record) + 1] <- 1

for (row in seq(length(test_spring), 1, by = -1)){
  for (col in seq(length(test_record), 1, by = -1)){
    update_arrangements(row, col, test_spring, test_record)
  }
}


update_arrangements(15,5,test_spring, test_record)





count_arrangements <- function(spring, record){
  #   print(paste0("testing ", paste0(spring, collapse = ""), " vs ", paste0(record, collapse = " ")))
  if (length(record) == 0){
    if(all(spring %in% c(".","?"))){
      #       print("record short - 1")
      return(1)
    } else {
      #       print("record short - 0")
      return(0)
    }
  } else if (length(spring) < sum(record) + length(record) - 1) {
    #     print("out of springs - 0")
    return(0)
  } else {
    if (spring[1] == "?"){
      #      print(paste0("bifurcation ", paste0(spring, collapse = "")))
      out <- count_arrangements(c("#",spring[-1]), record) +
        count_arrangements(spring[-1], record)
      #      print(paste0("bifurcation ", paste0(spring, collapse = ""), " returned ", out))
      return(out)
    } else if (spring[1] == "."){
      #       print(paste0("trim a dot ", paste0(spring[-1], collapse = "")))
      return(count_arrangements(spring[-1], record))
    } else {
      if (all(spring[1:record[1]] %in% c("#","?"))){
        if (length(spring) == record[1]){
          return(count_arrangements(c(), record[-1]))
        } else if (spring[record[1] + 1] %in% c(".","?")){
          #           print("group found")
          return(count_arrangements(spring[-1:-(record[1]+1)], record[-1]))
        } else{
          #           print("illegal start - 0")
          return(0)
        }
      } else
        #        print("illegal start - 0")
        return(0)
    }
  }
}

springs <- lapply(input_clean, function(line){
  line[1] %>% str_split("") %>% unlist
})

records <- lapply(input_clean, function(line){
  line[2] %>% str_split(",") %>% unlist %>% as.numeric
})

# Pt 1
(ans_1 <- mapply(memoised_count_arrangements, springs, records) %>% sum())

# Pt 2

springs_2 <- lapply(springs, function(line){
  c(line,"?",line,"?",line,"?",line,"?", line)
})

records_2 <- lapply(records, function(line){
  rep(line,5)
})

out <- c()

for (n in 1:1000){
  out[n] <- memoised_count_arrangements(springs_2[[n]], records_2[[n]])
  print(paste(n, out[n]))
}
