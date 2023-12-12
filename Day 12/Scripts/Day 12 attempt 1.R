source(here::here("common functions.R"))
library(memoise)

day <- 12

# Data input
input <- get_input(day)

input_clean <- str_split(input," ")

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

memoised_count_arrangements <- memoise(count_arrangements, cache = cachem::cache_mem(max_size = 15 * 1024^3))

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
