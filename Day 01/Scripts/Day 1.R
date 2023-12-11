source(here::here("common functions.R"))

day <- 1

# Data input
input <- get_input(day)

# Pt1

reg_of_interest <- "[0-9]"

(ans_1 <- input %>% 
  sapply(function(line){
    line %>%
      str_extract_all(reg_of_interest) %>% 
        unlist() %>% 
        .[c(1,length(.))] %>% 
        paste0(collapse = "") %>% 
        as.numeric()
}) %>%
  sum())

# Pt2

# Needs lookahead because filthy elveses read "eightwo" as 82.
reg_of_interest_2 <- "[0-9]|(on(?=e))|(tw(?=o))|(thre(?=e))|(four)|(fiv(?=e))|(six)|(seve(?=n))|(eigh(?=t))|(nin(?=e))"

decode <- list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, 
               "6" = 6, "7" = 7, "8" = 8, "9" = 9, "0" = 0,
               on = 1, tw = 2, thre = 3, four = 4, fiv = 5,
               six = 6, seve = 7, eigh = 8, nin = 9)

(ans_2 <- input %>% 
    sapply(function(line){
      
      line %>% 
        str_extract_all(reg_of_interest_2) %>% 
        unlist() %>%
        decode[.] %>%
        unlist %>% 
        .[c(1,length(.))] %>% 
        paste0(collapse = "") %>% 
        as.numeric()
    }) %>%
    sum())
