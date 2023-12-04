source(here::here("common functions.R"))

day <- 4

# Data input
input <- get_input(day)

input_clean <- lapply(input, function(line){
  tmp <- line %>% str_split(": +| +\\| +") %>% unlist
  
  card_id <- tmp[1] %>% str_remove("Card( )+") %>% as.numeric
  winning <- tmp[2] %>% str_split(" +") %>% unlist %>% as.numeric
  have    <- tmp[3] %>% str_split(" +") %>% unlist %>% as.numeric
  matches <- intersect(winning, have) %>% length()
  amount  <- 1 
  
  out <- list(matches = matches,
              amount = 1)
  return(out)
})
  
# Pt 1

(ans_1 <- input_clean %>% sapply(function(card){
  points <- card$matches %>% {ifelse(. == 0, 0, 2^(.-1))}
}) %>% 
  sum)

# Pt 2

for (card_id in 1:length(input_clean)){
  
  number_matches <- input_clean[[card_id]]$matches
  
  if(number_matches > 0){
    for (won_cards in (card_id+1):(card_id+number_matches)){
      input_clean[[won_cards]]$amount <- input_clean[[won_cards]]$amount + input_clean[[card_id]]$amount
    }
  }
}

(ans_2 <- input_clean %>% sapply(function(card){card$amount}) %>% sum)