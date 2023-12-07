source(here::here("common functions.R"))

day <- 7

# Data input
input <- get_input(day)
  
input_clean <- input %>% {tibble(tmp = .)} %>%
  separate(tmp, into = c("hand", "bid"), sep = " +") %>%
  mutate(bid = bid %>% as.numeric) %>%
  separate(hand, into = paste0("card_",0:5), sep = "", remove = F) %>%
  select(-card_0)

# A couple reused functions:
# From a long table, put counts of cards into wide format and assign type by prio.
assign_type_prio <- function(data){
  data %>% 
    arrange(hand, bid, -n) %>%
    mutate(colname = "type") %>%
    mutate(order = row_number(), .by = c("hand", "bid")) %>%
    pivot_wider(id_cols = c("hand","bid"),
                values_from = "n",
                names_from = c("colname","order")) %>%
    mutate(type_prio = case_when(
      type_1 == 5 ~ 1,
      type_1 == 4 ~ 2,
      type_1 == 3 & type_2 == 2 ~ 3,
      type_1 == 3 ~ 4,
      type_1 == 2 & type_2 == 2 ~ 5,
      type_1 == 2 ~ 6,
      TRUE ~ 7
    )) %>% 
    select(hand, bid, type_prio)
}

# Assign a tiebreaker score - each card gets 2 digits, and the higher the better.
assign_hand_prio <- function(data, cards_prio){
  data %>% 
    mutate(across(starts_with("card"), ~{cards_prio[.x] %>% as.numeric})) %>%
    mutate(hand_prio = 10E8 * card_1 + 10E6 * card_2 + 10E4 * card_3 + 10E2 * card_4 + 1*card_5) %>% 
    select(hand, bid, hand_prio)
}

# This is just identical for both parts so reusing.
find_winnings <- function(hands,types){
  hands %>% 
   left_join(types, by = c("hand","bid")) %>%
   arrange(-type_prio, hand_prio) %>%
   mutate(rank = row_number()) %>%
   summarise(ans = sum(rank * bid)) %>%
   pull(ans)
}

# Pt1

types_1 <- input_clean %>% 
  pivot_longer(cols = starts_with("card")) %>%
  count(hand, bid, value) %>%
  assign_type_prio()

card_prio_1 <- list("A" = 14, "K" = 13, "Q" = 12, "J" = 11, "T" = 10,
                    "9" =  9, "8" =  8, "7" =  7, "6" =  6, "5" =  5, 
                    "4" =  4, "3" =  3, "2" =  2)

hands_1 <- assign_hand_prio(input_clean, card_prio_1)

(ans_1 <- find_winnings(hands_1,types_1))

# Pt 2

# One sanity check:
(oops_all_jokers <- input_clean %>% filter(hand == "JJJJJ"))
# You gotta be joking.

types_2 <- input_clean %>% 
  pivot_longer(cols = starts_with("card")) %>%
  count(hand, bid, value)

types_2 <- types_2 %>% filter(value != "J") %>%
  left_join(types_2 %>% filter(value == "J") %>% rename(n_jokers = n) %>% select(-value), by = c("hand", "bid")) %>%
  mutate(n_jokers = coalesce(n_jokers, 0)) %>%
  arrange(hand, bid, -n) %>%
  mutate(n = if_else(row_number() == 1, n + n_jokers, n), .by = c("hand", "bid")) %>%
  select(-n_jokers) %>%
  bind_rows(oops_all_jokers %>% select(hand, bid) %>% mutate(value = "J", n = 5)) %>%
  assign_type_prio()

card_prio_2 <- card_prio_1
card_prio_2[["J"]] <- 1

hands_2 <- assign_hand_prio(input_clean, card_prio_2)

(ans_2 <- find_winnings(hands_2,types_2))