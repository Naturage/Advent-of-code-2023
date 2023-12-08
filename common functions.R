library(here)
library(dplyr)
library(httr)
library(readr)
library(tidyr)
library(stringr)
library(stringi)

get_input <- function(day){
  
  if(file.exists(here(paste0("Day ",day), "Data","input.rds"))){
    input <- read_rds(here(paste0("day ",day), "data","input.rds"))
  } else {
    suppressWarnings(session_id <- readLines(here("session.txt")))
    
    input <- GET(paste0("https://adventofcode.com/2023/day/",day,"/input"),
                 set_cookies(session = session_id)) %>%
      content(encoding = 'UTF-8') %>%
      read_lines()
    
    write_rds(input, here(paste0("day ",day), "data","input.rds"))
  }
  
  return(input)
}

strtoi_full <- function(binary_num, base = 2){
  if (binary_num == ""){
    return(0)
  } else if (nchar(binary_num) < 30){
    return(strtoi(binary_num, base = 2))
  } else
    return(strtoi_full(str_sub(binary_num, end = -31)) * 2**30 + strtoi(str_sub(binary_num, start = -30), base = 2))
}

# Quick function to find LCM to two numbers, using that LCM(a,b) * GCD(a,b) = a*b, and Euclid's algorithm - gcd(a,b) = gcd(a, b%%a).
gcd_math <- function(x, y) {
  if (x == 0){return(y)}
  else if (y == 0) {return(x)}
  else if (x > y) {return(gcd_math(x %% y, y))}
  else {return(gcd_math(x, y %% x))}
}

lcm_math <- function(x,y){
  return(x * y / gcd_math(x,y))
}