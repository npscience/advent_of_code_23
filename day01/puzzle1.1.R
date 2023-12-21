library(tidyverse)
library(stringr)
input <- read_table(file = "AdventOfCode/01/input.txt", col_names = "input")
input_extract <- input %>% 
  mutate(digits = gsub("\\D", "", input), 
         digit1 = as.integer(str_sub(digits, 1, 1)), 
         digit2 = as.integer(str_sub(digits, -1,-1)), 
         value = (digit1*10) + digit2)
output <- input_extract %>% 
  summarise(sum(value))
output
