library(tidyverse)
library(stringr)
input <- read_table(file = "01/input.txt", col_names = "input")
numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
srebmun <- purrr::map_chr(numbers, stringi::stri_reverse) # numbers written in reverse
input_extract <- input %>% 
  mutate(tupni = stringi::stri_reverse(input)) %>% 
  mutate(numbers1_loc = str_locate(input, paste0(numbers, collapse = "|")), # locate 1st instance of "one" etc (if any)
         numbers2_loc_rev = str_locate(tupni, paste0(srebmun, collapse = "|")), # locate last instance of "one" etc (using reversed)
         numbers2_loc = str_length(tupni)-(numbers2_loc_rev[,2]-1), # find start of last text string in non-reversed input
         digits = gsub("\\D", "", input), # extract all numeric digits in order
         digit1 = str_sub(digits, 1, 1), # extract 1st numeric digit
         digit2 = str_sub(digits, -1,-1), # extract last numeric digit
         digit1_loc = str_locate(input,digit1), # find location of 1st instance of 1st digit
         digit2_loc = stringi::stri_locate_last_regex(input,digit2)) %>% # find location of last instance of digit2
  mutate(value1 = case_when(
    is.na(numbers1_loc[,1]) ~ digit1, # if no text number, use digit1
    is.na(digit1_loc[,1]) ~ str_extract(input, paste0(numbers, collapse = "|")), # if no numeric number, use 1st text number
    numbers1_loc[,1]<digit1_loc[,1] ~ str_extract(input, paste0(numbers, collapse = "|")), # if text number starts before numeric, use text number
    .default = digit1), # otherwise, numeric starts first so use digit1
    .after = input) %>% 
  mutate(value2 = case_when(
    is.na(numbers2_loc) ~ digit2, # if no text number, use digit2
    is.na(digit2_loc) ~ str_sub(input, start = numbers2_loc, end = numbers2_loc+1), # if no numeric number, extract 1st 2 chars of last text number instance
    numbers2_loc > digit2_loc[,1] ~ str_sub(input, start = numbers2_loc, end = numbers2_loc+1), # if text number starts later than numeric, extract text (2 chars) do as above
    .default = digit2), # otherwise, numeric starts later so use digit2
    .after = value1) %>% 
  mutate(value1int = case_when(
    value1 %in% c("one","1") ~ 1,
    value1 %in% c("two","2") ~ 2,
    value1 %in% c("three","3") ~ 3,
    value1 %in% c("four","4") ~ 4,
    value1 %in% c("five","5") ~ 5,
    value1 %in% c("six","6") ~ 6,
    value1 %in% c("seven","7") ~ 7,
    value1 %in% c("eight","8") ~ 8,
    value1 %in% c("nine","9") ~ 9,
    .default = NA_integer_
  ), .after = value1) %>% 
  mutate(value2int = case_when(
    value2 %in% c("on","1") ~ 1,
    value2 %in% c("tw","2") ~ 2,
    value2 %in% c("th","3") ~ 3,
    value2 %in% c("fo","4") ~ 4,
    value2 %in% c("fi","5") ~ 5,
    value2 %in% c("si","6") ~ 6,
    value2 %in% c("se","7") ~ 7,
    value2 %in% c("ei","8") ~ 8,
    value2 %in% c("ni","9") ~ 9,
    .default = NA_integer_
  ), .after = value2) %>% 
  mutate(value = (value1int*10) + value2int, .after = input)

output <- input_extract %>% 
  summarise(sum(value))

output

# SOLVED
