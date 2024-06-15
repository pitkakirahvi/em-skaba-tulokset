
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(here)
library(jsonlite)
source("helpers.R")

data_files <- list.files(here("data"),  pattern = "*.txt")

first_read <- TRUE
for (file in data_files) {
  respondent <- str_remove(file, "veikkaus_")
  respondent <- str_remove(respondent, ".txt")
  
  data <- jsonlite::read_json(file.path(here("data", file)))
  
  p_matches <- tibble(
    respondent = respondent, 
    data = unlist(data$group_matches)
  ) %>% 
    separate(data, into = c("match", "guess"), sep = ":")
    
  
  p_round16 <- tibble(
    respondent = respondent, 
    guess = unlist(data$round16)
  )
  
  p_round8 <- tibble(
    respondent = respondent, 
    guess = unlist(data$round8)
  )
  
  p_round4 <- tibble(
    respondent = respondent, 
    guess = unlist(data$round4)
  )
  
  p_final <- tibble(
    respondent = respondent, 
    guess = unlist(data$final)
  )
  
  p_winner <- tibble(
    respondent = respondent, 
    guess = unlist(data$winner)
  )
  
  p_scorers <- tibble(
    respondent = respondent, 
    guess = unlist(data$scorers)
  )
  
  p_top_scorer <- tibble(
    respondent = respondent, 
    guess = unlist(data$top_scorer)
  )
  
  if (first_read) {
    matches <- p_matches
    round16 <- p_round16
    round8 <- p_round8
    round4 <- p_round4
    final <- p_final
    winner <- p_winner
    scorers <- p_scorers
    top_scorer <- p_top_scorer
    
    first_read <- FALSE
    
  } else {
    matches <- bind_rows(matches, p_matches)
    round16 <- bind_rows(round16, p_round16)
    round8 <- bind_rows(round8, p_round8)
    round4 <- bind_rows(round4, p_round4)
    final <- bind_rows(final, p_final)
    winner <- bind_rows(winner, p_winner)
    scorers <- bind_rows(scorers, p_scorers)
    top_scorer <- bind_rows(top_scorer, p_top_scorer)
  }
}

matches %>% write_local("veikkaus_alkulohko.tsv")
round16 %>% write_local("veikkaus_neljannesvaliera.tsv")
round8 %>% write_local("veikkaus_puolivaliera.tsv")
round4 %>% write_local("veikkaus_valiera.tsv")
final %>% write_local("veikkaus_finaali.tsv")
winner %>% write_local("veikkaus_voittaja.tsv")
scorers %>% write_local("veikkaus_maalitekijat.tsv")
top_scorer %>% write_local("veikkaus_maalikuningas.tsv")
