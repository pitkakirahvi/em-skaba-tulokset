library(rvest)
library(tidyverse)

get_group_matches <- function(group_name, path){
  
  home_teams <- path %>%
    read_html(encoding = "UTF-8") %>%
    html_elements(".fhome") %>%
    html_text() %>%
    str_trim()
  
  away_teams <- path %>%
    read_html(encoding = "UTF-8") %>%
    html_elements(".faway") %>%
    html_text() %>%
    str_trim() 
  
  score <- path %>%
    read_html(encoding = "UTF-8") %>%
    html_elements(".fscore") %>%
    html_text() %>%
    str_trim()
  
  matches  <- tibble(
    group = group_name,
    home = home_teams,
    away = away_teams,
    score = score
  ) 
  
  matches <- matches %>%
    separate(score, sep = "–", into = c("ghome", "gaway")) %>%
    filter(!is.na(gaway)) %>%
    mutate(result = case_when(
      ghome > gaway ~ "1",
      ghome == gaway ~ "x",
      ghome < gaway ~ "2"
    )) %>%
    mutate(match = paste0(home, "-", away)) %>%
    select(match, result)
  
}

group_a <- get_group_matches(
  "Group A",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_A" 
  )

group_b <- get_group_matches(
  "Group B",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_B" 
)

group_c <- get_group_matches(
  "Group C",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_C" 
)

group_d <- get_group_matches(
  "Group D",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_D" 
)

group_e <- get_group_matches(
  "Group E",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_E" 
)

group_f <- get_group_matches(
  "Group F",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_F" 
)

group_results <- bind_rows(
  group_a,
  group_b,
  group_c,
  group_d,
  group_e,
  group_f
) 
 rm(group_a, group_b, group_c, group_d, group_e, group_f)
