library(tidyverse)
library(here)
library(jsonlite)

data_files <- list.files(here("answers"),  
                         pattern = "*.txt")

first_read <- TRUE
for (file in data_files) {
  name <- strsplit(file, "_")[[1]][1]
  
  data <- jsonlite::read_json(file.path(here("answers", file)))
  
  p_matches <- tibble(!!name := data$group_matches) %>%
    unnest(all_of(name)) %>%
    separate(all_of(name), into = c("match", name), sep = ":")
  
  p_round16 <- tibble(!!name := data$round16) %>%
    unnest(all_of(name)) %>% 
    arrange(!!!syms(name))
  
  p_round8 <- tibble(!!name := data$round8) %>%
    unnest(all_of(name)) %>% 
    arrange(!!!syms(name))
  
  p_round4 <- tibble(!!name := data$round4) %>%
    unnest(all_of(name)) %>% 
    arrange(!!!syms(name))
  
  p_final <- tibble(!!name := data$final) %>%
    unnest(all_of(name)) %>% 
    arrange(!!!syms(name))
  
  p_winner <- tibble(!!name := data$winner) %>%
    unnest(all_of(name))
  
  p_scorers <- tibble(!!name := data$scorers) %>%
    unnest(all_of(name))
  
  p_top_scorer <- tibble(!!name := data$top_scorer) %>%
    unnest(all_of(name))
  
  rm(data)
  
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
    matches <- cbind(matches, p_matches[, name])
    round16 <- cbind(round16, p_round16)
    round8 <- cbind(round8, p_round8)
    round4 <- cbind(round4, p_round4)
    final <- cbind(final, p_final)
    winner <- cbind(winner, p_winner)
    scorers <- cbind(scorers, p_scorers)
    top_scorer <- cbind(top_scorer, p_top_scorer)
  }
  rm(p_matches,
     p_round16,
     p_round8,
     p_round4,
     p_final,
     p_winner,
     p_scorers,
     p_top_scorer)
}

save(matches, 
     round16,
     round8, 
     round4,
     final,
     winner,
     scorers,
     top_scorer,
     file = "rows.Rdata"
     )
