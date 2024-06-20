
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(here)
library(lubridate)
source("helpers.R")

# Load data ----

match_details <- read_remote("lahtotiedot_pelipaivat.tsv")
results_scorers <- read_remote("tulokset_maalintekijat.tsv")
results_group <- read_remote("tulokset_alkulohko.tsv")
result_round16 <- read_remote("tulokset_neljannesvaliera.tsv")
result_round8 <- read_remote("tulokset_puolivaliera.tsv")
result_round4 <- read_remote("tulokset_valiera.tsv")
result_final <- read_remote("tulokset_finaali.tsv")
result_winner <- read_remote("tulokset_mestari.tsv")

guesses_scorers <- read_remote("veikkaukset_maalitekijat.tsv")
guesses_top_scorer <- read_remote("veikkaukset_maalikuningas.tsv")
guesses_group <- read_remote("veikkaukset_alkulohko.tsv")
guesses_round16 <- read_remote("veikkaukset_neljannesvaliera.tsv")
guesses_round8 <- read_remote("veikkaukset_puolivaliera.tsv")
guesses_round4 <- read_remote("veikkaukset_valiera.tsv")
guesses_final <- read_remote("veikkaukset_finaali.tsv")
guesses_winner <- read_remote("veikkaukset_voittaja.tsv")


# Match details ----

match_details <- match_details %>%
  mutate(match = paste0(home, "-", away))

# Group points ----

points_group <- guesses_group %>%
  left_join(results_group, by = "match") %>%
  mutate(points = if_else(guess == result, 1, 0))

group_points_wide <- points_group %>%
  select(match, points, respondent) %>%
  pivot_wider(id_cols = match, names_from = respondent, values_from = points) %>%
  left_join(match_details %>% select(match, group), by = "match") %>%
  left_join(results_group, by = "match") %>%
  relocate(group, match, result) %>%
  arrange(group, match) %>%
  rename(Lohko = group, Ottelu = match, Tulos = result)

# Rounds ----

round16_points <- guesses_round16 %>%
  mutate(points = if_else(guess %in% result_round16, 1, 0))

round16_points_wide <- round16_points %>%
  pivot_wider(id_cols = guess, names_from = respondent, values_from = points) %>%
  filter(guess %in% result_round16) %>%
  replace(is.na(.), 0) %>%
  rename(Veikkaus = guess)

round8_points <- guesses_round8 %>%
  mutate(points = if_else(guess %in% result_round8, 1, 0))

round8_points_wide <- round8_points %>%
  pivot_wider(id_cols = guess, names_from = respondent, values_from = points) %>%
  filter(guess %in% result_round8) %>%
  replace(is.na(.), 0) %>%
  rename(Veikkaus = guess)

round4_points <- guesses_round4 %>%
  mutate(points = if_else(guess %in% result_round4, 1, 0))

round4_points_wide <- round4_points %>%
  pivot_wider(id_cols = guess, names_from = respondent, values_from = points) %>%
  filter(guess %in% result_round4) %>%
  replace(is.na(.), 0) %>%
  rename(Veikkaus = guess)

final_points <- guesses_final %>%
  mutate(points = if_else(guess %in% result_final, 1, 0))

final_points_wide <- final_points %>%
  pivot_wider(id_cols = guess, names_from = respondent, values_from = points) %>%
  filter(guess %in% result_final) %>%
  replace(is.na(.), 0) %>%
  rename(Veikkaus = guess)

winner_points <- guesses_winner %>%
  mutate(points = if_else(guess %in% result_winner, 1, 0))

winner_points_wide <- winner_points %>%
  pivot_wider(id_cols = guess, names_from = respondent, values_from = points) %>%
  filter(guess %in% result_winner) %>%
  replace(is.na(.), 0) %>%
  rename(Veikkaus = guess)

# Scorers ----

points_scorers <- guesses_scorers %>%
  left_join(results_scorers, by = c("guess" = "player")) %>%
  mutate(points = 0.5 * goals) %>%
  select(respondent, guess, goals, points)

points_scorers_wide <- points_scorers %>%
  group_by(guess) %>%
  summarise(
    goals = unique(goals, na.rm = TRUE),
    points = 0.5 * goals,
    Veikkaajat = paste0(respondent, collapse = ", ")
  ) %>%
  replace_na(list(goals = 0, points = 0)) %>%
  arrange(-goals) %>%
  rename(Veikkaus = guess, Maalit = goals, Pisteet = points)

results_top_scorer <- results_scorers %>%
  filter(goals > 3) %>%
  filter(goals == max(goals)) %>%
  mutate(points = 3)

points_top_scorer <- guesses_top_scorer %>%
  left_join(results_top_scorer, by = c("guess" = "player")) %>%
  replace_na(list(goals = 0, points = 0)) %>%
  select(respondent, guess, goals, points)

# Matchday ----

today_schedule <- match_details %>%
  filter(dmy(date) == Sys.Date())

matchday_games <- guesses_group %>% 
  filter(match %in% today_schedule$match) %>%
  group_by(match, guess) %>%
  summarise(
    Veikkaajat = paste0(respondent, collapse = ", ")
  ) %>%
  ungroup() %>%
  mutate(Veikkaus = factor(guess, levels = c("1", "x", "2"))) %>%
  arrange(match, Veikkaus) %>%
  select(match, Veikkaus, Veikkaajat) %>%
  rename(Ottelu = match)
  
matchday_scorers <- guesses_scorers %>%
  left_join(results_scorers, by = c("guess" = "player")) %>%
  filter(team %in% c(today_schedule$home, today_schedule$away)) %>%
  group_by(team, guess) %>%
  summarise(
    Veikkaajat = paste0(respondent, collapse = ", ")
  ) %>%
  ungroup() %>%
  arrange(team, guess)  %>%
  rename(Joukkue = team, Pelaaja = guess)

# Summary ----

respondent_points_group <- points_group %>%
  left_join(match_details %>% select(match, group), by = "match") %>%
  group_by(respondent, group) %>%
  summarise(points = sum(points, na.rm = TRUE)) %>%
  rename(osio = "group")

respondent_points_rounds <- 
  bind_rows(
    round16_points %>% mutate(osio = "neljännesvälierät"),
    round8_points %>% mutate(osio = "puolivälierät"),
    round4_points %>% mutate(osio = "välierät"),
    final_points %>% mutate(osio = "finaali"),
    winner_points %>% mutate(osio = "mestari")
  ) %>%
  group_by(respondent, osio) %>%
  summarise(points = sum(points, na.rm = TRUE))

respondent_points_scorers <- points_scorers %>%
  group_by(respondent) %>%
  summarise(points = 0.5 * sum(goals, na.rm = TRUE)) %>%
  mutate(osio = "maalintekijät")

respondent_points_top_scorer <- points_top_scorer %>%
  mutate(osio = "maalikuningas") %>%
  select(respondent, points, osio)

respondent_points_summary <- bind_rows(
  respondent_points_group,
  respondent_points_rounds,
  respondent_points_scorers,
  respondent_points_top_scorer
  )

# Table all guesses ----

guesses_group_wide <- guesses_group %>%
  pivot_wider(id_cols = match, names_from = respondent, values_from = guess) %>%
  rename(Ottelu = match)

guesses_round16_wide <- guesses_round16 %>%
  arrange(respondent, guess) %>%
  group_by(respondent) %>%
  summarise(Veikkaus = paste(guess, collapse = ", ")) %>%
  rename(Vastaaja = respondent)

guesses_round8_wide <- guesses_round8 %>%
  arrange(respondent, guess) %>%
  group_by(respondent) %>%
  summarise(Veikkaus = paste(guess, collapse = ", ")) %>%
  rename(Vastaaja = respondent)

guesses_round4_wide <- guesses_round4 %>%
  arrange(respondent, guess) %>%
  group_by(respondent) %>%
  summarise(Veikkaus = paste(guess, collapse = ", ")) %>%
  rename(Vastaaja = respondent)

guesses_final_wide <- guesses_final %>%
  arrange(respondent, guess) %>%
  group_by(respondent) %>%
  summarise(Veikkaus = paste(guess, collapse = ", ")) %>%
  rename(Vastaaja = respondent)

guesses_winner_wide <- guesses_winner %>%
  arrange(respondent, guess) %>%
  group_by(respondent) %>%
  summarise(Veikkaus = paste(guess, collapse = ", ")) %>%
  rename(Vastaaja = respondent)

guesses_scorers_wide <- guesses_scorers %>%
  arrange(respondent, guess) %>%
  group_by(respondent) %>%
  summarise(Veikkaus = paste(guess, collapse = ", ")) %>%
  rename(Vastaaja = respondent)

guesses_top_scorer_wide <- guesses_top_scorer %>%
  arrange(respondent, guess) %>%
  group_by(respondent) %>%
  summarise(Veikkaus = paste(guess, collapse = ", ")) %>%
  rename(Vastaaja = respondent)

# Plot guess summary ----

plot_teams <- function(df, title){
  df %>%
    group_by(guess) %>%
    summarise(veikkaukset = n()) %>%
    arrange(veikkaukset) %>%
    mutate(guess = as.factor(guess)) %>%
    ggplot(aes(x = reorder(guess, veikkaukset), y = veikkaukset)) + 
    geom_bar(stat = 'identity', fill = "lightblue") + 
    ylim(0, 13) +
    theme_fig +
    labs(x = NULL,
         title = title)
}

plot_to16 <- guesses_round16 %>%
  plot_teams("Joukkueet neljännesvälieriin")

plot_to8 <- guesses_round8 %>%
  plot_teams("Joukkueet puolivälieriin")

plot_to4 <- guesses_round4 %>%
  plot_teams("Joukkueet välieriin")

plot_final <- guesses_final %>%
  plot_teams("Joukkueet finaaliin")

plot_winner <- guesses_winner %>%
  plot_teams("Voittaja")

plot_scorers <- guesses_scorers %>%
  plot_teams("Maalintekijät")

plot_top_scorer <- guesses_top_scorer %>%
  plot_teams("Maalikuningas")

# Plot results ----

blues <- c("#5FB9D5", "#7AC6DC", "#8ACDE0", "#A2D9E7", "#B3E2EB", "#DDE8E9")
reds <- c("#ff4c4c", "#ff7f7f")
greens <- c("#a4fba6", "#4ae54a", "#30cb00", "#0f9200", "#006203")

respondent_points_summary <- respondent_points_summary %>%
  mutate(
    osio = as_factor(osio),
    osio = fct_relevel(
      osio,
      "maalikuningas",
      "maalintekijät",
      "välierät",
      "puolivälierät",
      "neljännesvälierät",
      "finaali",
      "mestari",
      "Lohko A",
      "Lohko B",
      "Lohko C",
      "Lohko D",
      "Lohko E",
      "Lohko F"
    )
  )

plot_summary <- respondent_points_summary %>%
  ggplot(aes(
    x = reorder(respondent, points),
    y = points,
    fill = osio,
    label = points
  )) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(reds, greens, blues)) +
  theme_fig +
  labs(x = NULL,
       title = "Veikkauksen pistetilanne",
       fill = NULL,
       x = "Veikkaaja",
       y = "Pisteet"
  ) +
  geom_text(
    aes(label = after_stat(y), group = respondent), 
    stat = 'summary', fun = sum, nudge_y = 0.1
  )

# Shiny ----

ui <- navbarPage(
  'EM-skaba 2024',
  id = 'mainNav',
  tabPanel(
    'Kokonaistilanne',
    value = 'Kokonaistilanne',
    fluidPage(
      tags$hr(),
      plotOutput("plot_summary"),
      tags$hr(),
      tags$strong('Päivän ottelut'),
      fluidRow(column(12, tableOutput('matchday_games'))),
      tags$hr(),
      tags$strong('Päivän maalintekijät'),
      fluidRow(column(12, tableOutput('matchday_scorers'))),
      tags$hr(),
    )
  ),
  tabPanel(
    'Pistetaulukot',
    value = 'Pistetaulukot',
    fluidPage(
      tags$strong('Pisteet alkulohkon peleistä'),
      fluidRow(column(12,
                      tableOutput('group_points_wide'))),
      tags$hr(),
      tags$strong('Pisteet maalintekijöistä'),
      fluidRow(column(12,
                      tableOutput('points_scorers_wide'))),
      tags$hr(),
      tags$strong('Pisteet neljännesvälierät'),
      fluidRow(column(12,
                      tableOutput('round16_points_wide'))),
      tags$hr(),
      tags$strong('Pisteet puolivälierät'),
      fluidRow(column(12,
                      tableOutput('round8_points_wide'))),
      tags$hr(),
      tags$strong('Pisteet välierät'),
      fluidRow(column(12,
                      tableOutput('round4_points_wide'))),
      tags$strong('Pisteet finaali'),
      fluidRow(column(12,
                      tableOutput('final_points_wide'))),
      tags$strong('Pisteet mestari'),
      fluidRow(column(12,
                      tableOutput('winner_points_wide'))),
      
    )
  ),
  tabPanel(
    'Veikkaukset',
    value = 'Veikkaus',
    fluidPage(
      tags$hr(),
      tags$strong('Lohkopelit'),
      fluidRow(column(12,
                      tableOutput('guesses_group_wide'))),
      tags$hr(),
      tags$strong('Neljännesvälierät'),
      fluidRow(column(12,
                      tableOutput('guesses_round16_wide'))),
      tags$hr(),
      tags$strong('Puolivälierät'),
      fluidRow(column(12,
                      tableOutput('guesses_round8_wide'))),
      tags$hr(),
      tags$strong('Välierät'),
      fluidRow(column(12,
                      tableOutput('guesses_round4_wide'))),
      tags$hr(),
      tags$strong('Finaali'),
      fluidRow(column(12,
                      tableOutput('guesses_final_wide'))),
      tags$hr(),
      tags$strong('Mestari'),
      fluidRow(column(12,
                      tableOutput('guesses_winner_wide'))),
      tags$hr(),
      tags$strong('Maalintekijät'),
      fluidRow(column(12,
                      tableOutput('guesses_scorers_wide'))),
      tags$hr(),
      tags$strong('Maalikuningas'),
      fluidRow(column(12,
                      tableOutput('guesses_top_scorer_wide')))
    )
  ),
  tabPanel(
    'Tilastoja veikkauksista',
    value = 'Yhteenveto',
    fluidPage(
      tags$hr(),
      plotOutput("plot1"),
      tags$hr(),
      plotOutput("plot2"),
      tags$hr(),
      plotOutput("plot3"),
      tags$hr(),
      plotOutput("plot4"),
      tags$hr(),
      plotOutput("plot5"),
      tags$hr(),
      plotOutput("plot6"),
      tags$hr(),
      plotOutput("plot7"),
    )
  ),
  tags$head(
    tags$style(
      'body{min-height: 600px; height: auto; max-width: 1200px; margin: auto;}
      .bottomSpacer{height: 100px;}
      .btn{background-color: steelblue; color: white;}'
    )
  )
)


server <- function(input, output, session) {
  # first page
  output$guesses_group_wide <- renderTable(guesses_group_wide, align = "c")
  output$guesses_round16_wide <- renderTable(guesses_round16_wide, align = "c")
  output$guesses_round8_wide <- renderTable(guesses_round8_wide, align = "c")
  output$guesses_round4_wide <- renderTable(guesses_round4_wide, align = "c")
  output$guesses_final_wide <- renderTable(guesses_final_wide, align = "c")
  output$guesses_winner_wide <- renderTable(guesses_winner_wide, align = "c")
  output$guesses_scorers_wide <- renderTable(guesses_scorers_wide, align = "c")
  output$guesses_top_scorer_wide <- renderTable(guesses_top_scorer_wide, align = "c")
  output$matchday_games <- renderTable(matchday_games, align = "c")
  output$matchday_scorers <- renderTable(matchday_scorers, align = "c")
  output$group_points_wide <- renderTable(group_points_wide, align = "c")
  output$round16_points_wide <- renderTable(round16_points_wide, align = "c")
  output$round8_points_wide <- renderTable(round8_points_wide, align = "c")
  output$round4_points_wide <- renderTable(round4_points_wide, align = "c")
  output$final_points_wide <- renderTable(final_points_wide, align = "c")
  output$winner_points_wide <- renderTable(winner_points_wide, align = "c")
  output$points_scorers_wide <- renderTable(points_scorers_wide, align = "l")
  output$plot1 <- renderPlot({
    plot_to16
  })
  output$plot2 <- renderPlot({
    plot_to8
  })
  output$plot3 <- renderPlot({
    plot_to4
  })
  output$plot4 <- renderPlot({
    plot_final
  })
  output$plot5 <- renderPlot({
    plot_winner
  })
  output$plot6 <- renderPlot({
    plot_scorers
  })
  output$plot7 <- renderPlot({
    plot_top_scorer
  })
  output$plot_summary <- renderPlot({
    plot_summary
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
