library(shiny)
library(tidyverse)
library(here)
library(lubridate)

# Load data ----

load(file = "rows.Rdata")

match_details <- read_delim("group_matches.txt",
                            delim = "\t",
                            locale = locale("fi"))

scorers_results <- read_delim("scorers_results.txt",
                              delim = "\t",
                              locale = locale("fi"))

group_results <- read_delim("group_results.txt",
                            delim = "\t",
                            locale = locale("fi"))

# Clean data ----

# Matchday
match_details <- match_details %>%
  mutate(match = paste0(home, "-", away),
         date = dmy(date))

# Group points
points_group <- matches %>%
  gather(veikkaaja, veikkaus, 2:ncol(.)) %>%
  right_join(group_results) %>%
  mutate(points = if_else(veikkaus == result, 1, 0))

group_points_t <- points_group %>%
  select(match, points, veikkaaja) %>%
  spread(veikkaaja, points) %>%
  left_join(match_details %>% select(match, group), by = "match") %>%
  left_join(group_results, by = "match") %>%
  relocate(group, match, result) %>%
  arrange(group, match)

# Rounds
result_round16 <- read_lines("round16.txt")
result_round8 <- read_lines("round8.txt")

round16_points <- round16 %>%
  gather(veikkaaja, veikkaus, 1:ncol(.)) %>%
  mutate(points = if_else(veikkaus %in% result_round16, 1, 0))

round16_points_t <- round16_points %>%
  spread(veikkaaja, points) %>%
  filter(veikkaus %in% result_round16) %>%
  replace(is.na(.), 0)

round8_points <- round8 %>%
  gather(veikkaaja, veikkaus, 1:ncol(.)) %>%
  mutate(points = if_else(veikkaus %in% result_round8, 1, 0))

round8_points_t <- round8_points %>%
  spread(veikkaaja, points) %>%
  filter(veikkaus %in% result_round8) %>%
  replace(is.na(.), 0)

# Scorers
points_scorers <- scorers %>%
  gather(veikkaaja, player) %>%
  left_join(scorers_results, by = "player") %>%
  mutate(points = 0.5 * goals) %>%
  select(veikkaaja, player, goals, points)

points_scorers_t <- points_scorers %>%
  group_by(player) %>%
  summarise(
    goals = unique(goals, na.rm = TRUE),
    points = 0.5 * goals,
    veikkaajat = paste0(veikkaaja, collapse = ", ")
  ) %>%
  replace_na(list(goals = 0, points = 0)) %>%
  arrange(-goals)

top_scorer_results <- scorers_results %>%
  filter(goals == max(goals)) %>%
  mutate(points = 3)

points_top_scorer <- top_scorer %>%
  gather(veikkaaja, player) %>%
  left_join(top_scorer_results, by = "player") %>%
  replace_na(list(goals = 0, points = 0)) %>%
  select(veikkaaja, player, goals, points)

# Veikkaajat
veikkaaja_gp <- points_group %>%
  left_join(match_details %>% select(match, group), by = "match") %>%
  group_by(veikkaaja, group) %>%
  summarise(points = sum(points)) %>%
  rename(osio = "group")

veikkaaja_rounds <- bind_rows(
  round16_points %>% mutate(osio = "round16"),
  round8_points %>% mutate(osio = "round8")
) %>%
  group_by(veikkaaja, osio) %>%
  summarise(points = sum(points, na.rm = TRUE))

veikkaaja_scorers <- points_scorers %>%
  group_by(veikkaaja) %>%
  summarise(points = 0.5 * sum(goals, na.rm = TRUE)) %>%
  mutate(osio = "maalintekijät")

veikkaaja_top_scorer <- points_top_scorer %>%
  mutate(osio = "maalikuningas") %>%
  select(veikkaaja, points, osio)

veikkaaja_points <- bind_rows(veikkaaja_gp,
                              veikkaaja_rounds,
                              veikkaaja_scorers,
                              veikkaaja_top_scorer)

# Plot theme ----

theme_fig <- theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")
  )

# Plot guess ----

plot_teams <- function(df, title){
  df %>%
    gather(player, teams) %>%
    group_by(teams) %>%
    summarise(veikkaukset = n()) %>%
    arrange(veikkaukset) %>%
    mutate(teams = as.factor(teams)) %>%
    ggplot(aes(x = reorder(teams, veikkaukset), y = veikkaukset)) + 
    geom_bar(stat = 'identity', fill = "lightblue") + 
    ylim(0, 13) +
    theme_fig +
    labs(x = NULL,
         title = title)
}

plot_to16 <- round16 %>%
  plot_teams("Joukkueet neljännesvälieriin")

plot_to8 <- round8 %>%
  plot_teams("Joukkueet puolivälieriin")

plot_to4 <- round4 %>%
  plot_teams("Joukkueet välieriin")

plot_final <- final %>%
  plot_teams("Joukkueet finaaliin")

plot_winner <- winner %>%
  plot_teams("Voittaja")

plot_scorers <- scorers %>%
  plot_teams("Maalintekijät")

plot_top_scorer <- top_scorer %>%
  plot_teams("Maalikuningas")

# Plot results ----

blues <- c("#5FB9D5", "#7AC6DC", "#8ACDE0", "#A2D9E7", "#B3E2EB", "#DDE8E9")
reds <- c("#ff4c4c", "#ff7f7f")
greens <- c("#a4fba6", "#4ae54a") #, "#30cb00", "#0f9200", "#006203")

veikkaaja_points <- veikkaaja_points %>%
  mutate(
    osio = as_factor(osio),
    osio = fct_relevel(
      osio,
      "maalikuningas",
      "maalintekijät",
      "round8",
      "round16",
      "Group A",
      "Group B",
      "Group C",
      "Group D",
      "Group E",
      "Group F"
    )
  )

veikkaaja_total_points <- veikkaaja_points %>%
  group_by(veikkaaja) %>%
  summarise(points = sum(points))

p_veikkaajat <- veikkaaja_points %>%
  ggplot(aes(
    x = reorder(veikkaaja, points),
    y = points,
    fill = osio,
    label = points
  )) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(reds, greens, blues)) +
  #geom_text(stat = 'identity',
  #          position = position_stack(vjust = .5),
  #          size = 4) +
  theme_fig +
  labs(x = NULL,
       title = "Veikkauksen pistetilanne:") +
  geom_text(
    data = veikkaaja_total_points,
    aes(
      x = reorder(veikkaaja, points),
      y = points,
      fill = NULL,
      label = points
    ),
    nudge_y = 5,
    size = 4.5,
    fontface = "bold"
  )

# Shiny ----

ui <- navbarPage(
  'EM-skaba 2021',
  id = 'mainNav',
  tabPanel(
    'Pisteet',
    value = 'Pisteet',
    fluidPage(
      tags$hr(),
      plotOutput("p_veikkaajat"),
      tags$hr(),
      tags$strong('Pisteet alkulohkon peleistä:'),
      fluidRow(column(12,
                      tableOutput('group_points_t'))),
      tags$hr(),
      tags$strong('Pisteet maalintekijöistä:'),
      fluidRow(column(12,
                      tableOutput('points_scorers_t'))),
      tags$hr(),
      tags$strong('Pisteet neljännesvälierät:'),
      fluidRow(column(12,
                      tableOutput('round16_points_t'))),
      tags$hr(),
      tags$strong('Pisteet puolivälierät:'),
      fluidRow(column(12,
                      tableOutput('round8_points_t'))),
      
    )
  ),
  tabPanel(
    'Veikkaukset',
    value = 'Veikkaus',
    fluidPage(
      tags$hr(),
      tags$strong('Lohkopelit:'),
      fluidRow(column(12,
                      tableOutput('matches'))),
      tags$hr(),
      tags$strong('Neljännesvälierät:'),
      fluidRow(column(12,
                      tableOutput('round16'))),
      tags$hr(),
      tags$strong('Puolivälierät:'),
      fluidRow(column(12,
                      tableOutput('round8'))),
      tags$hr(),
      tags$strong('Välierät:'),
      fluidRow(column(12,
                      tableOutput('round4'))),
      tags$hr(),
      tags$strong('Finaali:'),
      fluidRow(column(12,
                      tableOutput('final'))),
      tags$hr(),
      tags$strong('Mestari:'),
      fluidRow(column(12,
                      tableOutput('winner'))),
      tags$hr(),
      tags$strong('Maalintekijät:'),
      fluidRow(column(12,
                      tableOutput('scorers'))),
      tags$hr(),
      tags$strong('Maalikuningas:'),
      fluidRow(column(12,
                      tableOutput('top_scorer')))
    )
  ),
  tabPanel(
    'Yhteenvedot veikkauksista',
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
  output$matches <- renderTable(matches, align = "c")
  output$round16 <- renderTable(round16, align = "c")
  output$round8 <- renderTable(round8, align = "c")
  output$round4 <- renderTable(round4, align = "c")
  output$final <- renderTable(final, align = "c")
  output$winner <- renderTable(winner, align = "c")
  output$scorers <- renderTable(scorers, align = "c")
  output$top_scorer <- renderTable(top_scorer, align = "c")
  output$matchday <- renderTable(matchday, align = "c")
  output$group_points_t <- renderTable(group_points_t, align = "c")
  output$round16_points_t <- renderTable(round16_points_t, align = "c")
  output$round8_points_t <- renderTable(round8_points_t, align = "c")
  output$points_scorers_t <-
    renderTable(points_scorers_t, align = "l")
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
  output$p_veikkaajat <- renderPlot({
    p_veikkaajat
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
