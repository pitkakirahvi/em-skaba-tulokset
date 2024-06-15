
library(readr)
library(ggplot2)

# Readers ----

read_remote <- function(file_name){
  url <- paste0("https://raw.githubusercontent.com/pitkakirahvi/em-skaba-tulokset/main/data/", file_name)
  read_delim(url, delim = "\t", locale = locale("fi"))
}

read_local <- function(file_name){
  read_delim(file_name, here("data", file_name), delim = "\t", locale = locale("fi"))
}

write_local <- function(data, file_name){
  write_delim(data, here("data", file_name), delim = "\t")
}

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
