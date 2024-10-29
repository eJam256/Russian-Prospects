library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)
gs4_deauth()
all_players <- "https://docs.google.com/spreadsheets/d/1zgfn3G9yraz60WqHjZUGPQMBlPWdHz0fpFVimARD_CE/edit?usp=sharing"
prospects <- read_sheet(all_players, sheet = "Prospects")
prospects
NHL_players <- read_sheet(all_players, sheet = "NHLers")
NHL_players
control <- read_sheet(all_players, sheet = "Control")
control
control_actual <- read_sheet(all_players, sheet = "Control Actual")
control_actual
work_set <- bind_rows(list(prospects, NHL_players, control, control_actual))
work_set <- work_set %>% mutate(League_ID = as.factor(League_ID))
work_set
total_graph <- ggplot(data = work_set, 
       mapping = aes(x = Games_Played, y = Points, color = Position, shape = League_ID)) + 
  geom_point() + scale_y_continuous(breaks = seq(0, 130, by = 5)) + scale_x_continuous(breaks = seq(0, 82, by = 6))
summary(work_set)
forward_set <- work_set |> filter (Position == 'LW' | Position == 'RW' | Position == 'C')
forward_graph <- ggplot(data = forward_set, 
                        mapping = aes(x = Games_Played, y = Points, color = League_ID)) + 
  geom_point() + scale_y_continuous(breaks = seq(0, 130, by = 5)) + 
  scale_x_continuous(breaks = seq(0, 82, by = 6)) + labs(
    title = "Points by Forwards", x = "Games Played", y = "Points", color = "League" 
  ) + scale_color_hue(labels = c("MHL", "KHL", "VHL", "NHL", "CHL", "AHL"))
forward_graph
ggsave("forward_graph.png")
defense_set <- work_set |> filter(Position == 'LHD' | Position == 'RHD')
defense_graph <- ggplot(data = defense_set, 
                        mapping = aes(x = Games_Played, y = Points)) + 
  geom_point(mapping = aes(color = League_ID)) + scale_y_continuous(breaks = seq(0, 130, by = 5)) + 
  scale_x_continuous(breaks = seq(0, 82, by = 6)) + labs(
    title = "Points by Defenders", x = "Games Played", y = "Points", color = "League"
  ) + scale_color_hue(labels = c("MHL", "KHL", "VHL", "NHL", "CHL", "AHL"))
defense_graph
ggsave("defense_graph.png")
