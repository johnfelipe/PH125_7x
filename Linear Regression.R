# loading the required packages
library(Lahman)
library(dslabs)
library(tidyverse)

# looking at the Lahman library
?`Lahman-package`
# checking out the Teams table
str(Teams)

# exploring the relationship between home-runs (HR)/game and Runs (R)/game
# with a scatterplot. specifically do teams that hit more HR score more R?
# limiting the analysis to 1961 - 2001
HR_plot <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>% 
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
# ggsave("HR_plot.png")
HR_plot

# from the plot it's clear that as HR/G increase so do R/G

# now looking at the relationship between stolen bases (SB)/game and R/G
SB_plot <- Teams %>%  
  filter(yearID %in% 1961:2001) %>% 
  mutate(SB_per_game = SB/G, R_per_game = R/G) %>% 
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.50)
# ggsave("SB_plot.png")
SB_plot

# and now the relationship betwwen bases on balls (BB)/game and R/G
BB_plot <- Teams %>%  
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
ggsave("BB_plot.png")
BB_plot

# and now at-bats (AB)/game vs R/G
AB_plot <- Teams %>%  
  filter(yearID %in% 1961:2001) %>% 
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>% 
  ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
ggsave("AB_plot.png")
AB_plot
