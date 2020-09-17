library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

# Scatterplot of the relationship between HRs and wins

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R /G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
# Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#runs per game vs ab per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R/ G) %>%
  ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#winrate vs fielding errors
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(E_per_game = E / G, R_per_game = R/ G) %>%
  ggplot(aes(E_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# TRIPLES VS DOUBLES
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B/G, X3B_per_game = X3B/G) %>%
  ggplot(aes(X2B_per_game, X3B_per_game)) +
  geom_point(alpha = 0.5)
