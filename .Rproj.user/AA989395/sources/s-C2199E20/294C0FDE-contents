library(tidyverse)
install.packages("HistData")
library(HistData)
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

### Correlation Coefficient ###
rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

### Sample Correlation is a Random Variable ###
# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)
# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

# ASSESSMENT
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R/ G) %>%
  summarize( cor(AB_per_game, R_per_game))

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(E_per_game = E / G, W_per_game = W/ G) %>%
 summarize( cor(E_per_game, W_per_game))

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B/G, X3B_per_game = X3B/G) %>%
  summarize(cor(X2B_per_game, X3B_per_game))
