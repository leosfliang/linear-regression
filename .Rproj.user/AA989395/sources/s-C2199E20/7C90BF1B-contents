# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
# RSS MIN WHEN B1 AT 0.65
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))


####lm function ####
# fit regression line to predict son's height from father's height
fit <- lm(son~father, data = galton_heights)
fit

# summary statistics
summary(fit)


#### LSE is a randomvariable ####
# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
# looks normal because CLT applies
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics for one of the simulated data sets
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

# se  estimates from summary function closed to the se from monte carlo
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#p-value is prob of bigger than abs(t-value)
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>% summary

#LSE can be strongly correlated
lse %>% summarize(cor(beta_0, beta_1))

#However, the correlation depends on how the predictors are defined or transformed.
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,]) 



#### Predicted Variables are Random Variables ####
# plot predictions and confidence intervals
# the band is the CI
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
Y_hat

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

#plot the predictions and confidence intervals for our linear model of sons’ heights
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


#### ASSESSMENT ####
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#Q3
library(Lahman)

Teams %>% filter(yearID %in% 1961:2001) %>% mutate(runs_per_game = R/G,
                                                   BB_per_game = BB/G,
                                                   HR_per_game = HR/G) %>%
  lm(runs_per_game ~ BB_per_game + HR_per_game, data = .)

#Q4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

#Q7set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

model <- female_heights %>% lm(mother~daughter, data = .)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(mother = female_heights$mother)

#q9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles)) %>%
  filter(mean_singles >0.2) %>%
  nrow

Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_bb = mean(bb)) %>%
  filter(mean_bb >0.2) %>%
  nrow
  
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  select(playerID, singles, bb) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  inner_join(.,bat_02) 

corr1 <- bat_99_01 %>% summarize(cor(mean_singles,singles), cor(mean_bb,bb))

plot1 <- bat_99_01 %>% ggplot(aes(singles,mean_singles)) +
  geom_point()

plot2 <- bat_99_01 %>% ggplot(aes(bb, mean_bb)) + 
  geom_point()

grid.arrange(plot1,plot2)

bat_99_01 %>% lm(singles~mean_singles,data=.)
bat_99_01 %>% lm(bb~mean_bb, data=.)
