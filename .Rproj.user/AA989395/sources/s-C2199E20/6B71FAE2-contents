library(tidyverse)
# linear regression with two variables
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R~BB + singles + doubles +triples+ HR, data = .)
coefs <- tidy(fit, conf.int= T)
coefs


# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
# select most played position
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
#remove outfielders and pitchers
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()


#### Linear Programming ####
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 
# This algorithm chooses these 9 players:
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

# We note that these players all have above average BB and HR rates while the same is not true for singles.
my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))


#### regression fallacy ####
#The code to create a table with player ID, their names, and their most played position:
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# The code to create a table with only the ROY award winners and add their batting statistics:
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# The code to use the spread function to have one column for the rookie and sophomore years batting averages:
  ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

# The code to do the similar analysis on all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year):
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

# The code to see what happens to the worst performers of 2013:
arrange(two_years, `2013`)

# The code to see  the correlation for performance in two separate years:
qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))


# The code to use dslabs function rfalling_object to generate simulations of dropping balls:
library(dslabs)
falling_object <- rfalling_object()

# The code to draw the trajectory of the ball:
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

# The code to use the lm() function to estimate the coefficients:
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

# The code to check if the estimated parabola fits the data:
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

# The code to see the summary statistic of the regression:
# initial velocity of 0 is in the conf int for beta 1
#   not reject H0 that v0 = 0
tidy(fit, conf.int = TRUE)

#### ASSESSment
teamA <- c(1,2,4,1,0,1)
teamB <- c(1,1,6,2,1,0)
coefs %>% select(estimate) %>% summarise(A_R = sum(estimate*teamA), B_R = sum(estimate*teamB))

Teams %>% 
  filter(yearID %in% 1971) %>% 
  do(tidy(lm(R~BB+HR, data = .))) 

res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")
 

res %>%
  filter(term == "BB") %>%
  do(tidy(lm(estimate ~ yearID, data = .)))

    
#### ass 2 ####
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small %>%
  mutate(R_per_g = R/G) %>%
  lm(avg_attendance~R_per_g, data = .)

Teams_small %>%
  mutate(HR_per_g = HR/G) %>%
  lm(avg_attendance~HR_per_g, data = .)

Teams_small %>%
  lm(avg_attendance~W, data =.)

Teams_small %>%
  lm(avg_attendance~yearID, data =.)

Teams_small %>%
  mutate(R_per_g = R/G) %>%
  mutate(HR_per_g = HR/G) %>%
  summarise(corWR = cor(W,R_per_g), corWHR = cor(W,HR_per_g))

Teams_small %>%
  mutate(winsover10 = round(W/10),
         R_per_g = R/G, 
         HR_per_g = HR/G, 
         avg_attendance = attendance/G) %>%
  filter(winsover10 %in% 5:10) %>%
  group_by(winsover10) %>%
  select(winsover10, R_per_g,HR_per_g,avg_attendance) %>%
  do(tidy(lm(avg_attendance~R_per_g, data = .))) %>%
  filter(term == "R_per_g") %>%
  arrange(desc(estimate))

Teams_small %>%
  mutate(winsover10 = round(W/10),
         R_per_g = R/G, 
         HR_per_g = HR/G, 
         avg_attendance = attendance/G) %>%
  filter(winsover10 %in% 5:10) %>%
  group_by(winsover10) %>%
  select(winsover10, R_per_g,HR_per_g,avg_attendance) %>%
  do(tidy(lm(avg_attendance~HR_per_g, data = .))) %>%
  filter(term == "HR_per_g") %>%
  arrange(desc(estimate))

#4
fit <- Teams_small %>%
  mutate(R_per_g = R/G) %>%
  mutate(HR_per_g = HR/G) %>%
  lm(avg_attendance~ R_per_g + HR_per_g + W + yearID, data= .) 

pred_attend <- fit %>%
  summarise(fit1 = sum(c(1,5,1.2,80,2002)*estimate),
            fit2 = sum(c(1,5,1.2,80,1960)*estimate)) %>%
  .$fit1

year02data <- Teams %>% filter(yearID %in% 2002) %>% 
  mutate(avg_attendance = attendance/G, HR_per_g = HR / G, R_per_g = R/G)

predtion <- predict(fit,newdata = year02data)
year02att <- Teams %>%
  mutate(avg_attendance = attendance/G) %>% 
  filter(yearID %in% 2002) %>% pull(attendance)
cor(predtion,year02att)
