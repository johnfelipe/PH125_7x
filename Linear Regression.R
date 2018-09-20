# loading the required packages

library(dslabs)
library(tidyverse)

# looking at the Lahman library
library(Lahman)
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

# Correlation example using HistData package
library(HistData)
?`HistData-package`
# load the GaltonFamilies data set
data("GaltonFamilies")
# check the str of the data
str(GaltonFamilies)

# look at the relationship between the father's and 1st son's heights
# create a small data set

galton_heights <- as.tibble(GaltonFamilies %>% 
  filter(childNum == 1 & gender == "male") %>% 
  select(father, childHeight) %>%
  rename(son = childHeight))
galton_heights

# look at some summary stats
galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son))

# and now a plot
galton_heights %>% 
  ggplot(aes(father,son)) +
  geom_point(alpha = 0.5)

# let's check correlation. using the %>%  and the summarize func calling
# cor
galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son),cor(father,son))

# sample correlation is a random variable. here we're going to run # a monte carlo
# simulation a 1000x taking various sample sizes of 25, 50, 75
# "sample_n" samples an entire row, not just a single value like "sample"
# We use sample_n since we're looking at the correlation bewteen father & son paired
# heights
# Note that the obect "R" is a vector of values

B <- 1000
N <- 75
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    summarize(r = cor(father,son)) %>% 
    .$r
})

str(R)
# using R as a tibble
tibble(R) %>% 
  ggplot(aes(R)) +
  geom_histogram(binwidth = 0.05, color = "black")

tibble(R) %>% 
  summarize(mean(R), sd(R))

# for a large enough sample size then R ~ N, with
# E[R] = r  and SD = sqrt((1-r^2)/(N-2))
# use R to estimate r
tibble(R) %>% 
  ggplot(aes(sample = R)) +
  geom_qq() +
  geom_abline(intercept = mean(R),
              slope = sqrt((1 - mean(R)^2)/(N-2)))

# stratification expample. Predictig the heigth of a son given no information on father
# use the average of son

galton_heights %>% 
  summarize(mean(son), sd(son))
# if told the father is 72 inches then prediction based on/conditioned on the 
# father's height (son|father = 72)
galton_heights %>% 
  filter(round(father) == 72) %>% 
  summarize(mean(son), sd(son))

# strafying the fathers heights' to the nearest integer and plotting

galton_heights %>% 
  mutate(father_strata = factor(round(father))) %>% 
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# a plot sons' heights conditioned on the fathers height
galton_heights %>% 
  mutate(father = round(father)) %>% 
  group_by(father) %>% 
  summarize(son_conditional_ave = mean(son)) %>% 
  ggplot(aes(father, son_conditional_ave)) +
  geom_point()

# ploting the standardized father son heigths and adding a line
# with slope = to the correlation coef.
# Note "scale()" automatically computes the standardized heights

r <- galton_heights %>% 
      summarize(r = cor(father,son)) %>% 
      .$r
galton_heights %>% 
  mutate(father = round(father)) %>% 
  group_by(father) %>% 
  summarize(son = mean(son)) %>% 
  mutate(z_father = scale(father), z_son = scale(son)) %>% 
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)
  
#  using the regression line formula to the intercept and slope
# slope "m" = r*s_y/s_x. words m = cor(x,y) coef*sd predicted var/sd predictor var
# E[son|father]
mu_x <- mean(galton_heights$father) # predictor var mean
mu_y <- mean(galton_heights$ son) # predicted var mean
s_x <- sd(galton_heights$father) # preditor var SD
s_y <- sd(galton_heights$son) # predicted var SD
r <- cor(galton_heights$father,galton_heights$son)# cor coeff
m <- r*s_y/s_x
m_1 <- r*s_y/s_x # slope
b_1 <- mu_y - m*mu_x # intercept
m_1
b_1

# a plot of the sons heights vs fathers heights and the intercept and slope of regreesion line above
galton_heights %>% 
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b_1, slope = m_1)

# Bivariate Normal Distribution example
# plotting sons heigth vs standardized fathers height
# to check that Y|X=x is ~Normal

galton_heights %>% 
  mutate(z_father = round((father - mean(father))/sd(father))) %>% 
  filter(z_father %in% -2:2) %>% 
  ggplot() +
  stat_qq(aes(sample = son)) +
  facet_wrap(~z_father)

# computing E[father|son] regression line using the values above

m_2 <- r*s_x/s_y #slope
b_2 <- mu_x - m*mu_y
m_2
b_2
# a plot of the fathers heights vs sons heights and the intercept and slope of regreesion line above
galton_heights %>% 
  ggplot(aes(father,son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b_1, slope = m_1, color = "blue") +
  geom_abline(intercept = -b_2/m_2, slope = 1/m_2, color = "red")

# confounding example using baseball example 
Teams %>%
  filter(yearID %in% 1961:2001) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>% 
  summarize(cor(BB,HR), cor(Singles, HR), cor(BB, Singles))

# Stratification and Multivariant Regression
# making a HR stratified data set for example

dat <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(HR_strata = round(HR/G,1), BB_per_game = BB/G, R_per_game = R/G) %>% 
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

# make a plot of R_per_game vs BB_per_game for each HR_strata
dat %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)

# looking at slope R_per_game vs BB_per_game for each HR_strata

dat %>% 
  group_by(HR_strata) %>% 
  summarise(slope = cor(BB_per_game,R_per_game)*sd(R_per_game)/sd(BB_per_game))

# Stratification and Multivariant Regression
# making a BB stratified data set for example

dat <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB_strata = round(BB/G,1), HR_per_game = HR/G, R_per_game = R/G)%>% 
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)

# make a plot of R_per_game vs HR_per_game for each BB_strata
dat %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_strata)

# looking at slope R_per_game vs BB_per_game for each HR_strata

dat %>% 
  group_by(BB_strata) %>% 
  summarise(slope = cor(HR_per_game,R_per_game)*sd(R_per_game)/sd(HR_per_game))

lm(son ~ father, data = galton_heights)

galton_heights <- galton_heights %>% 
  mutate(father_centered = father - mean(father))

lm(son ~ father_centered, data = galton_heights)

# a function to compute RSS for a pair of data points

rss <- function(beta0,beta1, data){
  resid <- galton_heights$son - (beta0 + beta1 * galton_heights$father)
  return(sum(resid^2))
}

# looking/searching for the beta1 that minimizes the RSS w/ beta0 fixed at 25 and 36

beta1 <- seq(0,1, len= nrow(galton_heights))
results <- tibble(beta1 = beta1,
                  rss = sapply(beta1, rss, beta0 = 36))

filter(results, rss == min(rss))
results %>%
  ggplot(aes(beta1, rss)) +
  geom_line() +
  geom_line(aes(beta1, rss), col = 2)

# using the "lm' function to predict R_per_game based on BB_per_game and HR_per_game
dat <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR/G, BB_per_game = BB/G, R_per_game = R/G)
str(dat)
fit <- lm(R_per_game ~ HR_per_game + BB_per_game, data = dat)
summary(fit)

# using monte carlo simultion to estimate the LSE
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>%
    .$coef
})
lse <- tibble(beta_0 = lse[1,], beta_1 = lse[2,])
lse

# plotting beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% 
  ggplot(aes(beta_0)) +
  geom_histogram(binwidth = 5, color = 'black') 
p2 <- lse %>% 
  ggplot(aes(beta_1)) +
  geom_histogram(binwidth = 0.1, color = 'black') 
grid.arrange(p1,p2, ncol = 2)

sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>%
  summary

lse %>% 
  summarise(se_0 = sd(beta_0), se_1 = sd(beta_1))

galton_heights %>% 
  ggplot(aes(son,father)) +
  geom_point() +
  geom_smooth(method = "lm")

galton_heights %>% 
  mutate(Y_hat = predict(lm(son ~ father, data = .))) %>% 
  ggplot(aes(father, Y_hat)) + geom_line()

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
head(predictions)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
data
ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

tibble(id = c(1,2,3), func = c(mean, median, sd))

dat <- as.tibble(dat)
dat
dat %>% 
  group_by(HR_per_game) %>% 
  do(fit = lm(R_per_game ~ BB_per_game, data = .))

# playing witht the "broom" library

library(broom)

fit <- lm(R_per_game ~ BB_per_game, data = dat)
tidy(fit)
tidy(fit, conf.int = TRUE)

dat %>% 
  group_by(round(HR_per_game, 0.5)) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game, data = .), conf.int =TRUE))

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

# fitting a linear regression model w/ two predictors

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>% 
  lm(R ~ BB + HR, data =.)

tidy(fit, conf.int = TRUE)

# fitting a linear model w/ many predictors
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  select(BB, singles, doubles, triples, HR, R) %>% 
  lm(R ~., data = .)
coef <- tidy(fit, conf.int = TRUE)
coef

# using the fitted model to predict runs R_hat in 2002

Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>% 
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x = 0.1, cex = 2) +
  geom_abline()
coef$estimate
Team_A <- c(1,2,4,1,0,1)
sum(coef$estimate * Team_A)
Team_B <- c(1,1,6,2,1,0)
sum(coef$estimate * Team_B)

pa_per_game <- Batting %>% 
  filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  .$pa_per_game %>% 
  mean
pa_per_game

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

players %>% ggplot(aes(R_hat)) + 
  geom_histogram(binwidth = 0.5, color = "black")

players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

 players <- Fielding %>% 
  filter(yearID == 2002) %>%
  filter(!POS %in% c("OF","P")) %>%
  group_by(playerID) %>%
  top_n(1, G) %>%
  filter(row_number(G) == 1) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by="playerID")

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

library(reshape2)
library(lpSolve)
players <- players %>% filter(debut <= 1997 & debut > 1988)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
#> Using R_hat as value column: use value.var to override.
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

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

Batting %>% 
  filter(yearID %in% 1990:2001) %>% 
  group_by(playerID, yearID) %>%
  filter(BB + AB >= 300) %>%
  mutate(PA = BB + AB, 
         singles = (H-X2B-X3B-HR),
         OPS = BB / PA + 
           (singles + 2*X2B + 3*X3B + 4*HR)/AB,
         G = PA/pa_per_game, 
         BB = BB/G,
         singles = singles/G,
         doubles = X2B/G, 
         triples = X3B/G,
         HR = HR/G) %>%
  ungroup() %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ungroup %>%
  ggplot(aes(OPS, R_hat)) + 
  geom_point()

falling_object <- rfalling_object()
falling_object

falling_object %>% 
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

fit <- falling_object %>% 
  mutate(time_sq = time^2) %>% 
  lm(observed_distance ~ time + time_sq, data = .)
tidy(fit)

augment(fit) %>% 
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = 'blue')

tidy(fit, conf.int = TRUE)

# Correlation is NOT Causation example

N <- 25
G <- 10^6
sim_dat <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))

res <- sim_dat %>% 
  group_by(group) %>% 
  summarise(r = cor(X,Y)) %>% 
  arrange(desc(r))
res

sim_dat %>% 
  filter(group == res$group[which.max(res$r)]) %>% 
  ggplot(aes(X,Y)) +
  geom_point() +
  geom_smooth(method = 'lm')

res %>% 
  ggplot(aes(x = r)) +
  geom_histogram(binwidth = 0.1, color = 'black')

sim_dat %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~ X, data = .)))

# Correlation and Outlies - Spearman rank correlation

# generating a sample data set with zero correlation but one extreme outlier
set.seed(1)
x <- rnorm(100,100,1)
y <- rnorm(100,100,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

tibble(x,y) %>% ggplot(aes(x,y)) + geom_point(alpha = 0.5)

# and checking correlation - the one extreme outlier makes correlation almost 1!
cor(x,y)

# One solution is to look at correlation on the rank of the values
# the plot shows the transformaion removes the leverage of the outlier
x_rank <- rank(x)
y_rank <- rank(y)
tibble(x_rank,y_rank) %>% 
  ggplot(aes(x_rank, y_rank)) +
  geom_point(alpha = 0.5) 

# now correlation is closer to 0 where it should be
cor(x_rank,y_rank)
# can cause the "cor()" function to use Spearman
cor(x,y, method = "spearman")  
