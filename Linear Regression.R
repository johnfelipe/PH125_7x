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
r <- cor(galton_heights$father,galton_heights$son) # cor coeff
m <- r*s_y/s_x # slope
b <- mu_y - m*mu_x # intercept
m
b

# a plot of the sons heights vs fathers heights and the intercept and slope of regreesion line above
galton_heights %>% 
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

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

m <- r*s_x/s_y #slope
b <- mu_x - m*mu_y
m
b
# a plot of the fathers heights vs sons heights and the intercept and slope of regreesion line above
galton_heights %>% 
  ggplot(aes(son, father)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)
