library(tidyverse)
library(lubridate)
library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

d <- read_csv("./data/UKdriversKSI.txt") %>% 
  mutate(lKSI = log(KSI),
         Date = ymd("1969-01-31") %m+% months(0:191))

petrol <- read_csv("./data/logUKpetrolprice.txt", 
                   col_names = "log_price",
                   skip = 1)

d <- bind_cols(d, petrol)

d$w <- 0
d[170:192,'w'] <- 1

#7.1----

d1 <- list(y = d$lKSI,
           x = d$log_price,
           N = nrow(d),
           w = d$w,
           k = 12)

m1 <- stan(file = "m7_1.stan",
           data = d1)


e1 <- extract(m1)

probs <- c(0.025, 0.5, 0.975)

l <- as_tibble(e1$y_plot) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(lKSI = d$lKSI,
         diff = lKSI - med)

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_line(aes(y = lKSI), color = 'red') +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 6.1')

#this plot looks a little different than the book
#I'm sure there is a good reason but I am not inclined
#to spend the time to figure out why.
acf(l$diff, main = "Fig 7.5", sub = "plotting a little out of order")

waic(e1$log_lik)
loo(e1$log_lik)

#7.2----
m2 <- stan(file = "m7_2.stan",
           data = d1)


e2 <- extract(m2)

probs <- c(0.025, 0.5, 0.975)

l <- as_tibble(e2$y_plot) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(lKSI = d$lKSI,
         diff = lKSI - med)

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_line(aes(y = lKSI), color = 'red') +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 7.2')

as_tibble(e2$all_seasons) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  ggplot(aes(Date)) + 
  geom_line(aes(y = med)) +
  labs(title = "Fig 7.3")
  

l <- as_tibble(e2$y_hat) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(lKSI = d$lKSI,
         diff = lKSI - med)

l %>% 
  ggplot(aes(x = Date)) + 
  geom_line(aes(y = diff)) +
  labs(title = "Fig 7.4")

acf(l$diff, main = "Fig 7.6", sub = "leaving the seasonal stochastic")

waic(e2$log_lik)
loo(e2$log_lik)

#7.4----

d <- read_csv("./data ssts/Chapter_4/UKinflation.txt",
              col_names = c("IN"), skip = 1) %>%
  mutate(Date = seq.Date(from = ymd('1950-03-31'),
                         to = ymd('2001-12-31'),
                         by = '3 months'))

#set interventions for inflation shock quarters
d$w <- 0
d[which(d$Date %in% as.Date(c("1975-07-01", "1979-10-01"))),'w'] <- 1

d3 <- list(y = d$IN,
           k = 4,
           w = d$w,
           N = nrow(d))

m3 <- stan(file = "m7_3.stan",
           data = d3)

e3 <- extract(m3)

probs <- c(0.025, 0.5, 0.975)

l <- as_tibble(e3$y_plot) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(IN = d$IN,
         diff = IN - med)

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_line(aes(y = IN), color = 'red') +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 7.7a')

as_tibble(e3$all_seasons) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  ggplot(aes(Date)) + 
  geom_line(aes(y = med)) +
  labs(title = "Fig 7.7b")

l <- as_tibble(e3$y_hat) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(IN = d$IN,
         diff = IN - med)

l %>% 
  ggplot(aes(x = Date)) + 
  geom_line(aes(y = diff)) +
  labs(title = "Fig 7.4")

waic(e3$log_lik)
loo(e3$log_lik)
