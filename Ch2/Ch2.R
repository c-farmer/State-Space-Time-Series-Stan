library(tidyverse)
library(lubridate)
library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

d <- read_csv("./data/UKdriversKSI.txt") %>% 
  mutate(lKSI = log(KSI),
         Date = ymd("1969-01-31") %m+% months(0:191))

#2.1 ----
#local level model, deterministic level

d1 <- list(y = d$lKSI,
           N = nrow(d))

m1 <- stan(file = "m2_1.stan",
           data = d1)

e1 <- extract(m1)

probs <- c(0.025, 0.5, 0.975)

l <- as_tibble(e1$y_hat) %>% 
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
  labs(title = 'Fig 2.1')


l %>%
  ggplot(aes(x =Date, y = diff)) + 
  geom_line() + 
  labs(title = 'Fig 2.2')

acf(l$diff)  

waic(e1$log_lik)
loo(e1$log_lik)

#2.2 ----
#Stochastic level
d2 <- list(y = d$lKSI,
           N = nrow(d))

m2 <- stan(file = "m2_2.stan",
           data = d2)

e2 <- extract(m2)

probs <- c(0.025, 0.5, 0.975)

l <- as_tibble(e2$y_hat) %>% 
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
  labs(title = 'Fig 2.3')


l %>%
  ggplot(aes(x =Date, y = diff)) + 
  geom_line() +
  labs(title = 'Fig 2.4')

acf(l$diff)  

waic(e2$log_lik)
loo(e2$log_lik)

#2.3 ----

d <- read_tsv('./data/NorwayFinland.txt', 
              skip = 1, 
              col_names = c('Year', 'Nor', 'Fin')) %>%
  mutate(lNor = log(Nor), lFin = log(Fin),
         Date = 1970:2003)

d3 <- list(y = d$lNor,
           N = nrow(d))

m3 <- stan(file = "m2_2.stan",
           data = d3)

e3 <- extract(m3)

probs <- c(0.025, 0.5, 0.975)

l <- as_tibble(e3$y_hat) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(lNor = d$lNor,
         diff = lNor - med)

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_line(aes(y = lNor), color = 'red') +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 2.5')


l %>%
  ggplot(aes(x =Date, y = diff)) + 
  geom_line() +
  labs(title = 'Fig 2.6')

acf(l$diff)  

waic(e3$log_lik)
loo(e3$log_lik)
