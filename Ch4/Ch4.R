library(tidyverse)
library(lubridate)
library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#4.1 ----

d <- read_csv("./data/UKdriversKSI.txt") %>% 
  mutate(lKSI = log(KSI),
         Date = ymd("1969-01-31") %m+% months(0:191))
d %>%
  ggplot(aes(Date)) +
  geom_line(aes(y=lKSI)) +
  geom_vline(xintercept = seq.Date(as.Date("1969-01-31"), 
                                   by = "year", length.out = 16)) +
  labs(title = "Fig 4.1")


d1 <- list(y = d$lKSI,
           N = length(d$lKSI),
           k=12)

m1 <- stan(file = "m4_1c.stan",
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
  labs(title = 'Fig 4.2')

qtiles <- quantile(e1$mu_lvl, probs = probs)

tibble(Date = d$Date,
       lKSI = d$lKSI) %>%
  ggplot( aes(Date)) + 
  geom_line(aes(y = lKSI), color = 'red') +
  geom_hline(yintercept = qtiles[2]) +
  geom_ribbon(aes(ymin=qtiles[1], ymax=qtiles[3]),alpha=.2) +
  labs(title = 'Fig 4.3')


l <- as_tibble(e1$all_seasons) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) 

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  geom_hline(yintercept = 0) +
  labs(title = 'Fig 4.4')


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
  geom_line(aes(y=diff)) +
  geom_hline(yintercept = 0) +
  labs(title = 'Fig 4.5')

waic(e1$log_lik)
loo(e1$log_lik)

#4.2 ----
#Now both level and season go stochastic

d2 <- list(y = d$lKSI,
           N = length(d$lKSI),
           k = 12)

m2 <- stan(file = "m4_2.stan",
           data = d2)

e2 <- extract(m2)

probs <- c(0.025, 0.5, 0.975)

l <- as_tibble(e2$mu_lvl) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(lKSI = d$lKSI)

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_line(aes(y = lKSI), color = 'red') +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 4.6')

l <- as_tibble(e2$all_seasons) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value)

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  geom_hline(yintercept = 0) +
  labs(title = 'Fig 4.7')

l %>% filter(year(Date) == 1969) %>%
  ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  geom_hline(yintercept = 0) +
  labs(title = 'Fig 4.8')

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
  geom_line(aes(y=diff)) +
  labs(title = 'Fig 4.9')

waic(e2$log_lik)
loo(e2$log_lik)

#4.4 ----
#UK inflation example

d <- read_csv("./data ssts/Chapter_4/UKinflation.txt",
              col_names = c("IN"), skip = 1) %>%
  mutate(Date = seq.Date(from = ymd('1950-03-31'),
                to = ymd('2001-12-31'),
                by = '3 months'))

d3 <- list(y = d$IN,
           N = length(d$IN),
           k = 4)

m3 <- stan(file = "m4_2.stan",
           data = d3)

e3 <- extract(m3)

l <- as_tibble(e3$mu_lvl) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(IN = d$IN)

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_line(aes(y = IN), color = 'red') +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 4.10a')

as_tibble(e3$all_seasons) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>% 
  ggplot( aes(Date)) + 
  geom_line(aes(y=med)) +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 4.10b')

l <- as_tibble(e3$y_hat) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value) %>%
  mutate(IN = d$IN,
         diff = IN - med)

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=diff)) +
  geom_hline(yintercept = 0) +
  labs(title = 'Fig 4.10c')

waic(e3$log_lik)
loo(e3$log_lik)
