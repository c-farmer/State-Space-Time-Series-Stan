library(tidyverse)
library(lubridate)
library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#3.2 ----

d <- read_csv("./data/UKdriversKSI.txt") %>% 
  mutate(lKSI = log(KSI),
         Date = ymd("1969-01-31") %m+% months(0:191))


d1 <- list(y = d$lKSI,
           N = length(d$lKSI))

m1 <- stan(file = "m3_1.stan",
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
  labs(title = 'Fig 3.1')


slope <- as_tibble(e1$nu_hat) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value)

slope %>%
  ggplot(aes(x =Date)) + 
  geom_line(aes(y=med)) +
  labs(title = 'Fig 3.2',
       subtitle = 'Note the smal scale of the y axis')

slope %>%
  ggplot(aes(x =Date)) + 
  geom_line(aes(y=med)) +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 3.2 with Quantiles')

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=diff)) +
  labs(title = 'Fig 3.3')

acf(l$diff)  

waic(e1$log_lik)
loo(e1$log_lik)


#3.3 ----
#static linear trend

d2 <- list(y = d$lKSI,
           N = length(d$lKSI))

m2 <- stan(file = "m3_2.stan",
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
  labs(title = 'Fig 3.4')


acf(l$diff)  

waic(e2$log_lik)
loo(e2$log_lik)

#3.4 ----
#stochastic level and slope to Finnish fatalities
d <- read_tsv('./data/NorwayFinland.txt', 
              skip = 1, 
              col_names = c('Year', 'Nor', 'Fin')) %>%
  mutate(lNor = log(Nor), lFin = log(Fin),
         Date = 1970:2003)

d3 <- list(y = d$lNor,
           N = length(d$lNor))

m3 <- stan(file = "m3_1.stan",
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
  labs(title = 'Fig 3.5a')

slope <- as_tibble(e3$nu_hat) %>% 
  map_dfc(quantile, probs = probs) %>%
  bind_cols(level = c('low','med','high')) %>%
  gather(obs, value, -level) %>%
  mutate(Date = rep(d$Date, each = 3)) %>%
  select(-obs) %>%
  spread(level, value)

slope %>%
  ggplot(aes(x =Date)) + 
  geom_line(aes(y=med)) +
  labs(title = 'Fig 3.5b',
       subtitle = 'Note the smal scale of the y axis')

slope %>%
  ggplot(aes(x =Date)) + 
  geom_line(aes(y=med)) +
  geom_ribbon(aes(ymin=low, ymax=high),alpha=.2) +
  labs(title = 'Fig 3.5b with Quantiles')

l %>% ggplot( aes(Date)) + 
  geom_line(aes(y=diff)) +
  labs(title = 'Fig 3.6')

