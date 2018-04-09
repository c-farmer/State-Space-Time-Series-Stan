library(tidyverse)
library(lubridate)
library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

d <- read_csv("./data/UKdriversKSI.txt") %>% 
  mutate(lKSI = log(KSI),
         Date = ymd("1969-01-31") %m+% months(0:191))

#introduce the intervention vector to the data
d$w <- 0
d[170:192,'w'] <- 1

d1 <- list(y = d$lKSI,
           N = nrow(d),
           w = d$w)

m1 <- stan(file = "m6_1.stan",
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
  labs(title = 'Fig 6.1')

p <- sample(1:4000, 100, replace = TRUE)
lns <- tibble(int = e1$mu[p],
              sl = e1$lambda[p],
              xstart = min(d$w),
              xend = max(d$w),
              ystart = int + sl * xstart,
              yend = int + sl * xend)

d %>% 
  ggplot(aes(x = w)) + 
  geom_jitter(aes(y = lKSI), width = 0.01) +
  geom_segment(data = lns, 
               aes(x = xstart, y = ystart,
                   xend = xend, yend = yend),
               alpha = 0.05) +
  labs(title = "Fig 6.2")
  
l %>%
  ggplot(aes(Date, diff)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(title = "Fig 6.3")
  
waic(e1$log_lik)
loo(e1$log_lik)

#6.2----

m2 <- stan(file = "m6_2.stan",
             data = d1)
  
  
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
  labs(title = 'Fig 6.4')
  
l %>%
  ggplot(aes(Date, diff)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(title = "Fig 6.5")

waic(e2$log_lik)
loo(e2$log_lik)
