library(tidyverse)
library(lubridate)
library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#5.1 ----

d <- read_csv("./data/UKdriversKSI.txt") %>% 
  mutate(lKSI = log(KSI),
         Date = ymd("1969-01-31") %m+% months(0:191))

petrol <- read_csv("./data/logUKpetrolprice.txt", 
                   col_names = "log_price",
                   skip = 1)

d <- bind_cols(d, petrol)

d1 <- list(N = nrow(d),
           y = d$lKSI,
           x = d$log_price)

m1 <- stan(file = "m5_1.stan",
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
  labs(title = 'Fig 5.1')

p <- sample(1:4000, 100, replace = TRUE)
lns <- tibble(int = e1$mu_lvl[p],
              sl = e1$beta[p],
              xstart = min(d$log_price),
              xend = max(d$log_price),
              ystart = int + sl * xstart,
              yend = int + sl * xend)

d %>% 
  ggplot(aes(x = log_price)) + 
  geom_point(aes(y = lKSI)) +
  geom_segment(data = lns, 
               aes(x = xstart, y = ystart,
                   xend = xend, yend = yend),
               alpha = 0.05) +
  labs(title = "Fig 5.2")

l %>% ggplot(aes(Date, diff)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(title = "Fig 5.3")

waic(e1$log_lik)
loo(e1$log_lik)

#5.2----
d2 <- list(N = nrow(d),
           y = d$lKSI,
           x = d$log_price)

m2 <- stan(file = "m5_2.stan",
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
  labs(title = 'Fig 5.4')

l %>% ggplot(aes(Date, diff)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(title = "Fig 5.5")

waic(e1$log_lik)
loo(e1$log_lik)