library(tidyverse)
library(lubridate)

d <- read_csv("./data/UKdriversKSI.txt") %>% 
  mutate(lKSI = log(KSI),
         Date = ymd("1969-01-31") %m+% months(0:191))

d %>%
  ggplot(aes(Date,lKSI)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = "Fig 1.1")

d %>% 
  ggplot(aes(Date,lKSI)) + 
  geom_line() +
  labs(title = 'Fig 1.2')

m1 <- lm(lKSI ~ Date, data =d)

d %>% 
  mutate(residuals = residuals(m1)) %>%
  ggplot(aes(Date, residuals)) +
  geom_line() +
  labs(title = 'Fig 1.3')

acf(residuals(m1), main = "Fig 1.5")


