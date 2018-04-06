library(tidyverse)

d <- read_csv("./data/UKdriversKSI.txt") %>% 
  mutate(t = 1:nrow(d),
         lKSI = log(KSI))

d %>%
  ggplot(aes(t,lKSI)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = "Fig 1.1")

d %>% 
  ggplot(aes(t,lKSI)) + 
  geom_line() +
  labs(title = 'Fig 1.2')

m1 <- lm(lKSI ~ t, data =d)

tibble(t = d$t,
       residuals = residuals(m1)) %>%
  ggplot(aes(t, residuals)) +
  geom_line() +
  labs(title = 'Fig 1.3')

acf(residuals(m1), main = "Fig 1.5")


