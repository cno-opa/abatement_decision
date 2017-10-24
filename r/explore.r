
# Dependencies ------------------------------------------------------------

library(feather)
library(tidyverse)

raw <- read_feather("./data/decisions.feather")

x <- "characterize_the_amount_of_blight_on_the_same_block"

p <- raw %>%
  select(outcome, x = matches(x)) %>% 
  ggplot(aes(x = as.factor(outcome))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~ x)
print(p)
  



  