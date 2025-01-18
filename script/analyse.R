library(tidyverse)
library(ratdat)

ggplot(data = complete_old, aes(weight,hindfoot_length))+
  geom_point()

