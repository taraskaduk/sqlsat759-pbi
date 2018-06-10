library(tidyverse)

iris_gather <- iris %>% 
    gather("key", "size", -Species)
head(iris_gather)


iris_gather2 <- iris_gather %>% 
    separate(col = key, into = c('part', 'dimension'))
head(iris_gather2)

iris_summary <- iris_gather2 %>% 
    group_by(Species) %>% 
    summarise(size_avg = mean(size),
              size_sd = sd(size),
              size_med = median(size))
head(iris_summary)