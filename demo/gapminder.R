library(gapminder)
library(dplyr)
gapminder %>%
    filter(year == '2007') %>%
    mutate(lifeExpMonths = 12 * lifeExp) %>%
    arrange(desc(lifeExpMonths))