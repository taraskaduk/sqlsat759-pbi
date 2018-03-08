library(tidyverse)
library(modelr)

data("mtcars")

ggplot(mtcars, aes(x=wt, y=mpg, col=cyl, size=disp)) + geom_point()
model <- lm(mpg ~ wt + disp + cyl, data=mtcars)

mtcars <- mtcars %>% 
        rownames_to_column(var = 'car') %>% 
        add_predictions(model, var = 'mpg_pred') %>% 
        add_residuals(model, var = 'mpg_resid')

fake_data <- data_frame(wt = seq(1, 6, by = 0.1)) %>% 
        merge(data_frame(disp = seq(50, 500, by = 25)), all = TRUE) %>% 
        merge(data_frame(cyl = seq(4, 8, by = 2)), all = TRUE) %>% 
        add_predictions(model, var = 'mpg')
