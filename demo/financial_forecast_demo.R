# Library -----------------------------------------------------------------

# Make sure all these libraries are installed beforehand, or the code won't work otherwise.

library(tidyverse)
library(readxl)
library(xts)
library(forecast)
library(timetk)
library(lubridate)
library(corrplot)
library(sweep)


# You'll need to edit this path right here. Point it to where the data file is stored.
# Normally, you can use getwd(), here::here() or simply use R projects in R.
# However, Power BI sets a working directory in the internal temp folders, 
# and you'll have to hard code your real path to overcome this.


path <- "C:/Users/TKaduk/Google Drive/r/r-in-power-bi/demo"
financials_by_division <- read_csv(paste0('demo/financials_by_division.csv'))


# Transform: grouping and nesting -----------------------------------------
  

division_min_dates <- financials_by_division %>% 
  filter(amount != 0) %>% 
  group_by(division) %>%
  summarise(min_date = min(date))


financials_by_division_filtered <- financials_by_division %>% 
  left_join(division_min_dates, by = 'division') %>% 
  mutate(min_date = if_else(month(min_date) == 1, min_date, ceiling_date(min_date, unit = 'year'))) %>% 
  filter(date >= min_date & 
         min_date <= today() - months(15)) %>% 
  mutate(start = year(min_date)) %>% 
  select(-min_date)


# Exploration--------------------------

##Corr plot
cor_matrix <- financials_by_division_filtered %>% 
  select(-c(start,period)) %>% 
  filter(date >= ymd('20170101')) %>% 
  spread(pl_group, amount) %>% 
  select(-c(division, date)) %>% 
  as.matrix() %>% 
  cor()

corrplot(cor_matrix)

##Net Income
ggplot(financials_by_division_filtered %>% 
         group_by(division, date) %>% 
         summarise(amount = sum(amount)), 
       aes(x = date, y = amount, group = 1)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ division, scales = "free")

##Revenue
ggplot(financials_by_division_filtered %>% 
         filter(pl_group == 'Revenue'), 
       aes(x = date, y = amount, group = 1)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ division, scales = "free")

##PL Groups
ggplot(financials_by_division_filtered %>% 
         group_by(pl_group, date) %>% 
         summarise(amount = sum(amount)), 
       aes(x = date, y = amount, group = 1)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ pl_group, scales = "free")


# Transform: splitting into groups based on forecasting model appr --------


financials_nested <- financials_by_division_filtered %>%  
  group_by(pl_group, division, start) %>% 
  nest() %>%
  mutate(data_train = map(data, ~ filter(., period == 'train')))
  
financials_nested <- financials_nested %>% 
  mutate(ts = data %>% map(tk_ts, 
                           select = amount, 
                           start = 1, 
                           freq = 12),
         ts_train = data_train %>% map(tk_ts, 
                           select = amount, 
                           start = 1, 
                           freq = 12))


# Model -------------------------------------------------------------------



financials_modeled <- financials_nested %>% 
  mutate(ets_full = if_else(pl_group %in% c('Revenue', 'Cost of goods sold'),
                      ts %>% map(ets, model = 'ZZA', alpha = 0.6),
                      ts %>% map(ets, model = 'ZZZ', alpha = 0.7))) %>% 
  mutate(ets_train = if_else(pl_group %in% c('Revenue', 'Cost of goods sold'),
                      ts_train %>% map(ets, model = 'ZZA', alpha = 0.6),
                      ts_train %>% map(ets, model = 'ZZZ', alpha = 0.7)))

methods <- financials_modeled %>% filter(!is.na(ets_full)) %>% 
  mutate(glance = map(ets_full, sw_glance)) %>% 
  unnest(glance) %>% 
  select(division, pl_group, model.desc)

methods %>% 
  group_by(model.desc, pl_group) %>% 
  count()

h <- 26 - month(today())

financials_forecasted <- financials_modeled %>% 
  mutate(forecast_ets = ets_full %>% map(forecast.ets, h = h),
         acc_ets = forecast_ets %>% map(accuracy),
         forecast_ets_train = ets_train %>% map(forecast.ets, h = h))

financials_df <- financials_forecasted %>% 
  mutate(forecast_ets = forecast_ets %>% map(as_data_frame),
         forecast_ets = forecast_ets %>% map(rownames_to_column, var = 'date'),
         acc_ets = acc_ets %>% map(as_data_frame),
         forecast_ets_train = forecast_ets_train %>% map(as_data_frame),
         forecast_ets_train = forecast_ets_train %>% map(rownames_to_column, var = 'date'))


# Accuracy ----------------------------------------------------------------

model_accuracy <- financials_df %>% 
  select(pl_group, division, acc_ets) %>% 
  unnest(acc_ets) %>% 
  select(pl_group, division, ets_mape = MAPE)


# Unnest and write results ------------------------------------------------

financials_unnested <- financials_df %>% 
  unnest(forecast_ets) %>% 
  separate(col = date, into = c('month', 'year_n'), sep = ' ') %>% 
  mutate(year = as.integer(year_n) + start - 1,
         date = dmy(paste("01", month, year)),
         var = 'forecast') %>% 
  rename(amount = `Point Forecast`,
         lo95 = `Lo 95`,
         lo80 = `Lo 80`,
         hi80 = `Hi 80`,
         hi95 = `Hi 95`) %>% 
  select(-c(year_n, start, month, year))
    
financials_unnested_train <- financials_df %>% 
  unnest(forecast_ets_train) %>% 
  separate(col = date, into = c('month', 'year_n'), sep = ' ') %>% 
  mutate(year = as.integer(year_n) + start - 1,
         date = dmy(paste("01", month, year)),
         var = 'forecast_train') %>% 
  rename(amount = `Point Forecast`,
         lo95 = `Lo 95`,
         lo80 = `Lo 80`,
         hi80 = `Hi 80`,
         hi95 = `Hi 95`) %>% 
  select(-c(year_n, start, month, year))

financials_final <- financials_by_division_filtered %>%
  mutate(var = 'historical') %>% 
  union_all(financials_unnested) %>% 
  union_all(financials_unnested_train)

write_csv(financials_final, paste0('financials_forecast.csv'))

##sample
financials_final %>% 
        filter(division == 'Denver' & var != 'forecast_train') %>%  
ggplot(aes(x = date, y = amount, col = var, group = interaction('historical', 'forecast'))) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ pl_group, scales = "free")

##net income
# ggplot(financials_final %>% 
#          group_by(division, date, var) %>% 
#          summarise(amount = sum(amount)), 
#        aes(x = date, y = amount, col = var, group = interaction('historical', 'forecast'))
#        ) +
#   geom_line(na.rm = TRUE) +
#   facet_wrap(~ division, scales = "free")

##revenue
financials_final %>% 
        filter(year(date) %in% c(2016:2018) & var != 'forecast_train') %>%  
ggplot(aes(x = date, y = amount, col = var, group = interaction('historical', 'forecast'))) +
        geom_line(na.rm = TRUE) +
        geom_point(size = 1, alpha = 0.3) +
        facet_grid(pl_group ~ division, scales = "free") + 
        xlab("") + 
        theme(
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
                )

##ni
ggplot(financials_final %>% 
         filter(date < Sys.Date() & date > '2017-01-01') %>% 
         group_by(division, date, var) %>% 
         summarise(amount = sum(amount)), 
       aes(x = date, y = amount, col = var)
) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 3, alpha = 0.3) +
  facet_wrap(~ division, scales = "free")

rm(list=setdiff(ls(), "financials_final"))