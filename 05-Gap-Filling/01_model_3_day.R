library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library()



# Read data ---------------------------------------------------------------

tile_flow <- read_csv("Data/tile_flow_with_rain_STEP1.csv")
dwm_mngt <- read_csv("Data/dwm_mngt_STEP1.csv")



# FD Model ----------------------------------------------------------------

# prepare FD data 

tile_flow_FD <-
  tile_flow %>%
  group_by(siteid, plotid, dwm) %>%
  nest() %>%
  # count number of plots per site
  group_by(siteid) %>%
  mutate(plot_count = n_distinct(plotid)) %>%
  # select only FD plots
  filter(dwm == "FD") %>%
  # find sites where dwm treatment was swapped (fd_plot_count = plot_count = 2)
  mutate(fd_plot_count = n_distinct(plotid)) %>%
  # assign reps by taking into account plots with swapped treatment 
  mutate(rep = ifelse(plot_count == fd_plot_count, 1, NA),
         rep = ifelse(fd_plot_count == 1, fd_plot_count, rep),
         rep = ifelse(is.na(rep), 1:plot_count, rep)) %>%
  select(-ends_with("count")) %>%
  unnest() %>%
  # add season 
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall")),
         rep = as.factor(rep)) %>%
  select(siteid, plotid, dwm, rep, year, season , date, flow, rain = precip_on_site) %>%
  group_by(siteid, plotid, dwm, rep) %>%
  nest()
  

# Develop "Seasonal" Regression

# fit model
seasonal_reg_model <- function(df) {
  lm(flow ~ rain_3, data = df)
  }

#
tile_flow_FD %>%
  # calculate limitting tile flow as mean daily summer flow
  mutate(reg_data = map(.x = data, 
                        .f = ~ .x %>%
                          mutate(min_flow = mean(flow[season == "summer"], na.rm = TRUE)) %>%
                          # correct limitting flow so it is not > 0.3 mm/day
                          mutate(min_flow = ifelse(min_flow > 0.3, 0.3, min_flow)))) %>%
  # calculate 3-day average precip
  mutate(reg_data = map(.x = reg_data,
                        .f = ~ .x %>%
                          mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right")) %>%
                          # leave calculated 3-day avarage only for those days when it rained
                          mutate(rain_3 = ifelse(rain > 0, rain_3, 0)))) %>%
  # filter data to be used for regression 
  mutate(reg_data = map(.x = reg_data,
                        .f = ~ .x %>%
                          # remove days when there was no rain OR tile flow was below minimum limit
                          filter(flow > min_flow, rain_3 > 0))) %>%
  # fit model
  mutate(reg_model = map(reg_data, seasonal_reg_model)) %>%


  mutate(lim_summer_flow = map_dbl(.x = data, .f = ~ .x %>% 
                                     filter(season == "summer") %>%
                                     pull(flow) %>% 
                                     mean(na.rm = TRUE))) -> x
x$reg_data[1]




