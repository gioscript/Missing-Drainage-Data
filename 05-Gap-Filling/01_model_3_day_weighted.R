library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library(modelr)
library(broom)



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


# Develop "Seasonal" Regression model for predicting peak flows caused by precipitation event

# function to fit linear model
seasonal_reg_model <- function(df) {
  lm(flow ~ rain_3 - 1, data = df)
}

# function to calculate 3-day weighted rolling/moving average 
rollmean.weighted = function(x, W1 = 0.1, W2 = 0.3, W3 = 0.6) {
  if (sum(is.na(x)) == 3) {
    NA
  } else {
    replace(x[1], is.na(x[1]), 0) * W1 +
      replace(x[2], is.na(x[2]), 0) * W2 +
      replace(x[3], is.na(x[3]), 0) * W3 
  }
  sum(x*c(0.1, 0.3, 0.6))
}
  
tile_flow_FD_reg_model_weighted <-
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
                          mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right"),
                                 # calculated weighted moving average precip
                                 rain_3_weighted = rollapplyr(rain, width = 3, FUN = rollmean.weighted,
                                                              partial = FALSE, fill = NA),
                                 # handle days which are out of the rolling range = first two recoreds in this case
                                 rain_3_weighted = ifelse(is.na(rain_3_weighted), rain_3, rain_3_weighted)
                                 ) %>%
                          # leave calculated 3-day avarage only for those days when it rained
                          mutate(rain_3 = ifelse(rain > 0, rain_3_weighted, 0)))) %>%
  # filter data to be used for regression 
  mutate(reg_data = map(.x = reg_data,
                        .f = ~ .x %>%
                          # remove days when there was no rain OR tile flow was below minimum limit
                          filter(flow > min_flow, rain_3 > 0))) %>%
  # add season to nesting
  unnest(reg_data) %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest(.key = "reg_data") %>%
  # fit the model
  mutate(reg_model = map(reg_data, seasonal_reg_model))


# calculate recession slope for falling limb of the graph

# function to select peak and inflection points
peak_inflection <- function(df) {
  df %>%
    mutate(
      first =  ln_flow - lag(ln_flow),
      second = first - lag(first),
      point =  lead(ln_flow) - ln_flow,
      point1 = ifelse(is.na(point), "Pos", ifelse(point < 0, "Neg", "Pos")),
      point1a = ifelse(is.na(first), "Pos", ifelse(first < 0, "Neg", "Pos")),
      point2 = ifelse(is.na(second), "Pos", ifelse(second < 0, "Neg", "Pos"))
    ) %>%
    mutate(
      group1 = rep(seq_along(rle(point1)$length), rle(point1)$length),
      group1a = rep(seq_along(rle(point1a)$length), rle(point1a)$length),
      group2 = rep(seq_along(rle(point2)$length), rle(point2)$length)
    ) %>%
    # group1 = from START to END-1day or ressesion
    group_by(group1) %>%
    mutate(POINT = ifelse(point1 == "Neg" &
                            row_number() == 1, "peak", NA)) %>%
    group_by(group2) %>%
    mutate(POINT = ifelse(point2 == "Neg" &
                            row_number() == n(), "inf", POINT)) %>%
    ungroup() %>%
    select(-point1,-point1a,-point2) %>%
    mutate(group = ifelse(POINT == "peak", group1, group1a)) %>%
    select(-group1,-group1a,-group2) %>%
    filter(!is.na(POINT)) %>%  
    # remove single peaks|ubflections and double inflections
    group_by(group) %>%
    mutate(n = n(),
           count = 1:n()) %>%
    ungroup() %>%
    filter(n != 1,
           count < 3) %>%
    select(year, season, date, flow, rain, ln_flow, group, POINT) %>%
    # calculate duration of PEAK-TO-INFLECTION period and slope
    mutate(days_bw_pni = lead(date) - date,
           days = as.numeric(days_bw_pni),
           change = (lead(ln_flow) - ln_flow),
           slope = change/days)
}

# calculate average recession slope by season
recession_slope_FD <-
  tile_flow_FD %>%
  # log-transform flow data 
  mutate(data = map(.x = data, 
                    .f = ~ .x %>% 
                      # make sure to handle log(0) 
                      mutate(ln_flow = ifelse(flow == 0, NA, log(flow))))) %>%
  # find PEAK and INFLECTION points
  mutate(data = map(data, peak_inflection)) %>%
  unnest() %>%
  # select "peak" POINT to calculate slope of recessuin (falling) limb 
  filter(POINT == 'peak') %>%
  filter(!is.infinite(slope)) %>%
  # save intermediate data for plotting in Global Environment
  {. ->> recession_slope_dist_FD} %>%
  # calculate average number of days between peak and first point of inflection
  # and the slope of recession limb 
  group_by(siteid, plotid, season) %>%
  summarise(ave_days = mean(days),
            ave_slope = mean(slope),
            # calculate geometric and trimmed means because distributions of slope are right-skewed 
            trim_ave_slope = mean(slope, trim = 0.1),  # less conservative
            geom_ave_slope = -exp(mean(log(-slope))),  # more conservative
            harmonic_ave_slop = 1/(mean(1/slope))      # most conservative
  ) %>%
  ungroup()


# predict mssing data
tile_flow_FD_pred_weighted <-
  tile_flow_FD %>%
  unnest() %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest() %>%
  # combine regression model and recession slope with flow data
  left_join(tile_flow_FD_reg_model_weighted, by = c("siteid", "plotid", "dwm", "rep", "season")) %>%
  left_join(recession_slope_FD[1:7], by = c("siteid", "plotid", "season")) %>%
  select(-reg_data) %>%
  # add 3-day moving avarage rain to data
  mutate(data = map(.x = data,
                    .f = ~ .x %>%
                      mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right"),
                             rain_3_weighted = rollapplyr(rain, width = 3, FUN = rollmean.weighted,
                                                          partial = FALSE, fill = NA),
                             rain_3_weighted = ifelse(is.na(rain_3_weighted), rain_3, rain_3_weighted)
                             ) %>%
                      # leave calculated 3-day avarage only for those days when it rained
                      mutate(rain_3 = ifelse(rain > 0, rain_3_weighted, 0)))) %>%
  # add predictions of pick flows using actual data
  mutate(isnull = map_lgl(reg_model, is.null),  
         # predict pick flow only when there is a model available 
         data = ifelse(isnull == 1, 
                       data, 
                       map2(.x = reg_model,
                            .y = data,
                            .f = ~ augment(.x, newdata = .y)))) %>%
  unnest(data) %>%
  # choose predictions for days when it was raining and no flow measured
  mutate(pred = ifelse(.fitted < 0, 0, .fitted),  # eliminate predicted negative flows
         flow_pred = ifelse(is.na(flow) & rain > 0, pred, flow)) %>%
  select(-isnull, -pred, -.fitted, -.se.fit, -rain_3) %>%
  # predict flow of falling limb
  group_by(siteid, plotid) %>%
  mutate(LOGIC = ifelse(is.na(flow_pred), "Y", "N"),
         group = rep(seq_along(rle(LOGIC)$length), rle(LOGIC)$length)) %>%
  group_by(siteid, plotid, group) %>%
  mutate(count = 1:n(),
         count = ifelse(LOGIC == "N", 0, count)) %>%
  group_by(siteid, plotid) %>%
  # limit predicted flow to measured max tile flow
  mutate(flow_max = max(flow, na.rm = T),
         flow_pred = ifelse(flow_pred > flow_max, flow_max, flow_pred),
         flow_max = NULL) %>%
  mutate(group = NULL, 
         LOGIC = NULL,
         flow_pred2 = na.locf(flow_pred, na.rm = FALSE),
         # apply recession equation (Qi = Qi-1 * e^k, where k is seasonal recession slope)
         flow_pred2_mean = round(flow_pred2 * exp(ave_slope * count), 6),
         # use trimmed slope
         flow_pred2_trim = round(flow_pred2 * exp(trim_ave_slope * count), 6),
         # use geometric mean slope
         flow_pred2_geom = round(flow_pred2 * exp(geom_ave_slope * count), 6),
         flow_pred_mean = ifelse(is.na(flow_pred), flow_pred2_mean, flow_pred),
         flow_pred_trim = ifelse(is.na(flow_pred), flow_pred2_trim, flow_pred),
         flow_pred_geom = ifelse(is.na(flow_pred), flow_pred2_geom, flow_pred),
         flow_pred2 = NULL, flow_pred2_trim = NULL, flow_pred2_geom = NULL, count = NULL) %>%
  # remove predicitoins of flow in days when there was no rainfall data (see CRAWF)
  mutate(flow_pred_mean = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_mean),
         flow_pred_trim = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_trim),
         flow_pred_geom = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_geom))   

tile_flow_FD_pred_weighted %>%
  arrange(siteid, plotid, date) %>%
  filter(siteid == "DPAC") %>% filter(year > 2012) %>% filter(plotid == "SW") %>%
  mutate(DATE = update(date, year = 2012)) %>%
  ggplot(aes(x=DATE, group = plotid)) +
  geom_line(aes(y = flow_pred, colour = plotid), col = "black") +
  geom_point(aes(y = flow_pred), size = 1.2, col = "blue") +
  geom_point(aes(y = flow, colour = plotid), size = 0.9) +
  facet_grid(year ~ .) +
  theme_bw()


# CD Model with Season ----------------------------------------------------

# prepare CD data 

tile_flow_CD_season <-
  tile_flow %>%
  group_by(siteid, plotid, dwm) %>%
  nest() %>%
  # count number of plots per site
  group_by(siteid) %>%
  mutate(plot_count = n_distinct(plotid)) %>%
  # select only CD plots
  filter(dwm == "CD") %>%
  # find sites where dwm treatment was swapped (cd_plot_count = plot_count = 2)
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


# Develop "Seasonal" Regression model for predicting peak flows caused by precipitation event

tile_flow_CD_season_reg_model_weighted <-
  tile_flow_CD_season %>%
  # calculate limitting tile flow as mean daily summer flow
  mutate(reg_data = map(.x = data, 
                        .f = ~ .x %>%
                          mutate(min_flow = mean(flow[season == "summer"], na.rm = TRUE)) %>%
                          # correct limitting flow so it is not > 0.3 mm/day
                          mutate(min_flow = ifelse(min_flow > 0.3, 0.3, min_flow)))) %>%
  # calculate 3-day average precip
  mutate(reg_data = map(.x = reg_data,
                        .f = ~ .x %>%
                          mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right"),
                                 rain_3_weighted = rollapplyr(rain, width = 3, FUN = rollmean.weighted,
                                                              partial = FALSE, fill = NA),
                                 rain_3_weighted = ifelse(is.na(rain_3_weighted), rain_3, rain_3_weighted)
                          ) %>%
                          # leave calculated 3-day avarage only for those days when it rained
                          mutate(rain_3 = ifelse(rain > 0, rain_3_weighted, 0)))) %>%
  # filter data to be used for regression 
  mutate(reg_data = map(.x = reg_data,
                        .f = ~ .x %>%
                          # remove days when there was no rain OR tile flow was below minimum limit
                          filter(flow > min_flow, rain_3 > 0))) %>%
  # add season to nesting
  unnest(reg_data) %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest(.key = "reg_data") %>%
  # fit the model
  mutate(reg_model = map(reg_data, seasonal_reg_model))


# calculate recession slope for falling limb of the graph

# calculate average recession slope by season
recession_slope_CD_season <-
  tile_flow_CD_season %>%
  # log-transform flow data 
  mutate(data = map(.x = data, 
                    .f = ~ .x %>% 
                      # make sure to handle log(0) 
                      mutate(ln_flow = ifelse(flow == 0, NA, log(flow))))) %>%
  # find PEAK and INFLECTION points
  mutate(data = map(data, peak_inflection)) %>%
  unnest() %>%
  # select "peak" POINT to calculate slope of recessuin (falling) limb 
  filter(POINT == 'peak') %>%
  filter(!is.infinite(slope)) %>%
  # save intermediate data for plotting in Global Environment
  {. ->> recession_slope_dist_CD} %>%
  # calculate average number of days between peak and first point of inflection
  # and the slope of recession limb 
  group_by(siteid, plotid, season) %>%
  summarise(ave_days = mean(days),
            ave_slope = mean(slope),
            # calculate geometric and trimmed means because distributions of slope are right-skewed 
            trim_ave_slope = mean(slope, trim = 0.1),  # less conservative
            geom_ave_slope = -exp(mean(log(-slope))),  # more conservative
            harmonic_ave_slop = 1/(mean(1/slope))      # most conservative
  ) %>%
  ungroup()


# predict mssing data
tile_flow_CD_pred_weighted <-
  tile_flow_CD_season %>%
  unnest() %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest() %>%
  # combine regression model and recession slope with flow data
  left_join(tile_flow_CD_season_reg_model_weighted, by = c("siteid", "plotid", "dwm", "rep", "season")) %>%
  left_join(recession_slope_CD_season[1:7], by = c("siteid", "plotid", "season")) %>%
  select(-reg_data) %>%
  # add 3-day moving avarage rain to data
  mutate(data = map(.x = data,
                    .f = ~ .x %>%
                      mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right"),
                             rain_3_weighted = rollapplyr(rain, width = 3, FUN = rollmean.weighted,
                                                          partial = FALSE, fill = NA),
                             rain_3_weighted = ifelse(is.na(rain_3_weighted), rain_3, rain_3_weighted)
                      ) %>%
                      # leave calculated 3-day avarage only for those days when it rained
                      mutate(rain_3 = ifelse(rain > 0, rain_3_weighted, 0)))) %>%
  # add predictions of pick flows using actual data
  mutate(isnull = map_lgl(reg_model, is.null),  
         # predict pick flow only when there is a model available 
         data = ifelse(isnull == 1, 
                       data, 
                       map2(.x = reg_model,
                            .y = data,
                            .f = ~ augment(.x, newdata = .y)))) %>%
  unnest(data) %>%
  # choose predictions for days when it was raining and no flow measured
  mutate(pred = ifelse(.fitted < 0, 0, .fitted),  # eliminate predicted negative flows
         flow_pred = ifelse(is.na(flow) & rain > 0, pred, flow)) %>%
  # fix the problem of neglecting the first predicted zero flows (see East plot as AUGLA)
  group_by(siteid, plotid) %>%
  mutate(row_num = row_number()) %>% # need to find the first data-point for each plot
  mutate(flow_pred = ifelse(row_num == 1 & is.na(flow_pred), pred, flow_pred)) %>%
  select(-isnull, -pred, -.fitted, -.se.fit, -rain_3, -row_num) %>%
  # predict flow of falling limb
  group_by(siteid, plotid) %>%
  mutate(LOGIC = ifelse(is.na(flow_pred), "Y", "N"),
         group = rep(seq_along(rle(LOGIC)$length), rle(LOGIC)$length)) %>%
  group_by(siteid, plotid, group) %>%
  mutate(count = 1:n(),
         count = ifelse(LOGIC == "N", 0, count)) %>%
  group_by(siteid, plotid) %>%
  # limit predicted flow to measured max tile flow
  mutate(flow_max = max(flow, na.rm = T),
         flow_pred = ifelse(flow_pred > flow_max, flow_max, flow_pred),
         flow_max = NULL) %>%
  mutate(group = NULL, 
         LOGIC = NULL,
         flow_pred2 = na.locf(flow_pred, na.rm = FALSE),
         # apply recession equation (Qi = Qi-1 * e^k, where k is seasonal recession slope)
         flow_pred2_mean = round(flow_pred2 * exp(ave_slope * count), 6),
         # use trimmed slope
         flow_pred2_trim = round(flow_pred2 * exp(trim_ave_slope * count), 6),
         # use geometric mean slope
         flow_pred2_geom = round(flow_pred2 * exp(geom_ave_slope * count), 6),
         flow_pred_mean = ifelse(is.na(flow_pred), flow_pred2_mean, flow_pred),
         flow_pred_trim = ifelse(is.na(flow_pred), flow_pred2_trim, flow_pred),
         flow_pred_geom = ifelse(is.na(flow_pred), flow_pred2_geom, flow_pred),
         flow_pred2 = NULL, flow_pred2_trim = NULL, flow_pred2_geom = NULL, count = NULL) %>%
  # remove predicitoins of flow in days when there was no rainfall data (see CRAWF)
  mutate(flow_pred_mean = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_mean),
         flow_pred_trim = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_trim),
         flow_pred_geom = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_geom)) 


tile_flow_CD_pred_weighted %>% 
  arrange(siteid, plotid, date) %>%
  filter(siteid == "AUGLA") %>% #filter(year > 2013) %>% 
  filter(plotid == "West") %>%
  mutate(DATE = update(date, year = 2012)) %>%
  ggplot(aes(x=DATE, group = plotid)) +
  geom_col(aes(y = rain/20), fill = "skyblue", alpha = 0.4) +
  geom_line(aes(y = flow_pred_trim, colour = plotid), col = "black") +
  geom_point(aes(y = flow_pred_mean), col = "black") +
  geom_point(aes(y = flow_pred_geom), size = 1.2, col = "blue") +
  geom_point(aes(y = flow_pred_trim), size = 1.2, col = "orange") +
  geom_point(aes(y = flow, colour = plotid), size = 0.9) +
  facet_grid(year ~ .) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 20), xlim = ymd(20120401, 20120701))

# Combine predictions -----------------------------------------------------

# select only columns needed to combine
tile_flow_CD_season_pred %>% 
  select(siteid, plotid, date, flow_pred) -> CD_flow_season
tile_flow_CD_pred  %>%
  select(siteid, plotid, date, flow_pred_board = flow_pred) -> CD_flow_board
tile_flow_FD_pred %>%
  select(siteid, plotid, date, flow_pred) -> FD_flow_season

# combine all predicted and measured values
tile_flow_pred <- 
  bind_rows(CD_flow_season, FD_flow_season) %>%
  full_join(CD_flow_board, by = c("siteid", "plotid", "date")) %>%
  full_join(tile_flow, by = c("siteid", "plotid", "date")) %>%
  arrange(siteid, plotid, date) %>%
  select(siteid, plotid, dwm, year, date, flow, flow_pred, flow_pred_board, rain = precip_on_site)

tile_flow_pred %>%
  head(20)


tile_flow_pred %>% 
  mutate(flow_pred = ifelse(siteid == "DEFI_M" & year == 2010, flow, flow_pred),
         flow_pred = ifelse(siteid == "DPAC" & year == 2006, flow, flow_pred),
         flow_pred = ifelse(siteid == "DPAC" & year == 2017 & plotid %in% c("NE", "NW"), 
                            flow, flow_pred),
         flow_pred = ifelse(siteid == "HARDIN" & between(date, ymd(20091001), ymd(20100930)), 
                            flow, flow_pred),
         flow_pred = ifelse(siteid == "SERF_IA" & plotid == "S2" & year %in% c(2009:2010), 
                            flow, flow_pred),
         flow_pred = ifelse(siteid == "STJOHNS" & year == 2015 & plotid == "WN", NA, flow_pred)) %>%
  select(siteid:date, rain, flow, flow_pred) %>%
  mutate(comments = ifelse(is.na(flow) & !is.na(flow_pred), "predicted value", "")) ->
  tile_flow_new_weighted


tile_flow_new_weighted %>%
  filter(dwm %in% c("CD", "FD")) %>%
  mutate(season = factor(quarter(date), labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  arrange(siteid, plotid, date) %>%
  group_by(siteid, plotid, dwm) %>%
  nest() %>%
  group_by(siteid, dwm) %>%
  mutate(rep = 1:n(),
         rep = ifelse(siteid %in% c("AUGLA", "DEFI_M"), 1, rep)) %>% 
  unnest() %>%
  select(siteid, plotid, dwm, rep, year, season, date, flow_pred, comments) %>%
  write_csv(paste0("Output/Data/tile_flow_daily_filled_weighted_", Sys.Date(), ".csv"))




#  AUGLA   << seasonal pred is less stip and looks better {SEASONAL}
#  CRAWF   << Good {SEASONAL}  
#  DEFI_M  << remove pred in the first 9 months in 2010 {SEASONAL}
#  DPAC    << remove missing monthes in 2006 in all 4 plots and in 2017 in two plots NE and NW {SEASONAL}
#  HARDIN  << problems in 2008
#  HARDIN_NW   >>> Good {SEASONAL}
#  HENRY   << Good
#  SERF_IA << fix tile flow data as S4 in Aug-Sep 2017, should be zero
#  SERF_SD << Good
#  STJOHNS << OK
#  TIDE    << Use predictions from step 3




# Saving ANOVA data -------------------------------------------------------

read_csv("C:/Users/gio/OneDrive - Iowa State University/Projects/TD/Data/Data Requests/Matt Helmers/Control_Box_Outlet_MNGT/Data/tile_flow_daily_for_Lori_2018-11-27.csv") %>%
  filter(!(siteid == "MUDS2" & is.na(tile_flow_mm))) %>%
  full_join(tile_flow_new, by = c('siteid', 'plotid', 'dwm', 'year', 'date')) %>%
  # get rid of SD plots at SERF_IA
  filter(dwm %in% c("FD", "CD")) %>%
  # get rid of missing data in the begining of MUDS2
  mutate(flow = ifelse(siteid %in% c("MUDS2", "STORY", "UBWC"), tile_flow_mm, flow_pred)) %>%
  # select variables to be used at 
  select(site = siteid, plot = plotid, trt = dwm, rep, year, season, date, flow) %>%
  # remove data partaining to water year 2018
  filter(year < 2017) %>%
  write_csv(paste0("Output/Data/tile_flow_for_ANOVA_", Sys.Date(), ".csv"))


