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

tile_flow_FD_reg_model <-
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
  {. ->> recession_slope_dist} %>%
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
tile_flow_FD_pred <-
  tile_flow_FD %>%
  unnest() %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest() %>%
  # combine regression model and recession slope with flow data
  left_join(tile_flow_FD_reg_model, by = c("siteid", "plotid", "dwm", "rep", "season")) %>%
  left_join(recession_slope_FD[1:6], by = c("siteid", "plotid", "season")) %>%
  select(-reg_data) %>%
  # add 3-day moving avarage rain to data
  mutate(data = map(.x = data,
                    .f = ~ .x %>%
                      mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right")) %>%
                      # leave calculated 3-day avarage only for those days when it rained
                      mutate(rain_3 = ifelse(rain > 0, rain_3, 0)))) %>%
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
         flow_pred2 = round(flow_pred2 * exp(ave_slope * count), 6),
         # use trimmed slope
         flow_pred2_trim = round(flow_pred2 * exp(trim_ave_slope * count), 6),
         flow_pred = ifelse(is.na(flow_pred), flow_pred2, flow_pred),
         flow_pred_trim = ifelse(is.na(flow_pred), flow_pred2_trim, flow_pred),
         flow_pred2 = NULL, flow_pred2_trim = NULL, count = NULL) %>%
  # remove predicitoins of flow in days when there was no rainfall data (see CRAWF)
  mutate(flow_pred = ifelse(is.na(flow) & is.na(rain), NA, flow_pred),
         flow_pred_trim = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_trim))


# CD Model with Board -----------------------------------------------------

# prepare CD data 

# set up the last dates for board adjustment 
# and add the first date and depth for West plot at AUGLA
ending <- tribble(~siteid, ~ plotid, ~depth, ~tmsp,
                  "AUGLA",     "East",   54.46, "2008-01-18 00:00:00",
                  "AUGLA",     "East",      NA, "2012-06-17 23:00:00",
                  "AUGLA",     "West",  154.23, "2012-06-18 00:00:00",
                  "AUGLA",     "West",      NA, "2014-12-31 23:00:00",
                  "CRAWF",     "South",     NA, "2010-12-31 23:00:00",
                  "DEFI_M",    "East",      NA, "2011-12-31 23:00:00", # recheck
                  "DPAC",      "NW",        NA, "2017-12-31 23:00:00", # recheck
                  "DPAC",      "SE",        NA, "2017-12-31 23:00:00", # recheck
                  "HARDIN",    "South",     NA, "2012-12-31 23:00:00", # recheck
                  "HARDIN_NW", "South",     NA, "2011-12-31 23:00:00",
                  "HENRY",     "East",      NA, "2011-12-31 23:00:00",
                  "SERF_IA",   "S3",        NA, "2017-12-31 23:00:00",
                  "SERF_IA",   "S4",        NA, "2017-12-31 23:00:00",
                  "SERF_SD",   "Plot8",     NA, "2017-12-31 23:00:00",
                  "STJOHNS",   "WN",        NA, "2014-12-31 23:00:00",
                  "TIDE",      "H3",        NA, "2011-12-31 23:00:00",
                  "TIDE",      "H4",        NA, "2011-12-31 23:00:00") %>%
  mutate(tmsp = parse_date_time(tmsp, orders = "Ymd HMS"))

# categories DWM management data
dwm_mngt_filled <-
  dwm_mngt %>%
  # select only sites of interest
  filter(siteid %in% tile_flow_FD$siteid) %>%
  # add last dates for board hights 
  bind_rows(ending) %>%
  arrange(siteid, plotid, tmsp) %>%
  # assign board hights to the last dates
  mutate(depth_filled = na.locf(depth, na.rm = FALSE)) %>%  
  # categorize board heights 
  group_by(siteid, plotid) %>%
  mutate(max_depth = max(depth_filled, na.rm = TRUE),
         min_depth = min(depth_filled, na.rm = TRUE),
         # calculate depth that is regulated
         reg_depth = max_depth - min_depth) %>%
  # define upper and lower thresholds for board heights
  mutate(upper = min_depth + 0.20 * reg_depth,
         lower = max_depth - 0.25 * reg_depth) %>%
  # assign names to categories
  mutate(board_cat = ifelse(depth < upper, "close", "mid"),
         board_cat = ifelse(depth > lower, "open", board_cat)) %>%
  # add date
  mutate(date = as.Date(tmsp)) %>%
  select(-max_depth, -min_depth, -reg_depth, -upper, -lower)
  
# make a table that shows sites, plots and years that dwm data is available for
dwm_mngt_summary <- 
  dwm_mngt_filled %>%
  mutate(year = year(tmsp) %>% as.integer()) %>%
  count(siteid, plotid, year) %>%
  select(-n) %>%
  # add years for sites that did not adjusted board during the year
  bind_rows(tribble(~ siteid, ~ plotid, ~year,
                    "SERF_IA", "S3", 2013,
                    "SERF_IA", "S3", 2014,
                    "SERF_IA", "S4", 2013,
                    "SERF_IA", "S4", 2014,
                    "TIDE",    "H3", 2008,
                    "TIDE",    "H3", 2010,
                    "TIDE",    "H4", 2008,
                    "TIDE",    "H4", 2010))
  

# prepare CD data for model 
tile_flow_CD <-
  tile_flow %>%
  group_by(siteid, plotid, dwm) %>%
  nest() %>%
  # count number of plots per site
  group_by(siteid) %>%
  mutate(plot_count = n_distinct(plotid)) %>%
  # select only CD plots
  filter(dwm == "CD") %>%
  # find sites where dwm treatment was swapped (cd_plot_count = plot_count = 2)
  mutate(cd_plot_count = n_distinct(plotid)) %>%
  # assign reps by taking into account plots with swapped treatment 
  mutate(rep = ifelse(plot_count == cd_plot_count, 1, NA),
         rep = ifelse(cd_plot_count == 1, cd_plot_count, rep),
         rep = ifelse(is.na(rep), 1:plot_count, rep)) %>%
  select(-ends_with("count")) %>%
  unnest() %>%
  ungroup() %>% 
  # add season 
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall")),
         rep = as.factor(rep)) %>%
  select(siteid, plotid, dwm, rep, year, season , date, flow, rain = precip_on_site) %>%
  # filter data to include only those sites, plots, and years that have dwm management data
  inner_join(dwm_mngt_summary, by = c("siteid", "plotid", "year")) %>%
  # add dwm categories and fill NAs down using previous entries
  left_join(dwm_mngt_filled, by = c("siteid", "plotid", "date")) %>% 
  arrange(siteid, plotid, date) %>%
  group_by(siteid, plotid, dwm, rep) %>%
  fill(board_cat, depth_filled, .direction = "down") %>%
  # remove dates when there is not dwm info
  filter(!is.na(board_cat)) %>%
  # remove flow in days when boards were adjusted since it can reflect drainage which is not associated with rainfall
  mutate(flow_for_model = ifelse(is.na(depth), flow, NA)) %>%
  select(-tmsp) %>%
  # remove 2015 data at STJOHNS dues to possible errors
  filter(!(siteid == "STJOHNS" & year > 2014)) %>%
  nest()


# Develop Regression model for predicting peak flows caused by precipitation event

# function to fit linear model
cd_reg_model <- function(df) {
  lm(flow_for_model ~ rain_3 - 1, data = df)
}

tile_flow_CD_reg_model <-
  tile_flow_CD %>%
  # calculate limitting tile flow as mean daily summer flow
  mutate(reg_data = map(.x = data, 
                        .f = ~ .x %>%
                          mutate(min_flow = mean(flow_for_model[season == "summer"], na.rm = TRUE)) %>%
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
                          filter(flow_for_model > min_flow, rain_3 > 0))) %>%
  # add board_cat to nesting
  unnest(reg_data) %>%
  group_by(siteid, plotid, dwm, rep, board_cat) %>%
  nest(.key = "reg_data") %>%
  # fit the model
  mutate(reg_model = map(reg_data, cd_reg_model))


# calculate recession slope for falling limb of the graph

# function to select peak and inflection points
peak_inflection_CD <- function(df) {
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
    select(year, season, date, board_cat,flow, rain, ln_flow, group, POINT) %>%
    # calculate duration of PEAK-TO-INFLECTION period and slope
    mutate(days_bw_pni = lead(date) - date,
           days = as.numeric(days_bw_pni),
           change = (lead(ln_flow) - ln_flow),
           slope = change/days)
}

# calculate average recession slope by season
recession_slope_CD <-
  tile_flow_CD %>%
  # log-transform flow data 
  mutate(data = map(.x = data, 
                    .f = ~ .x %>% 
                      # make sure to handle log(0) 
                      mutate(ln_flow = ifelse(flow_for_model == 0, NA, log(flow_for_model))))) %>%
  # find PEAK and INFLECTION points
  mutate(data = map(data, peak_inflection_CD)) %>%
  unnest() %>%
  # select "peak" POINT to calculate slope of recessuin (falling) limb 
  filter(POINT == 'peak') %>%
  filter(!is.infinite(slope)) %>%
  # save intermediate data for plotting in Global Environment
  {. ->> recession_slope_dist_CD} %>%
  # calculate average number of days between peak and first point of inflection
  # and the slope of recession limb 
  group_by(siteid, plotid, board_cat) %>%
  summarise(ave_days = mean(days),
            ave_slope = mean(slope),
            # calculate geometric and trimmed means because distributions of slope are right-skewed 
            trim_ave_slope = mean(slope, trim = 0.1),  # less conservative
            geom_ave_slope = -exp(mean(log(-slope))),  # more conservative
            harmonic_ave_slop = 1/(mean(1/slope))      # most conservative
  ) %>%
  ungroup()


# predict mssing data
tile_flow_CD_pred <-
  tile_flow_CD %>%
  unnest() %>%
  group_by(siteid, plotid, dwm, rep, board_cat) %>%
  nest() %>%
  # combine regression model and recession slope with flow data
  left_join(tile_flow_CD_reg_model, by = c("siteid", "plotid", "dwm", "rep", "board_cat")) %>%
  left_join(recession_slope_CD[1:6], by = c("siteid", "plotid", "board_cat")) %>%
  select(-reg_data) %>%
  # add 3-day moving avarage rain to data
  mutate(data = map(.x = data,
                    .f = ~ .x %>%
                      mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right")) %>%
                      # leave calculated 3-day avarage only for those days when it rained
                      mutate(rain_3 = ifelse(rain > 0, rain_3, 0)))) %>%
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
  select(-isnull, -pred, -.fitted, -.se.fit, -rain_3, - flow_for_model, -row_num) %>%
  ungroup() %>%
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
         flow_pred2 = round(flow_pred2 * exp(ave_slope * count), 6),
         # use trimmed slope
         flow_pred2_trim = round(flow_pred2 * exp(trim_ave_slope * count), 6),
         flow_pred = ifelse(is.na(flow_pred), flow_pred2, flow_pred),
         flow_pred_trim = ifelse(is.na(flow_pred), flow_pred2_trim, flow_pred),
         flow_pred2 = NULL, flow_pred2_trim = NULL, count = NULL) %>%
  # remove predicitoins of flow in days when there was no rainfall data (see CRAWF)
  mutate(flow_pred = ifelse(is.na(flow) & is.na(rain), NA, flow_pred),
         flow_pred_trim = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_trim))



tile_flow_CD_pred %>%
  filter(siteid == "AUGLA") %>%
  group_by(siteid, plotid, year) %>%
  summarise(flow = sum(flow, na.rm = T),
            flow_pred = sum(flow_pred, na.rm = T),
            flow_pred_trim = sum(flow_pred_trim, na.rm = T)) %>%
  mutate(id = paste(siteid, plotid)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = flow_pred, color = id)) +
  geom_point(aes(y = flow, color = id)) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2006:2017))

tile_flow_CD_pred %>%
  arrange(siteid, plotid, date) %>%
  filter(siteid == "AUGLA") %>% #filter(year < 2012) %>%
  mutate(DATE = update(date, year = 2012)) %>%
  ggplot(aes(x=DATE, group = plotid)) +
  geom_line(aes(y = flow_pred, colour = plotid), col = "black") +
  geom_point(aes(y = flow_pred), size = 1.2, col = "blue") +
  geom_point(aes(y = flow, colour = plotid), size = 0.9) +
  facet_grid(year ~ .) +
  theme_bw()


#  AUGLA   << Good
#  CRAWF   << Good
#  DEFI_M  << problems in 2010, but it should be removed, or at least first 9 months
#  DPAC    << remove 2017 for NE
#  HARDIN  << problems in 2008
#  HARDIN_NW   >>> NO PREDICTIONS
#  HENRY   << Good
#  SERF_IA << fix tile flow data as S4 in Aug-Sep 2017, should be zero
#  SERF_SD << Good
#  STJOHNS << OK
#  TIDE    << Use predictions from step 3




# CD Model with Season ----------------------------------------------------

# prepare FD data 

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

# function to fit linear model
seasonal_reg_model <- function(df) {
  lm(flow ~ rain_3 - 1, data = df)
}

tile_flow_CD_season_reg_model <-
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
                          mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right")) %>%
                          # leave calculated 3-day avarage only for those days when it rained
                          mutate(rain_3 = ifelse(rain > 0, rain_3, 0)))) %>%
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
  {. ->> recession_slope_dist} %>%
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
tile_flow_CD_season_pred <-
  tile_flow_CD_season %>%
  unnest() %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest() %>%
  # combine regression model and recession slope with flow data
  left_join(tile_flow_CD_season_reg_model, by = c("siteid", "plotid", "dwm", "rep", "season")) %>%
  left_join(recession_slope_CD_season[1:6], by = c("siteid", "plotid", "season")) %>%
  select(-reg_data) %>%
  # add 3-day moving avarage rain to data
  mutate(data = map(.x = data,
                    .f = ~ .x %>%
                      mutate(rain_3 = rollmean(rain, 3, fill = rain/3, align = "right")) %>%
                      # leave calculated 3-day avarage only for those days when it rained
                      mutate(rain_3 = ifelse(rain > 0, rain_3, 0)))) %>%
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
         flow_pred2 = round(flow_pred2 * exp(ave_slope * count), 6),
         # use trimmed slope
         flow_pred2_trim = round(flow_pred2 * exp(trim_ave_slope * count), 6),
         flow_pred = ifelse(is.na(flow_pred), flow_pred2, flow_pred),
         flow_pred_trim = ifelse(is.na(flow_pred), flow_pred2_trim, flow_pred),
         flow_pred2 = NULL, flow_pred2_trim = NULL, count = NULL) %>%
  # remove predicitoins of flow in days when there was no rainfall data (see CRAWF)
  mutate(flow_pred = ifelse(is.na(flow) & is.na(rain), NA, flow_pred),
         flow_pred_trim = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_trim))



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
  tile_flow_new


tile_flow_new %>%
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
  write_csv(paste0("Output/Data/tile_flow_daily_filled_", Sys.Date(), ".csv"))
  

  

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


