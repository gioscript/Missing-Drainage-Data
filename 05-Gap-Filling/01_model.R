library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library(modelr)
library(broom)



# function to calculate 3-day weighted rolling/moving average 
rollmean.weighted = function(x, W1 = 0.25, W2 = 0.45, W3 = 0.30) {
  if (sum(is.na(x)) == 3) {
    NA
  } else {
    replace(x[1], is.na(x[1]), 0) * W1 +
      replace(x[2], is.na(x[2]), 0) * W2 +
      replace(x[3], is.na(x[3]), 0) * W3 
  }
  sum(x*c(W1, W2, W3))
}



# Read data ---------------------------------------------------------------

tile_flow <- read_csv("Data/tile_flow_with_rain_STEP1.csv")



# Prepaer data ------------------------------------------------------------

# distribution of on-site precipitation 
# ORIGINAL version
tile_flow %>% 
  filter(precip_on_site > 0) %>% 
  ggplot(aes(precip_on_site)) + 
  geom_density(adjust = 3) + 
  geom_vline(xintercept = 0.45) +
  xlim(0, 3) +
  theme_bw()
rain_limit = 0.45

# # NEW version
# tile_flow %>% 
#   filter(precip_on_site > 0) %>% 
#   mutate(season = quarter(date)) %>%
#   ggplot(aes(precip_on_site)) + 
#   geom_density(adjust = 3) + 
#   geom_vline(xintercept = 1.75) +
#   coord_cartesian(xlim = c(0, 30)) +
#   theme_bw() + 
#   facet_wrap(~ season)
# rain_limit = 1.75 

# add 3-day moving ave precipitation 
tile_flow_data <- 
  tile_flow %>%
  # select only Free and Controlled drainage plots
  filter(dwm %in% c("FD", "CD")) %>%
  group_by(siteid, plotid, dwm) %>%
  nest() %>%
  # count number of plots per site
  group_by(siteid) %>%
  mutate(plot_count = n_distinct(plotid)) %>%
  # find sites where dwm treatment was swapped
  group_by(siteid, dwm) %>%
  mutate(plot_count = n_distinct(plotid)) %>%
  group_by(siteid) %>%
  mutate(dwm_count = n_distinct(plotid)) %>% 
  # assign reps by taking into account plots with swapped treatment 
  mutate(rep = ifelse(plot_count == dwm_count, 1, NA),
         rep = ifelse(dwm_count == 1, dwm_count, rep),
         rep = ifelse(is.na(rep), 1:plot_count, rep)) %>% 
  select(-ends_with("count")) %>% 
  unnest() %>%
  # add season 
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall")),
         rep = as.factor(rep)) %>%
  select(siteid, plotid, dwm, rep, year, season , date, flow, rain = precip_on_site) %>%
  # calculate limitting tile flow as mean daily summer flow
  group_by(siteid, plotid, dwm, rep) %>%
  mutate(min_flow = mean(flow[season == "summer"], na.rm = TRUE),
         # correct limitting flow so it is not > 0.3 mm/day
         min_flow = ifelse(min_flow > 0.3, 0.3, min_flow)) %>%
  # calculate 3-day average precip
  group_by(siteid, plotid) %>%
  mutate(rain_3 = rollapplyr(rain, 3, mean, na.rm = TRUE, partial = TRUE),
         # calculated weighted moving average precip
         rain_3_weighted = rollapplyr(rain, width = 3, FUN = rollmean.weighted,
                                      partial = FALSE, fill = NA),
         # handle days which are out of the rolling range = first two recoreds in this case
         rain_3_weighted = ifelse(is.na(rain_3_weighted), rain_3, rain_3_weighted),
         # leave calculated 3-day avarage only for those days when it rained
         # OR rain was negligible (see the plot above)
         rain_3 = ifelse(rain > rain_limit, rain_3, 0),
         rain_3_weighted = ifelse(rain > rain_limit, rain_3_weighted, 0)) %>%
  group_by(siteid, plotid, dwm, rep) %>%
  nest()


# Develop "Seasonal" Regression model ------------------------------------- 
# predicting peak flows caused by precipitation event

# function to fit linear model
reg_model <- function(df) {
  lm(flow ~ rain_3 - 1, data = df)
}
reg_model_weighted <- function(df) {
  lm(flow ~ rain_3_weighted - 1, data = df)
}

tile_flow_reg_model <-
  tile_flow_data %>%
  # filter data to be used for regression 
  mutate(reg_data = map(.x = data,
                        .f = ~ .x %>%
                          # remove days when there was no rain OR tile flow was below minimum limit
                          filter(flow > min_flow, rain_3 > 0))) %>%
  # add season to nesting
  unnest(reg_data) %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest(.key = "reg_data") %>%
  # fit the model
  mutate(reg_model = map(reg_data, reg_model),
         reg_model_weighted = map(reg_data, reg_model_weighted))



# Calculate recession slope -----------------------------------------------
# slope of falling limb of the graph

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
recession_slope <-
  tile_flow_data %>%
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
  group_by(siteid, plotid, season, dwm) %>%
  summarise(ave_days = mean(days),
            # calculate trimmed, geometric and harmonic means since the distributions of slopes are right-skewed 
            ave_slope = mean(slope),
            ave_slope_trim = mean(slope, trim = 0.1),  # less conservative
            ave_slope_geom = -exp(mean(log(-slope))),  # more conservative
            ave_slope_harm = 1/(mean(1/slope))         # most conservative
  ) %>%
  ungroup()



# Predict mssing data -----------------------------------------------------

tile_flow_pred <-
  tile_flow_data %>%
  unnest() %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest() %>%
  # combine regression model and recession slope with flow data
  left_join(tile_flow_reg_model, by = c("siteid", "plotid", "dwm", "rep", "season")) %>%
  left_join(recession_slope[1:8], by = c("siteid", "plotid", "season", "dwm")) %>%
  select(-reg_data, -ave_days) %>% 
  gather(model_name, model, reg_model:reg_model_weighted) %>%
  # add predictions of pick flows using actual data
  mutate(isnull = map_lgl(model, is.null),
         # predict pick flow only when there is a model available 
         data = ifelse(isnull == 1, 
                       data, 
                       map2(.x = model,
                            .y = data,
                            .f = ~ augment(.x, newdata = .y)))) %>%
  select(siteid:model_name, starts_with("ave")) %>%
  unnest(data) %>% 
  # resolve the problem of skipping first data points when rain < rain_limit
    # for example after missing dates of precip there is a first record of 0 rain, 
    # we need to keep prediction of corresponding tile flow of 0, so we can predict down the timeline
  group_by(model_name, siteid, plotid) %>%
  # find the first non-NA value for precip within each site-plot
  mutate(is_rain = is.na(rain),
         group = rep(seq_along(rle(is_rain)$length), rle(is_rain)$length)) %>%
  group_by(model_name, siteid, plotid, group) %>%
  # select only those first non-NA values when there was a measurement
  mutate(is_first = ifelse(row_number() == 1 & is_rain == FALSE, "Y", "N")) %>%
  ungroup() %>%
  # choose predictions for days when it was raining and no flow measured
  mutate(pred = ifelse(.fitted < 0, 0, .fitted),  # eliminate predicted negative flows
         flow_pred = ifelse(is.na(flow) & rain > rain_limit, pred, flow),
         # add predictions for the first non-NA days when they are missing (due to rain < rain_limit)
         flow_pred = ifelse(is.na(flow_pred) & is_first == "Y", pred, flow_pred)) %>%
  select(siteid:rain, flow_pred, starts_with("ave")) %>%
  # predict flow of falling limb
  group_by(siteid, plotid, dwm, model_name) %>%
  mutate(LOGIC = ifelse(is.na(flow_pred), "Y", "N"),
         group = rep(seq_along(rle(LOGIC)$length), rle(LOGIC)$length)) %>% 
  group_by(siteid, plotid, dwm, model_name, group) %>%
  mutate(count = 1:n(),
         count = ifelse(LOGIC == "N", 0, count)) %>%
  group_by(siteid, plotid, dwm, model_name) %>%
  # limit predicted flow to measured max tile flow
  mutate(flow_max = max(flow, na.rm = T),
         flow_pred = ifelse(flow_pred > flow_max, flow_max, flow_pred),
         flow_max = NULL) %>% 
  select(-group, -LOGIC) %>%
  # apply recession equation (Qi = Qi-1 * e^k, where k is seasonal recession slope)
  mutate(flow_pred2 = na.locf(flow_pred, na.rm = FALSE),
         # use arithmetic mean slope
         flow_pred2_mean = round(flow_pred2 * exp(ave_slope * count), 6),
         # use trimmed mean slope
         flow_pred2_trim = round(flow_pred2 * exp(ave_slope_trim * count), 6),
         # use geometric mean slope
         flow_pred2_geom = round(flow_pred2 * exp(ave_slope_geom * count), 6),
         flow_pred_mean = ifelse(is.na(flow_pred), flow_pred2_mean, flow_pred),
         flow_pred_trim = ifelse(is.na(flow_pred), flow_pred2_trim, flow_pred),
         flow_pred_geom = ifelse(is.na(flow_pred), flow_pred2_geom, flow_pred)) %>%
  select(-starts_with("flow_pred2"), -count) %>%
  # remove predicitoins of flow in days when there was no rainfall data (see CRAWF)
  mutate(flow_pred_mean = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_mean),
         flow_pred_trim = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_trim),
         flow_pred_geom = ifelse(is.na(flow) & is.na(rain), NA, flow_pred_geom)) %>%
  select(-starts_with("ave_slope"))


# List years with predicted data for each site
tile_flow_pred %>%
  filter(model_name == "reg_model_weighted") %>%
  filter(is.na(flow) & !is.na(flow_pred_mean)) %>%
  ungroup() %>% count(siteid, year, plotid) #%>% filter(siteid == "STJOHNS")

# Count number of missing days in each site-plot-season
tile_flow_pred %>%
  arrange(model_name, siteid, plotid, date) %>%
  filter(model_name == 'reg_model_weighted') %>%
  mutate(season = quarter(date)) %>%
  group_by(siteid, plotid, year, season) %>%
  summarise(total = n(), 
            missing = sum(is.na(flow_pred_trim)),
            missing_percent = missing/total) %>%
  filter(siteid == "AUGLA") %>% # filter(year == 2009, plotid == "WN") %>%
  ungroup() %>%
  select(-total, -missing) %>%
  spread(plotid, missing_percent)

# Count number of predicted values
tile_flow_pred %>%
  arrange(model_name, siteid, plotid, date) %>%
  filter(model_name == 'reg_model_weighted') %>%
  mutate(season = quarter(date),
         pred = ifelse(is.na(flow) & !is.na(flow_pred_trim), "predicted", NA )) %>%
  group_by(siteid, plotid, year) %>%
  summarise(total = n(), 
            total_pred = sum(!is.na(pred))) %>% 
  ungroup() %>%
  filter(total_pred > 0) %>%
  arrange(desc(total_pred)) %>% 
  head(20)
# If number of predicted days per plot exceeds 150 in a calendar year, remove predictions
# Based on the above code predictions for folowing site-plot-years should be removed
    #     SiteID    PlotID   Year    Predicted    
    # 1   DEFI_M    East     2010    365
    # 2   DPAC      NE       2017    365
    # 3   DPAC      NW       2017    365
    # 4   HARDIN    North    2010    264
    # 5   SERF_IA   S2       2010    245
    # 6   SERF_IA   S2       2009    240
    # 7   HENRY     East     2010    239
    # 8   HENRY     West     2010    239
    # 9   STJOHNS   WN       2009    181
    # 10  STJOHNS   WS       2009    181
    # 11  DPAC      NE       2006    166
    # 12  DPAC      NW       2006    166
    # 13  DPAC      SE       2006    166
    # 14  DPAC      SW       2006    166

# Select predicted data ---------------------------------------------------

tile_flow_pred %>% 
  ungroup() %>%
  filter(model_name == 'reg_model_weighted') %>%
  select(siteid, plotid, dwm, rep, year, date, rain, flow, flow_pred_trim) %>%
  # remove predictions for plot-years when the whole year was missing
  mutate(flow_pred = case_when(siteid == "DEFI_M" & plotid == "East" & year == 2010 ~ NA_real_,
                               siteid == "DPAC" & year == 2006 ~ flow,
                               siteid == "DPAC" & year == 2017 & plotid %in% c("NW", "NE") ~ NA_real_,
                               siteid == "HARDIN" & year == 2010 & plotid == "North" ~ flow,
                               siteid == "HENRY" & year == 2010 ~ flow,
                               siteid == "SERF_IA" & year == 2009 & plotid == "S2" ~ flow,
                               siteid == "SERF_IA" & year == 2010 & plotid == "S2" ~ flow,
                               siteid == "STJOHNS" & year == 2009 ~ flow,
                               TRUE ~ flow_pred_trim)) %>%
  # comment predicted values that were left after above filter
  mutate(comments = ifelse(is.na(flow) & !is.na(flow_pred), "predicted", "")) %>%
  # add zero flows with corresponding comments for SERF_SD and HICKS_B
  mutate(comments = case_when(siteid == "SERF_SD" & month(date) %in% c(1:3) & is.na(flow_pred) ~ "filled in zero",
                              siteid == "HICKS_B" & month(date) %in% c(1:3) & is.na(flow) ~ "filled in zero",
                              TRUE ~ comments),
         flow_pred = case_when(siteid == "SERF_SD" & month(date) %in% c(1:3) & is.na(flow_pred) ~ 0,
                               siteid == "HICKS_B" & month(date) %in% c(1:3) & is.na(flow) ~ 0, 
                               TRUE ~ flow_pred)) %>%
  select(-flow_pred_trim) %>%
  ungroup() -> tile_flow_prediction



# read all data from UBWC
path_to_ubwc <- "C:/Users/gio/OneDrive - Iowa State University/Projects/TD/Data/Data Requests/Matt Helmers/Control_Box_Outlet_MNGT/FILLED - UBWC Tile Flow.xlsx"
tile_flow_ubwc <- vector(mode = 'list', length = length(excel_sheets(path_to_ubwc)))
for (i in seq_along(tile_flow_ubwc)) {
  tile_flow_ubwc[[i]] <- read_excel(path = path_to_ubwc, 
                                    sheet = i, 
                                    col_types = c("date", "numeric", "numeric")) %>%
    slice(-1)
}

tile_flow_ubwc_2005_2008 <-
  tile_flow_ubwc %>%
  bind_rows() %>%
  gather(key = plotid, value = flow, 2:3) %>%
  mutate(date = as_date(Date),
         site = "UBWC",
         plot = word(plotid),
         year = year(date) %>% as.integer(),
         season = factor(quarter(date), labels = c("Winter", "Spring", "Summer", "Fall")) %>%
           as.character()) %>%
  filter(year < 2009) %>%
  mutate(trt = "FD",
         rep = ifelse(plot == "B2", '1', '2'),
         comments = NA) %>%
  select(site, plot, trt, rep, year, season, date, flow, comments) 


# Save predicted data for ANOVA analysis ----------------------------------
tile_flow_prediction %>%
  filter(dwm %in% c("CD", "FD")) %>% 
  # get rid of erronous measurement at WN in STJOHNS in 2015
  mutate(flow_pred = ifelse(siteid == "STJOHNS" & year == 2015 & plotid == "WN", 
                            NA, flow_pred),
         comments  = ifelse(siteid == "STJOHNS" & year == 2015 & plotid == "WN", 
                            "removed", comments)) %>%
  # add season
  mutate(season = factor(quarter(date), labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  arrange(siteid, plotid, date) %>%
  select(siteid, plotid, dwm, rep, year, season, date, flow_pred, comments) %>% 
  # CORRECTIONS (added in 2019-02-14)
  # fix erroneous predictions at SERF_IA
  mutate(flow_pred = ifelse(siteid == 'SERF_IA' & plotid == 'S4' & year == 2017 & 
                              month(date) == 8 & comments == 'predicted', 0, flow_pred),
         comments = ifelse(siteid == 'SERF_IA' & plotid == 'S4' & year == 2017 & 
                             month(date) == 8 & comments == 'predicted', 'corrected', comments)) %>%
  # replace predictions at TIDE
  mutate(flow_pred = ifelse(siteid == 'TIDE' & comments == 'predicted', 0, flow_pred),
         comments = ifelse(siteid == 'TIDE' & comments == 'predicted', "", comments)) %T>%
  write_csv(paste0("Output/Data/tile_flow_daily_filled_", Sys.Date(), ".csv")) %>%
  # add sites with complete data {MUDS2, STORY, TIDE, UBWC}
  full_join(read_csv("C:/Users/gio/OneDrive - Iowa State University/Projects/TD/Data/Data Requests/Matt Helmers/Control_Box_Outlet_MNGT/Data/tile_flow_daily_for_Lori_2018-11-27.csv") %>%
              filter(siteid %in% c("MUDS2", "TIDE", "UBWC", "STORY")) %>%
              filter(!(siteid == "MUDS2" & is.na(tile_flow_mm))), 
            by = c('siteid', 'plotid', 'dwm', 'year', 'season', 'date')) %>%
  mutate(rep = ifelse(is.na(rep.y), rep.x, rep.y),
         flow = ifelse(is.na(tile_flow_mm), flow_pred, tile_flow_mm)) %>%
  select(site = siteid, plot = plotid, trt = dwm, rep, year, season, date, flow, comments) %>% 
  # FIX REPS (added in 2019-02-19 according to Lori's email)
  mutate(rep = case_when(site == 'STORY' & trt == "FD" & plot == "5" ~ "1",
                         site == 'STORY' & trt == "FD" & plot == "8" ~ "2",
                         site == 'STORY' & trt == "FD" & plot == "9" ~ "3",
                         site == 'STORY' & trt == "FD" & plot == "11" ~ "4",
                         TRUE                                        ~ rep)) %>%
  # add missing years from UBWC when both plots were in FD mode (Lori asked in 2019-01-14)
  bind_rows(tile_flow_ubwc_2005_2008) %>%
  arrange(site, plot, date) %>%
  write_csv(paste0("Output/Data/tile_flow_for_ANOVA_", Sys.Date(), ".csv"))




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



