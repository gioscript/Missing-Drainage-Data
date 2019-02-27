# this peice of code is used to find the best weights for calculating 3-day moving average precipitation

# based on visual results it looks that:
#   a. non-weighted (i.e. equally weighted) average performs better than 10-30-60
#   b. improvments over non-weighted average was achieved by giving more weight to the seond day precip (e.g. 25-45-30)


# function to fit linear model
reg_model <- function(df) {
  lm(flow ~ rain_3 - 1, data = df)
}
reg_model_weighted <- function(df) {
  lm(flow ~ rain_3_weighted - 1, data = df)
}


rain_limit = 0.45

# function to calculate 3-day weighted rolling/moving average 
rollmean.weighted = function(x, W1 = .25, W2 = .45, W3 = .30) {
  if (sum(is.na(x)) == 3) {
    NA
  } else {
    replace(x[1], is.na(x[1]), 0) * W1 +
      replace(x[2], is.na(x[2]), 0) * W2 +
      replace(x[3], is.na(x[3]), 0) * W3 
  }
  sum(x*c(W1, W2, W3))
}


# add 3-day moving ave precipitation 
data <- 
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


reg_models <-
  data %>%
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

# plot R2 values of two models agains each other
reg_models %>% 
  mutate(R_reg_model = reg_model  %>% map(glance) %>% map_dbl("r.squared"), 
         R_wei_model = reg_model_weighted  %>% map(glance) %>% map_dbl("r.squared"),
         n_of_points = reg_data %>% map_dbl(nrow)) %>% 
  ggplot(aes(R_reg_model, R_wei_model)) + 
  geom_point(aes(col=siteid)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~ season)


# plot regression models
reg_models %>% 
  mutate(R_reg_model = reg_model  %>% map(glance) %>% map_dbl("r.squared"), 
         R_wei_model = reg_model_weighted  %>% map(glance) %>% map_dbl("r.squared"),
         n_of_points = reg_data %>% map_dbl(nrow),
         Predictions = reg_model_weighted %>% map(augment)) %>%
  unnest(Predictions) %>% 
  filter(siteid %in% c("DPAC")) %>%
  # filter(flow > 1 & rain_3_weighted > 1) %>%
  ggplot(aes(x=rain_3_weighted, y=flow)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ I(x^2) - 1, se = FALSE, col = "green")+
  geom_smooth(method = "lm", formula = y ~ sqrt(x) - 1, se = FALSE, col = "red")+
  geom_smooth(method = "lm", formula = y ~ I(x^1.5) - 1, se = FALSE, col = "black")+
  geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE) +
  facet_grid(plotid ~ season, scales = "free_y") +
  theme_bw()


# plot R2 values of two models agains each other
reg_models %>% 
  mutate(R_reg_model = reg_model  %>% map(glance) %>% map_dbl("r.squared"), 
         R_wei_model = reg_model_weighted  %>% map(glance) %>% map_dbl("r.squared"),
         n_of_points = reg_data %>% map_dbl(nrow),
         Predictions = reg_model_weighted %>% map(augment)) %>%
  unnest(Predictions) %>% 
  filter(siteid %in% c("SERF_IA", "SERF_SD")) %>%
  # filter(flow > 1 & rain_3_weighted > 1) %>%
  ggplot(aes(x=flow)) +
  geom_density(adjust = 5, fill = "orange") +
  facet_grid(plotid ~ season, scales = "free_y") +
  theme_bw()
