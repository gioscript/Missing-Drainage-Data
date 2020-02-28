library(tidyverse)
library(lubridate)
library(readxl)
library(modelr)
library(broom)

# Read tile flow data -----------------------------------------------------

tile_flow <- read_csv("Data/tile_flow_with_reps.csv")

# assign reps numbers (arbitrary)
dwm_reps <- 
  tile_flow %>%
  count(siteid, plotid, dwm) %>%
  group_by(siteid, dwm) %>%
  mutate(rep = 1:n()) %>%
  ungroup() %>%
  select(siteid, plotid, dwm, rep) %>%
  mutate(rep = paste0("rep_", rep))

# add replication numbers
tile_flow_reps <-
  tile_flow %>%
  # assign rep numbers
  left_join(dwm_reps, by = c("siteid", "plotid", "dwm")) %>%
  select(-year, -plotid) %>%
  spread(rep, flow)



# Predict missing data using replicates -----------------------------------

# function to fit linear model to replicated plots
rep_model_1 <- function(df) {
  add_predictions(data = df, 
                  model = lm(rep_1 ~ rep_2 - 1, data = df), 
                  var = "pred_1")
  }

rep_model_2 <- function(df) {
  add_predictions(data = df, 
                  model = lm(rep_2 ~ rep_1 - 1, data = df), 
                  var = "pred_2")
}

# predict missing values by fiting models
tile_flow_fitted <- 
  tile_flow_reps %>%
  # add season as factor
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall"))) %>%
  group_by(siteid, dwm, season) %>% 
  nest() %>%
  # add predictions 
  mutate(data = map(data, rep_model_1),
         data = map(data, rep_model_2)) %>%
  unnest(data) %>%
  select(-season) %>%
  # transform table so predictions are next to acctual measurements
  gather(key = key, value = flow, rep_1:pred_2) %>%
  separate(key, into = c("key", "rep_number")) %>%
  spread(key, flow) %>%
  rename(flow = rep, rep = rep_number) %>%
  # assign plot ids
  mutate(rep = paste0("rep_", rep)) %>%
  left_join(dwm_reps, by = c("siteid", "rep", "dwm")) %>%
  select(siteid, plotid, dwm, date, flow, flow_pred_step_3 = pred) %>%
  # remove predictions of reps at DPAC in 2017
  mutate(flow_pred_step_3 = ifelse(siteid == "DPAC" & year(date) == 2017, NA, flow_pred_step_3)) %>%
  # remove predictions of reps at S2 in SERF_IA in 2009-2010
  mutate(flow_pred_step_3 = ifelse(siteid == "SERF_IA" & year(date) %in% 2009:2010, NA, flow_pred_step_3)) %>%
  
  # add comments for predicted values
  mutate(comments = ifelse(is.na(flow) & !is.na(flow_pred_step_3), "predicted via rep regression", NA),
         # replace predicted measurements with acctual 
         flow_pred_step_3 = ifelse(is.na(comments), flow, flow_pred_step_3)) %>%
  arrange(siteid, plotid, date)



