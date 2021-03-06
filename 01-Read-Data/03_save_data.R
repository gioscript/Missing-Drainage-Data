# Save tile flow data for Replicate Regression (STEP 3) -----------------

tile_flow_with_dwm %>%
  # select sites with more than 2 plots
  group_by(siteid) %>%
  mutate(n = length(unique(plotid))) %>%
  filter(n > 2) %>%
  # make sure that there are replicated treatments
  mutate(n = length(unique(dwm))) %>%
  filter(n > 1) %>%
  select(siteid, plotid, dwm, year, date, flow) %>%
  write_csv(path = paste0(dirname(getwd()), "/03-Rep-Regression/Data/tile_flow_with_reps.csv"))



# Save tile flow and on-site precipitation data for Regression Model (STEP 5) ----

tile_flow_with_dwm %>%
  left_join(weather_on_site_formated, by = c("siteid", "date")) %T>%
  # as an output in the same folder for Step 1
  write_csv(path = "Output/Data/tile_flow_with_rain_STEP1.csv") %>%
  # as an input in the folder for Step 5
  write_csv(path = paste0(dirname(getwd()), "/05-Gap-Filling/Data/tile_flow_with_rain_STEP1.csv"))



# Save dwm management data for Regression Model (STEP 5) ------------------

dwm_mngt %T>%
  # as an output in the Step 1
  write_csv(path = "Output/Data/dwm_mngt_STEP1.csv") %>%
  # as an input for the Step 5
  write_csv(path = paste0(dirname(getwd()), "/05-Gap-Filling/Data/dwm_mngt_STEP1.csv"))



# Save tile flow data for Gap-Filling Paper -----------------

tile_flow_with_dwm %>%
  # select site and plots used for paper 
  filter(dwm == 'FD' & siteid %in% c('SERF_IA', 'DPAC')) %>%
  # add precipitation data
  left_join(weather_on_site_formated, by = c("siteid", "date")) %>%
  # as an output in the same folder for Step 1
  write_csv(path = "../../Synthesis Papers/Gio - Gap Filling/Analysis/Data/Input_Data/daily_tile_flow.csv") 
