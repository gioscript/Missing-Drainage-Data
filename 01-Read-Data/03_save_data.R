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


