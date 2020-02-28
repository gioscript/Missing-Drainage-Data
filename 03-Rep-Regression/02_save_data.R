# Save predicted data -----------------------------------------------------

tile_flow_fitted %T>%
  # as an output in the same folder for Step 3
  write_csv(path = "Output/Data/tile_flow_fitted_STEP3.csv") %>%
  # as an input in the folder for Step 5
  write_csv(path = paste0(dirname(getwd()), "/05-Gap-Filling/Data/tile_flow_fitted_STEP3.csv"))



# Combine predicted data with other site data -----------------------------

# read tile flow data from STEP1
tile_flow_STEP1 <- read_csv('../01-Read-Data/Output/Data/tile_flow_with_rain_STEP1.csv')

# substitude missing data with predictions
tile_flow_STEP1 %>%
  full_join(tile_flow_fitted, by = c('siteid', 'plotid', 'dwm', 'date', 'flow')) %>%
  mutate(flow = ifelse(is.na(flow), flow_pred_step_3, flow)) %>%
  select(siteid, plotid, year, date, flow, dwm, precip_on_site) %>%
  # save as an input in the folder for Step 5
  write_csv(path = paste0(dirname(getwd()), "/05-Gap-Filling/Data/tile_flow_with_rain_STEP3.csv"))
  

