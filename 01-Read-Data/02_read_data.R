library(readxl)
library(tidyverse)
library(lubridate)



# Read tile flow data -----------------------------------------------------

tile_flow_unformated <- vector(mode = "list", length = length(list_of_Google_sheets_to_download))

for (i in seq_along(list_of_Google_sheets_to_download)) {
  print(paste("Reading tile flow data for", word(list_of_Google_sheets_to_download[i], -3), "..."))
  # read tab (sheet) names
  flow_sheet_tab_name = excel_sheets(paste0("Data/Tile-Flow/", list_of_Google_sheets_to_download[i], ".xlsx"))
  # add sub-list for each year-tab inside the main list (corresponding to the sites)
  tile_flow_unformated[[i]] = vector(mode = "list", length = length(flow_sheet_tab_name))
  for (j in seq_along(flow_sheet_tab_name)) {
    # ignore tab (sheet) for 2018
    if (flow_sheet_tab_name[j] == "2018") {
      print("..Skipping year 2018")
    } else {
      # read column headings
      flow_sheet_column_heading = read_excel(paste0("Data/Tile-Flow/", list_of_Google_sheets_to_download[i], ".xlsx"),
                                              sheet = j, n_max = 2) %>% names()
      # read data and add names to columns
      tile_flow_unformated[[i]][[j]] = read_excel(paste0("Data/Tile-Flow/", list_of_Google_sheets_to_download[i], ".xlsx"),
                                              sheet = j,
                                              # 'range' helps to avoid problems with empty cells
                                              range = cell_limits(c(3, 1),
                                                                  c(NA, length(flow_sheet_column_heading))),
                                              col_names = flow_sheet_column_heading,
                                              col_types = c("date", rep("text", length(flow_sheet_column_heading) -1)))
      # add columns with siteids and crop year
      tile_flow_unformated[[i]][[j]]$siteid = word(list_of_Google_sheets_to_download[i], -3)
      tile_flow_unformated[[i]][[j]]$year = flow_sheet_tab_name[j]
    }
  }
}

# combine tile flow data into tibble
tile_flow_formated <-
  tile_flow_unformated %>%
  # bind data by each site
  map(.f = bind_rows) %>%                        # OR lapply(function(x) {bind_rows(x)})
  # remove 'Time' column in any list element
  lapply(function(x) { x["Time"] <- NULL; x}) %>%
  # make plotid as column within each list element
  lapply(function(x) {x %>% 
      gather(key = plotid, value = flow, -c(siteid, year, Date)) %>%
      mutate(plotid = word(plotid))}) %>%
  # combine all site-years as tibble
  bind_rows() %>%
  select(siteid, plotid, year, date = Date, flow) %>%
  mutate(date = as_date(date),
         year = as.numeric(year),
         flow = as.double(flow))


# Read plot DWM treatment -------------------------------------------------

plot_DWM_trt <- read_excel("Data/TD Site Keys.xlsx",
           sheet = "DWM ID", skip = 1) %>%
  select(siteid = Site_ID, plotid = CS_ID, starts_with("Y")) %>%
  gather(key = year, value = dwm, starts_with("Y")) %>%
  mutate(year = parse_number(year),
         dwm = parse_character(dwm, na = "NA")) %>%
  filter(siteid %in% sites_CD$Site_ID,
         !is.na(dwm))
  
# add DWM treatment to tile flow data
tile_flow_with_dwm <-
  tile_flow_formated %>%
  left_join(plot_DWM_trt, by = c("siteid", "plotid", "year")) %>%
  # fix swaping of treatments at AUGLA
  mutate(dwm = ifelse(siteid == "AUGLA" & plotid == "East" & year == 2012 & date < ymd(20120618), 
                      "CD", dwm),
         dwm = ifelse(siteid == "AUGLA" & plotid == "West" & year == 2012 & date < ymd(20120618), 
                      "FD", dwm))



# Save tile flow data for Replicate Regression ----------------------------

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


