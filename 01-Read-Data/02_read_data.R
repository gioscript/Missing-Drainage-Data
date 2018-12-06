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



# Read on-site weather data -----------------------------------------------

weather_on_site_unformated <- vector(mode = "list", length = length(list_of_Weather_sheets_to_download))

for (i in seq_along(list_of_Weather_sheets_to_download)) {
  # exclude sites that has multiple on-site weather stations
  if (list_of_Weather_sheets_to_download[i]  %in% c("SERF_IA Weather")) {
    print(paste(list_of_Weather_sheets_to_download[i], "needs to be processed individually"))
  } else {
    print(list_of_Weather_sheets_to_download[i])
    # read column headings
    weather_sheet_column_heading = read_excel(paste0("Data/On-Site-Weather/", list_of_Weather_sheets_to_download[i] , ".xlsx"),
                                              sheet = "DAILY", n_max = 2) %>% names()
    # read data and add names to columns
    weather_on_site_unformated[[i]] = read_excel(paste0("Data/On-Site-Weather/", list_of_Weather_sheets_to_download[i] , ".xlsx"),
                                                 sheet = "DAILY", skip = 2, 
                                                 col_names = weather_sheet_column_heading,
                                                 col_types = c("date", rep("numeric", length(weather_sheet_column_heading) - 1)))
    # add column with site ID
    weather_on_site_unformated[[i]]$siteid = word(list_of_Weather_sheets_to_download[i])
  }
}

# read data from sites with multiple weather stations

# SERF_IA >>>

# Manual weather station 
weather_on_site_unformated_SERF_IA_Manual <-
  read_excel("Data/On-Site-Weather/SERF_IA Weather.xlsx",
             sheet = "MANUAL", skip = 2,
             col_names = c("Date", "Precipitation"))

# LevelRain weather station 
weather_on_site_unformated_SERF_IA_LevelRain <-
  read_excel("Data/On-Site-Weather/SERF_IA Weather.xlsx",
             sheet = "DAILY LevelRain", skip = 2,
             col_names = c("Date", "Precipitation"))

# ISU SM Network weather station 
weather_on_site_unformated_SERF_IA_ISUnetwork <-
  read_excel("Data/On-Site-Weather/SERF_IA Weather.xlsx",
             sheet = "DAILY ISUnetwork", skip = 2,
             col_names = c("Date", "Precipitation"))

# overlaping precipitation records were chosen according to Kristina Craft's recommendation (email from 2017-04-07)
weather_on_site_unformated_SERF_IA <-
  weather_on_site_unformated_SERF_IA_Manual %>%
  filter(year(Date) < 2012) %>%
  bind_rows(weather_on_site_unformated_SERF_IA_LevelRain %>%
              filter(year(Date) %in% c(2012, 2013))) %>%
  bind_rows(weather_on_site_unformated_SERF_IA_ISUnetwork %>%
              filter(year(Date) > 2013)) %>%
  # add missing Nov-Dec precip in 2011
  bind_rows(weather_on_site_unformated_SERF_IA_ISUnetwork %>%
              filter(year(Date) == 2011, month(Date) > 10)) %>%
  arrange(Date) %>%
  mutate(siteid = "SERF_IA")

# combine on-site weather data for all sites
weather_on_site_formated <-
  weather_on_site_unformated %>%
  bind_rows() %>%
  bind_rows(weather_on_site_unformated_SERF_IA) %>%
  select(siteid, date = Date, precip_on_site = Precipitation) %>%
  mutate(date = as.Date(date)) 



# Read DWM management  ----------------------------------------------------

dwm_mngt <-
  read_csv("Data/DWM Management.csv") %>%
  slice(-1) %>%
  # rename columns
  select(siteid = uniqueid, plotid = box_structure, 
         tmsp = outlet_date, depth = outletdepth) %>%
  # select CD sites only
  filter(siteid %in% sites_CD$Site_ID) %>%
  mutate(tmsp = parse_date_time(tmsp, "Ymd_HMS", truncated = 3),
         depth = as.numeric(depth)) %>%
  # remove entries with no depth data or no board changes (such as SERF_IA 2013-2014)
  filter(!is.na(depth), !is.na(tmsp)) %>%
  # remove board data at STJOHNS for FD plot
  filter(!(siteid == "STJOHNS" & plotid == "WS")) %>%
  # remove board data at AUGLA when West plot was at FD mode
  filter(!(siteid == "AUGLA" & plotid == "West" & tmsp < ymd(20120601))) %>%
  # expend DWM treatment for each plot (some sites list >1 plot in plotid)
  separate(plotid, into = c("plot1", "plot2"), sep = ',\ ?') %>%
  gather(key = plot, value = plotid, plot1:plot2, na.rm = TRUE) %>%
  select(siteid, plotid, tmsp, depth)
