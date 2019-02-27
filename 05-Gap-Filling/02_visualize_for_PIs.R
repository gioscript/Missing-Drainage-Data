library(readr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(wesanderson) # for colour pallets


# Modify data for plotting ------------------------------------------------

tile_flow_prediction %>%
  mutate(pred = ifelse(is.na(flow), flow_pred, NA)) %>%
  arrange(siteid, plotid, date) %>% 
  mutate(DATE = update(date, year = 2012)) %>%
  group_by(siteid) %>%
  # scale down rain to max tile flow
  mutate(RAIN = rain/max(rain, na.rm = TRUE)*max(flow_pred, na.rm = TRUE)) %>%
  ungroup() -> plot_tile_flow_data



# FIGURE 1. All Data Years ------------------------------------------------
plot_count <- 
  plot_tile_flow_data %>% 
  group_by(siteid) %>%
  summarise(count = n_distinct(plotid))

for (i in seq_along(plot_count$siteid)) {
  plot_tile_flow_data %>%
    left_join(plot_count, by = "siteid") %>%
    filter(siteid == plot_count$siteid[i]) %>%
    ggplot(aes(x = DATE, group = plotid)) +
    geom_col(aes(y = RAIN/count), fill = "skyblue", alpha = 0.45) +
    geom_line(aes(y = flow_pred, colour = plotid), size = 0.8) +
    geom_point(aes(y = flow_pred, colour = plotid), size = 1) +
    geom_point(aes(y = pred, colour = plotid), size = 2) +
    geom_point(data = . %>% filter(comments == "predicted"), 
               aes(y = -1), shape = 15, col = "green", size = 1.1) +
    facet_grid(year ~ .) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", name = NULL) +
    scale_y_continuous(name = "Tile Flow (mm)") +
    scale_colour_manual(values = c("#D67236", "#5B1A18", "#5F5647", "#F4BA7B")) +
    theme_bw()
  ggsave(filename = paste0("Output/Figs/model-predictions/", plot_count$siteid[i], "/",
                           plot_count$siteid[i], "_Fig1_All_Years.png"),
         width = 18, height = 10)
}


# Function to plot Fig 2 - sections of data 
section_plot <- function(DATA = plot_tile_flow_data, SITE, YEAR, 
                         GREEN_LINE = -0.5, RAIN_MULT = 1,
                         START = "0101", END = "1231") {
  DATA %>%
    filter(siteid == SITE) %>% 
    filter(year == YEAR) %>%
    ggplot(aes(x = DATE, group = plotid)) +
    geom_col(aes(y = RAIN * RAIN_MULT), fill = "skyblue", alpha = 0.45) +
    geom_line(aes(y = flow_pred, colour = plotid), size = 0.8) +
    geom_point(aes(y = flow_pred, colour = plotid), size = 1.2) +
    geom_point(aes(y = pred, colour = plotid), size = 2.5) +
    geom_point(aes(y = pred), colour = "white", size = 1.2, alpha = 0.9) +
    geom_point(data = . %>% filter(comments == "predicted"), 
               aes(y = GREEN_LINE), shape = 15, col = "green", size = 1.1) +
    facet_grid(plotid ~ .) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", name = NULL) +
    scale_y_continuous(name = "Tile Flow (mm)") +
    scale_colour_manual(values = c("#D67236", "#5B1A18", "#5F5647", "#F4BA7B")) +
    theme_bw() +
    labs(title = paste(SITE, YEAR)) +
    theme(text = element_text(size = 18),
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)) +
    coord_cartesian(xlim = ymd(paste0(2012, START), paste0(2012, END)))
  
  ggsave(filename = paste0("Output/Figs/model-predictions/", SITE, "/",
                           SITE, "_Fig2_Tile_Flow_", YEAR, START, "-", YEAR, END, ".png"),
         width = 18, height = 10)
}



# Function to plot Fig 3 - annual tile flow
annual_flow_plot <- function(DATA = plot_tile_flow_data, SITE,
                             GROUP = "dwm") {
  plot_tile_flow_data %>%
    filter(siteid == SITE) %>% 
    mutate(year = as.factor(year),
           flow = ifelse(is.na(flow), 0, flow),
           flow_pred = ifelse(is.na(flow_pred), 0, flow_pred)) %>%
    group_by(siteid, plotid, dwm, year) %>%
    summarise(flow_pred = sum(flow_pred),
              flow = sum(flow, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(dwm = factor(dwm, levels = c("FD", "CD"))) %>%
    ggplot(aes(x = year, fill = dwm, group = get(GROUP))) +
    geom_col(aes(y = flow_pred), position = position_dodge(width = 0.9), width = 0.85, alpha = 0.5) +
    geom_col(aes(y = flow), position = position_dodge(width = 0.9), width = 0.85) +
    
    {if(GROUP == "plotid") geom_text(aes(label = plotid, y = I(-10)), size = 2, 
                              position = position_dodge(width = 0.9))} +
    
    scale_fill_manual(values = c("skyblue2", "steelblue4")) +
    labs(x = NULL, y = "Annual Tile Flow (mm)", title = SITE) +
    theme_bw() +
    theme(text = element_text(size = 12), 
          plot.title = element_text(size = 16, hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
  ggsave(filename = paste0("Output/Figs/model-predictions/", SITE, "/",
                           SITE, "_Fig3_Annual_Tile_Flow.png"),
         width = 9, height = 5)
}



# Function to save data for each site
filled_flow_data <- function(SITE) {
  tile_flow_prediction %>%
    filter(siteid == SITE) %>%
    select(siteid, plotid, dwm, date, rain_mm = rain, flow_mm = flow_pred, comments) %>%
    write_csv(path = paste0("Output/Figs/model-predictions/", SITE, "/",
                            SITE, "_Tile_Flow_Filled.csv"))
}



# AUGLA -------------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2008
section_plot(SITE = "AUGLA", YEAR = 2008, RAIN_MULT = 5,
             END = "0531")

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "AUGLA")

# Save data
filled_flow_data(SITE = "AUGLA")



# CRAWF -------------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2010
section_plot(SITE = "CRAWF", YEAR = 2010, RAIN_MULT = 1,
             START = "0601", END = "0831") 

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "CRAWF")

# Save data
filled_flow_data(SITE = "CRAWF")



# DEFI_M ------------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2008
section_plot(SITE = "DEFI_M", YEAR = 2008, RAIN_MULT = 1,
             START = "0101", END = "0131") 
# predictions in 2009
section_plot(SITE = "DEFI_M", YEAR = 2009, RAIN_MULT = 1,
             START = "0701", END = "0831") 
section_plot(SITE = "DEFI_M", YEAR = 2009, RAIN_MULT = 1,
             START = "1001") 
# predictions in 2011
section_plot(SITE = "DEFI_M", YEAR = 2011, RAIN_MULT = 1,
             START = "0430", END = "0615")

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "DEFI_M")

# Save data
filled_flow_data(SITE = "DEFI_M")



# HARDIN ------------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2008
section_plot(SITE = "HARDIN", YEAR = 2008, RAIN_MULT = 1,
             START = "1001") 
# predictions in 2009
section_plot(SITE = "HARDIN", YEAR = 2009, RAIN_MULT = 1,
             START = "0101", END = "0430") 
section_plot(SITE = "HARDIN", YEAR = 2009, RAIN_MULT = 1,
             START = "0601", END = "0810") 
section_plot(SITE = "HARDIN", YEAR = 2009, RAIN_MULT = 1,
             START = "1001") 
# predictions in 2010
section_plot(SITE = "HARDIN", YEAR = 2010, RAIN_MULT = 1,
             START = "1001", END = "1215")

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "HARDIN")

# Save data
filled_flow_data(SITE = "HARDIN")



# HARDIN_NW ---------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2008
section_plot(SITE = "HARDIN_NW", YEAR = 2008, RAIN_MULT = 1,
             END = "0401") 

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "HARDIN_NW")

# Save data
filled_flow_data(SITE = "HARDIN_NW")



# HENRY -------------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2008
section_plot(SITE = "HENRY", YEAR = 2008, RAIN_MULT = 1,
             END = "0530") 

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "HENRY")

# Save data
filled_flow_data(SITE = "HENRY")



# STJOHNS -----------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2010
section_plot(SITE = "STJOHNS", YEAR = 2010, RAIN_MULT = 1,
             START = "0510", END = "0730")
# predictions in 2011
section_plot(SITE = "STJOHNS", YEAR = 2011, RAIN_MULT = 1,
             END = "0225")
section_plot(SITE = "STJOHNS", YEAR = 2011, RAIN_MULT = 1,
             START = "0710", END = "0830")

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "STJOHNS")

# Save data
filled_flow_data(SITE = "STJOHNS")



# DPAC --------------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2016
section_plot(SITE = "DPAC", YEAR = 2016, RAIN_MULT = 1,
             START = "1115") 
# predictions in 2017
section_plot(SITE = "DPAC", YEAR = 2017, RAIN_MULT = 1,
             END = "0315") 
section_plot(SITE = "DPAC", YEAR = 2017, RAIN_MULT = 1,
             START = "0315", END = "0515") 
section_plot(SITE = "DPAC", YEAR = 2017, RAIN_MULT = 1,
             START = "0515", END = "0805") 
section_plot(SITE = "DPAC", YEAR = 2017, RAIN_MULT = 1,
             START = "1015") 

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "DPAC", GROUP = "plotid")

# Save data
filled_flow_data(SITE = "DPAC")



# HICKS_B -----------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2012
section_plot(SITE = "HICKS_B", YEAR = 2012, RAIN_MULT = 1,
             START = "0315", END = "0615") 
section_plot(SITE = "HICKS_B", YEAR = 2012, RAIN_MULT = 1,
             START = "0801", END = "1025") 
# predictions in 2014
section_plot(SITE = "HICKS_B", YEAR = 2014, RAIN_MULT = 1,
             START = "0901", END = "1201") 
# predictions in 2015
section_plot(SITE = "HICKS_B", YEAR = 2015, RAIN_MULT = 1,
             START = "0601", END = "0915") 
# predictions in 2017
section_plot(SITE = "HICKS_B", YEAR = 2017, RAIN_MULT = 1,
             START = "1101") 

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "HICKS_B", GROUP = "plotid")

# Save data
filled_flow_data(SITE = "HICKS_B")



# SERF_IA -----------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2007
section_plot(SITE = "SERF_IA", YEAR = 2007, RAIN_MULT = 1,
             START = "0305", END = "0515") 
# predictions in 2008
section_plot(SITE = "SERF_IA", YEAR = 2008, RAIN_MULT = 1,
             START = "0305", END = "0425") 
# predictions in 2009
section_plot(SITE = "SERF_IA", YEAR = 2009, RAIN_MULT = 1,
             START = "0301", END = "0425") 
# predictions in 2011
section_plot(SITE = "SERF_IA", YEAR = 2011, RAIN_MULT = 1,
             START = "0801") 
# predictions in 2012
section_plot(SITE = "SERF_IA", YEAR = 2012, RAIN_MULT = 1,
             END = "0410") 
section_plot(SITE = "SERF_IA", YEAR = 2012, RAIN_MULT = 1,
             START = "1110") 
# predictions in 2014
section_plot(SITE = "SERF_IA", YEAR = 2014, RAIN_MULT = 1,
             START = "0305", END = "0425") 
section_plot(SITE = "SERF_IA", YEAR = 2014, RAIN_MULT = 1,
             START = "1101") 
# predictions in 2015
section_plot(SITE = "SERF_IA", YEAR = 2015, RAIN_MULT = 1,
             START = "0301", END = "0425") 
# predictions in 2016
section_plot(SITE = "SERF_IA", YEAR = 2016, RAIN_MULT = 1,
             START = "1110")
# predictions in 2017
section_plot(SITE = "SERF_IA", YEAR = 2017, RAIN_MULT = 1,
             END = "0215") 
section_plot(SITE = "SERF_IA", YEAR = 2017, RAIN_MULT = 1,
             START = "0305", END = "0515") 
section_plot(SITE = "SERF_IA", YEAR = 2017, RAIN_MULT = 1,
             START = "0701", END = "0905") 

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "SERF_IA", GROUP = "plotid")

# Save data
filled_flow_data(SITE = "SERF_IA")



# SERF_SD -----------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2015
section_plot(SITE = "SERF_SD", YEAR = 2015, RAIN_MULT = 1,
             START = "0315", END = "0601") 
section_plot(SITE = "SERF_SD", YEAR = 2015, RAIN_MULT = 1,
             START = "1001") 
# predictions in 2016
section_plot(SITE = "SERF_SD", YEAR = 2016, RAIN_MULT = 1,
             START = "0315", END = "0601")
section_plot(SITE = "SERF_SD", YEAR = 2016, RAIN_MULT = 1,
             START = "1001")

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "SERF_SD", GROUP = "plotid")

# Save data
filled_flow_data(SITE = "SERF_SD")



# TIDE --------------------------------------------------------------------
# Fig 2 Tile Flow
# predictions in 2007
section_plot(SITE = "TIDE", YEAR = 2007, RAIN_MULT = 1,
             START = "0615", END = "1015") 
# predictions in 2009
section_plot(SITE = "TIDE", YEAR = 2009, RAIN_MULT = 1,
             START = "0315", END = "0715")

# Fig 3 Annual Tile Flow by Treatment
annual_flow_plot(SITE = "TIDE", GROUP = "plotid")

# Save data
filled_flow_data(SITE = "TIDE")


