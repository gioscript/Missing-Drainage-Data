library(googlesheets)
library(tidyverse)
library(readxl)


# Download Google Sheets --------------------------------------------------


# Download site metadata --------------------------------------------------

# save "TD Site Keys" on local drive
gs_download(from = gs_key("1PjB63WtYRyYnasm5mmLUF2-WdHuc_3dA_q8iEVV_C28"),
            to = "Data/TD Site Keys.xlsx",
            overwrite = TRUE)

# read keys of all TD sites
td_sites <- read_excel("Data/TD Site Keys.xlsx",
                       sheet = "SITES", 
                       skip = 1)

# select only CD sites
sites_CD <- td_sites %>%
  filter(DWR_Type == "Controlled Drainage")

  
# Download tile flow data -------------------------------------------------

# get list of all google sheets that ends with "Tile Flow" in its title
list_of_Google_sheets <- gs_ls("Tile Flow")[c(1,3,6)]

# select sheets for download 
list_of_Google_sheets_to_download <-
  list_of_Google_sheets %>%
  select(sheet_title) %>%
  # select sites with CD treatment
  filter(word(sheet_title, -3) %in% sites_CD$Site_ID) %>%
  # remove CD sites that are not part of Gap-Filling Process
  filter(!(word(sheet_title, -3) %in% c("BATH_A", "FAIRM", "WILKIN1", "WILKIN2"))) %>%
  # there are different versions of tile flow sheets based on completeness of data
  # organize them accordingly for each site
  mutate(site = word(sheet_title, start = -3, end = -1),
         type = word(sheet_title, start = 1)) %>%
  mutate(type = ifelse(type == word(site, start = 1), "ORIGIN", type),
         site = word(site, start = 1)) %>%
  spread(key = type, value = sheet_title) %>%
  # KEYs for columns (suffixes of Tile Flow sheets)
    # ORIGIN = orginal tile flow data
    # GV     = Gio's calculated version of data (often based on cleaning & compilation of different versions of orginal data)
    # DAILY  = daily tile flow data that was aggregated by Gio or researcher, 
    #          during this process some gaps in hourly data were filled in using different approaches
    # FILLED = daily tile flow data that was filled using Gap-Filling Method
    # COMPLETE = this data was complete (without gaps from the begining)
  select(site, ORIGIN, GV, DAILY, FILLED, COMPLETE) %>%
  # select sheets corresponding to sites that do not have COMPLETE data
  mutate(DOWNLOAD = ifelse(is.na(DAILY), GV, DAILY),
         DOWNLOAD = ifelse(is.na(DOWNLOAD) & is.na(COMPLETE), ORIGIN, DOWNLOAD)) %>%
  filter(!is.na(DOWNLOAD)) %>%
  pull(DOWNLOAD)                                 # this is same as > .[["DOWNLOAD"]]

# save tile flow files on local drive
for (i in seq_along(list_of_Google_sheets_to_download)) {
  print(paste("downloading", list_of_Google_sheets_to_download[i], "..."))
  gs_download(from = gs_title(list_of_Google_sheets_to_download[i]),
              to = paste0("Data/Tile-Flow/", list_of_Google_sheets_to_download[i], ".xlsx"),
              overwrite = TRUE)
}



