Author: Giorgi Chighladze
Project: Transforming Drainge
Date: 2018-10-19

This folder contains materials related to gap filling procedures of tile flow data for Transforming Drainage (TD) Project. Description of the procedure can be found here:
http://

LIST OF SITES
-------------

1  AUGLA    
2  CRAWF    
3  DEFI_M   
4  DPAC     
5  HARDIN   
6  HARDIN_NW
7  HENRY    
8  HICKS_B  
9  MUDS2    
10 MUDS3_OLD
11 MUDS4    
12 SERF_IA  
13 SERF_SD  
14 STJOHNS  
15 STORY    
16 TIDE     
17 UBWC


DATA PROCESSING
---------------

SERF_IA >>>
    2018-11-12 
        TILE FLOW 2017 

        There are two versions of tile flow data for 2017. R scripts, input and output files, and other related materials used to calculate them can be fond in folder: ~\OneDrive - Iowa State University\Projects\TD\Data\Row Data\SERF_IA\Tile Flow 2017

        Both versions were calculated from the raw data - water depth in the control structure (file: Interpolated_feet_2017.csv)

        The first (main) version that is uploaded into the Google Drive ("SERF_IA Tile Flow") was minimally processed. In particular, water depth (in the control box) was cleaned of erroneous measurements and relatively small gaps were interpolated before calculating tile flow in mm (script: raw_FEET_data_cleaning.R). The data is reported at 15 min interval.

        The scond (gio's) version was additionally processed by predicting missing values using correlation between reps (script: raw_FEET_data_filling.R). This data have not been uploded to Google Drive, but it has been saved as an ouput file on local disk ("SERF_IA_daily_tile_flow_2017_interpolated.csv"). The data is also available in corresponding Rpoj via following R objects: `serf_tile_flow_hourly` (for hourly data) and `serf_tile_flow_hourly` (for daily data) in column "flow_interp".

        The second data should be used to compare with a tile flow predicted through Gap Filling Method.


DPAC >>>
    2018-11-14
        TILE FLOW 2017

        There are two versions of tile flow data for 2017. R scripts, input and output files, and other related materials used to calculate them can be fond in folder: ~\OneDrive - Iowa State University\Projects\TD\Data\Data Review\Data Review 2018

        The first (original) version that is available in the Google Drive ("DPAC Tile Flow") was not processed, hence it contains some erroneous measurements caused by daily battery failures (battery charge was not enough for enitre day - draining fast due to high power consumption by attached equipment). As a result, the original data contains regular gaps, usually during the night time. Furthermore, first readings after the gap are usually false, often manifesting as droped values. The data is reported at hourly interval. 

        The second (gio's) version that is uploaded into the Google Drive ("DAILY CLEAN - DPAC Tile Flow") was processed to handle the erroneous measurements and gaps present in the original data. Rproj "Data Review 2018" contains the code used to clean the data and interpolate the gaps (script: DPAC.R). The processed hourly data is in column "flow_interp" of R object `dpac_flow_2017_interp_GV`. The hourly data was aggregated into daily and saved in file called "DPAC_daily_tile_flow_2017_interpolated.csv". 

        NOTE: 
            - tile flow data was collected only in SW and SE plots in 2017
            - in Apr 6, 2017 there is a spike in tile flow at both plots ("SE" and "SW"), even though no rain is recorded




