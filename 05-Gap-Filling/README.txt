Author: Giorgi Chighladze
Project: Transforming Drainage
Date: 2018-10-19


This project contains implementation of Step 5 of a tile flow Gap Filling procedure for Transforming Drainage (TD) Project. For details of the procedure see the following materials:
    NEED TO ADD REFERENCES with LINKS

Data used for this procedure should be cleaned of erroneous entries prior to using as an input and aggregated into DAILY data (if needed) as described in the Gap Filling procedure from Step 1 to Step 4.

The results (filled data) are added to the TD Google Drive as an alternative to the original tile flow data. 
    NEED TO ADD NAMING CONVENTION USED IN GOOGLE DRIVE FOR DRAINAGE DATA 



2018-12-03
----------
3-Day Rolling Average Precipitation
The gap filling (model O1_model_3_day.R) uses a 3-day moving average on-site precipitation to predict missing tile flow data. Earlier trials (ADD LINK TO THE FOLDER WITH FIGURES) has shown that 2-day moving average was working better, but it has not been tested here. 

Average Recession Slope
The method assumes using of an arithmetic mean when calculating the average recession slope. However, box-plots of individual slopes (~/05-Fap-Filling/Output/Figs/recession-slopes/) revealed right-skewness of the data. Therefore, the average recession slope for each site-season was calculated using trimmed mean with 10% data removed from the both ends. Predicted tile flow showed relatively steep recessions (falling limb) for some sites, suggesting that using Geometric mean can be more appropriate estimate. 

Prediction Models
Missing tile flow was predicted separately for plots with FD and CD treatment. For FD plots data was grouped seasonally, while for CD plots data were grouped according to the board height in the control box, in order to reflect different flow regimes ("open", "closed" and "middle"). Predictions using board-height grouping did not produce consistent results across all sites (several sites had poor or messy estimates of missing data). Because of this I used seasonal grouping for CD plots, which gave consistent results. Subsequently, I decided to use the same (seasonal) grouping for both CD and FD plots. Even though this approach worked fine, in the future I would try to somehow incorporate board management.    

One additional benefit of dropping board heights based grouping is that there were less sites years with available dwm management information. It should also be mentioned that for last adjustment (of the board) we know the date, but we do not know how long it stayed in that position, causing additional problems and speculations.


2018-12-04
----------
Output for ANOVA
I inserted predictions in the original tile flow data, removed extra data points (such as 2017 data since the last year to include in the analysis was 2016, and pre-experimental period at MUDS2), and sent it to Lori for ANOVA analysis. 


2018-12-05
----------
Output for ANOVA
After the group meeting, the team decided to bring in 2017 data to the analysis. New file was generated for ANOVA analysis which included tile flow data for water year 2017. 


2018-12-06
----------
Output for ANOVA
Generated new weather file that included seasonal and annual precipitation data and corresponding wetness categories for water year 2017 for following sites: DPAC, HICKS_B, SERF_IA, SERF_SD. This calculations are done in different R project (\Projects\TD\Data\Data Requests\Matt Helmers\Control_Box_Outlet_MNGT\Control_Box_Outlet_MNGT.Rproj)


20188-12-10
-----------
Improve Prediction Model
Added threshold of 0.45 mm for rainfall. When on-site precipitation was below the threshold it was assumed that no rain occurred. Therefore no tile flow predictions were made on that date, but recession slope was estimated based on precious day's tile flow. This helped to address the problem of sharp drops when big precipitation in previous days was fallowed by tiny precipitation. Even those we used a 3-day moving average, these tiny precipitations still remained small, hence predicted tile flows were not adequate. Model could not overwrite those small drainage values with recession slope predictions, creating unusual drops (see DEFI_M in Jan 2008).

Improved filtering of fitted values. Before the filter was removing any first predictions if rain was less then the threshold value. Added code helped to resolve the problem of skipping the first fitted data points when rain < rain_limit. For example, a dataset starts with 5 days of missing tile flow data, with rainfall during the first 2 days being 0. While model will predict 0 flow for these 2 days, filtering process will eliminate them, because it only copies those predictions that occur when rain > rain_limit. But we need the first non-NA prediction to be copied in order to make predictions complete. NOTE, the only the first 0 flow needs to be copied, the following tile flow should be predicted using regression slope, which in this case will be 0 mm.

Optimized weights for calculating 3-day moving average precipitation. Now there are more weight on the second day as following: 25-45-30

Reviewing Predictions
The model based on weighted 3-day precipitation and trimmed recession slope were used for plotting. This model shows to predict better.
AUGLA {2008} 
    2008 - tile flow was predicted only at East plot during winter (69 days). Predictions look adequate.
CRAWF {2010}
    2010 - tile flow was predicted at both plots during summer (17 days at each plot). Predictions look good.
DEFI_M {2008-2011}
    2008 - tile flow predictions in Jan (3 days at each plot) looks good.
    2009 - tile flow predicted in both plots during winter, plus one point in summer at West plot (84 days at East and 54 days at West). Predictions looks fine, but slightly underestimated.
    2010 - whole year was predicted for East (365 days), no missing values at West plot. Even though values looks good, PREDICTIONS have to be REMOVED.
    2011 - tile flow was predicted at both plots in May (32 days at each plot). Predictions look adequate.
DPAC {2006, 2016-2017}
    2006 - predictions are made for all four plots in the first half of the year, when there was no study. REMOVE all PREDICTIONS.
    2016 - tile flow was predicted in Dec for all 4 plots and individual days in Jan at SE and NW. Predictions looks adequate.
    2017 - predictions looks very good for all 4 plots. Since at NE and NW the whole year was missing, PREDICTIONS have to be REMOVED.
HARDIN {2008-2010}
    Predictions does not look reasonable 
HARDIN_NW {2008}
    2008 - predicted 6 days in Jan at North and 1 day in Mar at South. Predictions are good.
HENRY {2008, 2010}
    2008 - tile flow predicted for the first 5 months at both plots (137 days at each plot). Predictions look reasonable.
    2010 - tile flow predicted for smaller gaps (missing picks) in the first half of the year and then the whole second half of the year was predicted at both plots (239 days predicted at each plot). Predictions look adequate, although summer predictions looks OVERESTIMATED at West plot.
    NEED TO CHECK PREDICTION MODEL AT WEST FOR summer
HICKS_B {2012, 2014-2017}
    2012 - tile flow mostly predicted at BE. Predictions does not look good. Summer is overestimated.
SERF_IA {2007-2012, 2014-2017}
    2007 - Good predictions in spring (Mar-Apr) at all four plots
    2008 - Good predictions in spring (Mar) at all four plots
    2009 - Good predictions at S5. Underestimating at S2. REMOVE S2 since it was not functioning for the next two year.
    2010 - whole year was predicted for S2. Need to be REMOVED.
    2011 - Predicted second half at all four plots. Predictions seems underestimated for FD plots, 
    NEED TO CHECK PREDICTION MODEL FOR ALL PLOTS IN fall
    2012 - Good predictions
    2014 - Predictions of FD are underestimated or CD are overestimated.
    2015 - Good predictions
    2016 - Similar to 2014
    2017 - Good predictions. Problematic is predicted tile flow in summer at S4. Should be removed!
SERF_SD {2015-2016}
    Add zero-flow for winter months
    2015 - Predictions looks fine. Winter at FD (Plot7) looks underestimated, but it matches with measurements in winter 2016
    2016 - Looks good.
STJOHNS {2009-2011}
    2009 - predictions looks fine, but there are a lot of missing precipitation dates. 
    2010 - Good
    2011 - Only 4 missing values will at WS. There is a gap in summer than need to be filled with zero-flow. In total 17 days are missing in summer out of 92 (~ 18.5 %)



