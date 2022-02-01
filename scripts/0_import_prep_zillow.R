# Zillow home-value index by neighborhood----###
# Starting this 11/30/21
# Make a separate script and then connect with the aim1_1_hex_to_prep_for_gsynth script
#Filename: 0_import_prep_zillow

library(sf)
library(tidyverse)
library(rgdal)
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(mapview)
library(here) #update 12/19/21 into new project.
#import zillow neighborhoods

#Final decision 11/30/21 use zip codes, not neighborhoods.
#Note the data are first downloaded from 
#https://www.zillow.com/research/data/
#specifically ZHVI All Homes (SFR, Condno/Co-op) Time Series, Smoothed, Seasonally Adjusted
#At the zip code level

#-------Load zip-code-tabulation area (zcta) geometry data-------######
setwd(here("data-input", "ATL area geo data", "zcta-2017"))
zcta2017 = rgdal::readOGR(
  dsn = "cb_2017_us_zcta510_500k", #relative path representing folder right above the shapefile
  layer = "cb_2017_us_zcta510_500k"
  )

zcta2017_sf = zcta2017 %>% 
  st_as_sf("sf") %>%
  st_transform(4326) %>% #set EPSG code
  dplyr::select(GEOID10, geometry) %>% 
  #to be consistent with zillow data below, call GEOID10 zcta
  rename(zcta = GEOID10)

#and save into processed folder.
setwd(here("data-processed"))
getwd()
save(zcta2017_sf, file = "zcta2017_sf.RData")

#-------Load Zillow monthly home value data by month-----#########
#import zillow zip-code data
setwd(here("data-input", "zillow-data"))
zhvi_zip_by_mo = readr::read_csv("Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")


#grab the study-month lookup from data-processed
setwd(here("data-processed"))
load(file = "lookup_study_month.RData") #created in 3_1_import_strava_numeric
lookup_study_month


#filter to atlanta area and only to the months of interest for our study
zhvi_zip_atl_by_mo = zhvi_zip_by_mo %>% 
  filter(Metro=="Atlanta-Sandy Springs-Roswell") %>% 
  #rename the region-name field to be zcta (zip-code tabulation area)
  #go to lower case for my own style
  rename(
    zcta = RegionName,
    metro_name = Metro,
    city = City) %>% 
  #only select the years you want, the zcta, and the city.
  #Georgia is obvious. other id variable not needed.
  #leave the metro name as a reminder for how we're restricting the data at this stage
  dplyr::select(zcta, metro_name, city, starts_with("2016"), starts_with("2017"), starts_with("2018")) %>% 
  pivot_longer(
    cols = starts_with("201"),
    names_to = "date_zillow",
    values_to = "zhvi" #the actual home value
  ) %>% 
  #make a year and a month variable like you've done elsewhere
  mutate(
    year = lubridate::year(date_zillow),
    month = lubridate::month(date_zillow)
  ) %>% 
  #link study month
  #note drop the date variable. zillow reports the last day of the month, whereas my date fields in this study
  #have been the first day of the month. it doesn't matter. I'm not going to the resolution of the day, so just
  #drop your zillow-based date
  dplyr::select(-date_zillow) %>% 
  left_join(lookup_study_month, by = c("month", "year"))

#I need a lookup for zcta-zhvi-study_month
lookup_zcta_study_month_zhvi = zhvi_zip_atl_by_mo %>% 
  dplyr::select(zcta, study_month, zhvi)

#----Restrict zip-code geometry data to the Atlanta area----######
#first, just get one month of each zip code above
zhvi_zip_atl_1_mo = zhvi_zip_atl_by_mo %>% 
  filter(study_month==1)

zcta2017_atl_sf = zcta2017_sf %>% 
  left_join(zhvi_zip_atl_1_mo, by = "zcta") %>% 
  filter(metro_name == "Atlanta-Sandy Springs-Roswell")
  
zcta2017_atl_sf %>% 
  mapview(zcol = "zhvi")


#-----save for future use------#####
setwd(here("data-processed"))
save(zhvi_zip_atl_by_mo, file = "zhvi_zip_atl_by_mo.RData")
save(zcta2017_atl_sf, file = "zcta2017_atl_sf.RData")
save(lookup_zcta_study_month_zhvi, file = "lookup_zcta_study_month_zhvi.RData")

load("zcta2017_atl_sf.RData")
zcta2017_atl_sf %>% mapview()