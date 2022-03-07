#-------------------------------------------------------------------#
# The purpose of this code is to contextualize the areas            #
# where trails were built in terms of median home value and race    #
#-------------------------------------------------------------------#

#filename: 0_misc_prep_atlanta_census

#Revised 11/30/21
#paring this down and cleaning up. making much simpler.

library(tidyverse)
library(sf)
library(mapview) #loads leeaflet.
library(tidycensus)
library(here)

#-------Gather ACS data from 2015-2019 5-year ACS-----##############

#note that they don't want us to compare 5-year ACS data year over year
#https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data.html
#using 2015-2019 5-year ACS because that roughly puts my years of interest in the middle of the period.
#don't compare over time.
vars_acs_2019 = load_variables(2019, "acs5", cache=TRUE)
library(tidycensus)
options(tigris_use_cache = TRUE)
atl_tracts_acs5_20152019_geo  = get_acs(
  geography = "tract", 
  year=2019, #setting to 2019 as midpoint of study. otherwise, it will take the most recent.
  cache_table = TRUE,
  state = "GA",
  #fulton, douglas, cobb, dekalb, gwinett, henry, clayton, fayette
  #121, 097, 067, 089, 135, 151, 063 , 113
  county =  c("121", "097", "067", "089", "135", "151", "063", "113"), 
  keep_geo_vars = FALSE, 
  output = "wide",
  survey = "acs5", 
  geometry = TRUE, #takes a long time so just do this once
  variables = c(
    
    #12/11/21 I'm renaming these to be shorter. faster coding mike...let's go
    #race (dichotomizing as white or else)
    #add an underscore to all of these, because the package automatically adds E and M
    #to the end of each var
    pop_tot_ = "B01003_001",
    race_tot_ = "B02001_001",
    race_w_ = "B02001_002",
    race_b_ = "B02001_003",
    
    #median home value (use h_val to shorten)
    h_val_med_ = "B25077_001",
    h_val_med_tot_ = "B25075_001", #don't need total if just getting median...
    
    #median household income (take the continuous value instead of categories)
    #12/11/21 changing from hh_inc_ to hh_inc. shorten words...
    hh_inc_med_ = "B19013_001",
    hh_inc_med_tot_ = "B19019_001",
    
    #median age
    age_med_ = "B01002_001",
    
    #travel time to work (#aggregate travel time to work)
  #  trav_time_to_work_aggr_ = "B08013_001"), #omit
  

    #   means of transportation to work (trans to w)
    t_to_w_tot_ = "B08301_001",
    t_to_w_car_ = "B08301_002",
    t_to_w_public_ = "B08301_010",
    t_to_w_bike_ = "B08301_018",
    t_to_w_walk_ = "B08301_019",
    t_to_w_other_ = "B08301_020"
    
  )
)
                    
              
#mapview(atl_tracts_acs5_20152019_geo, zcol = "race_bE")

#save to the analysis data folder rather than the other one to avoid having to change your working directory
setwd(here("data-processed"))
save(atl_tracts_acs5_20152019_geo, file = "atl_tracts_acs5_20152019_geo.RData")

#-----------restrict to the study area------------------########
#load the aim 1 basemap (larger than 5.5 miles)
setwd(here("data-processed"))
load(file = "bmap_unary_union.RData")
load(file = "atl_tracts_acs5_20152019_geo.RData")
names(atl_tracts_acs5_20152019_geo)
#leave the both in there for backwards compatibility. I don't know why we called it both.
#this was called atl_tracts_both. I'm calling it _a1 to denote that it's used for aim 1

#Splitting up the spatial intersection step for faster bootstrapping. 
#getting an error with rename_with so just going to grab the geo and then re-link it.
#ugh, this is def a bug.
atl_tracts_a1_int_bmap_geo = atl_tracts_acs5_20152019_geo %>%
  st_transform(4326) %>%
  st_intersection(bmap_unary_union) %>% 
  mutate( 
    #calculate sf stuff first.
    tract_area_m2 = as.numeric(st_area(geometry)),
    tract_area_mi2 = tract_area_m2*0.000000386102,
    tract_id = row_number()) %>%    #make a tract_id variable for linking. don't trust GEOID
  #remove the geometry before using the rename_with function and then link the geo back in
  #because rename_with doesn't seem to work with sf objects: https://github.com/r-spatial/sf/issues/1472
  #rename E to  and M to _moe for margin of error
  #update 12/10/21 I do need to keep the margins of error.
  #use rename_with to do this dynamically. see
  # https://dplyr.tidyverse.org/reference/rename.html
  #
  #not sure what the ~ means or why we need .x but asi es
  dplyr::rename_with( ~gsub("_E", "", .x, fixed = TRUE)) %>% 
  dplyr::rename_with(~gsub("_M", "_moe", .x, fixed = TRUE))  %>% 
  mutate(
      #can also calculate SD here rather than in your function
    #------calculate standard deviation based on MOE-------######    
    pop_tot_sd = (pop_tot_moe/1.645), 
    race_tot_sd = (race_tot_moe/1.645), 
    race_b_sd = (race_b_moe/1.645), 
    race_w_sd = (race_w_moe/1.645),
    
    #variance is same as white, as simply adding (subtracting) a constant
    race_nw_sd = (race_w_moe/1.645),
    
    #only for bike to work among the transport ones and the total
    t_to_w_bike_sd = (t_to_w_bike_moe/1.645),
    t_to_w_tot_sd = (t_to_w_tot_moe/1.645),
    h_val_med_sd = (h_val_med_moe/1.645),
    hh_inc_med_sd = (hh_inc_med_moe/1.645),#wtf? says it's not found.makes no sense.
    age_med_sd = (age_med_moe/1.645)  
    )

nrow(atl_tracts_a1_int_bmap_geo)

names(atl_tracts_a1_int_bmap_geo)
save(atl_tracts_a1_int_bmap_geo, file = "atl_tracts_a1_int_bmap_geo.RData")
#save an aspatial version for speed in the bootstap
atl_tracts_a1_int_bmap_nogeo = atl_tracts_a1_int_bmap_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(atl_tracts_a1_int_bmap_nogeo, file = "atl_tracts_a1_int_bmap_nogeo.RData")

atl_tracts_a1_geo = atl_tracts_a1_int_bmap_geo %>% 
  mutate(
    #see https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20180418_MOE.pdf
    #for description of the MOE
    #It is 1.645 x the SE or 1.654 * sqrt(variance)
    #90 percent confidence level
    
    #Note for transforming estimates (products, sums, ratios, etc.)
    #check out these resources. There are some covariance tables in case can't assume independence:
    #https://www.census.gov/programs-surveys/acs/data/variance-tables.html
    #https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20180418_MOE.pdf
    #12/10/21 not a priority but should eventually do.
        #actually think can assume independence for many of them.
        #https://en.wikipedia.org/wiki/Variance#Product_of_independent_variables
    
    #Decision: actually just bootstrap. Easier to avoid independence assumptions this way.
    #So all we need is the standard deviation of each var which is 
    #simply (1/1.645)* the MOE as reported from ACS

    #prop for proportion
    race_w_prop = race_w/race_tot,  
    race_b_prop = race_b/race_tot,
    race_nw_prop = 1-race_w_prop, #nw for nonwhite (i.e, 1-white)
    race_o_prop = 1-race_b_prop - race_w_prop,    #I'd like an other category (black, white, other)
    t_to_w_bike_prop = t_to_w_bike/t_to_w_tot, 

    pop_dens_mi2 = pop_tot/tract_area_mi2 
    ) %>% 
  #so I can see the vars easier, print them in this order
  dplyr::select(
    tract_id, starts_with("pop_tot"), starts_with("pop_dens"),
    starts_with("race_tot"), starts_with("race_w"), starts_with("race_b"),starts_with("race_nw_"), starts_with("race_o_"),
    starts_with("age_me"),
    starts_with("hh_inc"),
    starts_with("h_val"), starts_with("t_to_w_tot"), starts_with("t_to_w_b"), starts_with("tract_ar"),
    everything(), -NAME, -GEOID
  )

names(atl_tracts_a1_geo)

setwd(here("data-processed"))
save(atl_tracts_a1_geo, file = "atl_tracts_a1_geo.RData")

#----------------------checks-------------------------------------------#
# options(scipen = 100)
 # atl_tracts_a1_geo %>%  dplyr::select(contains("bike")) %>% View()
 # atl_tracts_a1_geo %>%  dplyr::select(contains("t_to_w")) %>% View()
# atl_tracts_a1_geo %>%  dplyr::select(contains("race")) %>% View()
