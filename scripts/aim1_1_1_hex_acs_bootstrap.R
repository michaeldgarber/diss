
#filename: aim1_1_1_hex_acs_bootstrap

#This script bootstraps your atlanta ACS variables to create confidence intervals around them for aim 1
#Date began (and finished :) ) 12/11/21

#load the truncated normal so we can make sure there are no negative numbers
#install.packages("truncnorm")
library(truncnorm)
library(tidyverse)
library(sf)
library(here)

#Load files. You have to initially vary the distribution around the ACS vars at the census tract
#level, and then you can link each iteration to your hexagon geometry and them sum, etc.

#For speed, everything can be aspatial. You can just use look-up tables.
setwd(here("data-processed"))
load(file = "atl_tracts_a1_geo.RData")
load(file = "atl_tracts_a1_int_bmap_nogeo.RData")
load(file = "lookup_hex_tract_area_nogeo.RData")
load(file = "lookup_hex_geo.RData")
load("lookup_hex_area_mi2_nogeo.RData")
names(atl_tracts_a1_geo)


#--------1. Define the function---------------####
#-------1.1. Wrangle the ACS data at the tract level------######
bootstrap_hex_tracts_acs <-function(s_id_val){
    
  atl_tracts_a1_s = atl_tracts_a1_int_bmap_nogeo %>% 
    mutate(
      #use the normal distribution (rnorm) because they give us a MOE which
      #can be converted into standard deviation
      #Note 12/11/21, I initially renamed these to indicate that they were sampled, 
      #as in pop_tot_s,
      #but I think for simplicity, I don't need to do that. 
      #I will know that they're sampled based
      #on the fact that they're in this _s dataset.
      
      pop_tot = truncnorm::rtruncnorm(n=n(), mean=pop_tot, sd=pop_tot_sd, a=0),
      pop_dens_mi2 = pop_tot/tract_area_mi2,
      
      race_tot = truncnorm::rtruncnorm(n=n(), mean=race_tot, sd=race_tot_sd, a=0,),
      #the upper bound of each of these is the total for race for that iteration; 
      #otherwise we'll have percents over 1 sometimes
      race_b = truncnorm::rtruncnorm(n=n(), mean=race_b, sd=race_b_sd, a=0, b=race_tot),
      race_w = truncnorm::rtruncnorm(n=n(), mean=race_w, sd=race_w_sd, a=0, b=race_tot),
      
      race_nw = race_tot-race_w,
      
      race_w_prop = race_w/race_tot,  
      race_b_prop = race_b/race_tot,
      race_nw_prop = race_nw/race_tot,
      race_o_prop = 1-race_b_prop - race_w_prop,    #let variance carry through. that's the beauty of bootstrap
      race_o_prop = case_when( #prevent zeros
        race_o_prop <0 ~ 0,
        TRUE ~ race_o_prop),
      t_to_w_tot = truncnorm::rtruncnorm( n=n(), mean=t_to_w_tot, sd=t_to_w_tot_sd, a=0),
      t_to_w_bike = truncnorm::rtruncnorm(n=n(), mean=t_to_w_bike, sd=t_to_w_bike_sd, a=0, b=t_to_w_tot),
      t_to_w_bike_prop = t_to_w_bike/t_to_w_tot, 
  
      age_med= truncnorm::rtruncnorm(n=n(), mean=age_med, sd=age_med_sd, a=0),
      h_val_med= truncnorm::rtruncnorm(n=n(), mean=h_val_med, sd=h_val_med_sd, a=0),
      hh_inc_med= truncnorm::rtruncnorm(n=n(), mean=hh_inc_med, sd=hh_inc_med_sd, a=0)
    ) %>% 
    #k, now just keep the vars you need.
    dplyr::select( -ends_with("moe"),    -ends_with("sd")) %>% 
    dplyr::select(
        tract_id, starts_with("pop_tot"), starts_with("pop_dens"),starts_with("tract_ar"),
        starts_with("race_tot"), starts_with("race_w"), 
        starts_with("race_b"),
        starts_with("race_o"),
        starts_with("race_n"),
        starts_with("t_to_w_tot"), starts_with("t_to_w_b"),
        starts_with("age_me"),
        starts_with("hh_inc"),
        starts_with("h_val")
    )
  
  #------1.2. Link tracts with hex and summarize by hex----#############
  #roll up population by hex because it's needed for the areal weighting at this stage
  #note the _s denotes something that's getting iterated through.
  hex_pop_rollup_nogeo_s = lookup_hex_tract_area_nogeo %>% 
    left_join(atl_tracts_a1_s, by = "tract_id") %>% 
    mutate(
      hex_tract_pop = pop_dens_mi2*hex_tract_area_mi2 #pop density is from the simulated tract-level
    ) %>% 
    group_by(hex_id) %>% 
    summarise(
      hex_pop = sum(hex_tract_pop, na.rm = TRUE)
    ) %>% 
    as_tibble()
  
  hex_acs_wrangle_nogeo_s = lookup_hex_tract_area_nogeo %>% 
    left_join(atl_tracts_a1_s, by = "tract_id") %>% #bring in simulated tract-level stuff
    left_join(hex_pop_rollup_nogeo_s, by = "hex_id") %>% #the simulated hex-level pop
    #and the remainder of the wrangling exactly as done in the main estimate data
    mutate(
      #have to calculate this again
      hex_tract_pop             = pop_dens_mi2*hex_tract_area_mi2, #pop density is from the simulated tract-level
      tract_pop_prop_hex        = hex_tract_pop /hex_pop, #population weight
      race_w_prop_wt_int        = tract_pop_prop_hex*race_w_prop,
      race_nw_prop_wt_int       = tract_pop_prop_hex*race_nw_prop,
      race_b_prop_wt_int        = tract_pop_prop_hex*race_b_prop,
      race_o_prop_wt_int        = tract_pop_prop_hex*race_o_prop, #addded 11:58 pm 12/11/21; other
      age_med_wt_int            = tract_pop_prop_hex*age_med,
      hh_inc_med_wt_int         = tract_pop_prop_hex*hh_inc_med,
      h_val_med_wt_int          = tract_pop_prop_hex*h_val_med,  #wt by pop not area
      t_to_w_bike_prop_wt_int   = tract_pop_prop_hex*t_to_w_bike_prop
    ) %>% 
    group_by(hex_id) %>% 
    summarise(
      race_w_prop       = sum(race_w_prop_wt_int, na.rm=TRUE),
      race_nw_prop      = sum(race_nw_prop_wt_int, na.rm=TRUE),
      race_b_prop       = sum(race_b_prop_wt_int, na.rm=TRUE),
      race_o_prop       = sum(race_o_prop_wt_int, na.rm=TRUE),
      age_med           = sum(age_med_wt_int, na.rm=TRUE),
      hh_inc_med        = sum(hh_inc_med_wt_int, na.rm = TRUE),
      h_val_med         = sum(h_val_med_wt_int, na.rm = TRUE),
      t_to_w_bike_prop  = sum(t_to_w_bike_prop_wt_int, na.rm=TRUE) 
    ) %>% 
    ungroup() %>% 
    #recalculate population density as well 
    left_join(hex_pop_rollup_nogeo_s, by = "hex_id") %>% 
    left_join(lookup_hex_area_mi2_nogeo, by = "hex_id") %>% 
    mutate( 
      hex_pop_dens_mi2 = hex_pop/hex_area_mi2,
      s_id = s_id_val
    )

return(hex_acs_wrangle_nogeo_s) 

}


#-----check the function------#######
hex_acs_wrangle_geo_1 = bootstrap_hex_tracts_acs(1) %>%
  left_join(lookup_hex_geo, by = "hex_id") %>%
  st_sf()
library(mapview)
hex_acs_wrangle_geo_1 %>% mapview(zcol = "race_w_prop")
hex_acs_wrangle_geo_1 %>% mapview(zcol = "race_b_prop")
hex_acs_wrangle_geo_1 %>% mapview(zcol = "hh_inc_med")
hex_acs_wrangle_geo_1 %>% mapview(zcol = "hex_pop_dens_mi2")

#------2. Run the function------############
n_boot_reps = 1000
s_id_val_list <- seq(from = 1, to = n_boot_reps, by = 1)

boot_hex_acs_wrangle_nogeo  = s_id_val_list %>% 
  map_dfr(bootstrap_hex_tracts_acs) %>% 
  dplyr::select(s_id, hex_id, everything())
  arrange(hex_id, s_id)

save(boot_hex_acs_wrangle_nogeo, file = "boot_hex_acs_wrangle_nogeo.RData")
names(boot_hex_acs_wrangle_nogeo)
#took about 3 min

boot_hex_acs_wrangle_nogeo

#-----Summarize results------####
#summarize by the buffer-based exposure area

#you could also do the same by th el




