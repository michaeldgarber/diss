#filename:2_0_wrangle_bmap_merge_osm_edge

#12/5/21 I decided to split this off from the other 2_wrangle_basemap code.
#Note that all of this code used to be in 2_wrangle_basemap before 12/5/21
#The reason for splitting it off is that, in theory, 
#it should not need to be run very often at all, only
#if the actual geometry of either the OSM data or the basemap changes, 
#which should be rare if ever again

#I also decided, because it's easier to keep track, I'm not going to include
#most of the covariates in this code. Just the variables necessary to complete the merges.

#-----0. Load packages----------------------####
library(tidyverse)
library(sf)
library(mapview) #loads leeaflet.
library(readxl)
library(RColorBrewer) 
library(viridisLite)
library(viridis)
library(here)


#-----COMMENT OUT INITIAL IMPORT------#########
# ####------------1 Edges Basemap-----------------------------------------------------#####
# ####-----------1.1. Import basemap---------------------####
# # #Directory for edges basemap

# #Note updated project-based directory directory 12/19/21
# setwd(here("data-input", "New Strava purchase - 2018", "Data", "20161001_20180630"))
# 
# #Load the shapefile
# basemap2018 = rgdal::readOGR(dsn = "Edges", #relative path representing folder right above the shapefile
#                                  layer = "emory_atlanta_osm_20180713")

#note, the basemap is the same for both of the two data deliveries.

# #convert it to sf and save it.
# edge_bmap_20180630_sf  = basemap2018 %>%
#   st_as_sf("sf") %>%
#   st_transform(4326) %>%
#   mutate(
#     edge_id = as.character(ID),
#     osm_id_strava = OSM_ID, #rename the osm_id in strava to clarify its source
#     osm_name_strava = OSM_NAME
#     ) %>%
#   rename(
#     #apparently these are used to determine whether a street is more of an east-west 
#street or a north-south street
#     #so keep.
#     x_coord_1 = X1,
#     x_coord_2 = X2,
#     y_coord_1 = Y1,
#     y_coord_2 = Y2
#   ) %>%
#     mutate(
#       #a few others that were used in aim 2 that I'd prefer to define here.
#       #adding these 5/29/20
#       #is the street more of an east west street or more of a north south street?
#       #they are dependent on these weird coord vars so just add here.
#       x_abs_diff = abs(x_coord_1-x_coord_2),
#       y_abs_diff = abs(y_coord_1-y_coord_2),
#       #north-south street if y_abs is greater than x_abs (rather than an east west street)
#       north_south = case_when(
#         y_abs_diff > x_abs_diff ~ 1,
#         TRUE ~ 0
#     )) %>%
# 
#   #select variables before they get merged in with the osm basemap
#   ##drop to avoid ambiguity
#   dplyr::select(-ID, -OSM_ID, -OSM_NAME,
#                 - OSM_META, -OSM_SOURCE, -OSM_TARGET, -CLAZZ, -FLAGS, -SOURCE,
#                 -TARGET, -KM, -KMH, -COST,- REVERSE_CO
#                 ) %>%
#   dplyr::select(edge_id, osm_id_strava, osm_name_strava, everything())
# 
# # names(edge_bmap_20180630_sf)
# # # #-------save--------------#
# #save in processed data only 12/19/21; I had before saved in Strava 2018. don't like that.
# setwd(here("data-processed"))
# save(edge_bmap_20180630_sf, file = "edge_bmap_20180630_sf.RData")

####-----------1.2. Link basemap with OSM data---------------------####
#this code depends on the code '0_1_wrangle_osm_...'

#among the goals of this process is to exclude interstates from the strava data.
#use the part of the osm that's inside of i-285. there is no strava data basemap outside of 285.


#dim(f_l_all_i285_nogeo) #77,464 rows. so there will be about #12,000 that don't link.
#will be interesting to filter those to see where/what they are.

#-----END COMMENT OUT INITIAL IMPORT------#########
#----------1.2.1. Aspatial join - simply link on OSM ID--------------------#####
#note the new wd 10/9/2020
setwd(here("data-processed"))
#These are created in 1_wrangle_osm
# load(file = "all_h_osm_wrangle_both_nogeo.RData")
# load(file = "all_h_osm_wrangle_both_geo.RData")
load(file = "lookup_osm_indicator.RData")
load(file = "edge_bmap_20180630_sf.RData") #note this can be loaded from analysis data as of 12/5/21x
names(edge_bmap_20180630_sf)
dim(edge_bmap_20180630_sf) #89,737 rows.
library(tidyverse)
library(sf)

#----initial lookups based on the initial basemap so I can get rid of the variables----#######
#just calling it a look-up since that's really what it is. I have so many other datasets that 
#merge by edge_id
lookup_edge_id_strava_bmap_init_vars_nogeo = edge_bmap_20180630_sf %>% 
  st_set_geometry(NULL)
save(lookup_edge_id_strava_bmap_init_vars_nogeo, file = "lookup_edge_id_strava_bmap_init_vars_nogeo.RData")
names(lookup_edge_id_strava_bmap_init_vars_nogeo)

lookup_edge_id_xy_coord = edge_bmap_20180630_sf %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(edge_id, 
                x_abs_diff, y_abs_diff, north_south, 
                contains("coord"))
names(lookup_edge_id_xy_coord)
save(lookup_edge_id_xy_coord, file = "lookup_edge_id_xy_coord.RData")

lookup_osm_id_strava_osm_name_strava = edge_bmap_20180630_sf %>% 
  st_set_geometry(NULL) %>% 
  distinct(osm_id_strava, osm_name_strava)
nrow(lookup_osm_id_strava_osm_name_strava) #make sense. fewer.
lookup_osm_id_strava_osm_name_strava %>% 
  group_by(osm_id_strava) %>% 
  summarise(n=n()) #one-to-one. osm_id_strava is all you need here.
nrow(edge_bmap_20180630_sf)
save(lookup_osm_id_strava_osm_name_strava, file = "lookup_osm_id_strava_osm_name_strava.RData")

#renaming it to be called aspatial geo (asp_geo) to remember that although
#it's the dataset that arises from the asptial join, it still has geometry.
bmap_edge_join_asp_geo = edge_bmap_20180630_sf %>%
  #12/5/21 for example, here, I don't use any of the OSM variables except for osm_id_osm,
  #so just leave them out and use the  file = "lookup_osm_indicator.RData" instead
  left_join(lookup_osm_indicator, by = c("osm_id_strava" = "osm_id_osm")) %>%
  mutate(
    #indicator variable for whether the strava basemap linked with the OSM basemap I pulled
    join_aspatial = case_when(
      osm_indicator > 0 ~ 1,
      TRUE ~ 0),
    #Oh, I see. This works here because they're actually the same...it's an aspatial join.
    osm_id_osm = osm_id_strava #not sure why I'm doing this but I am
  ) %>% 
  #udpated to have fewer vars 12/5/21
  #remove the x/y coord variables
  dplyr::select(
    edge_id, 
    starts_with("osm_id"),
    starts_with("osm_name"),
#    starts_with("osm_ind"), #drop this as it will link back in.
    starts_with("join") #will only grab join_aspatial here
    ) 

class(bmap_edge_join_asp_geo)
names(bmap_edge_join_asp_geo) #okay, I decided I'm leaving these strava basemap variables
#note I updated wd 12/5/21
setwd(here("data-processed"))
save(bmap_edge_join_asp_geo, file = "bmap_edge_join_asp_geo.RData")

# bmap_edge_join_asp_geo %>% filter(beltline==1) %>% mapview()
table(bmap_edge_join_asp_geo$join_aspatial)

# create an intermediate look-up table so I don't have to 
# run the full bmap_edge_join_asp_geo through the code below
n_distinct(bmap_edge_join_asp_geo$edge_id)
nrow(bmap_edge_join_asp_geo) #you're not repeating edges, so you can create an edge-id-goemetry lookup
lookup_bmap_edge_join_asp_geo = bmap_edge_join_asp_geo %>% 
  dplyr::select(edge_id, geometry)
save(lookup_bmap_edge_join_asp_geo, file = "lookup_bmap_edge_join_asp_geo.RData")

#and then I need a no-geo version which we'll use to link back into the lookup_bmap_edge_join_asp_geo
bmap_edge_join_asp_nogeo = bmap_edge_join_asp_geo %>% 
  st_set_geometry(NULL)
save(bmap_edge_join_asp_nogeo, file = "bmap_edge_join_asp_nogeo.RData")
names(bmap_edge_join_asp_nogeo)

#----------1.2.2. Spatial join-----------------------------------#####
# 12/5/21 note the spatial join takes forever (6 hours+) so see if you can just run it one more time
# Try to use st_simplify() for the buffers before running.
# https://www.r-bloggers.com/2021/03/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
#the ones that didn't join you need to do a spatial join and then rbind them back on.

#note the new wd 10/9/2020
setwd(here("data-processed"))
load(file = "lookup_all_h_osm_wrangle_buff_20ft_geo.RData")
load(file = "lookup_all_h_osm_wrangle_both_geo.RData")
#install.packages("rmapshaper")
library(tidyverse)
library(sf)
library(mapview)
# library(rmapshaper)
# library(concaveman) 
packageVersion("sf")

#Revising 12/5/5/21 to only intersect the look-up table and then link everything back by edge id
#note that lookup_all_h_osm_wrangle_buff_20ft_geo is a look up by osm_id that 
#can be used to link in the rest of the data
#and you can then use the no_geo lookup
names(all_h_osm_wrangle_both_nogeo)
names(lookup_all_h_osm_wrangle_buff_20ft_geo)
# nrow(bmap_edge_join_sp_geo)
st_crs(bmap_edge_join_asp_geo)
st_crs(lookup_all_h_osm_wrangle_buff_20ft_geo)

#------Spatial join prep intermediate steps-------##############
# Simplify the universe of OSM data that should be merged with the basemap that didn't merge aspatially #
#I'm doing this to make the osm data much smaller to speed up the spatial join below.
#It's unfortunate that I have to do this
bmap_edge_didnt_join_asp_buff_20ft = bmap_edge_join_asp_geo %>% #sp for spatial join
  filter(join_aspatial==0) %>% 
  dplyr::select(edge_id, starts_with("join")) %>% 
  #convert to feet
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  #to make the process below even faster, you can consider making this even smaller, to say 20 feet.
  st_buffer(20) %>% #50 feet will make the resulting dataset below smaller.  rather than 100.
  st_transform(4326) %>% 
  st_simplify() %>% 
  st_make_valid() 

names(bmap_edge_didnt_join_asp_buff_20ft)
#save to here.
save(bmap_edge_didnt_join_asp_buff_20ft, file = "bmap_edge_didnt_join_asp_buff_20ft.RData")
#I'd prefer to also st_union but it takes forever.

#mapview(bmap_edge_didnt_join_asp_buff_20ft)
#maybe that's enough. now try to join your OSM data with the not-joining edge basemap

lookup_all_h_osm_wrangle_intersecting_not_joining_bmap = lookup_all_h_osm_wrangle_both_geo %>% 
  #largest=TRUE just keeps one. it does look like largest=true slows it down. ugh.
  st_join(bmap_edge_didnt_join_asp_buff_20ft, left = TRUE, largest = FALSE) %>% 
  filter(join_aspatial==0) %>% 
  group_by(osm_id_osm) %>% #I only care about which intersect...
  #really the purpose is just making a smaller dataset.
  #so largest=FALSE should be fine...
  slice(1) %>% #grab just one in every group
  ungroup() %>% 
  #get back to only osm_id_osm and geometry
  dplyr::select(starts_with("osm_id"), geometry)
#oh wow, this actually worked fairly quickly 12/5/21 1:20 pm. 
#I thought it wouldn't work without the st_union  it seemed to.
#so now I can merge this with the basemap below insteaed of the huge one. hopefully faster.

save(
  lookup_all_h_osm_wrangle_intersecting_not_joining_bmap, 
  file = "lookup_all_h_osm_wrangle_intersecting_not_joining_bmap.RData")

names(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap)

nrow(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap) #100 ft buffer above was 8419
nrow(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap) #50 is just 6193. great. so 2,000 fewer obs
nrow(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap) #20 foot buffer around the edges above takes it down to 4651
#mapview(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap) #okay, fine.
#make a 20-foot buffer.
#here, edge-ids should be able to repeat.
n_distinct(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap$osm_id_osm)
#this is a ridiculous name, but I should save it.

lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft = lookup_all_h_osm_wrangle_intersecting_not_joining_bmap %>% 
  #convert to feet
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_buffer(20) %>% 
  st_transform(4326) %>% 
  st_simplify()  
nrow(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft)
save(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft,
     file = "lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft.RData")

#mapview(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft)

n_distinct(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap$osm_id_osm)
# mapview(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap)
nrow(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap)
#now merge the all_h_osm data with this st_unioned buffer to limit the n obs drastically

# all_h_osm_wrangle_buff_20ft_simplify_geo = all_h_osm_wrangle_buff_20ft_geo %>% 
#   st_simplify()
# object.size(all_h_osm_wrangle_buff_20ft_simplify_geo) #didn't change much. don't take this route.

#test on fewer obs. okay, this is ridiculously slow.
#-------load spatial join intermediate files again here-------------#######
setwd(here("data-processed"))
load(file = "lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft.RData")
load(file = "bmap_edge_join_asp_nogeo.RData")
load(file = "bmap_edge_join_asp_geo.RData")
load(file = "lookup_osm_id_length_osm_remeasure_m.RData")
library(tidyverse)
library(sf)
library(mapview)
nrow(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft)
bmap_edge_join_asp_geo %>%   filter(join_aspatial==0) %>%  nrow()
start_time = Sys.time()
bmap_edge_join_asp_geo_small_test = bmap_edge_join_asp_geo %>% 
  filter(join_aspatial==0) %>%  
  dplyr::select(edge_id, geometry) %>% 
  slice(1:2) %>% 
  #trying st_intersection instead
  #  st_intersection(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft) 
  # st_join(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft, left = TRUE, largest = FALSE)    #try without largest=TRUE
  st_join(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft,
          left = TRUE,
          largest = TRUE ) %>% 
  mutate(     #just an indicator variable
    join_spatial = case_when(
      #this is the open streets map id in the data that I pulled directly 
      #from osm (not strava's version)
      is.na(osm_id_osm)== TRUE ~ 0,
      TRUE ~ 1
    )) 
end_time = Sys.time() 
end_time-start_time
#30 seconds per 2 obs is ridiculous. that's 15 s per obs.
#Okay, I got it down to 10s s per obs which is still way too slow
((15*5495)/60)/60 #so it could take like a full day
((10*5495)/60)/60 #14 hours. too slow.
((0.75*5495)/60)/60 #14 hours. too slow.

#-----very slow code - has not been updated since June 2021--------#################
#I expect this code takes about 15 hours on my Lenovo 16 GB ram
#note no osm name but we do have osm id, which will allow 
#us to link all osm vars later.
names(bmap_edge_join_asp_geo) 
names(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft)
bmap_edge_join_sp_geo = bmap_edge_join_asp_geo %>% #sp for spatial join
  #limit to those that didn't join. this is only 5495 obs. I'm surprised it doesn't go faster.
  filter(join_aspatial==0) %>%  
  #keep only the geometry and the join_aspatial column for faster spatial join...trust me 12/5/21
  dplyr::select(edge_id, geometry) %>% 
  #12/5/21 revising this to only include the geometry to make it hopefully faster.
  #This is taking much longer than expected. I wonder if the largest=TRUE is what's causing it.
  #You could try to omit that, measure, and then sort ascending. 
  #No, that won't work, actually.
  #largest=true is the only way to do it. an alternative would be to sort ascending and take the edge id
  #corresponding to the longest osm_id_osm but that won't necesarilly 
  #return the osm_id with the largest overlap
  #with that edge_id
  #hmm, largest=FALSE is much faster but it doesn't return exactly what we want.
  st_join(lookup_all_h_osm_wrangle_intersecting_not_joining_bmap_buff_20ft, 
          left = TRUE, 
          largest = TRUE) %>% #FALSE is much faster but we have to use TRUE. It may take a full day.
  mutate(     #just an indicator variable
    join_spatial = case_when(
      #this is the open streets map id in the data that I pulled directly 
      #from osm (not strava's version)
      is.na(osm_id_osm)== TRUE ~ 0,
      TRUE ~ 1
    )) 

save(bmap_edge_join_sp_geo, file = "bmap_edge_join_sp_geo.RData")
#10/12/20 reminder - the reason this isn't including things like sykes park, etc.
#is that this is ultimately a left join starting from the strava basemap with data,
# and those OSM features were added later after the Strava basemap was created.


####-------------1.2.3 Join aspatial and spatial join together----------------------------####
#12/5/21 code that you wouldn't repeat but doing here once to remove the OSM vars 
#from the old version of bmap_edge_join because they're not updated.
#Or, rather, including the vars I'd expect it to have once the code finanlly runs
# bmap_edge_join_save_just_in_case = bmap_edge_join_geo
# save(bmap_edge_join_save_just_in_case, file = "bmap_edge_join_save_just_in_case.RData")
# load(file = "bmap_edge_join_save_just_in_case.RData")
# #now overwrite the old version with one with fewer vars.
# bmap_edge_join_geo = bmap_edge_join_save_just_in_case %>%
#   dplyr::select(edge_id, osm_id_osm, osm_id_strava, osm_name_strava, join_aspatial,
#                 join_spatial,   geometry)
# names(bmap_edge_join_geo) #good to go. this is how it will look.


#Note!! The aspatial one you join has to be the one that's EXCLUDING all of the
#osm_ids that weren't matched, of course. otherwise you're counting twice.

#the two that aren't in the aspatial join are 'osm_id_osm' and 'join_spatial'. that makes sense.
#add empty columns into the aspatial join data before stacking the two.

bmap_edge_join_geo = bmap_edge_join_asp_geo %>%
  filter(join_aspatial == 1) %>%  #filter to the aspatial join matches only
  bind_rows(bmap_edge_join_sp_geo) 
  #I decided not to add the other vars. can link in using the ID variables later.

#note the last time this was saved was 6/2/21. And it was saved as bmap_edge_join_geo
setwd(here("data-processed"))
save(bmap_edge_join_geo, file = "bmap_edge_join_geo.RData")
bmap_edge_join_nogeo = bmap_edge_join_geo %>% 
  st_set_geometry(NULL)
save(bmap_edge_join_geo, file = "bmap_edge_join_geo.RData")
#Here, you should be saving a look-up table linking each edge_id with its best osm_id

n_distinct(bmap_edge_join_geo$edge_id)
n_distinct(bmap_edge_join_geo$osm_id_osm) #46114
n_distinct(bmap_edge_join_geo$osm_id_strava) #48568
#bmap_edge_join_geo  %>% filter(beltline==1) %>% mapview() #eastside trail still there. 
# n_distinct(bmap_edge_join_geo$osm_id_osm)
# n_distinct(bmap_edge_join_geo$edge_id)

#what about an osm_id vs osm_id_strava lookup?


#-----lookup edge_id with osm_id-----------##########
#this is really the holy grail that everything above is trying to do.
lookup_edge_id_osm_id_osm = bmap_edge_join_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(edge_id, osm_id_osm)
save(lookup_edge_id_osm_id_osm, file = "lookup_edge_id_osm_id_osm.RData")
nrow(lookup_edge_id_osm_id_osm)
nrow(bmap_edge_join_geo) #good. same
#this is more ipmortant than the strava one below


#also while we're here look up osm_id_osm with osm_id_strava
lookup_osm_id_osm_osm_id_strava = bmap_edge_join_geo %>% 
  distinct(osm_id_osm, osm_id_strava)

n_distinct(lookup_osm_id_osm_osm_id_strava$osm_id_osm)
nrow(lookup_osm_id_osm_osm_id_strava)
save(lookup_osm_id_osm_osm_id_strava, file = "lookup_osm_id_osm_osm_id_strava.RData")


#12/5/21 I'm not linking the rest of the variables yet
#If so, you'd do:
# %>% 
#   #left join the OSM information back in by osm_id. yes, these are unique
#   left_join(all_h_osm_wrangle_both_nogeo, by = "osm_id_osm") %>% 
#   #left join the edge_id information back in by edge_id
#   left_join(bmap_edge_join_asp_nogeo, by = "edge_id") %>% 
#   dplyr::select(
#     edge_id,
#     starts_with("osm_id"),
#     starts_with("osm_name"),
#     osm_indicator,
#     starts_with("join"),
#     everything()
#   )  
