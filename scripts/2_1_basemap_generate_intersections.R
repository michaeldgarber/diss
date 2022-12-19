# The purpose of this code is to create intersections.

#Note Strava did provide an intersection file, but it's easier for me to create a new
#datset of intersections based on the basemap I creaed.
#10/18/2020 MDG
# run may of 2021, then again April 2022
#Revised 4/12/22 to include the here package and update file paths and 
#other clean-up to be sure consistent
#with basemap work done for aim 1
  #this code goes quite a bit faster with my Macbook (under a half hour)

#to refresh yourself further, see your diary document under this heading:
#5/29/21 dissertation aim 3 paper re-working intersection crossings

#Here Jul 21, 2022 just making sure everything is up to date.
#And I renamed some of the "most major" variables to be more consistent with their 
#edge-level versions

#Here Sep 2, 2022: Recall how these are generated.
#Some come from named roads, and others come from unnamed roads
#but those that are either trunk, primary, secondary, tertiary, residential, living street
library(tidyverse)
library(sf)
library(mapview)
library(here)

setwd(here("data-processed"))
load("bmap_edge_join_wrangle.RData") #created in 2_wrangle_basemap.R, of course
load("lookup_osm_name_highway_combo_id_edge_id.RData") #confirmed created in 2_wrangle_basemap.R
load("lookup_osm_name_highway_combo_id.RData") #confirmed created in 2_wrangle_basemap.R
load("mp_sf_5halfmi.RData") #created  0_misc_XY_make buffer areas around places.R
load("mp_sf_1mi.RData") #created  0_misc_XY_make buffer areas around places.R
load("mp_sf_halfmi.RData") #created  0_misc_XY_make buffer areas around places.R


bmap_5halfmi_excl_sidewalk = bmap_edge_join_wrangle %>% 
  #for exclusions perhaps
  #excludes the one you removed already for the length duplicate calculation...
  #note, this is not how you would analyze the data, 
  #but IT IS useful for doing these intersection calculations.
  filter(infra_exclude_for_length_except_blvd  !=1) %>% 
  #create a custom inclusion that will leave the 5th and W peachtree intersection
  #otherwise excluding the short ones because they tend to be surrounding intersections, 
  #and they create a mess.
  mutate(
    length_short_biltmore  = case_when(
    #these are at the biltmore, and I want this intersection included, but 
    #these edges are shorter than 15 m, so they would otherwise be excluded.
       edge_id == "350859" | edge_id == "350858" ~ 1,
    TRUE ~ 0
        ),
    #I decided to shorten this 10/25 - from 15 m to 5 m
    length_m_over5_or_biltmore = case_when(
      length_short_biltmore == 1 ~ 1,
      length_m > 5 ~ 1,
      TRUE ~ 0
    )
    ) %>% 
  filter(length_m_over5_or_biltmore==1) %>% 
  st_intersection(mp_sf_5halfmi)  %>% 
  st_transform(
  "+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
       +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") 

save(bmap_5halfmi_excl_sidewalk, file = "bmap_5halfmi_excl_sidewalk.RData")
table(bmap_5halfmi_excl_sidewalk$length_short_biltmore)
table(bmap_5halfmi_excl_sidewalk$infra_trail_dirt_or_paved)
table(bmap_5halfmi_excl_sidewalk$infra_trail_dirt_or_paved)
nrow(bmap_5halfmi_excl_sidewalk)
table(bmap_5halfmi_excl_sidewalk$highway_trunk)


# Make a unioned geometry for each highway-name id---------
#The idea here is to create distinct named roads as their own objects.
#need to restrict this to non-trails.
table(bmap_5halfmi_excl_sidewalk$infra_6cat_none)
bmap_5halfmi_excl_sidewalk_notrails = bmap_5halfmi_excl_sidewalk %>% 
  filter(infra_trail_dirt_or_paved==0)

nrow(bmap_5halfmi_excl_sidewalk_notrails)
n_distinct(bmap_5halfmi_excl_sidewalk$osm_name_highway_combo_id)

union_osm_name_group <-function(osm_name_highway_combo_id_val){
  
  unioned_group = bmap_5halfmi_excl_sidewalk_notrails %>% 
    
    filter(osm_name_highway_combo_id ==osm_name_highway_combo_id_val) %>% 
    #making this narrower..decision to make it bigger for certain intersection combos. 
    #It was 25.
    st_buffer(2) %>%  
    st_union() %>% 
    st_as_sf() %>% 
    mutate(osm_name_highway_combo_id = osm_name_highway_combo_id_val)
  
  return(unioned_group)
}

list_combo_id = bmap_5halfmi_excl_sidewalk_notrails %>% 
  filter( is.na(osm_name_highway_combo_id)==FALSE) %>% 
  st_set_geometry(NULL) %>% 
  distinct( osm_name_highway_combo_id) %>% 
  pull()

# hi = union_osm_name_group(
#   osm_name_highway_combo_id_val=list_combo_id[15]
# 
# )
# mapview(hi)
bmap_union_highway_osm_combo = list_combo_id %>% 
  map_dfr(union_osm_name_group) %>% 
  st_transform(4326)
save(bmap_union_highway_osm_combo, file = "bmap_union_highway_osm_combo.RData")
names(bmap_union_highway_osm_combo)
st_crs(bmap_union_highway_osm_combo)
table(bmap_union_highway_osm_combo$osm_name_highway_combo_id)


#an easier way:
bmap_union_highway_osm_combo_alt = bmap_5halfmi_excl_sidewalk %>% 
  filter( is.na(osm_name_highway_combo_id)==FALSE) %>% 
  group_by(osm_name_highway_combo_id) %>% 
  summarise(n_segs=n()) %>% 
  ungroup() %>% 
  st_transform(4326)

nrow(bmap_union_highway_osm_combo_alt)
names(bmap_union_highway_osm_combo_alt)


# Generate unary union of areas that did not appear in the osm-name-highway data------------
#bring in a unary unioned version of all dirt and paved trails that 
#might cross a roadway and 
#DO NOT have a name so they wouldn't be in the above.
table(bmap_edge_join_wrangle$infra_6cat_none_abbrev)
union_trail_p = bmap_5halfmi_excl_sidewalk %>% 
  filter(infra_6cat_none_abbrev == "trail_p") %>% 
  st_buffer(10) %>%   #intentionally smaller buffer with the trails
  st_union() %>% 
  st_as_sf() %>% 
  st_transform(4326) %>% 
  mutate(infra_6cat_none_abbrev = "trail_p") %>% 
  rename(geometry = x)

#mapview(union_trail_p)

union_trail_d = bmap_5halfmi_excl_sidewalk %>% 
  filter(infra_6cat_none_abbrev == "trail_d") %>% 
  st_buffer(10) %>%   
  st_union() %>% 
  st_as_sf() %>% 
  st_transform(4326) %>% 
  mutate(infra_6cat_none_abbrev = "trail_d") %>% 
  rename(geometry = x)

names(union_trail_d)

table(bmap_edge_join_wrangle$highway_9cat)
bmap_no_name_primary = bmap_5halfmi_excl_sidewalk %>% 
  filter(infra_trail_dirt_or_paved ==0) %>%    #excluding trails
  filter(highway_9cat == "primary road") %>% 
  filter(is.na(osm_name_osm)==TRUE) %>% 
  filter(is.na(osm_name_strava)==TRUE)  %>% 
  st_union() %>% 
  #makes sense that we want our primary roadways to be a big buffer in case boulevards
  st_buffer(25)   

#bmap_no_name_primary %>% mapview() #no need to use.
table(bmap_5halfmi_excl_sidewalk$highway_trunk)
bmap_no_name_freedom_pkwy = bmap_5halfmi_excl_sidewalk %>%
  filter(infra_trail_dirt_or_paved ==0) %>%    #excluding trails
  filter(highway_trunk == 1) %>%
  filter(is.na(osm_name_osm)==TRUE) %>%
  filter(is.na(osm_name_strava)==TRUE)  %>%
  st_union() %>%
  #make it a small buffer so it doesn't overlap with the trail..the boulevard shoudl be OK
  st_buffer(1)  

#mapview(bmap_no_name_freedom_pkwy) # just one segment. don't worry about it.

  
#mapview(bmap_no_name_primary) #no need to use.
table(bmap_edge_join_wrangle$highway_9cat)
bmap_no_name_secondary = bmap_5halfmi_excl_sidewalk %>% 
  filter(infra_trail_dirt_or_paved ==0) %>%    #excluding trails
  filter(highway_9cat == "secondary road") %>% 
  filter(is.na(osm_name_osm)==TRUE) %>% 
  filter(is.na(osm_name_strava)==TRUE)  %>% 
  st_union() %>%
  st_buffer(2) %>% #small
  st_as_sf() %>% 
  mutate(highway_9cat = "secondary road") %>% 
  st_transform(4326) %>% 
  rename(geometry = x) %>% 
  mutate(no_name=1)

#mapview(bmap_no_name_secondary)#ugly don't use it.
  
bmap_no_name_tertiary = bmap_5halfmi_excl_sidewalk %>% 
  filter(infra_trail_dirt_or_paved ==0) %>%    #excluding trails
  filter(highway_9cat == "tertiary road") %>% 
  filter(is.na(osm_name_osm)==TRUE) %>% 
  filter(is.na(osm_name_strava)==TRUE)  %>% 
  st_union() %>% 
  st_buffer(5) %>% 
  st_as_sf() %>% 
  mutate(highway_9cat = "tertiary road") %>% 
  st_transform(4326) %>% 
  rename(geometry = x) %>% 
  mutate(no_name=1)

bmap_no_name_tertiary %>% mapview()


bmap_no_name_residential = bmap_5halfmi_excl_sidewalk %>% 
  filter(infra_trail_dirt_or_paved ==0) %>%    #excluding trails. this should avoid some of the weirdness
  filter(highway_9cat == "residential road") %>% 
  filter(is.na(osm_name_osm)==TRUE) %>% 
  filter(is.na(osm_name_strava)==TRUE)  %>% 
  st_union() %>% 
  st_buffer(5) %>% # can be small 
  st_as_sf() %>% 
  mutate(highway_9cat = "residential road") %>% 
  st_transform(4326) %>% 
  rename(geometry = x) %>% 
  mutate(no_name=1)


# bmap_no_name_unclassified_service = bmap_5halfmi_excl_sidewalk %>% 
#   #the ones you care about have a name, so don't worry about those here.
#   filter(infra_trail_dirt_or_paved ==0) %>%    #excluding trails
#   filter(highway_9cat == "unclassified or service") %>% 
#   filter(is.na(osm_name_osm)==TRUE) %>% 
#   filter(is.na(osm_name_strava)==TRUE)  %>% 
#   st_union() %>% 
#   st_buffer(5) %>% 
#   st_as_sf() %>% 
#   mutate(highway_9cat = "unclassified or service") %>% 
#   mutate(
#     highway_service_unclass_include = 1) %>% 
#   st_transform(4326) %>% 
#   rename(geometry = x) %>% 
#   mutate(no_name=1)


bmap_no_name_living_street = bmap_5halfmi_excl_sidewalk %>% 
  filter(infra_trail_dirt_or_paved ==0) %>%    #excluding trails
  filter(highway_9cat == "living street") %>% 
  filter(is.na(osm_name_osm)==TRUE) %>% 
  filter(is.na(osm_name_strava)==TRUE)  %>% 
  st_union() %>% 
  st_buffer(5) %>% 
  st_as_sf() %>% 
  mutate(highway_9cat = "living street") %>% 
  st_transform(4326) %>% 
  rename(geometry = x) %>% 
  mutate(no_name=1)

#mapview(bmap_no_name_living_street) #sure use if you want

# Save all of those
save(union_trail_p , file = "union_trail_p.RData")
save(union_trail_d , file = "union_trail_d.RData")
save(bmap_no_name_primary, file = "bmap_no_name_primary.RData")
save(bmap_no_name_secondary, file = "bmap_no_name_secondary.RData")
save(bmap_no_name_tertiary, file = "bmap_no_name_tertiary.RData")
save(bmap_no_name_residential, file = "bmap_no_name_residential.RData")
#save(bmap_no_name_unclassified_service, file = "bmap_no_name_unclassified_service.RData")
save(bmap_no_name_living_street, file = "bmap_no_name_living_street.RData")
  


# Bindrow those that have OSM-NAME-COMBO ID with those that didn't------

load("lookup_osm_name_infra_combo_id.RData")
load("lookup_infra_6cat_legend_none.RData")
load("lookup_osm_name_highway_9cat_combo_id.RData")
load("bmap_union_highway_osm_combo.RData")


# load(file = "bmap_union_highway_osm_combo.RData")
# load(file = "union_trail_d.RData")
# load(file = "union_trail_p.RData")
# load(file = "bmap_no_name_tertiary.RData")
# load(file = "bmap_no_name_residential.RData")
# load(file = "bmap_no_name_unclassified_service.RData")
# load(file = "bmap_no_name_living_street.RData")



## bmap_union_wrangle-----
#View(lookup_osm_name_highway_combo_id)
bmap_union_wrangle = bmap_union_highway_osm_combo %>% #all roadways with a name
  rename(geometry = x) %>% 
  left_join(lookup_osm_name_highway_9cat_combo_id, by= "osm_name_highway_combo_id") %>% 
  ##this should bring in highway_9cat
  #NOTE! This is not a 1-to-1 lookup. just for reference. it picks the most protected, descending.
  left_join(lookup_osm_name_infra_combo_id, by = "osm_name_highway_combo_id") %>%  
  bind_rows(
    union_trail_d,
    union_trail_p,
    #note trunk, primary, and secondary are intentionally left out.
    bmap_no_name_tertiary,
    bmap_no_name_residential,
#    bmap_no_name_unclassified_service, #avoiding this on purpose for speed 10/25/2020
    bmap_no_name_living_street
    ) %>% 
    #10/24/2020
  
  ## special note re. unclassified service---------------
  #selectively picking a few ONLY. I'm otherwise not including these because 
  #it's too difficult (not feasible)
  #see code related to the few that didn't join.
  
  mutate(
    highway_service_unclass_keep = case_when(
      grepl("Brotherton Transportation", osm_name_osm) ~ 1,
      grepl("Plum", osm_name_osm) ~ 1,
      grepl("York Ave", osm_name_osm) ~ 1,
      grepl("Williams Street", osm_name_osm) ~ 1,
      highway_9cat == "unclassified or service" ~0
    ),
    keep = case_when(
    highway_service_unclass_keep == 1 ~ 1,
    highway_9cat == "unclassified or service" ~ 0,
    TRUE ~ 1),
    
  #I need an intersection grouping for the bootstrapping...so I need to keep track of those which
  #have this osm_name_highway lookup and which do not
 osm_name_highway_combo_id_miss = case_when(
      is.na(osm_name_highway_combo_id)==TRUE ~ 1,
      TRUE ~0 )
 )%>% 
  filter(keep == 1)

save(bmap_union_wrangle, file = "bmap_union_wrangle.RData")
n_distinct(bmap_union_wrangle$osm_name_highway_combo_id)
table(bmap_union_wrangle$osm_name_highway_combo_id_miss)
table(bmap_union_wrangle$osm_name_highway_combo_id)
table(bmap_union_wrangle$highway_9cat)
table(bmap_union_wrangle$highway_service_unclass_keep)
names(bmap_union_wrangle)
osm_name_64 =bmap_union_wrangle %>% filter(osm_name_highway_combo_id == 64) 
osm_name_23 =bmap_union_wrangle %>% filter(osm_name_highway_combo_id == 23) 
osm_name_1 =bmap_union_wrangle %>% filter(osm_name_highway_combo_id == 1) 

#10/22/ 530 pmI fixed the duplicates. the problem was the lookup tables.
 
#if there are any duplicates, get rid of them by sorting
#One thing to note is that even if these are duplicated, 
#the highway type - 9 is constant throughout.
#So that means that the below code where you restrict to highway type should still 
#work equally as well.
#load(file = "bmap_union_wrangle.RData")


## a few checks-----------------

class(bmap_union_wrangle$geometry)
table(bmap_union_wrangle$highway_9cat)
names(bmap_union_wrangle)
nrow(bmap_union_wrangle)
#turn off spherical geometry
sf::sf_use_s2(FALSE) 
#----------generate intersections for each combination of highway type -------###
#These take a while.

union_highway_primary = bmap_union_wrangle %>%  
  filter(highway_9cat=="primary road") %>% 
  st_union() %>% 
  st_as_sf() %>%  
  mutate(highway_9cat = "primary road") 


#freedom parkway
union_highway_trunk = bmap_union_wrangle %>%  
  filter(highway_9cat=="trunk road") %>% 
  st_union() %>% 
  st_as_sf() %>%  
  mutate(highway_9cat = "trunk road") 

#mapview(union_highway_trunk)

union_highway_secondary = bmap_union_wrangle %>% 
  filter(highway_9cat=="secondary road") %>% 
  st_union() %>% 
  st_as_sf() %>%  
  mutate(highway_9cat = "secondary road") 

union_highway_tertiary = bmap_union_wrangle %>% 
  filter(highway_9cat=="tertiary road") %>% 
  st_union() %>% 
  st_as_sf() %>% 
  mutate(highway_9cat = "tertiary road") 

union_highway_residential = bmap_union_wrangle %>% 
  filter(highway_9cat=="residential road") %>% 
  st_union() %>% 
  st_as_sf() %>% 
  mutate(highway_9cat = "residential road")


union_highway_unclassified_service = bmap_union_wrangle %>% 
  filter(highway_9cat=="unclassified or service") %>% 
  st_union() %>% 
  st_as_sf() %>% 
  mutate(highway_9cat = "unclassified or service")

mapview(union_highway_unclassified_service)
union_highway_living_street = bmap_union_wrangle %>% 
  filter(highway_9cat == "living street") %>% 
  st_union() %>% 
  st_as_sf() %>% 
  mutate(highway_9cat = "living street")

save(union_highway_primary, file = "union_highway_primary.RData")
save(union_highway_trunk, file = "union_highway_trunk.RData")
save(union_highway_secondary, file = "union_highway_secondary.RData")
save(union_highway_tertiary, file = "union_highway_tertiary.RData")
save(union_highway_residential, file = "union_highway_residential.RData")
save(union_highway_unclassified_service, file = "union_highway_unclassified_service.RData")
save(union_highway_living_street, file = "union_highway_living_street.RData")

## Load the unioned geometries--------------
#load(file = "union_highway_primary.RData")


# Each roadway against all of the others--------
## Primary vs the others-------------------
primary_v_trunk = union_highway_primary %>%  
  st_intersection(union_highway_trunk) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(40) %>% #pretty big buffer for this combo. not always this big necessarily 
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_highway_trunk=1
  )

#mapview(primary_v_trunk) #perfect size.
primary_v_secondary = union_highway_primary %>%  
  st_intersection(union_highway_secondary) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(40) %>% #pretty big buffer for this combo. not always this big necessarily 
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_highway_secondary=1
    )

# mapview(primary_v_secondary)


primary_v_tertiary = union_highway_primary %>% 
  st_intersection(union_highway_tertiary) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(35) %>%  
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_highway_tertiary=1
  )

#mapview(primary_v_tertiary)

primary_v_residential = union_highway_primary %>%
  st_intersection(union_highway_residential) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(50) %>%  #so that the euclid moreland intersection merges as one
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_highway_residential=1
  )


# mapview(primary_v_residential) #some of these look a little weird but roll with it


primary_v_unclassified_service = union_highway_primary %>% 
  st_intersection(union_highway_unclassified_service) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  #note the smaller buffer area to be more precise since service/unclass is so numerous
  st_buffer(20) %>% 
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_highway_unclassified_service=1
  )

#mapview(primary_v_unclassified_service) #cool. this picked up the west end marta one
  

primary_v_living_street = union_highway_primary %>% 
  st_intersection(union_highway_living_street) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(5) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_highway_living_street=1
  )

#  mapview(union_highway_living_street) 
# mapview(primary_v_living_street)  #no data; cool

primary_v_trail_p = union_highway_primary %>% 
  st_intersection(union_trail_p) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(40) %>%  #there aren't that many so go ahead and make it big if you want
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_trail_p =  1
  )

# mapview(primary_v_trail_p) #this looks fantastic

primary_v_trail_d = union_highway_primary %>% 
  st_intersection(union_trail_d) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(40) %>%  #there aren't that many so go ahead and make it big if you want
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_trail_d =  1
  )

#mapview(primary_v_trail_d)

#save
save(primary_v_trunk, file = "primary_v_trunk.RData")
save(primary_v_secondary, file ="primary_v_secondary.RData")
save(primary_v_tertiary, file = "primary_v_tertiary.RData")
save(primary_v_residential, file = "primary_v_residential.RData")
save(primary_v_unclassified_service, file = "primary_v_unclassified_service.RData")
save(primary_v_trail_p, file = "primary_v_trail_p.RData")
save(primary_v_trail_d, file = "primary_v_trail_d.RData")


## trunk vs the others-------------------------
trunk_v_secondary = union_highway_trunk %>%  
  st_intersection(union_highway_secondary) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(45) %>% #pretty big buffer for this combo. not always this big necessarily 
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_trunk = 1,
    intersects_highway_secondary=1
  )

# mapview(trunk_v_secondary) #one is a bridge

trunk_v_tertiary = union_highway_trunk %>% 
  st_intersection(union_highway_tertiary) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(35) %>%  
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_trunk = 1,
    intersects_highway_tertiary=1
  )


#mapview(trunk_v_tertiary)

trunk_v_residential = union_highway_trunk %>%
  st_intersection(union_highway_residential) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(20) %>%  
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_trunk = 1,
    intersects_highway_residential=1
  )

# mapview(trunk_v_residential)
trunk_v_unclassified_service = union_highway_trunk %>% 
  st_intersection(union_highway_unclassified_service) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  #note the smaller buffer area to be more precise since service/unclass is so 
  #numerous
  st_buffer(10) %>%  
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_trunk = 1,
    intersects_highway_unclassified_service=1
  )

#mapview(trunk_v_unclassified_service) # nod ata
trunk_v_living_street = union_highway_trunk %>% 
  st_intersection(union_highway_living_street) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(5) %>%  #note the smaller buffer area to be more precise 
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_trunk = 1,
    intersects_highway_living_street=1
  )

# mapview(union_highway_living_street) 
# mapview(trunk_v_living_street)  #no data; cool

trunk_v_trail_p = union_highway_trunk %>% 
  st_intersection(union_trail_p) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(15) %>%  #there aren't that many so go ahead and make it big if you want
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_trunk = 1,
    intersects_trail_p =  1
  )

# mapview(trunk_v_trail_p) #good job. the overlap isn't an issue.

trunk_v_trail_d = union_highway_trunk %>% 
  st_intersection(union_trail_d) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(1) %>%  #there aren't that many so go ahead and make it big if you want
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_trunk = 1,
    intersects_trail_d =  1
  )

# mapview(trunk_v_trail_d)

save(trunk_v_secondary, file ="trunk_v_secondary.RData")
save(trunk_v_tertiary, file = "trunk_v_tertiary.RData")
save(trunk_v_residential, file = "trunk_v_residential.RData")
save(trunk_v_unclassified_service, file = "trunk_v_unclassified_service.RData")
save(trunk_v_unclassified_service, file = "trunk_v_unclassified_or_service.RData")
save(trunk_v_trail_p, file = "trunk_v_trail_p.RData")
save(trunk_v_trail_d, file = "trunk_v_trail_d.RData")

## Secondary vs the others----------------------------
secondary_v_tertiary = union_highway_secondary %>% 
  st_intersection(union_highway_tertiary) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(40) %>%  #this looks like a good size.
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_secondary = 1,
    intersects_highway_tertiary=1
  )


#mapview(secondary_v_tertiary) #lots of downtown intersections. good job!

secondary_v_residential = union_highway_secondary %>% 
  st_intersection(union_highway_residential) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(30) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_secondary = 1,
    intersects_highway_residential=1
  )


#mapview(secondary_v_residential) #this fixed my candler park and sidewalk issue!! good job.

table(bmap_union_wrangle$highway_9cat)
secondary_v_unclassified_service = union_highway_secondary %>% 
  st_intersection(union_highway_unclassified_service) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(30) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_secondary = 1,
    intersects_highway_unclassified_service=1
  )

#mapview(secondary_v_unclassified_service)

      #10/19/2020
      #yea that ruins clairmont. clairmont must be double coded with sidewalks or something..
      #n decatur too. these are basically unusable at the moment, too sensitive. 
      #not specific 10/19/2020

      #10/24/2020 I redid it to restrict to those service streets where cases occurred. 
      #I picked up the one
      #at brotherton transportation mall.


secondary_v_living_street = union_highway_secondary %>% 
  st_intersection(union_highway_living_street) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(30) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_secondary = 1,
    intersects_highway_living_street=1
  )


#mapview(secondary_v_living_street)#no data

secondary_v_trail_p = union_highway_secondary %>% 
  st_intersection(union_trail_p) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(15) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_secondary = 1,
    intersects_trail_p =  1
  )

#mapview(secondary_v_trail_p)
#yea, so howard and stone mountain trail is one big intersection... even at at 10 ft buffer. fix that.
#a lot of these are kinda messed up. -ivan allen, n highland, howard, 
#otherwise correct, e.g., at north and the path or north and the beltline (that's a bridge)
#update 10/22/ is it better? no, still not great in that there is some paralleling. oh well. 
# mapview(secondary_v_trail_p) 
 
#update 10/25 - the narrower buffer around the original in the function fixed it.

  #--------test code to disaggregate from st_union to individual polygons--------------#
  # secondary_v_trail_p_by_feature = secondary_v_trail_p %>% 
  #   st_cast("POLYGON") #cool this works as proof of concept
  # 
  # mapview(secondary_v_trail_p_by_feature)
  # nrow(secondary_v_trail_p_by_feature)


secondary_v_trail_d = union_highway_secondary %>% 
  st_intersection(union_trail_d) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(30) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_secondary = 1,
    intersects_trail_d =  1
  )
# mapview(secondary_v_trail_d)  #these look good.

# save
save(secondary_v_tertiary, file = "secondary_v_tertiary.RData")
save(secondary_v_residential, file = "secondary_v_residential.RData")
save(secondary_v_unclassified_service, file = "secondary_v_unclassified_service.RData")
save(secondary_v_trail_p, file = "secondary_v_trail_p.RData")
save(secondary_v_trail_d, file = "secondary_v_trail_d.RData")


## Tertiary vs the others--------#####
#you already have primary v tertiary
#you already have secondary v tertiary
#you already have tertiary v trunk
tertiary_v_residential = union_highway_tertiary %>% 
  st_intersection(union_highway_residential) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(20) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_tertiary = 1,
    intersects_highway_residential=1
  )

#mapview(tertiary_v_residential)
#not terrible but look at clifton and mclendon in candler park. the damn sidewalks. also austin ave. 
#and 16th st
#update 10/25/2020 - no issue with candler park!

tertiary_v_unclassified_service = union_highway_tertiary %>% 
  st_intersection(union_highway_unclassified_service) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(20) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_tertiary = 1,
    intersects_highway_unclassified_service=1
  )

# mapview(tertiary_v_unclassified_service) 
  
  #10/19/2020again. can't use it b/c of the sidewalks. maybe disaggregate.
  #great - this picked up the intersections I wanted.
  #10/25 - data look much better


tertiary_v_trail_p = union_highway_tertiary %>% 
  st_intersection(union_trail_p) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(10) %>%   #intentionally making the buffer size small
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_tertiary = 1,
    intersects_trail_p =1
  )

#look at the westside trail area, and perry road is coded as a trail?
#It's named the silver comet connector..
#  mapview(tertiary_v_trail_p)  #update 10/25/20 - excellent!

tertiary_v_trail_d = union_highway_tertiary %>% 
  st_intersection(union_trail_d) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(30) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_tertiary = 1,
    intersects_trail_d =1
  )

#mapview(tertiary_v_trail_d)  #again look good

save(tertiary_v_residential, file = "tertiary_v_residential.RData")
save(tertiary_v_unclassified_service, file = "tertiary_v_unclassified_service.RData")
save(tertiary_v_trail_p, file = "tertiary_v_trail_p.RData")
save(tertiary_v_trail_d, file = "tertiary_v_trail_d.RData")

## Residential vs the others----------------
#you already have primary v residential
#you already have secondary v residential
#you have tertiary v residential

residential_v_unclassified_service = union_highway_residential %>% 
  st_intersection(union_highway_unclassified_service) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(10) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_residential = 1,
    intersects_highway_unclassified_service =1
  )

mapview(residential_v_unclassified_service) #a mess. don't use.

residential_v_trail_p = union_highway_residential %>% 
  st_intersection(union_trail_p) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(20) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_residential = 1,
    intersects_trail_p =1
  )


#NOTE this is where a crash actually ocurred. euclid / north / path
#look at lena st and wylie st and woodbine
#it may actually make sense to call it the full intersection with wylie and the path..dunno. 
#No, I disagree. It should only be an intersection where it actually intersects, 
#which is at krog and the eastside trail farther down.
#  mapview(residential_v_trail_p) #lena st. 
    #update 10/25 this looks great!

residential_v_trail_d = union_highway_residential %>% 
  st_intersection(union_trail_d) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(10) %>%   
  st_transform(4326) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_residential = 1,
    intersects_trail_d =1
  )


# mapview(residential_v_trail_d)  

# save
save(residential_v_unclassified_service, file = "residential_v_unclassified_service.RData")
save(residential_v_trail_p, file = "residential_v_trail_p.RData")
save(residential_v_trail_d, file = "residential_v_trail_d.RData")


#-----load in case you haven't run them yet----###
# load(file = "primary_v_tertiary.RData")
# load(file = "primary_v_trunk.RData")
# load(file = "primary_v_residential.RData")
# load(file = "primary_v_unclassified_service.RData")

## Bind rows intersections of different types-------
inters_between_types = primary_v_secondary %>% 
  bind_rows(
    
    primary_v_tertiary,
    primary_v_trunk,
    primary_v_residential,
    primary_v_unclassified_service,
    primary_v_trail_d,
    primary_v_trail_p, #fix this?
    #note this intentionally excludes service roads and the living street.
    
    trunk_v_secondary,
    trunk_v_tertiary,
    trunk_v_residential, #No others
    trunk_v_trail_p, #No others
    trunk_v_trail_d,
    
    
    secondary_v_tertiary,
    secondary_v_residential,
    secondary_v_unclassified_service,
    secondary_v_trail_p, #fix this
    secondary_v_trail_d,

    tertiary_v_residential,
    tertiary_v_unclassified_service,
    tertiary_v_trail_p,
    tertiary_v_trail_d,
    
    residential_v_trail_p,
    residential_v_trail_d
  ) %>% 
  mutate(
    intersection_type_number = row_number()
  )

names(inters_between_types)
save(inters_between_types, file = "inters_between_types.RData")
inters_between_types_union = inters_between_types %>%  st_union()
save(inters_between_types_union, file = "inters_between_types_union.RData")

#okay, so you'll decide how to smash those together if you want with a union.
#that's the problem with the full path-lines. they won't union well.
rainbow_22 = rainbow(22)
# mapview(inters_between_types,
#         zcol = "intersection_type_number",
#         lwd=0,
#         col.regions = rainbow_22,
#         color = rainbow_22)


# mapview(inters_between_types_any_primary)
# 
# #mapview of intersections with trails- #
# inters_between_types %>%
#   filter(	highway_9cat_intersects_trail_p==1) %>%
#   mapview(lwd=0,
#           col.regions = "orange",
#           color = "orange")


## Find sym difference of pairwise (done so far) with the basemap------

st_crs(bmap_union_wrangle)
st_crs(inters_between_types_union)

#https://cran.r-project.org/web/packages/sf/vignettes/sf3.html
#Begin with the basemap that excludes service and unclassified roads.
#It's too much data to handle, and we're not using it anyway atm 10/21/2020
#decision 10/25 to NOT do this because you've already limited whta you've included from 
#service/unclass to this point
# bmap_union_wrangle_no_unclass = bmap_union_wrangle %>%  
#   filter(highway_9cat != "unclassified or service")
# save(bmap_union_wrangle_no_unclass, file = "bmap_union_wrangle_no_unclass.RData")


bmap_diff_inters_between_types = bmap_union_wrangle %>% 
#  st_intersection(monpon_sf_1mi) %>% 
  st_difference(inters_between_types_union)

# save
save(bmap_diff_inters_between_types, file = "bmap_diff_inters_between_types.RData")
#mapview(bmap_diff_inters_between_types)


# Intersecting roadways of the same type-----------
#Write a function for each.
#make a version in feet to limit what is done inside the function
bmap_diff_inters_between_types_ft = bmap_diff_inters_between_types %>% 
  st_transform(
  "+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") 
table(bmap_diff_inters_between_types_ft$highway_9cat)
## Trunk with itself---
#I'm here Sep 2, 2022: I'm going to trust previous self that this isn't needed.
# bmap_diff_inters_trunk_ft = bmap_diff_inters_between_types_ft %>% 
#   filter(highway_9cat=="trunk road") 

#bmap_diff_inters_trunk_ft %>% mapview()
## Primary with itself----------
table(bmap_diff_inters_between_types_ft$highway_9cat)
bmap_diff_inters_primary_ft = bmap_diff_inters_between_types_ft %>% 
  filter(highway_9cat=="primary road") 

#mapview(bmap_diff_inters_primary_ft)
intersect_with_itself_primary <-function(osm_name_highway_combo_id_val){
  
  bmap_this_type_minus_this = bmap_diff_inters_primary_ft %>% 
    filter(osm_name_highway_combo_id != osm_name_highway_combo_id_val) %>%
    st_union()
  
  bmap_intersect_with_this = bmap_diff_inters_primary_ft %>% 
    filter(osm_name_highway_combo_id == osm_name_highway_combo_id_val) %>% 
    st_buffer(10) %>%   
    st_intersection(bmap_this_type_minus_this) %>% 
    st_union() %>% 
    st_sf() %>% 
    mutate(
      osm_name_highway_combo_id = osm_name_highway_combo_id_val,
      intersects_highway_primary = 1,
      intersects_self = 1
    )
  
  return(bmap_intersect_with_this)
  
}

id_list_primary = bmap_diff_inters_primary_ft %>%
  st_set_geometry(NULL) %>%
  distinct(osm_name_highway_combo_id) %>% 
  pull()

inters_w_itself_primary = id_list_primary %>%
  map_dfr(intersect_with_itself_primary)  

#mapview(inters_w_itself_primary) 
#this is fine. the intersection between ponce and monroe has already been created.
#so these represent all of the intersections between primary roadways that haven't already 
#been designated as intersections.
#that's great and what we want.
save(inters_w_itself_primary, file = "inters_w_itself_primary.RData")
inters_w_itself_primary_union = inters_w_itself_primary %>% 
  st_union() %>% 
  st_sf() %>% 
  st_buffer(20) %>%   
  st_transform(4326) %>% 
  mutate(
    intersects_highway_primary = 1,
    intersects_self = 1
  )

save(inters_w_itself_primary_union, file = "inters_w_itself_primary_union.RData")
#mapview(inters_w_itself_primary_union)

## Secondary with itself--------
table(bmap_diff_inters_between_types_ft$highway_9cat)
bmap_diff_inters_secondary_ft = bmap_diff_inters_between_types_ft %>%
  filter(highway_9cat=="secondary road") 

#mapview(bmap_diff_inters_secondary_ft)
intersect_with_itself_secondary <-function(osm_name_highway_combo_id_val){
  
  bmap_this_type_minus_this = bmap_diff_inters_secondary_ft %>% 
    filter(osm_name_highway_combo_id != osm_name_highway_combo_id_val) %>%
    st_union()
  
  bmap_intersect_with_this = bmap_diff_inters_secondary_ft %>% 
    filter(osm_name_highway_combo_id == osm_name_highway_combo_id_val) %>% 
    st_buffer(10) %>%   
    st_intersection(bmap_this_type_minus_this) %>% 
    st_union() %>% 
    st_sf() %>% 
    mutate(
      osm_name_highway_combo_id = osm_name_highway_combo_id_val,
      intersects_highway_secondary = 1,
      intersects_self = 1
    )
  
  return(bmap_intersect_with_this)
  
}

id_list_secondary = bmap_diff_inters_secondary_ft %>%
  st_set_geometry(NULL) %>%
  distinct(osm_name_highway_combo_id) %>% 
  pull()

inters_w_itself_secondary = id_list_secondary %>%   
  map_dfr(intersect_with_itself_secondary) 

#mapview(inters_w_itself_secondary) 
save(inters_w_itself_secondary, file = "inters_w_itself_secondary.RData")
inters_w_itself_secondary_union = inters_w_itself_secondary %>% 
  st_union() %>% 
  st_sf() %>% 
  st_buffer(20) %>%   
  st_transform(4326) %>% 
  mutate(
    intersects_highway_secondary = 1,
    intersects_self = 1
    
  )
save(inters_w_itself_secondary_union, file = "inters_w_itself_secondary_union.RData")
# mapview(inters_w_itself_secondary_union)

## Tertiary with itself---------
table(bmap_diff_inters_between_types_ft$highway_9cat)
bmap_diff_inters_tertiary_ft = bmap_diff_inters_between_types_ft %>%  
  filter(highway_9cat=="tertiary road") 

#mapview(bmap_diff_inters_tertiary_ft)
intersect_with_itself_tertiary <-function(osm_name_highway_combo_id_val){
  
  bmap_this_type_minus_this = bmap_diff_inters_tertiary_ft %>% 
    filter(osm_name_highway_combo_id != osm_name_highway_combo_id_val) %>%
    st_union()
  
  bmap_intersect_with_this = bmap_diff_inters_tertiary_ft %>% 
    filter(osm_name_highway_combo_id == osm_name_highway_combo_id_val) %>% 
    st_buffer(10) %>%   
    st_intersection(bmap_this_type_minus_this) %>% 
    st_union() %>% 
    st_sf() %>% 
    mutate(
      osm_name_highway_combo_id = osm_name_highway_combo_id_val,
      intersects_highway_tertiary = 1,
      intersects_self = 1
    )
  
  return(bmap_intersect_with_this)
  
}

id_list_tertiary = bmap_diff_inters_tertiary_ft %>%
  st_set_geometry(NULL) %>%
  distinct(osm_name_highway_combo_id) %>% 
  pull()

inters_w_itself_tertiary = id_list_tertiary %>%  
  map_dfr(intersect_with_itself_tertiary) 

save(inters_w_itself_tertiary, file = "inters_w_itself_tertiary.RData")
inters_w_itself_tertiary_union = inters_w_itself_tertiary %>% 
  st_union() %>% 
  st_sf() %>% 
  st_buffer(20) %>%   
  st_transform(4326) %>% 
  mutate(
    intersects_highway_tertiary = 1,
    intersects_self = 1
  )
save(inters_w_itself_tertiary_union, file = "inters_w_itself_tertiary_union.RData")

## Residential with itself----------
bmap_diff_inters_residential_ft = bmap_diff_inters_between_types_ft %>%  
  filter(highway_9cat=="residential road") 

intersect_with_itself_residential <-function(osm_name_highway_combo_id_val){
  
  bmap_this_type_minus_this = bmap_diff_inters_residential_ft %>% 
    filter(osm_name_highway_combo_id != osm_name_highway_combo_id_val) %>%
    st_union()
  
  bmap_intersect_with_this = bmap_diff_inters_residential_ft %>% 
    filter(osm_name_highway_combo_id == osm_name_highway_combo_id_val) %>% 
    st_intersection(bmap_this_type_minus_this) %>% 
    st_buffer(10) %>%   
    st_union() %>% 
    st_sf() %>% 
    mutate(
      osm_name_highway_combo_id = osm_name_highway_combo_id_val,
      intersects_highway_residential = 1,
      intersects_self = 1
    )
  
  
  return(bmap_intersect_with_this)
  
}

id_list_residential = bmap_diff_inters_residential_ft %>%
  st_set_geometry(NULL) %>%
  distinct(osm_name_highway_combo_id) %>% 
  pull()

id_list_residential

id_list_residential_test = id_list_residential %>%  
  as_tibble() %>% 
  slice(1:100) %>% 
  pull()

residential_start_time = timestamp()
residential_start_time

# I'm going to leave this commented out, as it takes a while.
#It should not be needed to be updated. Residential roads are coded correctly.
#  inters_w_itself_residential = id_list_residential %>% 
#    map_dfr(intersect_with_itself_residential)
# residential_stop_time = timestamp()
# residential_stop_time
# save(inters_w_itself_residential, file = "inters_w_itself_residential.RData")

#do not load this if you run it above. otherwise, do.
load(file = "inters_w_itself_residential.RData") 
inters_w_itself_residential_union = inters_w_itself_residential %>% 
  st_sf() %>% 
  st_buffer(10) %>%   
  st_transform(4326) %>% 
  mutate(
    intersects_highway_residential = 1,
    intersects_self = 1
  )

save(inters_w_itself_residential_union, file = "inters_w_itself_residential_union.RData")
# mapview(inters_w_itself_residential_union)


## Service streets with themselves-------
#no need to do this. it won't be anything of use. you got what you need.


## Combine---------
#load the ones that you don't need to re-run in this code
load("inters_w_itself_primary_union.RData")
load("inters_w_itself_secondary_union.RData")
load("inters_w_itself_tertiary_union.RData")
load("inters_w_itself_residential_union.RData")

inters_w_itself_alltypes_union = inters_w_itself_primary_union %>% 
  bind_rows(
    inters_w_itself_secondary_union,
    inters_w_itself_residential_union, #this one takes a while to run
    inters_w_itself_tertiary_union
    #fine to ignore any self-intersections with freedom parkway
  )

save(inters_w_itself_alltypes_union, file = "inters_w_itself_alltypes_union.RData")
names(inters_w_itself_alltypes_union)

# Select a few intersections that have cases that were missed for one reason or another ------
#the crashes are located at:
#york - unclassified
#brotherton - unclassified (transportation mall)
#Plum Street - service
#5th st and west peachtree -- wtf. that should be something.

# summarize so that you can tear apart-----------------
inters_all_by_unioned_type = inters_between_types %>% 
  bind_rows(inters_w_itself_alltypes_union)

save(inters_all_by_unioned_type, file = "inters_all_by_unioned_type.RData")

#inters_all_by_unioned_type %>% st_set_geometry(NULL) %>% View()

#mapview(inters_all_by_unioned_type)

#different strategy. 
#below you have your major blended file to which you are going to left join stuff
#so here just make unioned geometries that have an intersection with any highway type

## load geometries to make these again-----------
load("inters_all_by_unioned_type.RData")
#inters_all_by_unioned_type %>% st_set_geometry(NULL) %>% View()

inters_any_primary_union = inters_all_by_unioned_type %>% 
  filter(intersects_highway_primary==1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_primary = 1
  ) 


inters_any_trunk_union = inters_all_by_unioned_type %>% 
  filter(intersects_highway_trunk==1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_trunk = 1
  ) 

inters_any_secondary_union = inters_all_by_unioned_type %>% 
  filter(intersects_highway_secondary==1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_secondary = 1
  ) 

inters_any_tertiary_union = inters_all_by_unioned_type %>% 
  filter(intersects_highway_tertiary==1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_tertiary = 1
  ) 

inters_any_residential_union = inters_all_by_unioned_type %>% 
  filter(intersects_highway_residential==1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_residential = 1
  ) 


inters_any_unclassified_service_union = inters_all_by_unioned_type %>% 
  filter(intersects_highway_unclassified_service==1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_highway_unclassified_service = 1
  ) 

#mapview(inters_any_unclassified_service_union)

inters_any_trail_p_union = inters_all_by_unioned_type %>% 
  filter(intersects_trail_p==1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_trail_p = 1
  ) 

inters_any_trail_d_union = inters_all_by_unioned_type %>% 
  filter(intersects_trail_d==1) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(
    intersects_trail_d = 1
  ) 

#mapview(inters_any_trail_p_union)
names(inters_all_by_unioned_type)

# create single smashed versions of each intersection between types---------------
#some of them overlap.


## Working with trails--------------------
#10/25/2020 - having issues - maybe add a buffer or make valid---#
inters_all_union_no_trails = inters_all_by_unioned_type %>% 
  filter(is.na(intersects_trail_p)==TRUE) %>% #excluding intersections with trails
  #  filter(is.na(intersects_trail_d)==TRUE) %>% #forget about dirt trails
  st_transform(
    "+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
   +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
  ) %>%
  st_buffer(1) %>%  
  st_union() %>% 
  st_sf() %>% 
  st_transform(4326) %>% 
#  st_make_valid() %>% 
  st_cast("POLYGON") %>% 
  mutate(
    intersects_trail_p_original =0)

nrow(inters_all_union_no_trails)
#mapview(inters_all_union_no_trails)
inters_all_union_yes_trails = inters_all_by_unioned_type %>% 
  filter( intersects_trail_p ==1 ) %>% #not using dirt trails
  st_union() %>% 
  st_sf() %>% 
  st_cast("POLYGON") %>% 
  mutate(
    intersects_trail_p_true =1)  


#the trails that ARE NOT ALREADY part of an intersection - a full union of them
inters_all_union_no_trails_union = inters_all_union_no_trails %>% 
  st_union() %>% 
  st_sf()

#mapview(inters_all_union_no_trails_union)
  
#the input geometry inters_all_union_no_trails_union is throwing an error
# mapview(inters_all_union_yes_trails)
# mapview(inters_all_union_no_trails_union)
inters_all_union_yes_trails_diff = inters_all_union_yes_trails %>% 
  st_difference(inters_all_union_no_trails_union) %>% 
  st_sf()

#mapview(inters_all_union_yes_trails_diff)
#update 10/25 - that worked
st_crs(inters_all_union_yes_trails)
st_crs(inters_all_union_no_trails_union)

inters_all_union_both_notyet = inters_all_union_no_trails %>% 
  st_join(inters_all_union_yes_trails, left = TRUE, largest = TRUE) %>% 
  st_sf() %>% 
  bind_rows(inters_all_union_yes_trails_diff) %>%
  #bring back in those with trails that 
  #didn't intersect the others
  st_cast("POLYGON") 

## Make the trail island intersections--------------
#I know. Make it a little bigger. If by making it a little bigger, 
#it covers anything, then it goes. 
#If not, that means it's on an island, and you can keep it. 
#If not get rid of it since it's covered by what's already there.

blob_no_trail = inters_all_union_both_notyet %>% 
  filter(is.na(	intersects_trail_p_original)==FALSE) %>% st_union() %>% st_sf() %>% 
  mutate(already_there=1)
#mapview(blob_no_trail)
inters_trail_p_island = inters_all_union_both_notyet %>% 
  filter(intersects_trail_p_true==1 &
           is.na(intersects_trail_p_original)==TRUE
  ) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(2) %>% 
  st_transform(4326) %>% 
  st_join(blob_no_trail, left = TRUE, largest = TRUE) %>% 
  mutate(
    already_there = case_when(
      already_there==1 ~ 1,
      TRUE ~0
    )
  ) %>% 
  filter(already_there==0) %>%  #only keep those that wouldn't otherwise be there! - 
  #like freedom parkway and moreland here
  mutate( 
    intersects_trail_p_island =1
  )
# mapview(inters_trail_p_island)  


#----final geo excluding the extraneous trail intersections-----######
inters_all_union_both_int = inters_all_union_no_trails %>% 
  bind_rows(inters_trail_p_island) %>% 
  st_join(inters_any_primary_union, left=TRUE ) %>% 
  st_join(inters_any_trunk_union, left=TRUE ) %>% 
  st_join(inters_any_secondary_union, left=TRUE) %>%
  st_join(inters_any_tertiary_union, left=TRUE) %>%
  st_join(inters_any_residential_union, left=TRUE) %>%
  st_join(inters_any_unclassified_service_union, left = TRUE) %>% #Note this is just those few with cases
  st_join(inters_any_trail_p_union, left=TRUE) %>% 
  st_join(inters_any_trail_d_union, left=TRUE)

#I have these here because the st_join takes a while, so if you want to add new variable names just use this
#    inters_all_union_both = inters_all_union_both %>% 
inters_all_union_both = inters_all_union_both_int %>% 
  mutate(
    #I deleted intersects_highway_most_major_4cat
    #note 7/26/22
    #I'm not making a 6-cat version, because I didn't make intersections
    #with unclassified / service roads, because they're too numerous and disorganized.
    #actually, 
    #note 2: this is confusing because the other 5cat collapses trunk and primary.
    #I resolve it appropriately in 
    #scripts/8_link intersections_with_basemap_wrangle.R
    #Update July 30, 2022: I'm collapsing trunk and primary, including
    #intersects_highway_unclassified_service, andcalling it
    #5-cat ordered, consistent with the edge version
    intersects_highway_most_major_6cat = case_when(
      intersects_highway_trunk==1 ~ "trunk", #1
      intersects_highway_primary==1 ~ "primary", #2
      intersects_highway_secondary==1 ~ "secondary", #3
      intersects_highway_tertiary==1 ~ "tertiary", #4
      intersects_highway_residential==1 ~ "residential", #5
      intersects_highway_unclassified_service== 1 ~"unclassified or service" #6
    ),
    #July 30, 2022
    #this nomenclature is very long, but it's consistent with the edge version, so go with it
    intersects_highway_most_major_6cat_ordered_with_trunk = case_when(
      intersects_highway_trunk==1 ~ "0-trunk",
      intersects_highway_primary==1 ~ "1-primary",
      intersects_highway_secondary==1 ~ "2-secondary",
      intersects_highway_tertiary==1 ~ "3-tertiary",
      intersects_highway_residential==1 ~ "4-residential",
      intersects_highway_unclassified_service== 1 ~"5-unclassified or service" 
    ),
    
    #July 30, 2022
    #this nomenclature is very long, but it's consistent with the edge version, so go with it
    intersects_highway_most_major_5cat_ordered_collapse_trunk = case_when(
      intersects_highway_trunk==1 ~ "1-trunk or primary",
      intersects_highway_primary==1 ~ "1-trunk or primary",
      intersects_highway_secondary==1 ~ "2-secondary",
      intersects_highway_tertiary==1 ~ "3-tertiary",
      intersects_highway_residential==1 ~ "4-residential",
      intersects_highway_unclassified_service==1 ~ "5-unclassified or service" 
    ),

    #11/5/2020 Note, I was going to create a collapsed variable here, but I created
    #it below instead because this code relies on too much above it.
    #I'm defining these here because the one above has too much preceding it. 
    #Ideally it'd be higher.
    #Update July 26, 2022: on my Mac, speed isn't as much of an issue. run here
      intersects_highway_most_major_2_2_ordered = case_when(
        intersects_highway_most_major_5cat_ordered_collapse_trunk == "1-trunk or primary" ~ "1-prim-sec",
        intersects_highway_most_major_5cat_ordered_collapse_trunk == "2-secondary" ~ "1-prim-sec",
        intersects_highway_most_major_5cat_ordered_collapse_trunk == "3-tertiary" ~ "3-tert-res",
        intersects_highway_most_major_5cat_ordered_collapse_trunk == "4-residential" ~ "3-tert-res",
        TRUE ~ "5-other" #weird that this is 5-..but whatevs. consistent with basemap.
      )  ) %>% 
    #simplify the notation. 
    #Update 5/30/21 But stick with the most naming convention to keep reminding yourself 
    #that it's the most major highway
    #Note Jul 21, 2022 that I don't use the short naming (hwy_x) in the edge basemap.
    #Update July 26, 2022: I'm going to remove these hwy_x variables from
    #here because I want to reserve them for a slightly different construct.
    #In general, throughout the manuscript, I'm focusing less on the "most major" idea,
    #except for off-street paved trails, where it's informative because they're not otherwise
    #on the road. they were called hwy_2_int_most, hwy_4_int_most, and hwy_5_int_most

  #sort by highway type
  arrange(intersects_highway_most_major_6cat_ordered_with_trunk) %>% 
  mutate(
    inters_id = row_number()
  ) 

save(inters_all_union_both, file = "inters_all_union_both.RData")

#are there any duplicate intersection ids? No there aren't
table(inters_all_union_both$intersects_highway_most_major_4cat)
table(inters_all_union_both$intersects_highway_most_major_6cat_ordered_with_trunk)
table(inters_all_union_both$intersects_highway_most_major_5cat_ordered_collapse_trunk)

## Exclude certain intersections with trails---------------
### Exclude bridges over trails-------------
#MDG thought: this should come earlier in the code. 
load(file = "inters_all_union_both.RData")
load(file = "bmap_union_wrangle.RData")
names(inters_all_union_both)
#----Geocode GPS coordinates along the Beltline and other bridges over off-street trails---#
#Update July 25, 2022 adding virginia and beltline
#Also exclude those on the south side of the stone mountain trail on the south side,
#as they're not really intersections unless you actually have to cross a road, like
#Comment: I assume I didn't simply pick the intersections out by
#intersection ID in case my numbering system changed for intersections
#such that it would mess up the linking? A spatial join is more robust.
trail_p_exclude_bridge = data.table::data.table(
  bridge_name = c(
    "beltline_ponce",
    "beltline_north",
    "beltline_ralph_mcgill",
    "beltline_freedom_bridge1",
    "beltline_freedom_bridge2",
    "beltline_freedom_bridge3",
    "beltline_n_highland",
    "beltline_edgewood",
    "beltline_fulton_terrace",
    "beltline_wst_langhorn",
    "beltline_wst_westview",
    "beltline_wst_murphy",
    "beltline_wst_lee",
    "beltline_wst_lawton",
    "beltline_wst_rda_centerline",
    "beltline_wst_lucile", #one of these actually does cross at street level, but one does not.
    "beltline_wst_mlk",
    "freedom_trail_highland",
    "freedom_trail_randolph", #needs to go a bit farther north
    "howard_east_lake",
    #keep going sequentially July 25, 2022
    "beltline_virginia_ne_1", #creating two objects because there are two blobs to remove
    "beltline_virginia_ne_2",
    "freedom_pkwy_sampson_ped_bridge_1",
    "freedom_pkwy_sampson_ped_bridge_2"
    ),
  lat = c(
    33.773348,
    33.771083, 
    33.767487, 
    33.764263,
    33.764633, 
    33.764462,
    33.761350,
    33.754318, 
    33.749132, 
    33.746494, 
    33.747343, 
    33.727715, 
    33.727947, 
    33.732591,
    33.739012, 
    33.742374, 
    33.753537, 
    33.761353, 
    33.760857, #a touch north of the actual trail but that's where the intersection is coded 
    33.765467 ,
    33.781105,
    33.781181,
    33.762342, #sampson bridge
    33.762233

    ),
  lon = c(
    -84.364553,
    -84.363952,
    -84.360782,
    -84.359611,
    -84.359571,
    -84.359582,
    -84.361617,
    -84.365517,
    -84.357735,
    -84.430591,
    -84.429989,
    -84.416526,
    -84.416900,
    -84.423976,
    -84.433126,
    -84.433948,
    -84.426035,
    -84.366898,
    -84.367865,
    -84.311617,
    -84.367905, #beltline and virginia
    -84.367747,
    -84.365092, #sampson bridge 1
    -84.365032 #sampson bridge 2
    )
  ) %>% 
  as_tibble() %>% 
  st_as_sf(
    crs=4326,
    coords = c("lon", "lat")) %>% 
  st_sf() %>% 
  mutate(trail_p_exclude_bridge=1)

#checks on bridges
class(trail_p_exclude_bridge)
mv_trail_p_exclude_bridge = trail_p_exclude_bridge %>%
  mapview(
    layer.name = "bridges",
    col.regions = "red")
mv_inters_final_trail_p = inters_all_union_both %>% 
  filter(intersects_trail_p==1) %>%
  mapview(
  col.regions = "blue",
  layer.name = "trail_int")

table(bmap_edge_join_wrangle$infra_6cat)
mv_buffered = bmap_edge_join_wrangle %>% 
  filter(infra_6cat == "bike_lane_buffered") %>% 
  mapview(col.regions = "green")

mv_inters_final_trail_p + mv_trail_p_exclude_bridge + mv_buffered

### Exclude other ineligible intersections-------------
#There are some other intersections that aren't really intersections.
#These instances are three-way intersections between two streets, and
#the trail is on the side of the trail opposite the intersection,
#so the bicyclist actually never has to cross the other street.
#These should not be considered intersections with trails.
#(Don't want to create immortal person-time.)
trail_p_exclude_other = data.table::data.table(
  int_name = c(
    "gary_ave_nw", #by bankhead marta
    "palifox_dr_ne", #all of these are stone mountain trail 
    "ridgecrest_rd_ne",
    "chief_matthews_rd",
    "melrose_ave",
    "drexel_ave",
    "lansdowne_ave",
    "greenwood_cir",
    "patillo_way"
  ),

    lat=c(
      33.772560,
      33.763985, 
      33.764548, 
      33.765495, 
      33.766025, 
      33.765667, 
      33.766406,
      33.767979, 
      33.768865
          ),
    lon = c(
      -84.429497,
      -84.318030,
      -84.316982,
      -84.311080,
      -84.308923,
      -84.310025,
      -84.307941,
      -84.303024,
      -84.300362
      )
    )   %>% 
  as_tibble() %>% 
  st_as_sf(
    crs=4326,
    coords = c("lon", "lat")) %>% 
  st_sf() %>% 
  mutate(trail_p_exclude_other=1)
    
mv_trail_p_exclude_other = trail_p_exclude_other %>%
  mapview(
    layer.name = "exclude-other",
    col.regions = "red")
mv_inters_final_trail_p = inters_all_union_both %>% 
  filter(intersects_trail_p==1) %>%
  mapview(
    col.regions = "blue",
    layer.name = "trail_int")

mv_trail_p_exclude_other + mv_inters_final_trail_p

#Link both of the exclusions to make a little dataset
names(inters_all_union_both)
inters_trail_p_exclude  = inters_all_union_both %>% 
  st_join(trail_p_exclude_bridge, left = TRUE) %>% 
  st_join(trail_p_exclude_other, left = TRUE) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  dplyr::select(inters_id, bridge_name, int_name) %>% 
  mutate(
    trail_p_exclude=case_when(
      is.na(int_name)==FALSE ~1,
      is.na(bridge_name)==FALSE ~1,
      TRUE ~0
    )) %>% 
  filter(trail_p_exclude==1)
    
inters_trail_p_exclude
save(inters_trail_p_exclude, file = "inters_trail_p_exclude.RData" )
nrow(inters_trail_p_exclude)

# One more thing - linking osm_highway_id to each intersection---------------
# setwd(here("data-processed"))
# load("inters_all_union_both.RData")
# load("bmap_union_wrangle.RData")
library(tidyverse)
library(sf)

nrow(inters_all_union_both)
#Do this by the most major highway type. 
#That way you can sample your intersections by their osm-id group

table(bmap_union_wrangle$highway_9cat)
class(bmap_union_wrangle)
table(bmap_union_wrangle$osm_name_highway_combo_id)

## Most major intersecting roadway ------------------
#I repeat this so function it

bmap_union_mutate_select<-function(df){
  df %>% 
    mutate(length_osm_name_highway_combo_id = st_length(geometry)) %>% 
    dplyr::select(
      osm_name_highway_combo_id, osm_name_highway_combo_id_miss, 
      length_osm_name_highway_combo_id, 
      geometry
      )
}

#trunk
table(bmap_union_wrangle$highway_9cat)
bmap_union_wrangle_trunk = bmap_union_wrangle %>% 
  filter(highway_9cat=="trunk road") %>% 
  bmap_union_mutate_select()

table(inters_all_union_both$intersects_highway_most_major_6cat)
inters_all_union_both_trunk = inters_all_union_both %>% 
  filter(intersects_highway_most_major_6cat=="trunk") %>% 
  st_join(bmap_union_wrangle_trunk) %>% 
  mutate(osm_name_highway_combo_id_most_major = osm_name_highway_combo_id) %>% 
  dplyr::select(-osm_name_highway_combo_id)


#primary
table(bmap_union_wrangle$highway_9cat)
bmap_union_wrangle_primary = bmap_union_wrangle %>% 
  filter(highway_9cat=="primary road") %>% 
  bmap_union_mutate_select()

table(inters_all_union_both$intersects_highway_most_major_6cat)
inters_all_union_both_primary = inters_all_union_both %>% 
  filter(intersects_highway_most_major_6cat=="primary") %>% 
  st_join(bmap_union_wrangle_primary) %>% 
  mutate(osm_name_highway_combo_id_most_major = osm_name_highway_combo_id) %>% 
  dplyr::select(-osm_name_highway_combo_id)

table(inters_all_union_both_primary$osm_name_highway_combo_id_most_major)# great. that worked.

#secondary
table(bmap_union_wrangle$highway_9cat)
bmap_union_wrangle_secondary = bmap_union_wrangle %>% 
  filter(highway_9cat=="secondary road") %>% 
  bmap_union_mutate_select()

table(inters_all_union_both$intersects_highway_most_major_6cat)
inters_all_union_both_secondary = inters_all_union_both %>% 
  filter(intersects_highway_most_major_6cat=="secondary") %>% 
  st_join(bmap_union_wrangle_secondary) %>% 
  mutate(osm_name_highway_combo_id_most_major = osm_name_highway_combo_id) %>% 
  dplyr::select(-osm_name_highway_combo_id)

table(inters_all_union_both_secondary$osm_name_highway_combo_id_most_major)# great. that worked.

#tertiary
table(bmap_union_wrangle$highway_9cat)
bmap_union_wrangle_tertiary = bmap_union_wrangle %>% 
  filter(highway_9cat=="tertiary road") %>% 
  bmap_union_mutate_select()


table(inters_all_union_both$intersects_highway_most_major_6cat)
inters_all_union_both_tertiary = inters_all_union_both %>% 
  filter(intersects_highway_most_major_6cat=="tertiary") %>% 
  st_join(bmap_union_wrangle_tertiary) %>% 
  mutate(osm_name_highway_combo_id_most_major = osm_name_highway_combo_id) %>% 
  dplyr::select(-osm_name_highway_combo_id)

table(inters_all_union_both_tertiary$osm_name_highway_combo_id_most_major)# great. that worked.


#residential
table(bmap_union_wrangle$highway_9cat)
bmap_union_wrangle_residential = bmap_union_wrangle %>% 
  filter(highway_9cat=="residential road") %>% 
  bmap_union_mutate_select()


table(inters_all_union_both$intersects_highway_most_major_6cat)
inters_all_union_both_residential = inters_all_union_both %>% 
  filter(intersects_highway_most_major_6cat=="residential") %>% 
  st_join(bmap_union_wrangle_residential) %>% 
  mutate(osm_name_highway_combo_id_most_major = osm_name_highway_combo_id) %>% 
  dplyr::select(-osm_name_highway_combo_id)

table(inters_all_union_both_residential$osm_name_highway_combo_id_most_major)# great. that worked.

## stack them-----------------
inters_all_union_both_most_major_highway = inters_all_union_both_trunk %>% 
  bind_rows(
    inters_all_union_both_primary,
    inters_all_union_both_secondary,
    inters_all_union_both_tertiary,
    inters_all_union_both_residential
  ) %>% 
  arrange(inters_id,
          intersects_highway_most_major_6cat,
          desc(length_osm_name_highway_combo_id)
          ) %>% 
  #great, that worked. now group by intersection ID and slice the top one
  group_by(inters_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    osm_name_highway_combo_id_most_major_max = max(osm_name_highway_combo_id_most_major, na.rm=TRUE),
    osm_name_highway_combo_id_most_major_w_miss = case_when(
      is.na(osm_name_highway_combo_id_most_major)==TRUE ~
        #add the intersection ID to make it different
        osm_name_highway_combo_id_most_major_max + inters_id, 
      TRUE ~ osm_name_highway_combo_id_most_major
    ),
    
    #update 11/17/2020 - to be simple, let's call this group_id
    #THE one with miss is actually what you want!!
    group_id = osm_name_highway_combo_id_most_major_w_miss 
  )

# inters_all_union_both_most_major_highway %>%
#   st_set_geometry(NULL) %>%
#   dplyr::select(inters_id,
#                 osm_name_highway_combo_id_most_major,
#                 osm_name_highway_combo_id_most_major_w_miss,
#                 osm_name_highway_combo_id_miss,
#                 osm_name_highway_combo_id_most_major_max
#                 ) %>%
#   View()

#In the edge code, I made a uniform distribution for the ones that didn't have a group.
#Should I do that here, too? I don't think so.
#see bmap_edge_join_group_miss_yes in wrangle_basemap.R Code
#not that many. shouldn't make a difference.
table(inters_all_union_both_most_major_highway$osm_name_highway_combo_id_miss) 
summary(inters_all_union_both_most_major_highway$osm_name_highway_combo_id_most_major_max)
nrow(inters_all_union_both_most_major_highway)
names(inters_all_union_both_most_major_highway)

nrow(inters_all_union_both_most_major_highway)
nrow(inters_all_union_both)
n_distinct(inters_all_union_both_most_major_highway$inters_id)
n_distinct(inters_all_union_both_most_major_highway$osm_name_highway_combo_id_most_major)
n_distinct(inters_all_union_both_most_major_highway$osm_name_highway_combo_id_most_major_w_miss)


# Final dataset including the most major osm name group-------------
setwd(here("data-processed"))
load("inters_all_union_both.RData")
load("lookup_inters_osm_name_highway_combo_id_most_major.RData")
load("inters_trail_p_exclude.RData")
inters_trail_p_exclude
inters_final = inters_all_union_both %>% 
  left_join(lookup_inters_osm_name_highway_combo_id_most_major, by = "inters_id") %>% 
  

  #July 25, 2022 update:
  #exclude the bridges and the other trail intersections that shouldn't be coded
  #as such
  left_join(inters_trail_p_exclude, by = "inters_id") %>% 
  filter(is.na(trail_p_exclude)==TRUE)
  
save(inters_final, file = "inters_final.RData")
#load("inters_final.RData")
table(inters_final$trail_p_exclude_bridge)
nrow(inters_final)
names(inters_final)
##with the bridges = 7853; excluding bridges = 7835
#7/25/22 update: down to 7819
#9/2/22 a few more b/c of a couple new piedmont park trails picke dup

#a no-geo version
inters_final_nogeo = inters_final %>% st_set_geometry(NULL)
save(inters_final_nogeo, file = "inters_final_nogeo.RData")

table(inters_final$intersects_highway_most_major_6cat)
table(inters_final$intersects_highway_most_major_2_2_ordered)
names(inters_final)
names(inters_final_nogeo)


## lookups----------------
## lookup group id and osm-name-highway-combo----------
lookup_inters_geo = inters_final %>%  
  dplyr::select(inters_id, geometry)
setwd(here("data-processed"))
save(lookup_inters_geo, file = "lookup_inters_geo.RData")

lookup_inters_osm_name_highway_combo_id_most_major = inters_final %>% 
  st_set_geometry(NULL) %>% 
  distinct(inters_id,
           osm_name_highway_combo_id_most_major, 
           osm_name_highway_combo_id_most_major_w_miss,
           group_id) %>% 
  as_tibble()

save(lookup_inters_osm_name_highway_combo_id_most_major,
     file = "lookup_inters_osm_name_highway_combo_id_most_major.RData")
nrow(lookup_inters_osm_name_highway_combo_id_most_major)

load("inters_final_nogeo.RData")
lookup_inters_group_id = inters_final_nogeo %>% 
  distinct(inters_id, group_id) %>% 
  as_tibble()
save(lookup_inters_group_id, file = "lookup_inters_group_id.RData")
## lookup most major highway category---------
#this is included in the below as well, but I'm including this in case I only want these variables,
#specifically.
names(inters_final_nogeo)
lookup_inters_highway_most_major = inters_final_nogeo %>% 
  distinct(inters_id, 
           intersects_highway_most_major_6cat,
           intersects_highway_most_major_6cat_ordered_with_trunk,
           intersects_highway_most_major_5cat_ordered_collapse_trunk,
           intersects_highway_most_major_2_2_ordered,
           intersects_highway_most_major_6cat) %>% 
  as_tibble()
save(lookup_inters_highway_most_major, file = "lookup_inters_highway_most_major.RData")

nrow(inters_final_nogeo)
lookup_inters_highway_most_major
## selected intersection look-up variables-------------
#all the intersection vars to link back in after a summarise
lookup_inters_vars_to_link = inters_final_nogeo %>% 
  dplyr::select(
    inters_id, 
    starts_with("intersects_h"), 
    starts_with("osm_name_h"),
#    starts_with("hwy") #7/26/22 should be irrelevant now
    starts_with("group"),
  ) %>% 
  as_tibble()

save(lookup_inters_vars_to_link, file = "lookup_inters_vars_to_link.RData")
names(lookup_inters_vars_to_link)

#----------how many intersections per group?------#########
load("inters_final_nogeo.RData")
n_distinct(inters_final_nogeo$osm_name_highway_combo_id_most_major)
n_distinct(inters_final_nogeo$group_id)
n_distinct(inters_final_nogeo$inters_id)
n_distinct(inters_final_nogeo$inters_id)/n_distinct(inters_final_nogeo$group_id)


# Mapviews-------------
setwd(here("data-processed"))
load("inters_final.RData")
sf::sf_use_s2(FALSE)

## intersections by highway type-----------
table(inters_final$intersects_highway_most_major_6cat_ordered_with_trunk)
table(inters_final$intersects_highway_most_major_5cat_ordered_collapse_trunk)
#summarise the dataset by roadway type so that the mapview is smaller

inters_most_major_hwy_grouped=inters_final %>% 
  #much easier to see if you do:
  st_centroid() %>% 
  group_by(intersects_highway_most_major_6cat_ordered_with_trunk) %>% 
  summarise(n=n())

library(RColorBrewer)
set2_5 = RColorBrewer::brewer.pal(n=5, name = "Set2")
mv_inters_most_major_hwy_grouped =inters_most_major_hwy_grouped %>% 
  mapview(
#    cex=1,
    col.regions = set2_5,
#    color=set2_5,
    layer.name = "Most major roadway type at intersection",
    zcol = "intersects_highway_most_major_6cat_ordered_with_trunk")

mv_inters_most_major_hwy_grouped

## Intersections with any intersection with a given roadway type--------
#I'm curious what intersections, if any, cross with unclassified, etc.
names(inters_final)
inters_final %>% 
  filter(intersects_highway_unclassified_service==1) %>% 
  st_centroid() %>% #easier to see
  mapview()

inters_final %>% 
  filter(intersects_trail_p==1) %>% 
  st_centroid() %>% #easier to see
  mapview()


  
## a mapview of intersections against infrastructure------------
#Let's restrict to a 2-mile radius for speed
load("mp_sf_2mi.RData")
mv_inters_mp_sf_2mi = inters_final %>% 
  st_intersection(mp_sf_2mi) %>% 
  mapview(
    layer.name = "inters",
    col.regions = "red")

mv_inters_mp_sf_2mi

#Map of infra created here 
#diss/scripts/aim-3-results/aim3_figs.R
#Closely following that script summer 2022
load("lookup_edge_mo_infra_nogeo.RData") #created in 2_wrangle_basemap
load("lookup_bmap_edge_geo.RData") #geometry lookup
load("lookup_infra_exclude_for_length_nogeo.RData") #exclude dupes, maybe
library(RColorBrewer)
dark2_5 = RColorBrewer::brewer.pal(n=5, name = "Dark2")
mv_infra_mo_23_mp_sf_2_mi =lookup_edge_mo_infra_nogeo %>% 
  filter(study_month ==23) %>% 
  left_join(lookup_bmap_edge_geo, by = "edge_id") %>% 
  left_join(lookup_infra_exclude_for_length_nogeo, by = "edge_id") %>% 
  st_sf() %>% 
  #  filter(ribbon_after_study==0) %>% #shouldn't be needed since we're specifying study month above
  filter(infra_exclude_for_length==0) %>% #this is for the visualization but not the calculation
  filter(infra_6cat_long_legend_nodirt !="none") %>% 
  #  filter(infra_6cat_long_legend_nodirt !="5-Sharrow") %>% 
  st_intersection(mp_sf_2mi)  %>% 
  mapview(
    zcol =  "infra_6cat_long_legend_nodirt",  
    color = dark2_5,
    layer.name = "Infrastructure" ,
    lwd=4,
    alpha=0.9
  ) 

## 7/21/22  - check beltline intersections---------
#no intersections on the beltline as should be
#looks good! good job!
mv_inters_mp_sf_2mi + mv_infra_mo_23_mp_sf_2_mi


