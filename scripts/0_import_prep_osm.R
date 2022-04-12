#Import OSM data for the Atlanta area
#Date began 9/17/18
#Date revised 2/5/19
#Revised 9/23/2020
#Here again 4/19/21 (but not repeating thedata pull)

#------Load packages--------#
library(tidyverse)
library(ggmap)
library(ggspatial) #this loads sp
#library(sp)
library(rgdal) 
library(sf)
library(raster)
library(mapview) #loads leaflet.
library(readxl)
library(RColorBrewer)
library(osmdata)
library(tmap)
library(tmaptools)
library(viridis)
library(viridisLite)
library(here) 
#adding here for portability 12/16/21
#Note this may be the one exception where I'm not going to re-run anything
#and thus it doesn't matter where I save things.

#---see https://wiki.openstreetmap.org/wiki/Key:highway#Roads for descriptions of the values----#


#Update from 9/23/20 - I'm going to call these data again, as I think they will be more accurate.
#That may have implications for how the osm_id matches up moving forward, but I'm here for that.

#the previous pull was from two years ago, so I believe these data will be more accurate.

#---------aims 2 and 3 specific summary subsets----------#########
setwd(here("data-processed"))
getwd()
load(file = "mp_sf_ft.RData")
load(file = "mp_sf_5halfmi.RData") #restricting the boundary because this is the geographic extent of the cases
# 
#------------1. Pull the data for Fulton COunty (both motorized and non-motorized) -----####
# #--grab I-285
# i285_convhull = fmotorl %>%
#   filter(osm_name_osm == "Perimeter") %>%
#   st_transform( "+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999 +
#                 y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
#   st_buffer(400) %>% #50 foot ....the reason for this is to get one single area rather than
#                     #the two lanes in either direction
# # 
#   #now merge it together into a single polygon, I think
#   st_union(by_feature=FALSE) %>%
#   #now, can I get its convex hull??
#   st_convex_hull() %>% #tada!! this is close but not perfect. one other option
#   st_union(by_feature=FALSE) %>%
# 
#   #convert it to meters so it lines up with the CRS of the infrastructure
#    st_transform(6446)
# 
# 
# mapview(i285_convhull)

#save the convex hull to the drive
#new working directory at here 12/16/21
#save(i285_convhull,  file = "i285_convhull.RData")
#this is useful. restrict all of the OSM to data within this.
# load(file = "i285_convhull.RData")
# i285_convhull_4326 = i285_convhull %>% st_transform(4326)
# save(i285_convhull_4326,  file = "i285_convhull_4326.RData")

setwd(here("data-processed"))
load(file = "i285_convhull_4326.RData")


#Update 9/24/20, within the step, 
#I am restricting to lines and to inside I-285. That should speed up the process
#and restrict the number of steps.

#another update 9/24/2020 - I'm getting rid of the f superscript and trying to simplify this code.
options(viewer = NULL) #send viewer to the browser
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

#--------DO NOT RUN-----------------###############
#12/16/21 
#running this code now after the analysis is done could result in new names/osm ids
#that will mess up the code later on.
#This can all just be saved and uploaded somewhere for replicability.

# #-----------1.1 motorized (i.e. cars/trucks) 'highways'-----------------------####
#interstate highways and major motorways
motorway = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf()

  #extract the line sf object from the list and restrict to within I-285
motorway_meta = motorway$meta %>% as_tibble() #extract the timestamp of when the data were pulled
#https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html#3_the_osmdata_object

nrow(motorway$osm_polygons)  #are there any polygons? #none
motorway_line_i285 = motorway$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(motorway_meta)
rm(motorway)

 #trunk roads -- large state roads, etc. that aren't interstates / freeways,
# #e.g. freedom parkway
trunk = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'trunk') %>%
  osmdata_sf()

nrow(trunk$osm_polygons)  #are there any polygons? #none
trunk_meta = trunk$meta %>% as_tibble() #extract the timestamp of when the data were pulled
trunk_line_i285 = trunk$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(trunk_meta)

# #primary roads - next most important road, e.g. ponce
primary = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'primary') %>%
  osmdata_sf()

primary_polygons_lines = primary$osm_polygons %>% 
  st_intersection(i285_convhull_4326) #none
rm(primary_polygons_lines)

primary_meta = primary$meta %>% as_tibble() 
primary_line_i285 = primary$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(primary_meta)

# #secondary roads, e.g. north avenue (where it's not residential)
secondary = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'secondary') %>%
  osmdata_sf()

secondary_polygons_lines = secondary$osm_polygons %>% 
  st_intersection(i285_convhull_4326) #none
rm(secondary_polygons_lines)

secondary_meta = secondary$meta %>% as_tibble() 
secondary_line_i285 = secondary$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(secondary_meta)

mapview(secondary_line_i285) #note! cycleway.right = track at 10th st! - 
#also note there is a highway_1 variable in it. that was pulled - which is great.
#also edgewood, it says "lane"
#ralph david abernathy doesn't have anything, though

# #tertiary roads
tertiary = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'tertiary') %>%
  osmdata_sf()

tertiary_polygons_lines = tertiary$osm_polygons %>% 
  st_intersection(i285_convhull_4326) #3. disregard
mapview(tertiary_polygons_lines, lwd=10)

rm(tertiary_polygons_lines)

tertiary_meta = tertiary$meta %>% as_tibble() 
tertiary_line_i285 = tertiary$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(tertiary_meta)


# #residential roads (this is the big one)
residential = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'residential') %>%
  osmdata_sf()

residential_polygons_lines = residential$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%  #379. 
  st_cast("LINESTRING") %>% 
  mutate(polygon_initially = 1)

mapview(residential_polygons_lines, lwd=10) #okay, there are a few, incorporated below

residential_meta = residential$meta %>% as_tibble() 
residential_line_i285 = residential$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(residential_meta) %>% 
  bind_rows(residential_polygons_lines) #add in the polygons (cul de sacs)
rm(residential)

residential_line_i285 %>% filter(polygon_initially==1) %>% mapview()

#link roads for each of these -- see https://wiki.openstreetmap.org/wiki/Highways
motorway_link= opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'motorway_link') %>%
  osmdata_sf()

motorway_link_polygons_lines = motorway_link$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%   
  st_cast("LINESTRING") %>% 
  mutate(polygon_initially = 1) #0

motorway_link_meta = motorway_link$meta %>% as_tibble() 
motorway_link_line_i285 = motorway_link$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(motorway_link_meta)

trunk_link = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'trunk_link') %>%
  osmdata_sf()
trunk_link_meta = trunk_link$meta %>% as_tibble() 
trunk_link_line_i285 = trunk_link$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(trunk_link_meta)

primary_link = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'primary_link') %>%
  osmdata_sf()
primary_link_meta = primary_link$meta %>% as_tibble() 
primary_link_line_i285 = primary_link$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(primary_link_meta)


secondary_link = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'secondary_link') %>%
  osmdata_sf()
secondary_link_meta = secondary_link$meta %>% as_tibble() 
secondary_link_line_i285 = secondary_link$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(secondary_link_meta)

tertiary_link = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'tertiary_link') %>%
  osmdata_sf()
tertiary_link_meta = tertiary_link$meta %>% as_tibble() 
tertiary_link_line_i285 = tertiary_link$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(tertiary_link_meta)

#this is an empty set
# residential_link = opq(bbox = "Fulton County, Georgia, USA") %>%
#   add_osm_feature(key = 'highway', value = 'residential_link') %>%
#   osmdata_sf()

#this is an empty set
# motorway_junction = opq(bbox = "Fulton County, Georgia, USA") %>%
#   add_osm_feature(key = 'highway', value = 'motorway_junction') %>%
#   osmdata_sf()


#unclassified and service roads
unclassified = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'unclassified') %>%
  osmdata_sf()
unclassified_meta = unclassified$meta %>% as_tibble() 
unclassified_line_i285 = unclassified$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(unclassified_meta)

service = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'service') %>%
  osmdata_sf()

service_polygons_lines = service$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%  
  st_cast("LINESTRING") %>% 
  mutate(polygon_initially = 1) #316
mapview(service_polygons_lines)

service_meta = service$meta %>% as_tibble() 
service_line_i285 = service$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(service_meta) %>% 
  bind_rows(service_polygons_lines)

#remove the large ones
rm(service, residential)

#road of unknown type: (road)
#empty set
road = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'road') %>%
  osmdata_sf()
road_meta = road$meta %>% as_tibble() 
road_line_i285 = road$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(road_meta)
#mapview(road_line_i285)
rm(road, road_meta, road_line_i285)


track = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'track') %>%
  osmdata_sf()
track_meta = track$meta %>% as_tibble() 
track_line_i285 = track$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(track_meta) #note, this includes a golf course

mapview(track_line_i285)

#-----------1.2 non-motorized (i.e. bike/ped) 'highways'-----------------------####
#Most recently pulled these 5/15/19
#Now updating 9/24/2020

#------after I updated Southside park, Sykes Park, and Path 400
#non-specific paths
path = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'path') %>%
  osmdata_sf()

path_polygons_lines = path$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%  
  st_cast("LINESTRING") %>% 
  mutate(polygon_initially = 1) #316
mapview(path_polygons_lines) #I got chastain park trail

path_meta = path$meta %>% as_tibble() 
path_line_i285 = path$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(path_meta) %>% 
  bind_rows(path_polygons_lines)

mapview(path_line_i285)
# #cycle ways
cycleway = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'cycleway') %>%
  osmdata_sf()

cycleway_polygons_lines = cycleway$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%    
  st_cast("LINESTRING")  %>% 
  mutate(polygon_initially = 1)
nrow(cycleway_polygons_lines) #2
mapview(cycleway_polygons_lines, lwd=10) #okay, there are a few

cycleway_meta = cycleway$meta %>% as_tibble() 
cycleway_line_i285 = cycleway$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(cycleway_meta) %>% 
  bind_rows(cycleway_polygons_lines)


mapview(cycleway_line_i285) #note the presence of start date for the beltline!
cycleway_line_i285 %>% filter(polygon_initially==1) %>% mapview()

# #I want to grab footways that ARE NOT sidewalks.
footway = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'footway') %>%
  add_osm_feature(key='footway' , value = '!sidewalk') %>% #works to exclude sidewalks
  osmdata_sf()
footway_polygons_lines = footway$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%    
  st_cast("LINESTRING")  %>% 
  mutate(polygon_initially = 1)
nrow(footway_polygons_lines) #192
mapview(footway_polygons_lines, lwd=10)  

footway_meta = footway$meta %>% as_tibble() 
footway_line_i285 = footway$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(footway_meta) %>% 
  bind_rows(footway_polygons_lines)

mapview(footway_line_i285) #this got the beltline southside trail in Atlanta BeltLine Southside Trai

#pedestrian
pedestrian = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'pedestrian') %>%
  osmdata_sf()
pedestrian_polygons_lines = pedestrian$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%   
  st_cast("LINESTRING")  %>% 
  mutate(polygon_initially = 1) #8
mapview(pedestrian_polygons_lines, lwd=10)  
pedestrian_meta = pedestrian$meta %>% as_tibble() 
pedestrian_line_i285 = pedestrian$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(pedestrian_meta) %>% 
  bind_rows(pedestrian_polygons_lines)
mapview(pedestrian_line_i285) #so some piedmont park, some grant park

# #living street classification
living_street = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'living_street') %>%
  osmdata_sf()
living_street_polygons_lines = living_street$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%   
  st_cast("LINESTRING")  %>% 
  mutate(polygon_initially = 1) #none

living_street_meta = living_street$meta %>% as_tibble() 
living_street_line_i285 = living_street$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(living_street_meta) 

#under construction
construction = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'construction') %>%
  osmdata_sf()
construction_polygons_lines = construction$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%  
  st_cast("LINESTRING")  %>% 
  mutate(polygon_initially = 1)  #none

construction_meta = construction$meta %>% as_tibble() 
construction_line_i285 = construction$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(construction_meta) 

mapview(construction_line_i285) #this got path400!

# proposed
proposed = opq(bbox = "Fulton County, Georgia, USA") %>%
  add_osm_feature(key = 'highway', value = 'proposed') %>%
  osmdata_sf()
proposed_polygons_lines = proposed$osm_polygons %>% 
  st_intersection(i285_convhull_4326) %>%  #379. 
  st_cast("LINESTRING")  %>% 
  mutate(polygon_initially = 1) #none

proposed_meta = proposed$meta %>% as_tibble() 
proposed_line_i285 = proposed$osm_lines %>% 
  st_intersection(i285_convhull_4326) %>% 
  bind_cols(proposed_meta) 

mapview(proposed_line_i285)



#----------1.3 Save the files to disk------------------####
#12/19/21 an exception to my rule of just dumping everything in data-processed.
#These are already in a separate folder, and because they take forever to run, keep them there
#but put it underneath data-processed.
#It will also not be wise to re-pull the OSM data at this point, because the OSM-IDs may
#have changed, and because some of the code downstream relies on the OSM-IDs, it could create
#issues.

#moved this 12/19/21
#C:\Users\mdg71\Dropbox\EMORY\General research\Dissertation\diss-proj\data-processed\osm data
setwd(here("data-processed", "osm data"))
#save these to disk since they take a while to pull.
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY", "General research",  
                "Dissertation", "R_data", "osm data"))
#note static working directory

save(motorway_line_i285, file = "motorway_line_i285.RData")
save(trunk_line_i285, file = "trunk_line_i285.RData")
save(primary_line_i285, file = "primary_line_i285.RData")
save(secondary_line_i285, file = "secondary_line_i285.RData")
save(tertiary_line_i285, file = "tertiary_line_i285.RData")
save(residential_line_i285, file = "residential_line_i285.RData")
save(service_line_i285, file = "service_line_i285.RData")
save(unclassified_line_i285, file = "unclassified_line_i285.RData")

save(motorway_link_line_i285, file = "motorway_link_line_i285.RData")
save(trunk_link_line_i285, file = "trunk_link_line_i285.RData")
save(primary_link_line_i285, file = "primary_link_line_i285.RData")
save(secondary_link_line_i285, file = "secondary_link_line_i285.RData")
save(tertiary_link_line_i285, file = "tertiary_link_line_i285.RData")

save(footway_line_i285, file = "footway_line_i285.RData")
save(pedestrian_line_i285, file = "pedestrian_line_i285.RData")
save(path_line_i285, file = "path_line_i285.RData")
save(track_line_i285, file = "track_line_i285.RData")
save(cycleway_line_i285, file = "cycleway_line_i285.RData")

#--------Load all of these---------------#
#moved here 12/19/21
#C:\Users\mdg71\Dropbox\EMORY\General research\Dissertation\diss-proj\data-processed\osm data
setwd(here("data-processed", "osm data"))

# setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY", "General research",  
#                 "Dissertation", "R_data", "osm data"))
# #note static working directory
# load(file = "motorway_line_i285.RData")
# load(file = "trunk_line_i285.RData")
# load(file = "primary_line_i285.RData")
# load(file = "secondary_line_i285.RData")
# load(file = "tertiary_line_i285.RData")
# load(file = "residential_line_i285.RData")
# load(file = "service_line_i285.RData")
# load(file = "unclassified_line_i285.RData")
# load(file = "living_street_line_i285.RData") #doesn't load. not saved anywhere
# 
# load(file = "motorway_link_line_i285.RData")
# load(file = "trunk_link_line_i285.RData")
# load(file = "primary_link_line_i285.RData")
# load(file = "secondary_link_line_i285.RData")
# load(file = "tertiary_link_line_i285.RData")
# 
# load(file = "footway_line_i285.RData")
# load(file = "pedestrian_line_i285.RData")
# load(file = "path_line_i285.RData")
# load(file = "track_line_i285.RData")
# load(file = "cycleway_line_i285.RData")


#--------------------2. Wrangle the osm data----------------------#####
#stack them together
all_highway_i285 = motorway_line_i285 %>% 
  bind_rows(
    motorway_link_line_i285,
    
    trunk_line_i285,
    trunk_link_line_i285,
    primary_line_i285,
    primary_line_i285,
    secondary_line_i285,
    secondary_link_line_i285,
    tertiary_line_i285,
    tertiary_link_line_i285,
    residential_line_i285,
    
    service_line_i285,
    unclassified_line_i285,
    
    living_street_line_i285,
    footway_line_i285,
    pedestrian_line_i285,
    path_line_i285,
    track_line_i285,
    cycleway_line_i285,
    
    construction_line_i285,
    proposed_line_i285
    ) %>% 
  #drop some variables that you know you don't want
  dplyr::select(
    -starts_with("LandPro"), -taxi, -dog, -snowmobile, -ski
  ) 



setwd(here("data-processed", "osm data"))
#save(all_highway_i285, file = "all_highway_i285.RData") #last saved Oct 9 2020
load(file = "all_highway_i285.RData")
setwd(here("data-processed"))
save(all_highway_i285, file = "all_highway_i285.RData")

#-------2.1. Get rid of dupes-----##########
#---UPDATE 12/16/21 changing working directory-------######
#-----before you go through all of this, are there any duplicate osm_ids?--###

#------END DO NOT RUN----------------#####
setwd(here("data-processed")) #note new working directory
load("all_highway_i285.RData")

lookup_osm_id_geo = all_highway_i285 %>% 
  dplyr::select(osm_id, geometry)

dupe_osm_id = all_highway_i285  %>% 
  mutate(
    osm_missing_airport_onramps =  case_when(
      is.na(osm_id)==TRUE ~ 1,
      TRUE ~ 0)) %>% 
  #exclude those
  filter(osm_missing_airport_onramps==0) %>% 
  st_set_geometry(NULL) %>% 
  group_by(osm_id) %>% 
  summarise(n_appearances=n()) %>%
  filter(n_appearances>1) %>% 
  mutate(dupe_group_number  =row_number()) %>% #this do it by group
  ungroup() %>% 
  left_join(lookup_osm_id_geo, by = "osm_id") %>% 
  st_as_sf() %>% 
  mutate(dupe_osm_id = 1)


dupe_osm_id_nogeo = dupe_osm_id %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
all_highway_i285_dupes = all_highway_i285 %>% 
  mutate(
    osm_missing_airport_onramps =  case_when(
      is.na(osm_id)==TRUE ~ 1,
      TRUE ~ 0)) %>% 
  #exclude those
  filter(osm_missing_airport_onramps==0) %>% 
  left_join(dupe_osm_id_nogeo, by = "osm_id") %>% 
  filter(dupe_osm_id == 1) %>% 
  arrange(osm_id) %>% 
  # dplyr::select(
  #   osm_id, name, bicycle, tiger.name_base, n_appearances, dupe_osm_id, dupe_group_number, geometry 
  # ) %>% 
  #group by dupe group and slice the first obs
  group_by(dupe_group_number) %>% 
  slice(1) %>%  #take the first of the duplicate pair.
  ungroup()


all_highway_i285_not_dupes = all_highway_i285 %>% 
  mutate(
    osm_missing_airport_onramps =  case_when(
      is.na(osm_id)==TRUE ~ 1,
      TRUE ~ 0)) %>% 
  #exclude those
  filter(osm_missing_airport_onramps==0) %>% 
  left_join(dupe_osm_id_nogeo, by = "osm_id") %>% 
  filter(is.na(dupe_osm_id == TRUE)) #duplicate is missing 

all_highway_dupes_rid = all_highway_i285_not_dupes %>% 
  bind_rows(all_highway_i285_dupes) %>% 
  mutate(  length_osm_m = as.numeric(st_length(geometry))) %>% 
  
  
        #for speed in subsequent code, remove some variables
        #------------organize and keep variables------#######
      dplyr::select(
        osm_id,
        name,
        name_1,
        old_name,
        alt_name,
        starts_with("cycleway"), 
        start_date,
        starts_with("highway"), 
        contains("bicycle"),
        motor_vehicle,
        HFCS, 
        contains("surface"),
        paved,
        tracktype,
        contains("sidewalk"),
        contains("foot"),
        hiking,
        contains("mtb."),
        construction,       route,
        service,
        traffic_calming,
        #      delivery,   driveway,
        #     buses,       moped,       minibus,       golf_cart,
        proposed,
      starts_with("length"), #12/5/21 eventually I will get rid of the original length field but it stays in for now
        #      starts_with("tiger"),
        contains("tiger.name"),
        #      starts_with("maxspeed"),
        #      starts_with("minspeed"),
        #    history,
        #     OSM_version,
        #      contains("oneway"),
        #      wikipedia,
        timestamp,
        starts_with("dupe"),
        n_appearances,
        geometry 
        
        #and that's all you're keeping. keep more if you must by adding them in.
        
        #      everything()
          )



#--checks before more wrangling---#
names(all_highway_i285)
summary(all_highway_dupes_rid$length_osm_m)
table(all_highway_i285$highway)
table(all_highway_i285$highway_1)
nrow(all_highway_dupes_rid) #89584
#View(all_highway_i285)

####save-----#####
#apparently I wanted to save this both places. confusing but keep to make sure all pipeline works.
setwd(here("data-processed", "osm data"))
save(all_highway_dupes_rid, file = "all_highway_dupes_rid.RData")
setwd(here("data-processed"))
save(all_highway_dupes_rid, file = "all_highway_dupes_rid.RData")

#stop here and go to the 0_1_wrangle_osm_ ... code
#for how you classify the infrastructure, etc.