#-------Load Packages-----------------
library(sf)
library(tidyverse)
library(ggmap) #note you register the google key here: 
#/~Work/software/configure-r/configure_register_ggmap.R
library(raster)
library(mapview) #loads leaflet.
library(RColorBrewer) 
library(here) #updated 12/16/21 to add everything into the main analysis data folder and not the buffers
here()

# Geocoding-----------
## Monroe and Ponce intersection and create buffers of varying sizes----------    

#some things I'm doing throughout: 
#making versions called monpon_ and mp_ in case I load one vs the other in spots
#saving everything to the main analysis-data folder via here()

#the address to geocode.
monroe_ponce = as_tibble("697 Monroe Dr NE, Atlanta, GA, 30308") 
mp = rename(monroe_ponce, address = value) #rename the variable so it has a name
monroe_ponce_geo = mutate_geocode(mp, address, force = TRUE)

#convert to sf.
monpon_sf = monroe_ponce_geo %>%
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

mapview(monpon_sf)
#convert to a coordinate system that supports feet
#that has been working for other dat.a
monpon_sf_ft = monpon_sf %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "Monroe & Ponce Intersection") %>% 
  dplyr::select(-address) #just in case that messes something up. not needed.

setwd(here("data-processed"))
save(monpon_sf_ft, file = "monpon_sf_ft.RData")
st_crs(monpon_sf_ft) #good. in feet.
mp_sf_ft = monpon_sf_ft #because I use this name in places
save(mp_sf_ft, file = "mp_sf_ft.RData")

monpon_sf_1mi = monpon_sf_ft %>% st_buffer(5280) %>%   st_transform(4326)
monpon_sf_halfmi = monpon_sf_ft %>% st_buffer(5280/2) %>%   st_transform(4326)
monpon_sf_2mi = monpon_sf_ft %>% st_buffer(2*5280) %>%   st_transform(4326)

setwd(here("data-processed"))
save(monpon_sf_halfmi, file = "monpon_sf_halfmi.RData")
save(monpon_sf_1mi, file = "monpon_sf_1mi.RData")
save(monpon_sf_2mi, file = "monpon_sf_2mi.RData")

mp_sf_1mi = monpon_sf_1mi
mp_sf_halfmi = monpon_sf_halfmi
mp_sf_2mi = monpon_sf_2mi
save(mp_sf_halfmi, file = "mp_sf_halfmi.RData")
save(mp_sf_1mi, file = "mp_sf_1mi.RData")
save(mp_sf_2mi, file = "mp_sf_2mi.RData")

monpon_sf_5mi = monpon_sf_ft %>% 
  st_buffer(26400) %>%
  mutate(radius_name = "5-mi radius, Monroe & Ponce") %>% 
  st_transform(4326)
  
monpon_sf_5halfmi = 
  monpon_sf_5mi = monpon_sf_ft %>%
  st_buffer(29040) %>%
  mutate(radius_name = "5-and-half-mi radius, Monroe & Ponce") %>% 
  st_transform(4326)

monpon_sf_8mi =  monpon_sf_ft %>%
  st_buffer(42240) %>%
  mutate(radius_name = "8-mi radius, Monroe & Ponce") %>% 
  st_transform(4326)

save(monpon_sf_5mi, file = "monpon_sf_5mi.RData")
save(monpon_sf_5halfmi, file = "monpon_sf_5halfmi.RData")
save(monpon_sf_8mi, file = "monpon_sf_8mi.RData")

#simplify the names and save. I typically use the mp vesrion throughout the rest of the code rather than the monpon version
mp_sf_5mi = monpon_sf_5mi
mp_sf_5halfmi = monpon_sf_5halfmi
mp_sf_8mi = monpon_sf_8mi

save(mp_sf_5mi, file = "mp_sf_5mi.RData")
save(mp_sf_5halfmi, file = "mp_sf_5halfmi.RData")
save(mp_sf_8mi, file = "mp_sf_8mi.RData")

#make a 12-mile one so it goes all the way around 285 and you can slice everything else out from it for
mp_sf_12mi =  monpon_sf_ft %>%
  st_buffer(63360) %>%
  mutate(radius_name = "12-mi radius, Monroe & Ponce") %>% 
  st_transform(4326)

#make a 13-mile version, so all of i285 is captured

mp_sf_13mi =  monpon_sf_ft %>%
  st_buffer(68640) %>%
  mutate(radius_name = "13-mi radius, Monroe & Ponce") %>% 
  st_transform(4326)

save(mp_sf_12mi, file = "mp_sf_12mi.RData")
save(mp_sf_13mi, file = "mp_sf_13mi.RData")


#-------------------------------------------------------------------#
# Geocode Lenox and create buffers--------####
#-------------------------------------------------------------------#
#the purpose of this is to more easily investigate my
#OSM additions to Path 400

Lenox_Square = as_tibble("Lenox Square, Peachtree Road Northeast, Atlanta, GA") 
Lenox_Square2 = rename(Lenox_Square, address = value) #rename the variable so it has a name

Lenox_Square_geo = mutate_geocode(Lenox_Square2, address, force = TRUE)

#convert to sf.
Lenox_Square_geo_sf = Lenox_Square_geo %>%
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

#convert to a coordinate system that supports feet
#that has been working for other dat.a
Lenox_Square_geo_sf_ft = Lenox_Square_geo_sf %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "Lenox_Square")

st_crs(Lenox_Square_geo_sf_ft) #good. in feet.

#make a 2.5 mile radius
2.5*5280
Lenox_Square_geo_sf_ft_2.5mi =  Lenox_Square_geo_sf_ft %>%
  st_buffer(13200) %>%
  mutate(radius_name = "2.5-mi radius, Lenox Square") %>%
  st_transform(4326)


setwd(here("data-processed"))
save(Lenox_Square_geo_sf_ft_2.5mi, file = "Lenox_Square_geo_sf_ft_2.5mi.RData")

#-------------Geocode Southside Park-----------------------------------------#############

Southside_Park_MTB = as_tibble("Southside Park Trail Parking, Atlanta, GA") 
Southside_Park_MTB2 = rename(Southside_Park_MTB, address = value) #rename the variable so it has a name
Southside_Park_MTB_geo = mutate_geocode(Southside_Park_MTB2, address, force = TRUE)
#convert to sf.
Southside_Park_MTB_geo_sf = Southside_Park_MTB_geo %>%
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

#convert to a coordinate system that supports feet
#that has been working for other dat.a
Southside_Park_MTB_geo_sf_ft = Southside_Park_MTB_geo_sf %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "Southside_Park_MTB")

st_crs(Southside_Park_MTB_geo_sf_ft) #good. in feet.

#make a 1 mile radius
1*5280
Southside_Park_MTB_geo_sf_ft_1mi =  Southside_Park_MTB_geo_sf_ft %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, Southside_Park_MTB") %>%
  st_transform(4326)


setwd(here("data-processed"))
save(Southside_Park_MTB_geo_sf_ft_1mi, file = "Southside_Park_MTB_geo_sf_ft_1mi.RData")


#-------------Geocode Sykes Park-----------------------------------------#############
Sykes_Park = as_tibble("Sykes Park, Dodson Drive Connector, East Point, GA") 
Sykes_Park2 = rename(Sykes_Park, address = value) #rename the variable so it has a name
Sykes_Park_geo = mutate_geocode(Sykes_Park2, address, force = TRUE)
#convert to sf.
Sykes_Park_geo_sf = Sykes_Park_geo %>%
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

#convert to a coordinate system that supports feet
#that has been working for other dat.a
Sykes_Park_geo_sf_ft = Sykes_Park_geo_sf %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "Sykes_Park")

st_crs(Sykes_Park_geo_sf_ft) #good. in feet.

#make a 1 mile radius
1*5280
Sykes_Park_geo_sf_ft_1mi =  Sykes_Park_geo_sf_ft %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, Sykes_Park") %>%
  st_transform(4326)


save(Sykes_Park_geo_sf_ft_1mi, file = "Sykes_Park_geo_sf_ft_1mi.RData")

#-------------Geocode Westside Trail location-----------------------------------------#############
#to more easily view infrastrucutre around the westside trail
# 1.75 miles around
# 1355 Ralph David Abernathy Blvd SW, Atlanta, GA 30310

rda_shell = as_tibble("1355 Ralph David Abernathy Blvd SW, Atlanta, GA 30310") 
rda_shell2 = rename(rda_shell, address = value) #rename the variable so it has a name
rda_shell_geo = mutate_geocode(rda_shell2, address, force = TRUE)
#convert to sf.
rda_shell_geo_sf = rda_shell_geo %>%
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

#convert to a coordinate system that supports feet
#that has been working for other dat.a
rda_shell_geo_sf_ft = rda_shell_geo_sf %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "rda_shell")

st_crs(rda_shell_geo_sf_ft) #good. in feet.

#make a 2 mile radius
2*5280
rda_shell_geo_sf_ft_2mi =  rda_shell_geo_sf_ft %>%
  st_buffer(10560) %>%
  mutate(radius_name = "2-mi radius, rda_shell") %>%
  st_transform(4326)

mapview(rda_shell_geo_sf_ft_2mi)

save(rda_shell_geo_sf_ft_2mi, file = "rda_shell_geo_sf_ft_2mi.RData")

load("all_h_osm_wrangle_both_geo.RData")
all_h_osm_wrangle_both_geo %>% 
  dplyr::select(starts_with("osm"), starts_with("infra"), starts_with("project")) %>% 
  st_intersection(rda_shell_geo_sf_ft_2mi) %>% 
  mapview(zcol = "infra_6cat_none_abbrev")

#-------------Geocode Proctor Creek----------------------------------------#############
proctor_creek_trail = as_tibble("64 Edwin Pl NW, Atlanta, GA 30318") 
proctor_creek_trail2 = rename(proctor_creek_trail, address = value) #rename the variable so it has a name

proctor_creek_trail_geo = mutate_geocode(proctor_creek_trail2, address, force = TRUE)

#convert to sf.
proctor_creek_trail_geo_sf = proctor_creek_trail_geo %>%
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

#convert to a coordinate system that supports feet
#that has been working for other dat.a
proctor_creek_trail_geo_sf_ft = proctor_creek_trail_geo_sf %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "proctor_creek_trail")

st_crs(proctor_creek_trail_geo_sf_ft) #good. in feet.

#make a 1.5 mile radius
1.5*5280
proctor_creek_trail_geo_sf_ft_1.5mi =  proctor_creek_trail_geo_sf_ft %>%
  st_buffer(7920) %>%
  mutate(radius_name = "1.5-mi radius, proctor_creek_trail") %>%
  st_transform(4326)

#mapview(proctor_creek_trail_geo_sf_ft_1.5mi)
save(proctor_creek_trail_geo_sf_ft_1.5mi, file = "proctor_creek_trail_geo_sf_ft_1.5mi.RData")

#-------------Geocode Mason Mill Tennis Center---------------------------------------#############

library(sf)
library(ggmap)
mason_mill_tennis = as_tibble("DeKalb Tennis Center") 
mason_mill_tennis2 = rename(mason_mill_tennis, address = value) #rename the variable so it has a name
mason_mill_tennis_geo = mutate_geocode(mason_mill_tennis2, address, force = TRUE)
#convert to sf.
mason_mill_tennis_geo_sf = mason_mill_tennis_geo %>%
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

#convert to a coordinate system that supports feet
#that has been working for other dat.a
mason_mill_tennis_geo_sf_ft = mason_mill_tennis_geo_sf %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "mason_mill_tennis")

st_crs(mason_mill_tennis_geo_sf_ft) #good. in feet.

#make a 1 mile radius
1*5280
mason_mill_tennis_geo_sf_ft_1mi =  mason_mill_tennis_geo_sf_ft %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, mason_mill_tennis") %>%
  st_transform(4326)

mapview(mason_mill_tennis_geo_sf_ft_1mi)

save(mason_mill_tennis_geo_sf_ft_1mi, file = "mason_mill_tennis_geo_sf_ft_1mi.RData")

# load("all_h_osm_wrangle_both_geo.RData")
# all_h_osm_wrangle_both_geo %>% 
#   dplyr::select(starts_with("osm_"),
#                 starts_with("project")) %>% 
#   st_intersection(mason_mill_tennis_geo_sf_ft_1mi) %>% 
#   mapview()

#-------------Grant Park---------------------------------------#############
grant_park = as_tibble("Grant Park, Atlanta, GA") 
grant_park2 = rename(grant_park, address = value) #rename the variable so it has a name
grant_park_geo_sf = grant_park2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "grant_park")

#mapview(grant_park_geo_sf)
grant_park_geo_sf_1mi =  grant_park_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, grant_park") %>%
  st_transform(4326)

5280*2
grant_park_geo_sf_2mi =  grant_park_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, grant_park") %>%
  st_transform(4326)


# mapview(grant_park_geo_sf_1mi)
# mapview(grant_park_geo_sf_2mi)
save(grant_park_geo_sf, file = "grant_park_geo_sf.RData")
save(grant_park_geo_sf_1mi, file = "grant_park_geo_sf_1mi.RData")
save(grant_park_geo_sf_2mi, file = "grant_park_geo_sf_2mi.RData")

#-------------Deepdene Park---------------------------------------#############
deepdene_park = as_tibble("Deepdene Park, Atlanta, GA") 
deepdene_park2 = deepdene_park %>% 
  rename(address = value) #rename the variable so it has a name
deepdene_park_geo_sf = deepdene_park2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "deepdene_park")

#mapview(deepdene_park_geo_sf)
deepdene_park_geo_sf_1mi =  deepdene_park_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, deepdene_park") %>%
  st_transform(4326)

deepdene_park_geo_sf_2mi =  deepdene_park_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, deepdene_park") %>%
  st_transform(4326)


#mapview(deepdene_park_geo_sf_1mi)
# mapview(deepdene_park_geo_sf_2mi)
save(deepdene_park_geo_sf, file = "deepdene_park_geo_sf.RData")
save(deepdene_park_geo_sf_1mi, file = "deepdene_park_geo_sf_1mi.RData")
save(deepdene_park_geo_sf_2mi, file = "deepdene_park_geo_sf_2mi.RData")

#-------------Morningside Nature Preserve---------------------------------------#############
morningside_nature_pres = as_tibble("Morningside Nature Preserve, Atlanta, GA") 
morningside_nature_pres2 = morningside_nature_pres %>% 
  rename( address = value) #rename the variable so it has a name

morningside_nature_pres_geo_sf = morningside_nature_pres2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "morningside_nature_pres")

#mapview(morningside_nature_pres_geo_sf)
morningside_nature_pres_geo_sf_1mi =  morningside_nature_pres_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, morningside_nature_pres") %>%
  st_transform(4326)

morningside_nature_pres_geo_sf_2mi =  morningside_nature_pres_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, morningside_nature_pres") %>%
  st_transform(4326)


# mapview(morningside_nature_pres_geo_sf_1mi)
# mapview(morningside_nature_pres_geo_sf_2mi)
save(morningside_nature_pres_geo_sf, file = "morningside_nature_pres_geo_sf.RData")
save(morningside_nature_pres_geo_sf_1mi, file = "morningside_nature_pres_geo_sf_1mi.RData")
save(morningside_nature_pres_geo_sf_2mi, file = "morningside_nature_pres_geo_sf_2mi.RData")

# Orme Park-----------

#back gnard------#######
names(all_h_osm_wrangle_both_geo)
back_gnard_sf_1mi =all_h_osm_wrangle_both_geo %>% 
  filter(osm_id_osm == 605932975) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_buffer(5280) %>%
  st_transform(4326)
save(back_gnard_sf_1mi, file = "back_gnard_sf_1mi.RData")


#-----skyhaven-----####
skyaven_dirt = as_tibble("Skyhaven  Rd SE, Atlanta, GA") 
skyaven_dirt2 = rename(skyaven_dirt, address = value) #rename the variable so it has a name

skyaven_dirt_geo_sf = skyaven_dirt2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "skyaven_dirt")

#mapview(skyaven_dirt_geo_sf)
skyaven_dirt_geo_sf_1mi =  skyaven_dirt_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, skyaven_dirt") %>%
  st_transform(4326)

skyaven_dirt_geo_sf_2mi =  skyaven_dirt_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, skyaven_dirt") %>%
  st_transform(4326)


# mapview(skyaven_dirt_geo_sf_1mi)
# mapview(skyaven_dirt_geo_sf_2mi)
save(skyaven_dirt_geo_sf, file = "skyaven_dirt_geo_sf.RData")
save(skyaven_dirt_geo_sf_1mi, file = "skyaven_dirt_geo_sf_1mi.RData")
save(skyaven_dirt_geo_sf_1mi, file = "skyaven_dirt_geo_sf_2mi.RData")

#-----Key Road (prison farm, etc.)-----####
key_road_se = as_tibble("Key  Rd SE, Atlanta, GA") 
key_road_se2 = rename(key_road_se, address = value) #rename the variable so it has a name

key_road_se_geo_sf = key_road_se2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "key_road_se")

#mapview(key_road_se_geo_sf)
key_road_se_geo_sf_1mi =  key_road_se_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, key_road_se") %>%
  st_transform(4326)

key_road_se_geo_sf_2mi =  key_road_se_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, key_road_se") %>%
  st_transform(4326)


# mapview(key_road_se_geo_sf_1mi)
# mapview(key_road_se_geo_sf_2mi)
save(key_road_se_geo_sf, file = "key_road_se_geo_sf.RData")
save(key_road_se_geo_sf_1mi, file = "key_road_se_geo_sf_1mi.RData")
save(key_road_se_geo_sf_2mi, file = "key_road_se_geo_sf_2mi.RData")

#-------------Lionel Hampton-Beecher Hills Park---------------------------------------#############
lionel_hampton = as_tibble("Lionel Hampton-Beecher Hills Park, Atlanta, GA") 
lionel_hampton2 = rename(lionel_hampton, address = value) #rename the variable so it has a name

lionel_hampton_geo_sf = lionel_hampton2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "lionel_hampton")

#mapview(lionel_hampton_geo_sf)
lionel_hampton_geo_sf_1mi =  lionel_hampton_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, lionel_hampton") %>%
  st_transform(4326)

lionel_hampton_geo_sf_2mi =  lionel_hampton_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, lionel_hampton") %>%
  st_transform(4326)


# mapview(lionel_hampton_geo_sf_1mi)
# mapview(lionel_hampton_geo_sf_2mi)
save(lionel_hampton_geo_sf, file = "lionel_hampton_geo_sf.RData")
save(lionel_hampton_geo_sf_1mi, file = "lionel_hampton_geo_sf_1mi.RData")
save(lionel_hampton_geo_sf_2mi, file = "lionel_hampton_geo_sf_2mi.RData")

#-------------Decatur---------------------------------------#############

decatur_center = as_tibble("Decatur, GA") 
decatur_center2 = rename(decatur_center, address = value) #rename the variable so it has a name

decatur_center_geo_sf = decatur_center2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "decatur_center")

#mapview(decatur_center_geo_sf)
decatur_center_geo_sf_1mi =  decatur_center_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, decatur_center") %>%
  st_transform(4326)

decatur_center_geo_sf_2mi =  decatur_center_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, decatur_center") %>%
  st_transform(4326)


# mapview(decatur_center_geo_sf_1mi)
# mapview(decatur_center_geo_sf_2mi)
save(decatur_center_geo_sf, file = "decatur_center_geo_sf.RData")
save(decatur_center_geo_sf_1mi, file = "decatur_center_geo_sf_1mi.RData")
save(decatur_center_geo_sf_2mi, file = "decatur_center_geo_sf_2mi.RData")

#-------------Five Points---------------------------------------#############

five_points = as_tibble("Five Points, GA") 
five_points2 = rename(five_points, address = value) #rename the variable so it has a name

five_points_geo_sf = five_points2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "five_points")

#mapview(five_points_geo_sf)
five_points_geo_sf_1mi =  five_points_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, five_points") %>%
  st_transform(4326)

five_points_geo_sf_2mi =  five_points_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, five_points") %>%
  st_transform(4326)


# mapview(five_points_geo_sf_1mi)
# mapview(five_points_geo_sf_2mi)
save(five_points_geo_sf, file = "five_points_geo_sf.RData")
save(five_points_geo_sf_1mi, file = "five_points_geo_sf_1mi.RData")
save(five_points_geo_sf_2mi, file = "five_points_geo_sf_2mi.RData")


#very small radius around west peachtree and 5th
#------biltmore hotel-----#######

biltmore_w_peachtree = as_tibble("855 W Peachtree St NW, Atlanta, GA 30308") 
biltmore_w_peachtree2 = rename(biltmore_w_peachtree, address = value) #rename the variable so it has a name

biltmore_w_peachtree_geo_sf = biltmore_w_peachtree2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "biltmore_w_peachtree")
biltmore_w_peachtree_geo_sf = biltmore_w_peachtree2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "biltmore_w_peachtree")

#mapview(biltmore_w_peachtree_geo_sf)
biltmore_w_peachtree_geo_sf_1mi =  biltmore_w_peachtree_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, biltmore_w_peachtree") %>%
  st_transform(4326)

biltmore_w_peachtree_geo_sf_halfmi =  biltmore_w_peachtree_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, biltmore_w_peachtree") %>%
  st_transform(4326)

save(biltmore_w_peachtree_geo_sf_halfmi, file = "biltmore_w_peachtree_geo_sf_halfmi.RData")

#-------park tavern (tenth and monroe)----------#####
#Park Tavern, 10th Street Northeast, Atlanta, GA
park_tavern = as_tibble("Park Tavern, 10th Street Northeast, Atlanta, GA") 
park_tavern2 = rename(park_tavern, address = value) #rename the variable so it has a name

park_tavern_geo_sf = park_tavern2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "park_tavern")
park_tavern_geo_sf = park_tavern2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "park_tavern")

#mapview(park_tavern_geo_sf)
park_tavern_geo_sf_1mi =  park_tavern_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, park_tavern") %>%
  st_transform(4326)

park_tavern_geo_sf_2mi =  park_tavern_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, park_tavern") %>%
  st_transform(4326)

park_tavern_geo_sf_halfmi =  park_tavern_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, park_tavern") %>%
  st_transform(4326)

save(park_tavern_geo_sf_halfmi, file = "park_tavern_geo_sf_halfmi.RData")
save(park_tavern_geo_sf_1mi, file = "park_tavern_geo_sf_1mi.RData")
save(park_tavern_geo_sf_2mi, file = "park_tavern_geo_sf_2mi.RData")

#--------goldsboro park - north and euclid freedom parkway----#########
goldsboro_park = as_tibble("Goldsboro Park, Atlanta, GA") 
goldsboro_park2 = rename(goldsboro_park, address = value) #rename the variable so it has a name


goldsboro_park_geo_sf = goldsboro_park2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "goldsboro_park")

#mapview(goldsboro_park_geo_sf)
goldsboro_park_geo_sf_1mi =  goldsboro_park_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, goldsboro_park") %>%
  st_transform(4326)

goldsboro_park_geo_sf_halfmi =  goldsboro_park_geo_sf %>%
  st_buffer(5280/2) %>%
  st_transform(4326)

5280*2
goldsboro_park_geo_sf_2mi =  goldsboro_park_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, goldsboro_park") %>%
  st_transform(4326)


#  mapview(goldsboro_park_geo_sf_1mi)
# mapview(goldsboro_park_geo_sf_2mi)
save(goldsboro_park_geo_sf, file = "goldsboro_park_geo_sf.RData")
save(goldsboro_park_geo_sf_1mi, file = "goldsboro_park_geo_sf_1mi.RData")
save(goldsboro_park_geo_sf_2mi, file = "goldsboro_park_geo_sf_2mi.RData")
save(goldsboro_park_geo_sf_halfmi, file = "goldsboro_park_geo_sf_halfmi.RData")


#----The Beltline and other bridges over off-street trails---########
trail_p_bridges = data.table::data.table(
  bridge_name = c("beltline_north"),
  lat = c(33.773348),
  lon = c(-84.364553)
  ) %>% 
  st_as_sf(
    crs=4326,
    coords = c("lon", "lat"))

trail_p_bridges %>% mapview()
mapview(trail_p_bridges)

# Emory Point-------
emory_point = as_tibble("Emory Point, Atlanta, GA") 
emory_point2 = emory_point %>% 
  rename(address = value) #rename the variable so it has a name
emory_point_geo_sf = emory_point2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "emory_point")

#mapview(emory_point_geo_sf)
emory_point_geo_sf_1mi =  emory_point_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, emory_point") %>%
  st_transform(4326)

emory_point_geo_sf_2mi =  emory_point_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, emory_point") %>%
  st_transform(4326)


mapview(emory_point_geo_sf_1mi)
mapview(emory_point_geo_sf_2mi)
save(emory_point_geo_sf, file = "emory_point_geo_sf.RData")
save(emory_point_geo_sf_1mi, file = "emory_point_geo_sf_1mi.RData")
save(emory_point_geo_sf_2mi, file = "emory_point_geo_sf_2mi.RData")

#check west buckhead -------
#Let's geocode underwood hills park
underwood_hills = as_tibble("Underwood Hills Park, Atlanta, GA") 
underwood_hills2 = underwood_hills %>% 
  rename(address = value) #rename the variable so it has a name
underwood_hills_geo_sf = underwood_hills2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "underwood_hills")

#mapview(underwood_hills_geo_sf)
underwood_hills_geo_sf_1mi =  underwood_hills_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, underwood_hills") %>%
  st_transform(4326)

underwood_hills_geo_sf_2mi =  underwood_hills_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, underwood_hills") %>%
  st_transform(4326)


mapview(underwood_hills_geo_sf_1mi)
mapview(underwood_hills_geo_sf_2mi)
save(underwood_hills_geo_sf, file = "underwood_hills_geo_sf.RData")
save(underwood_hills_geo_sf_1mi, file = "underwood_hills_geo_sf_1mi.RData")
save(underwood_hills_geo_sf_2mi, file = "underwood_hills_geo_sf_2mi.RData")


# Bankhead----
bankhead = as_tibble("Bankhead, Atlanta, GA") 
bankhead2 = bankhead %>% 
  rename(address = value) #rename the variable so it has a name
bankhead_geo_sf = bankhead2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "bankhead")

#mapview(bankhead_geo_sf)
bankhead_geo_sf_1mi =  bankhead_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, bankhead") %>%
  st_transform(4326)

bankhead_geo_sf_2mi =  bankhead_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, bankhead") %>%
  st_transform(4326)


mapview(bankhead_geo_sf_1mi)
mapview(bankhead_geo_sf_2mi)
save(bankhead_geo_sf, file = "bankhead_geo_sf.RData")
save(bankhead_geo_sf_1mi, file = "bankhead_geo_sf_1mi.RData")
save(bankhead_geo_sf_2mi, file = "bankhead_geo_sf_2mi.RData")

# Decatur-----
decatur = as_tibble("Decatur, GA") 
decatur2 = decatur %>% 
  rename(address = value) #rename the variable so it has a name
decatur_geo_sf = decatur2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "decatur")

#mapview(decatur_geo_sf)
decatur_geo_sf_1mi =  decatur_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, decatur") %>%
  st_transform(4326)

decatur_geo_sf_2mi =  decatur_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, decatur") %>%
  st_transform(4326)


mapview(decatur_geo_sf_1mi)
mapview(decatur_geo_sf_2mi)
save(decatur_geo_sf, file = "decatur_geo_sf.RData")
save(decatur_geo_sf_1mi, file = "decatur_geo_sf_1mi.RData")
save(decatur_geo_sf_2mi, file = "decatur_geo_sf_2mi.RData")

# East Lake--------
east_lake = as_tibble("East Lake, Atlanta, GA") 
east_lake2 = east_lake %>% 
  rename(address = value) #rename the variable so it has a name
east_lake_geo_sf = east_lake2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "east_lake")

#mapview(east_lake_geo_sf)
east_lake_geo_sf_1mi =  east_lake_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, east_lake") %>%
  st_transform(4326)

east_lake_geo_sf_2mi =  east_lake_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, east_lake") %>%
  st_transform(4326)


mapview(east_lake_geo_sf_1mi)
mapview(east_lake_geo_sf_2mi)
save(east_lake_geo_sf, file = "east_lake_geo_sf.RData")
save(east_lake_geo_sf_1mi, file = "east_lake_geo_sf_1mi.RData")
save(east_lake_geo_sf_2mi, file = "east_lake_geo_sf_2mi.RData")

## Edgewood, Atlanta-------
edgewood = as_tibble("Edgewood, Atlanta, GA") 
edgewood2 = edgewood %>% 
  rename(address = value) #rename the variable so it has a name
edgewood_geo_sf = edgewood2 %>% 
  mutate_geocode(address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  mutate(geocoded_name = "edgewood")

#mapview(edgewood_geo_sf)
edgewood_geo_sf_1mi =  edgewood_geo_sf %>%
  st_buffer(5280) %>%
  mutate(radius_name = "1-mi radius, edgewood") %>%
  st_transform(4326)

edgewood_geo_sf_2mi =  edgewood_geo_sf %>%
  st_buffer(10560) %>%
  mutate(radius_name = "1-mi radius, edgewood") %>%
  st_transform(4326)


mapview(edgewood_geo_sf_1mi)
mapview(edgewood_geo_sf_2mi)
save(edgewood_geo_sf, file = "edgewood_geo_sf.RData")
save(edgewood_geo_sf_1mi, file = "edgewood_geo_sf_1mi.RData")
save(edgewood_geo_sf_2mi, file = "edgewood_geo_sf_2mi.RData")

