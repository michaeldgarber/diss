# Wrangle OSM data to prep for Aim 3

#filename: 1_wrangle_osm
#Revised 9/28/2020
# Revised 12/4/21 for aim 1
# Here 9/2/22 to clean up headings

# Load from here per 9/26/2020 work-------- 
library(tidyverse)
library(lubridate)
library(sf)
library(raster)
library(mapview) #loads leeaflet.
library(readxl)
library(RColorBrewer)
library(viridis)
library(viridisLite)
library(here) #update 12/16/21


#Created ~/Dropbox/Work/General research/Dissertation/diss/scripts/0_import_prep_osm.R
setwd(here("data-processed"))
load(file = "all_highway_dupes_rid.RData") 

#see 0_import_prep for dupe removal code, and if you want to keep more OSM variables,
#you could include them there


names(all_highway_dupes_rid)
# create all_h_osm_wrangle_geo---------------
all_h_osm_wrangle_geo = all_highway_dupes_rid %>% 
  #create a numeric OSM field. the 'osm' subscript is useful to indicate that the id comes from
  #'osm' directly (I pulled it directly) rather than the corresponding field that Strava pulled (osm_id_strava)
  #useful to differentiate the aspatial joins with the spatial join
  mutate(
    osm_id_osm = as.numeric(as.character(osm_id)),
    osm_name_osm = name, #again to differentiate from OSM_NAME in the Strava file
    osm_indicator =1 ) %>%      #an indicator for when you join with strava

  #drop osm_id and name to avoid ambiguity
  dplyr::select(-osm_id, -name) %>% 
  
#  table(all_h_osm_wrangle_both_geo$highway)
  ## highway classification, incl. major vs. residential----------
  # For posterity, I'm keeping this highway classification, as it's used throughout
  # subsequent code ... but it could be more precise and improved,
  # as I do in the 2_wrangle_basemap code
  mutate( 
    highway_original = highway , #keep the original version around in case you need it.
    
    #a few that need to be manually coded early on so they are coded correctly below.
    #(realized by checking and re-running 10/11/2020)
    highway = case_when(
      
      #this one was coded as unclassified and should be residential
      osm_id_osm == 507013986 ~ "residential",
      
      #this happens quite a bit. a protected cycletrack gets its own geometry 
      #(not coded as a road),
      #so then when I want to classify it by roadway, 
      #I lose its roadway classification.
      
      grepl("Portman PATH", osm_name_osm) ~ "tertiary",
      grepl("Peachtree Center Cycle Track", osm_name_osm) ~ "tertiary",
      
      #PATH parkway along luckie st is a little different, since it does go off the street
      #briefly, but I still think it's worth coding as whatever luckie st is
      osm_id_osm == 179238152 ~ "tertiary",
      
      #west trinity place in decatur and west ponce also should be tertiary.
      #it is a conventional bike lane
      osm_id_osm == 414497325 ~ "tertiary",
      osm_id_osm == 414497300 ~ "tertiary",
      osm_id_osm == 414497321 ~ "tertiary",
      osm_id_osm == 414497304 ~ "tertiary",
      osm_id_osm == 414497330 ~ "tertiary",
      osm_id_osm == 414497315 ~ "tertiary",
      osm_id_osm == 414497333 ~ "tertiary",
      
      #same thing on the prado at ansley park. should be residential
      osm_id_osm == 415536346 ~ "residential",
      osm_id_osm == 415536348 ~ "residential",
      osm_id_osm == 415536349 ~ "residential",
      
      #pryor st downtown - near buffered bike lane
      osm_id_osm == 341473058 ~ "tertiary",

      
      TRUE ~ highway_original) ,
    
    #make an indicator for those.
    highway_parallel_recode = case_when(
      grepl("Portman PATH", osm_name_osm) ~ 1,
      grepl("Peachtree Center Cycle Track", osm_name_osm) ~ 1,
      osm_id_osm == 179238152 ~ 1,
      osm_id_osm == 414497325 ~ 1,
      osm_id_osm == 414497300 ~ 1,
      osm_id_osm == 414497321 ~ 1,
      osm_id_osm == 414497304 ~ 1,
      osm_id_osm == 414497330 ~ 1,
      osm_id_osm == 414497315 ~ 1,
      osm_id_osm == 415536346 ~ 1,
      osm_id_osm == 415536348 ~ 1,
      osm_id_osm == 415536349 ~ 1,
      osm_id_osm == 341473058 ~ 1,
      osm_id_osm == 414497333 ~ 1,
      
      TRUE ~ 0) ,

  #collapse the highway variable to fewer categories, so it's easier to visualize
    highway_6cat = case_when(
      
      #needle, haystack
     grepl("primary", highway) ~ "primary-tertiary road",
     grepl("trunk", highway) ~ "trunk road",
     grepl("secondary", highway) ~ "primary-tertiary road",
     #note that this includes interstates here but it wont'eventually
     #recoded from interstate v high speed road
     grepl("motorway", highway) ~ "trunk road", 
     grepl("tertiary", highway) ~ "primary-tertiary road",
     grepl("residential", highway) ~ "residential road",
     grepl("cycle", highway) ~ "path - paved or not",
     grepl("foot", highway) ~ "path - paved or not",
     grepl("path", highway) ~ "path - paved or not",
     grepl("pedestr", highway) ~ "path - paved or not", #pedestrian
     grepl("track", highway) ~ "path - paved or not",
     grepl("uncla", highway) ~ "unclassified or service",
     grepl("service", highway) ~ "unclassified or service",
     grepl("living", highway) ~ "living street",
     TRUE ~ NA_character_)
    ,
    #another one
    highway_9cat = case_when(
      grepl("primary", highway) ~ "primary road",
      grepl("trunk", highway) ~ "trunk road",
      grepl("secondary", highway) ~ "secondary road",
      grepl("motorw", highway) ~ "trunk road",
      grepl("tertiary", highway) ~ "tertiary road",
      grepl("residential", highway) ~ "residential road",
      grepl("cycle", highway) ~ "cycleway",
      grepl("foot", highway) ~ "path - paved or not",
      grepl("path", highway) ~ "path - paved or not",
      grepl("pedestr", highway) ~"path - paved or not",  #pedestrian
      grepl("track", highway) ~ "path - paved or not",
      grepl("uncla", highway) ~ "unclassified or service",
      grepl("service", highway) ~ "unclassified or service",
      grepl("living", highway) ~ "living street",
      TRUE ~ NA_character_)
    ,
    
    #the aim 2 major or res variable. Make it here instead per 10/5/2020. 
    major_or_res = case_when(
      highway_6cat == "primary-tertiary road" ~ 1,
      highway_6cat == "residential road" ~ 0),
    #convert to integer for speed
    major_or_res = as.integer(major_or_res),
  
  #make a version that is categorical letting me know what the highway cat is if missing
  major_or_res_highway_all = case_when(
    highway_6cat == "primary-tertiary road" ~ "primary-tertiary road",
    highway_6cat == "residential road"~ "residential road",
    TRUE ~ highway),
  
  major_or_res_highway_6cat = case_when(
    highway_6cat == "primary-tertiary road" ~ "primary-tertiary road",
    highway_6cat == "residential road"~ "residential road",
    TRUE ~ highway_6cat),
  
    major_or_res_other = case_when(
    highway_6cat == "primary-tertiary road" ~ "primary-tertiary road",
    highway_6cat == "residential road"~ "residential road",
    TRUE ~ "other"),
    

  #add names to a few osm_ids so that they can be more dynamically 
  #classified by that nme
  #below
    osm_name_osm = 
      case_when(
        osm_id_osm == 23217746 ~ "Trolley Line Trail",
        osm_id_osm == 169957934 ~ "Freedom Park Connector",
        osm_id_osm == 24617423 ~ "Freedom Park Connector",
        osm_id_osm == 24617426 ~ "Freedom Park Connector",
        osm_id_osm == 764043743 ~ "Atlanta BeltLine Eastside Trail",
        TRUE ~ osm_name_osm
      ),
    
    beltline = case_when(
      grepl("beltline", osm_name_osm) ~ 1,
      grepl("BeltLine", osm_name_osm) ~ 1,
      grepl("Beltline", osm_name_osm) ~ 1,
      grepl("Belt Line", osm_name_osm) ~ 1,
      grepl("belt line", osm_name_osm) ~ 1,
      TRUE ~ 0 ),
  
  ## 2017 Atlanta Report and Dissertation Aim 1 infrastructure----------------------------
  ### Westside trail-------------------------------------------------------------------
  #except for the little dirt part, but this happens to work (see 'Westside Interim' below)
  #12/7/21 this actually works to exclude the unpaved part.
  project_westside_trail_paved = case_when(
    grepl("Atlanta BeltLine Westside Trail", osm_name_osm) ~ 1
  ),
  
  #12/7/21 note you can be more specific here ("dirt"), 
  #actually, to differentiate the paved from the unpaved
  project_westside_trail_dirt_interim = case_when(
    osm_id_osm == 669818486 ~ 1,
    TRUE ~ 0),
  #this is softer code than
    # grepl("Westside Interim", osm_name_osm)==TRUE ~ 1,
    # TRUE ~ 0),

  ### Tech Parkway and Luckie St -----------------------------------------------------
  #Tech Parkway
  #Note, the search-term-based way would work, using osm_name_osm contains PATH Parkway
  #but to be more specific, I'm doing it this way
  project_tech_parkway = case_when(
    osm_id_osm %in% c(
      179237451,
      179237448,
      179238151,
      526759934 #I missed this one - added June 1, 2023
    ) ~ 1,
  ),

  #annoyingly, this segment crosses North Ave. oh well, this will be your luckie st section.
  #Update 10/21/2020 - you update these downstream in the wrangle_basemap code.
  project_luckie_st_lane_protected = case_when( 
    osm_id_osm %in% c(
      310948306,
      340371479,
      179238152,
      41374791
          ) ~ 1
    ),

  #because they are viewed as a joint intervention.
  project_path_parkway  = case_when(
    project_tech_parkway==1 ~ 1,
    project_luckie_st_lane_protected==1 ~ 1 )  ,
  
  ### Eastside trail extension -------------------------------------------------
  #there is a duplicate just south of irwin st to edgewood
  # one stops at edgewood - 7764043743 - maybe get rid of this.
  #but don't get rid of it until you link with Strava because there may have been some 
  #ridership that was snapped to it.
  #and then there is this with goes under the bridge - 722838793 - seems more legit, 
  #but doesn't stop at edgewood
  
  #pasting in some of your other code for reference
  # NAME == "D_EastsideTrail_WylieExtension" ~ "est1", #this one opened first. numbering by date.
  # NAME == "D_EastsideTrail_IrwintoEdgewood" ~ "est2", #then this part.
  # NAME == "D_EastsideTrail_WylietoKirkwood" ~ "est3", #and finally this one.
  # NAME == "D_EastsideTrail_Kwood2Memorial" ~ "est4",  #and this is still dirt as of spring 2019
  #the eastside trail new segments
  
  #the problem with this is it was closed at the edgewood bridge for quite some time.
  #may need to import the version I drew, instead. Trying to minimize that to keep this open source
  #it opened all the way to dekalb avenue 2/8/2019
  project_eastside_trail_irwin_to_dekalb = case_when(
    osm_id_osm == 722838793 ~1 ),
  
  project_eastside_trail_wylie_extension = case_when(
      osm_id_osm == 601780166 ~ 1 ),

  project_eastside_trail_krog_tunnel = case_when(
    osm_id_osm == 722838795 ~ 1 ),
  
  #this segment goes to Mauldin, not Kirkwood, but, in reality,
  #the segment went to kirkwood and was open briefly, and then there was a fence at kirkwood, so it makes
  #more sense to call it kirkwood for the temporal variable
  #But the main point is that it didn't open all the way to Memorial until after 2018
  project_eastside_trail_wylie_to_fulton_terr = case_when(
    osm_id_osm == 79430013 ~ 1  ),
  
  #NOT OPEN during dissertation timeframe! opened 7/11/2019
  project_eastside_trail_mauldin_to_memorial = case_when(
    osm_id_osm == 46184493 ~ 1,
    osm_id_osm == 46184492 ~ 1 ),
  
  #bill kennedy to the i-20 overpass
  project_eastside_trail_bk_to_i20 = case_when(
    osm_id_osm == 722838134 ~ 1 ),
  
  #the OG beltline opened 10/1/2012, by the way. create an indicator for the OG eastside trail and the extension
  project_eastside_trail_s_extension = case_when(
    project_eastside_trail_irwin_to_dekalb == 1 ~ 1,
    project_eastside_trail_wylie_to_fulton_terr == 1 ~ 1,
    project_eastside_trail_wylie_extension == 1 ~ 1,
    project_eastside_trail_mauldin_to_memorial == 1 ~ 1,
    project_eastside_trail_krog_tunnel==1 ~1,
    project_eastside_trail_bk_to_i20 == 1 ~ 1,
    TRUE ~0 #don't always have to use a 0, but it's helpful here
      ),
  
  #this will pick up any of the paved sections, 
  #so excluding the dirt section in Piedmont Park
  project_eastside_trail_paved = case_when(
    grepl("Atlanta BeltLine Eastside Trail", osm_name_osm) ~ 1 ,
    TRUE ~0),
  
  #and then to get the OG 2012 beltline (10/1/2012), use this
  project_eastside_trail_2012 = case_when(
    project_eastside_trail_paved==1 & project_eastside_trail_s_extension == 1 ~ 0,
    project_eastside_trail_paved==1 ~ 1
    ),
  
  ### South Peachtree Creek extension---------------------------------------------------
  #the new segments going towards n druid hills
  # 740764236 740764229 740764228 740764237 740764227
  
  #the segment along clairmont lake 606130772 740764190 - done later
  #the segment that goes to the home - I think this was done around the same time as the seg to n druid hills 606130770
  #segment under clairmont to lullwater - 606130774
  #    name_section_long == "S Peachtree Creek Trail - MM Park to NDH Rd" ~ lubridate::ymd(20170624)
  project_s_peachtree_creek_mm_to_ndh = case_when(
    osm_id_osm == 740764236 ~ 1,
    osm_id_osm == 740764229 ~ 1,
    osm_id_osm == 740764228 ~ 1,
    osm_id_osm == 740764237 ~ 1,
    osm_id_osm == 740764227 ~ 1
  ),
  #the intersection that spurs off of the other trail and goes towards the retiremnet home and the lake
  #until the boardwalk
  project_s_peachtree_creek_int_to_clairmont_lake = case_when( osm_id_osm == 606130770 ~ 1),
  project_s_peachtree_creek_clairmont_lake = case_when(
    osm_id_osm == 606130772 ~ 1,
    osm_id_osm == 740764190 ~ 1
    ),
  project_s_peachtree_creek_bridge_under_clairmont = case_when(osm_id_osm == 606130774 ~ 1),
  
  ### McDonough St Protected Bike Lane ---------------------------------------------------
  project_n_mcdonough_st_lane_protected = case_when(osm_id_osm == 84395983 ~ 1),
  
  ### RDA buffered lane (Dec 2018) ------------------------------
  #"RDA Buffered Bike Lane" ~ lubridate::ymd(20171201),#rough guess in december 1, 2017
  #okay, we need Ralph David Abernathy Buffered Bike Lane - Cascade to MLK
  # 112561983 629305886 146842406 629305885 181272963
  #12/17/21 you can look on street view and correct this. it's actually not all a buffered lane
  #split this up into conventional vs buffered
  #rename this. was called project_rda_bike_lane_buff_cascade_mlk.
  #now call break in two and call them
  #project_rda_cascade_mlk_bike_lane_buff
  #project_rda_cascade_mlk_bike_lane_conv
  project_rda_cascade_mlk_bike_lane_conv = case_when(
    osm_id_osm ==112561983 ~ 1, #section just nw of cascade. 
    osm_id_osm ==629305886 ~ 1, #nw to lucile. almost 100% conventional. one tiny part with a buffer
    TRUE ~ 0),
  project_rda_cascade_mlk_bike_lane_buff = case_when(
    #actually a buffered lane. intermittent, but mostly buffered.
    #note at ontario where it approaches the cemetery, there's a trail on the west side
    #and only a buffer on the east side of the road (nw bound)
    #you lose the buffer at the very end of it, where racine st comes in.
    #it's a conventional lane to lake ave near i-20, but osm segments aren't short enough,
    #and then it gets buffered again to MLK
    osm_id_osm ==146842406 ~ 1, 
    osm_id_osm ==629305885 ~ 1, #between lake ave and MLK
    osm_id_osm ==181272963 ~ 1 #between lake ave and MLK
    ),
  
  #create a single indicator for date-related things. assume all done about dec 2018
  project_rda_cascade_mlk_bike_lane_dec2018 = case_when(
    project_rda_cascade_mlk_bike_lane_conv==1 ~1,
    project_rda_cascade_mlk_bike_lane_buff==1~1,
    TRUE ~0
  ),

  ## 2018 Atlanta report-----------------------------------------------------------
  #https://www.atlantaga.gov/home/showdocument?id=40599
  #Useful to group them as an indicator so you can apply date easier
  #(Beginning them with a project prefix so they'll be easier to grab in dplyr::select)
  #(I didn't refer to beltline as a project_ because it's special)
    project_milton_ave_bike_lane_conv = case_when(
      osm_id_osm == 564186903 ~ 1),
    
    #Sylvan Road
    #(I renamed this _buff_conv as a reminder that part of it is buffered)
    #project_sylvan_bike_lane_conv
    project_sylvan_bike_lane_buff_conv = case_when(
      osm_id_osm == 9253163 ~ 1,
      #part of this segment is a buffered bike lane..north of Westside Trail to Murphy
      #12/17/21 changed in the edges in the basemap code
      osm_id_osm == 563533835 ~ 1),
    
  #hard-code Ivan Allen Gateway (February 2018)
    project_ivan_allen_gateway = case_when(
      osm_id_osm == 306007056 ~ 1,
      osm_id_osm == 79387240 ~ 1,
      osm_id_osm == 79387238 ~ 1,
      osm_id_osm == 169925545 ~ 1,
      osm_id_osm == 116942278 ~ 1),
  
    #ralph david abernathy - march 2018 - buffered
    #per p. 15 of report: https://www.atlantaga.gov/home/showdocument?id=40599
    #12/17/21 streetview confirmation. there is something that starts at pullman right underneath the connector
  #and then heads east a bit. this current classification is way too generous.
  #I need to chop this up further in the wrangle_basemap code
  #this does more harm than good. define it instead in the basemap-based code.
    # project_rda_bike_lane_buff_2018mar = case_when(
    #   osm_id_osm == 9276119 ~ 1,
    #   osm_id_osm == 442591111 ~ 1,),
      
    #note, there should be another RDA buffered zone, too, per aim 1 infra (yup, above)
  
    #lawton st -1 mile. see below; some of this is sharrow on one side. assume
    #classify it as the following: if it's a sharrow/conventional, call it conventional
    project_lawton_bike_lane_conv = case_when(
      osm_id_osm == 116945108 ~ 1),
  
    #the report says May 2018 (This is aim 1 dissertation infrastructure; that's OK)
    project_proctor_creek_greenway = case_when(
      grepl("Proctor Creek G", osm_name_osm) ~ 1
    ),
    
  #august 2018
    project_college_ave_bike_lane_conv = case_when(
      osm_id_osm ==  74920286 ~ 1,
      osm_id_osm ==       41505331 ~ 1
    ),

    #ormewood and united around the same time - GDOT work
    project_ormewood_bike_lane_buff = case_when(
      osm_id_osm == 507017527 ~ 1 ),
  
    project_ormewood_sharrow = case_when( #between woodland and moreland
      osm_id_osm == 9263148 ~ 1 ),
  
    #united - GDOT work summer 2018
    #12/16/21 confirmed this is just a sharrow, not a bike_lane_conv
    #I had previously called this project_united_bike_lane_conv_2018summe 
    project_united_sharrow_2018summer = case_when(
      osm_id_osm == 9242595 ~ 1,
      osm_id_osm == 630644164 ~1 ),
  
    #classify the remaining bike alnes on United as different
    #because they are in worse shape. this goes to woodland ave se
    project_united_bike_lane_conv_old = case_when(
      osm_id_osm == 821504528 ~1,
      osm_id_osm == 507017523 ~ 1,
      osm_id_osm == 9266774 ~ 1   ),
    
    #northwest beltline connector October 2018
    project_bitsy_grant_trail = case_when(
      grepl("Bitsy Grant Connector", osm_name_osm) ~ 1
    ),
  
    #marietta st corridor
    project_marietta_st_bike_lane_conv_2018Dec = case_when(
      osm_id_osm == 169925546 ~1,
      osm_id_osm == 111663207 ~1 ),
  
  
  ## PATH 400-----------------------------
  #10/11/2020 to-do: this includes proposed. don't worry for now
  #12/4/21 making this more specific
    #hmm, so the osm_id_osm isn't granula renough to differntiate
    #between the completed and in-development phases.
    #going to have to define it at the edge-id level
  #leave this here so that you can, in general, still
  #define path 400 as off-street paved trail at some point in the future
  #but it will need to be defined more specifically by time period
  project_path_400 = case_when(
    grepl("PATH 400", osm_name_osm) ~ 1,
    grepl("PATH400", osm_name_osm) ~ 1 
  ),
  
  ## Clifton corridor - changed summer 2017--------
  #https://news.emory.edu/stories/2016/06/er_clifton_streetscape/campus.html
  #From that page:
  #The Clifton Streetscape project will make travel to Emory facilities both 
  #more efficient and more enjoyable, with improvements for vehicles, bicycles 
  #and pedestrians.
  
  #An almost half-mile segment of Clifton Road will be resurfaced and widened, 
  #and a bike lane will be added along the east side. A "sharrow," or shared bike-vehicular lane, 
  #will remain on the west side of Clifton Road.
  #  Completion of the Clifton Streetscape will coincide with the summer 2017 opening of the 
  #new hospital wing.
  #Before August 2017, these were sharrows
  #After, there were some bike lanes and sharrows
  project_clifton_n_decatur_to_e_row_before_201708=case_when(
    #Before, they were just sharrows. After, there was a sharrow on the southbound
    #side and a bike lane on the northbound.
    #I can't accommodate both types of lanes in my data right now, so I'm going to 
    #call it a bike lane after and a sharrow before.
    #This project goes from N Decatur to Eagle Row
    osm_id_osm %in% c(
      28687906,# N Decatur to Uppergate Dr
      80428422 #Uppergate to Eagle Row
            )~ 1),
  
  #these are forever sharrows
  project_clifton_e_row_to_emory_conf_hotel=case_when(
    osm_id_osm%in% c(
      40547249,# Just north of Haygood
      95340819, #Michael St to Houston Mill
      95340818# Houston Mill to Emory Conf Hotel
    )~ 1),
  
  #Looks like as of Jul 2015 (before study),
  #Clifton gets a bike lane westbound (Emory Point side)
  #and keeps its sharrow eastbound (CDC side)
  #This could be another example of terrible design in your discussion section.
  #actually it's mostly a bike lane to Emory Point northeast bound
  project_clifton_emory_conf_hotel_to_emory_point=case_when(
    osm_id_osm %in% c(
      #northwest of 1599 building - Emory Conf hotel
      733656907,#north side of road
      80428423, #south side of road
      525755399# continuing west
    )~ 1),
  #This is mostly a sharrow but contains a bit of
  #conventional bike lane on the eastern part east of CDC parkway.
  #It will have to be fixed in the subsequent code.
  project_clifton_emory_point_to_briarcliff=case_when(
    osm_id_osm %in% c(
      566526999
    )~ 1),
  
  ## 10th (tenth) st protected bike lane-----
  #June 1, 2023
  #In my fixes, I seem to have lost part of the 10th st
  #lane
  project_10th_st_protected_lane=case_when(
    osm_id_osm %in% c(
      505912700,75359071
    )~ 1),

  ## Peachtree Center protected bike lane-----
  #June 1, 2023 
  #There was a crash that occurred on this cycle track,
  #but it didn't get classified as occurring on it, 
  #because there are two parallel segments.
  #I need to classify everything as having occurred on
  #the protected bike lane, and include one of the lines for length
  #calling it east because there are two parallel features,
  #and the one on the west is already coded as protected.
  #I have to assume that all ridership occurring
  #on these segments occurred in the pbl, as I've done for 10th St.
  #I'm surprised I haven't done this yet.
  project_peachtree_center_pbl_east=case_when(
    osm_id_osm %in% c(
      108618203,
      595386368,
      595386367,#added June 5th 2023
      591623771,
      630696899
        )~1),
  
  ##Ferst Dr NW----------
  #A missing conventional bike lane segment - Ferst Dr
  #Noticing this June 1, 2023
  projet_ferst_dr_nw_conv=case_when(
    osm_id_osm %in% c(
    9240894,
    179238154,
    525768628,#was coded as a sharrow
    314113138,#was coded as a sharrow
    526762112#this was coded as a sharrow. it's a conv. lane
    )~1),
  
  project_ferst_dr_nw_sharrow=case_when(
    osm_id_osm %in% c(
    314113141,#this is a great sharrow
    314113140,
    179238153,#at the intersection a sharrow
    278751254,#technically a sharrow here
    525767405
    )~1),
  
  ## Sharrows on Ponce and other in Decatur--------
  #adding these June 3, 2023
  project_sharrows_decatur=case_when(
    #Ponce sharrows from E Trinity Place to
    #Sam's Crossings
    osm_id_osm %in% c(
        85333205,
        329993826,
        80428429,
        196664358,
        569509214,
        9181217,
        730129063,
        74846297,
        43308963,
        
        #Sycamore Dr North of Ponce in Decatur had a sharrow
        #Mar 2017
        #Assuming all the way to Church
        #June 3, 2023
        9171974,
        9171973,
        77078502,
        74837999,
        630069543

    )~1),
  
  ## Sharrows - southeast Atlanta---------
  project_sharrows_cottage_grove_oakview_se=case_when(
    osm_id_osm %in% c(
      #June 3, 2023
      #I checked Glendale. No sharrows.
      #McDonough St has sharrows south of College
      #at least south to Griffin Cir. Until the road
      #name changes
      #South McDonough St
      9188301,
      74927685,
      74819491,
      
      #Cottage Grove Ave SE
      #looks like sharrows added somewhere Late 2016
      #early 2017
      #And Oakview Rd SE as well got sharrows about the same
      #time. Both visible in Oct 2017 Street View
      9170117,
      51297081,#Oakview
      51297456,
      51297464,
      143309391
    )~1),
  
  #I checked Carter Ave SE
  
  #Note I tried to add the missing section of the Trolley Line Trail
  #near El Tesoro
  #between Montgomery St Se and Anniston Ave SE,
  #but the segment didn't appear in my basemap layer


  

  ## Dirt trails (projects and groups)-------------------------------------
  #Ira B and Mason Mill Trails across Clairmont
  #Ira B and Mason Mill Trails
  project_ira_b_mason_mill = case_when( 
    #some hardcoding in ira b / mason mill area (most of these I drew)
    osm_id_osm == 72602019 ~ 1,
    osm_id_osm == 72617733 ~ 1,
    osm_id_osm == 605940197 ~ 1,
    osm_id_osm == 72604128 ~ 1,
    osm_id_osm == 148748569 ~ 1,
    osm_id_osm == 148748570 ~ 1,
    osm_id_osm == 72603370  ~ 1,
    
    grepl("Ira B", osm_name_osm)    ~ 1,
    grepl("Ira b", osm_name_osm)   ~ 1,
    grepl("ira b", osm_name_osm)   ~ 1,
     highway == "path" & 
      ( grepl("Mason Mill", osm_name_osm) |
          grepl("11 swit", osm_name_osm)  |
          grepl("Privet U", osm_name_osm)) ~ 1),

  project_lullwater_dirt = case_when(
    osm_id_osm == 6462683 ~ 1, #this one is gravel and is quite official
    osm_id_osm == 25397744 ~ 1,
    osm_id_osm == 605940190 ~ 1,
    osm_id_osm == 606130776 ~ 1,
    osm_id_osm ==352901441 ~ 1,
    osm_id_osm == 72914777 ~ 1,
    osm_id_osm == 25601348 ~ 1,
    osm_id_osm == 352901444 ~ 1,
    osm_id_osm == 25397785 ~ 1,
    osm_id_osm == 352901443 ~ 1,
    osm_id_osm ==239883839 ~ 1,
    osm_id_osm == 352901442 ~ 1,
    osm_id_osm == 25397717 ~ 1,
    osm_id_osm == 25508448 ~ 1
  ),
  
  project_morningside_nature_pres = case_when(
    osm_id_osm == 135206796 ~ 1,
    osm_id_osm == 135206785 ~ 1,
    osm_id_osm == 292496725 ~1,
    osm_id_osm == 135206786 ~ 1,
    osm_id_osm == 292496723 ~ 1,
    osm_id_osm == 292496724 ~ 1,
    osm_id_osm == 135206808 ~ 1,
    osm_id_osm == 135206810 ~ 1,
    osm_id_osm == 292496729 ~ 1,
    grepl("Morningside alt", osm_name_osm)  ~ 1 #morningside alt entrance that I mapped
  ),
  
  project_lenox_wildwood = case_when(
    osm_id_osm == 135413056 ~ 1,
    osm_id_osm == 135413058 ~ 1
  ),
  
  project_johnson_taylor = case_when(
    osm_id_osm == 136915611 ~ 1,
    osm_id_osm == 136926131 ~ 1,
    osm_id_osm == 136926132 ~ 1,
    osm_id_osm == 136915612 ~ 1,
    osm_id_osm == 605939676 ~ 1, #I think I coded this one
    osm_id_osm == 605939675 ~ 1
  ),
  
  #some dirty mustache trails that I'm missing
  #kirkwood urban forest
  #children's farm
  #skyhaven if it's theere
  project_intrenchment_prison_farm = case_when(
    osm_id_osm == 617832505 ~ 1,
    osm_id_osm == 393524419 ~ 1,
    osm_id_osm == 372416970 ~ 1,
    osm_id_osm == 372416971 ~ 1,
    osm_id_osm == 398074244 ~ 1,
    osm_id_osm == 398074239 ~ 1,
    osm_id_osm == 398074242 ~ 1,
    osm_id_osm == 372417529 ~ 1,
    
    osm_id_osm == 401312312 ~ 1,
    osm_id_osm == 372416909 ~ 1,
    osm_id_osm == 393523714 ~ 1,
    osm_id_osm == 372416909 ~ 1,
    osm_id_osm == 401312314 ~ 1,
    osm_id_osm == 372417531 ~ 1,
    osm_id_osm == 393523714 ~ 1,
    osm_id_osm == 393525604 ~ 1,
    osm_id_osm == 405782624 ~ 1
  ),
  

    #mountain bike trails
      #This pulls Southside Park and Sykes Park MTB. May not always work, though.
    mtb = case_when(grepl("MTB", osm_name_osm) ~ 1 ),
  
    mtb_less_official = case_when(
        osm_id_osm == 605939674 ~ 1, #under briarcliff road across the creek
        
        grepl("Trolley Line Tr Spur", osm_name_osm) ~ 1, #dirt trails in kirkwood
        grepl("Kirkwood Urban Forest", osm_name_osm) ~  1, 
        
        osm_id_osm == 605940187 ~ 1, #peavine creek spur that I drew
        osm_id_osm == 162242300 ~ 1,
        osm_id_osm == 65811647 ~ 1, #other side of the tracks on clifton
        
        #wd thompson park
        osm_id_osm == 744371700 ~ 1,
        osm_id_osm == 744371714 ~ 1,
        osm_id_osm == 744371701 ~ 1,
        osm_id_osm == 744371708 ~ 1,
        
        #decatur cemetery
        osm_id_osm == 25779649 ~ 1,
        osm_id_osm == 25779669 ~ 1,
        osm_id_osm == 205781302 ~ 1,
        osm_id_osm == 205781296 ~ 1,
        osm_id_osm == 605932981 ~ 1, #cemetery connector
        osm_id_osm == 346614505 ~ 1,
        grepl("Decatur Cemetery spur", osm_name_osm) ~ 1,
        osm_id_osm == 76112697 ~ 1, #the way out
        osm_id_osm == 152471388 ~ 1,
        
        #glenn creek nature preserve
        osm_id_osm == 150955084 ~ 1,
        osm_id_osm == 150955074 ~ 1,
        osm_id_osm == 150951345 ~ 1,
        osm_id_osm == 150955077 ~ 1,
        osm_id_osm == 150955066 ~ 1,
        
        #some decatur children's farm / waldorf
        osm_id_osm == 605932977 ~ 1,
        osm_id_osm == 566446309 ~ 1,
        osm_id_osm == 494689874 ~ 1,
        osm_id_osm == 494689875 ~ 1,
        osm_id_osm == 605932976 ~ 1,
        
        
        #back gnard and southeast decatur
        osm_id_osm == 605932975 ~ 1,
        osm_id_osm == 430869487 ~ 1,
        osm_id_osm == 430869485 ~ 1,
        osm_id_osm == 430869488 ~1 ,
        
        #lake claire park cut through
        osm_id_osm == 210469294 ~ 1,
        osm_id_osm == 210469292 ~ 1,
        
        
        #skyhaven
        osm_id_osm == 220490141 ~ 1,
        grepl("skyhaven", osm_name_osm)  ~ 1,
        
        osm_id_osm == 605937611 ~ 1, #brownwood (just north of skyhaven)
        
        osm_id_osm == 398076588 ~ 1, #somewhere in SE Atlanta I don't recognize
      
        
        #lake charlotte nature preserve and east of southside mtb park, just north 285
        osm_id_osm == 372416735 ~ 1,
        osm_id_osm == 372416725 ~ 1,
        osm_id_osm == 372416732 ~ 1,
        osm_id_osm ==372417788 ~ 1,  #this one connects to the trails
        osm_id_osm == 372416727 ~ 1,
        osm_id_osm == 372416731 ~ 1,
        osm_id_osm == 372416733 ~ 1, 
        osm_id_osm == 344169924 ~ 1,
        osm_id_osm == 372416726 ~ 1,
        osm_id_osm == 344169924 ~ 1,
        
        osm_id_osm == 372417787 ~1,  #more lake charlotte nature preserve
        
        #a segment at kirkwood soccer fields
        osm_id_osm == 362815445 ~1,
        
        #a segment in southwest atlanta
        osm_id_osm == 506762008 ~ 1,
        osm_id_osm == 352907364 ~ 1, #dirt in lionel hampton
        
        project_lullwater_dirt == 1 ~ 1,
        project_ira_b_mason_mill == 1 ~ 1,
        project_morningside_nature_pres == 1 ~ 1,
        project_johnson_taylor == 1 ~1, #Daniel Johnson Herbert Taylor
        project_lenox_wildwood == 1  ~ 1,
        project_intrenchment_prison_farm ==1 ~ 1,
        
        grepl("Frazer F", osm_name_osm) ~ 1, #Frazer Forest that I coded
        grepl("Deepdene", osm_name_osm) ~ 1,  #Deepdene Park that I coded,
        osm_id_osm == 155481627 ~ 1, #next to lullwater road near golf course
        grepl("Fernbank El", osm_name_osm) ~ 1 #Fernbank elementary trails
    ),
  

  ## Additional manual work October 2020--------------
  # MDG here is where the manual work is! early October 2020-----#
  
  ### Pre-existing infrastructure (aim 1)------------------------------------------
  #My process here is to review PDFs and my older infrastructure file to see 
  #what was already there
  #to make sure it matches what the cities (Atlanta and Decatur) have
  
  #Begin with buffered bike lanes
  #Per Streetview, 2014 or earlier for almost all of it.
  #Just say 1/1/2016 for all of these and then can be more precise if needed thereafter,
  #but it's irrelevant for this project, since the point is it precedes the data.
  #west of the home depot / whole foods plaza  / beltline to myrtle, then west of myrtle, 
  #it's conventional to juniper,
  #then it stops
  
  # project_ponce_lane_buff_pre2016 = case_when(
  #   
  # ),
  #Note the above was moved to the 2_wrangle_basemap.R code
  
  ### classify off-street trail -  paved-----------------------
  #update 1/3/22 note these are fixed further down in 2_wrangle_basemap to be dichotomous (1,0)
    infra_off_street_trail_paved = case_when(
      project_path_400 == 1 ~ 1,
      project_westside_trail_paved == 1 ~ 1,
      project_ivan_allen_gateway == 1 ~ 1,
      project_proctor_creek_greenway == 1 ~ 1,
      project_bitsy_grant_trail==1 ~ 1, #this was already classified as such but can be explicit so the date is correct
      project_tech_parkway == 1 ~ 1, 
      
      grepl("Trolley Line Trail", osm_name_osm) ~ 1,
      osm_id_osm == 40940376 ~1 , #part of the trolley trail; south of coan park
      
      #both of these work but note that the segments, defined above, open at different times, 
      #per date variables below
      grepl("Atlanta BeltLine Eastside Trail", osm_name_osm) ~ 1, 
      grepl("South Peachtree Cr", osm_name_osm) ~ 1,  
      
      grepl("Stone Mountain Trail", osm_name_osm)      ~1,
      grepl("Lionel Hampton P", osm_name_osm) ~ 1, #Lionel Hampton Path Trail
      grepl("Southtowne PATH", osm_name_osm) ~ 1, #Southtowne Path Trail
      
      grepl("South River PATH", osm_name_osm) ~ 1, #South River PATH Trail
      
      #a paved trail in decatur cemetery
      osm_id_osm == 25565529 ~ 1,
      
      
      # a little intersection cut through off clairemont. it was coded wrongly as a conventional bike lane
      #by the infra 2016 file. this will prevent it from doing that.
      osm_id_osm == 9189985 ~ 1,
      
      #Akers Mill Road Paved Trail

      #classify by more general criteria
      highway=="path" & surface == "concrete" ~ 1,
      highway=="path" & surface == "paved" ~ 1,
      highway=="pedestrian" & surface == "paved" ~ 1, #picks up a path in decatur
      highway=="pedestrian" & surface == "concrete" ~ 1, #picks up paths at GT; technically they are
      highway=="pedestrian" & surface == "asphalt"  ~1,  #piedmont park trails. this counts.
      
      #this picks up a few that are more like "sidewalks" but I think we can count almost all
      #of these as off-street paved trails. this picks up several more piedmont park trails 
      highway == "footway" & surface == "paved" ~ 1
    ),
    
  ### classify off-street trail  - dirt------------------
  # General note - as you classify this, you may want to say it's NOT the preceding one--#
    infra_off_street_trail_dirt = case_when(
      grepl("Interim BeltLine Eastside Trail", osm_name_osm) ~ 1,
      grepl("Atlanta BeltLine Southside Trail", osm_name_osm) ~ 1,
      grepl("Atlanta BeltLine NW", osm_name_osm) ~ 1, #I drew this
      grepl("BeltLine NW Path placeholder", osm_name_osm) ~ 1,
      grepl("Westside Interim", osm_name_osm) ~1, #westside interim dirt trail
      osm_id_osm == 362814722 ~ 1, #Pullman Trail: dirt path south of tracks in kirkwood
      
      grepl("Active Oval", osm_name_osm) ~ 1,

      #Cheshire Farm Trail    - it's gravel 
      grepl("Cheshire Farm Tr", osm_name_osm) ~ 1,
      grepl("Frazer F", osm_name_osm) ~ 1, #Frazer Forest that I coded
      grepl("Deepdene", osm_name_osm) ~ 1, #Deepdene Park that I coded
      
      mtb_less_official == 1 ~ 1, #this should bring in quite a few from above
      mtb == 1 ~ 1,

      #constitutional lakes 
      osm_id_osm == 344169006 ~ 1,
      osm_id_osm == 344169922 ~ 1,
      osm_id_osm == 344169921 ~ 1,
      osm_id_osm == 344169155 ~ 1,
      osm_id_osm == 344169153 ~ 1,

      
      #more general criteria based on highway category and surface- #
      #more general criteria. no false positives here. I checked. This picks up a trail or two
      #that are truly unpaved in Piedmont Park as well as some other park dirt paths
      highway_6cat == "path - paved or not" & surface == "unpaved" ~1 ,
      
      #this one works. note it picks up the palisades (West Palisades), where bikes cannot go.
      surface == "dirt"  ~1,
      surface == "woodchips" ~1, #this picks up like 10 dirt trails. not even woodchips there, I don't think.
            #but they are truly dirt trails
      
      #there are just a few (10 or so) with surface == ground but they are specific
      #this includes the children's farm and a few other truly dirt trails
      surface == "ground" ~1
      
    ),
    
    ### classify bike lane - protected---------------------
    infra_bike_lane_protected = 
      case_when(
        project_peachtree_center_pbl_east==1~1,#adding June 1, 2023
        grepl("Portman PATH", osm_name_osm) ~ 1,
        grepl("Peachtree Center Cycle", osm_name_osm) ~ 1,
        project_luckie_st_lane_protected==1 ~1,
        project_n_mcdonough_st_lane_protected == 1 ~1,
        project_10th_st_protected_lane==1~1,
        # cycleway == "track" ~ 1, #not universally true so don't use this criterion
      ) ,
    
    ### classify bike lane - buffered-------------------
  #12/17/21 this is too generous. much of this rda lane is actually not buffered.
    infra_bike_lane_buffered =  case_when(
      project_ormewood_bike_lane_buff == 1  ~ 1,
      project_rda_cascade_mlk_bike_lane_buff==1 ~ 1
    ),
    
  ### classify bike lane - conventional--------------------------------
  #12/19/21 what about eagle row? confirmed it's there. you're good. proceed.
  #June 1, 2023 - it gets added from my summer 2016 code
    infra_bike_lane_conventional =  case_when(
      #hard code some
      project_milton_ave_bike_lane_conv==1 ~ 1,
      project_rda_cascade_mlk_bike_lane_conv==1 ~1, #the RDA lanes that are actually conventional
      
      #classifying as conventional but will correct some edges later
      project_sylvan_bike_lane_buff_conv==1 ~ 1, 
      project_lawton_bike_lane_conv == 1 ~ 1,
      project_united_bike_lane_conv_old == 1 ~ 1, 
      project_marietta_st_bike_lane_conv_2018Dec ==1 ~ 1,
      
      #12/16/21 you had forgotten about college (august 2018 open)
      project_college_ave_bike_lane_conv ==1 ~1, 
      
      
      project_clifton_emory_conf_hotel_to_emory_point==1~1,#added Jun 1, 2023
      
      projet_ferst_dr_nw_conv==1~1,#added June 1, 2023
      
      cycleway.right=="lane" ~ 1  #double check this condition
    ),
    
  ### classify bike lane - sharrow-----------------------
    infra_sharrow =  case_when(
      project_ormewood_sharrow == 1 ~ 1,
      
      #changed this to sharrow 12/16/21
      project_united_sharrow_2018summer == 1 ~ 1, 
      
      #June 1, 2023 - Clifton work - note the default should be sharrows
      #and then it changes to conventional bike lane in spots, per above project,
      #but that will be accomplished in the next code where we add the longitudinal information
      project_clifton_n_decatur_to_e_row_before_201708==1~1,
      
      #always sharrows - added June 1, 2023
      project_clifton_e_row_to_emory_conf_hotel==1~1,
      project_clifton_emory_point_to_briarcliff==1~1,
      
      #adding these sharrows - June 3, 2023
      project_sharrows_decatur==1~1,
      project_sharrows_cottage_grove_oakview_se==1~1,#June 3rd, 2023
      #these two are cycleway.right == shared_lane, but I'm not using that generally
      #since there is a false positive. These are westview
      osm_id_osm == 31021597 ~ 1,
      osm_id_osm == 9236218 ~ 1,
      
      project_ferst_dr_nw_sharrow==1~1,#added June 2023
      
      cycleway == "shared_lane" ~ 1, #cumberland (9268685) is an example 
      
      cycleway.left == "shared_lane" ~1 #this works, too
    ),
  
  
  ### crosswalks (footway==crossing)-------------------
  #These appear to be classified correctly, broadly. I'd like to exclude them
  #or at least mark them
  footway_crossing = case_when(
    #excluding ashford druid hills lake, which is marked as a crossing for some reason
    grepl("Ashford Druid Hills Lak", osm_name_osm) ~ 0,
    footway == "crossing" ~1,
    TRUE ~ 0),
   
  ## classify 6-category infra variable--------------------------------
  #Update 10/9/2020 I'm calling this pre merge because below
  #I merge everything that qualifies as "none" with my previously created dataset with infrastructure from 2016
  #so that it then runs through the subsequent code, I'm going to call the final one infra_6cat
    infra_6cat_pre_merge = case_when(
      infra_off_street_trail_paved==1 ~ "off_street_trail_paved",
      infra_off_street_trail_dirt==1 ~ "off_street_trail_dirt",
      infra_bike_lane_protected==1 ~ "bike_lane_protected",
      infra_bike_lane_buffered==1 ~ "bike_lane_buffered",
      infra_bike_lane_conventional==1 ~ "bike_lane_conventional",
      infra_sharrow==1 ~ "sharrow",
      TRUE ~ "none"
      ),
  
  infra_pre_merge_none = case_when(   #renaming to none from missing
    infra_6cat_pre_merge== "none" ~ 1,
    TRUE ~ 0
  ),
  
  ## dissertation infrastructure aim 1 indicator variable--------------------
  #a dissertation aim 1 indicator
  diss_a1_any = case_when(
    #note that the westsidetrail paved indicator includes the segment near the old
    #rail building that is not paved.
    project_westside_trail_paved    ==  1 ~1,
    project_proctor_creek_greenway   == 1 ~ 1,
    project_path_parkway             == 1 ~ 1,
    
    #note, that my dissertation only intends to include to edgewood bridge, 
    #but it shouldn't functionally
    #matter with respect to the hexagons
    project_eastside_trail_irwin_to_dekalb == 1 ~ 1, 
    project_eastside_trail_wylie_to_fulton_terr == 1 ~ 1,
    project_eastside_trail_wylie_extension == 1 ~ 1,
    project_eastside_trail_krog_tunnel == 1 ~ 1,
    
    #peachtree creek segments
    project_s_peachtree_creek_mm_to_ndh == 1 ~ 1,
    project_s_peachtree_creek_bridge_under_clairmont == 1 ~ 1,
    project_s_peachtree_creek_int_to_clairmont_lake == 1 ~ 1,
    
    #and the two that are being considered as confounders
    project_rda_cascade_mlk_bike_lane_dec2018 == 1~ 1,
    project_n_mcdonough_st_lane_protected == 1 ~1,
    TRUE ~ 0
      ),
  
  ### alternate definitions of your dissertation aim 1 grouped infra------------
  diss_a1_wst = case_when(
    project_westside_trail_paved    ==  1 ~1,
    TRUE ~0),
  
  #I'm going to call this PCG instead. Right? why not. 
  #I guess I used PRO as PC could be peachtree creek
  diss_a1_pro = case_when(
    project_proctor_creek_greenway  == 1 ~ 1,
    TRUE ~0),
  
      #grouped westside trail proctor creek greenway
  diss_a1_wst_pro = case_when(
    project_proctor_creek_greenway  == 1 ~ 1,
    project_westside_trail_paved    ==  1 ~1,
    TRUE ~0),
  
  diss_a1_lsl = case_when(
    project_luckie_st_lane_protected==1~1,
    TRUE ~0),
  
  diss_a1_gtp = case_when(
    project_tech_parkway == 1~1,
    TRUE ~0),
    #grouped lsl gtp
  diss_a1_lsl_gtp = case_when(
    project_luckie_st_lane_protected==1~1,
    project_tech_parkway == 1~1,
    TRUE ~0),
  
  #est sections:
  #note in your other file, est1 is Eastside Trail - Wylie - Krog to Corridor
                          # est2 is Eastside Trail - Irwin to Edgewood Bridge
                          # est3 is Eastside Trail - Wylie to Kirkwood (coded as Fulton terrace here)
  #we don't have perfect alignment with that here. we can use, instead
  diss_a1_est1 = case_when(
    project_eastside_trail_wylie_extension == 1~1,
    project_eastside_trail_krog_tunnel == 1 ~1,
    TRUE ~0
  ),
  diss_a1_est2 = case_when(
    #not technically the same but won't functionally matter for the hexagons
    #but don't use for measurements. use your other one for measurements.
    project_eastside_trail_irwin_to_dekalb ==1 ~ 1,
    TRUE ~0
    ),
  
  diss_a1_est3 = case_when(
    project_eastside_trail_wylie_to_fulton_terr==1 ~ 1,
    TRUE ~ 0),
  
  #and that's it. nothing else was done for EST
  #note there are a few other est extensions below that we've not included in diss
  #which have opened after, e.g., on my birthday of july 2019
  diss_a1_est = case_when(
    diss_a1_est1==1 ~ 1,
    diss_a1_est2==1 ~ 1,
    diss_a1_est3==1 ~ 1,
    TRUE ~ 0
  ),
  
  #s peachtree creek trail. in the other dataset:
      #  pct 1 is S Peachtree Creek Trail - MM Park to NDH Rd
      # pct2 is S Peachtree Creek Trail - Starvine Way to bridge under Clai
       #that's it. no more pct during the dissertation phase. the next one opens october 2018
  #    project_s_peachtree_creek_mm_to_ndh==1 ~  lubridate::ymd(20170624),
  # project_s_peachtree_creek_int_to_clairmont_lake == 1 ~  lubridate::ymd(20170624), #guess.
  # project_s_peachtree_creek_bridge_under_clairmont == 1 ~ lubridate::ymd(20180420),
  # project_s_peachtree_creek_clairmont_lake == 1 ~ lubridate::ymd(20181022), #definitely latest.
  #okay, I'm going to say that that little intersection part was also done at this time and
  #I'm going to include it in the same part
  diss_a1_pct1 = case_when(
    project_s_peachtree_creek_mm_to_ndh ==1 ~1,
    project_s_peachtree_creek_int_to_clairmont_lake==1 ~1,
    TRUE ~0
    ),
  diss_a1_pct2 = case_when(
    #note the OSM name of this is
#    South Peachtree Creek PATH Trail - Bridge Under Clairmont to Starvine Way
    #this also covers the part to starvine way. yup, just confirmed.
    project_s_peachtree_creek_bridge_under_clairmont == 1 ~ 1,
    TRUE ~0
  ),

  diss_a1_pct = case_when(
    diss_a1_pct1 ==1 ~1,
    diss_a1_pct2 == 1 ~ 1,
    TRUE ~0
  ),

  diss_a1_mcd = case_when(
    project_n_mcdonough_st_lane_protected ==1 ~1,
    TRUE ~ 0
  ),

  #cas for cascade
  diss_a1_rda_cas = case_when(
    project_rda_cascade_mlk_bike_lane_dec2018==1 ~ 1,
    TRUE ~ 0
  ),

  # Note that these aim-1-related variables 
    #are finished up in the 2_wrangle_basemap code
  


  ## date variables-----------------------
  #Note, in my 1_diss_infra code, I use the term ribbon to indicate opening,
  #so do that here, too, for internal consistency
        #yr_ribbon in that code. Here, I'd prefer ribbon_year
        #mo_ribbon in that code. Here, I'd prefer ribbon_month
        #date_ribbon in that code. Here, I'd prefer ribbon_date
  ribbon_date = case_when(
    
    #Per review of 2017 report or consistent with dissertation infrastructure
    project_westside_trail_paved == 1 ~ lubridate::ymd(20170929), #might overwrite above, or not.
    project_tech_parkway == 1 ~ lubridate::ymd(20171128), #note, it was quite rideable before this time.
    project_luckie_st_lane_protected == 1 ~ lubridate::ymd(20170601),
    project_proctor_creek_greenway == 1  ~ lubridate::ymd(20180507), #to be consistent with dissertation aim 1
    #this was per 2018 report, but including it here because it's part of diss aim 1 infra
    
    project_eastside_trail_wylie_extension == 1 ~ lubridate::ymd(20170901),
    project_eastside_trail_krog_tunnel == 1 ~ lubridate::ymd(20170901),
    project_eastside_trail_wylie_to_fulton_terr == 1 ~ lubridate::ymd(20171023), #the date corresponds to kirkwood
    project_eastside_trail_irwin_to_dekalb == 1 ~ lubridate::ymd(20170901), #open date corresponds to edgewood
    project_eastside_trail_bk_to_i20 == 1 ~ lubridate::ymd(20190601), #guess, but def after march 2019
    project_eastside_trail_mauldin_to_memorial == 1 ~ lubridate::ymd(20190711), #my birthday 2019
    
    project_eastside_trail_2012 == 1 ~ lubridate::ymd(20121001), #wikipedia 
    
    #s peachtree creek
    project_s_peachtree_creek_mm_to_ndh==1 ~  lubridate::ymd(20170624),
    project_s_peachtree_creek_bridge_under_clairmont == 1 ~ lubridate::ymd(20180420),
    project_s_peachtree_creek_int_to_clairmont_lake == 1 ~  lubridate::ymd(20170624), #guess.
    project_s_peachtree_creek_clairmont_lake == 1 ~ lubridate::ymd(20181022), #definitely latest.
    
    project_n_mcdonough_st_lane_protected == 1 ~ lubridate::ymd(20170901),
    
    #rough guess in december 1, 2017 for the RDA changes
    #https://www.atlantaga.gov/home/showdocument?id=34089
    project_rda_cascade_mlk_bike_lane_dec2018==1 ~ lubridate::ymd(20171201),  
    
    #12/4/21 we are correcting this. much of this was proposed only and was not complete.
    #other dissertation infrastructure I considered (Path 400)
    #project_path_400 == 1 ~ lubridate::ymd(20161215),
    
    #Per review of 2018 report
    #https://www.atlantaga.gov/home/showdocument?id=40599
    project_milton_ave_bike_lane_conv == 1 ~ lubridate::ymd(20171201),
    project_sylvan_bike_lane_buff_conv==1 ~ lubridate::ymd(20180201), #all same date, regardless of conv v buff
    project_ivan_allen_gateway == 1 ~ lubridate::ymd(20180201),
    # project_rda_bike_lane_buff_2018mar == 1 ~ lubridate::ymd(20180301), #commented out 12/17/21 b/c other code
    project_lawton_bike_lane_conv == 1 ~ lubridate::ymd(20180401),
    project_college_ave_bike_lane_conv == 1 ~ lubridate::ymd(20180801), #include aim 1 even though towards end
    project_ormewood_bike_lane_buff == 1  ~ lubridate::ymd(20180701),
    project_ormewood_sharrow == 1  ~ lubridate::ymd(20180701), 
    project_united_sharrow_2018summer == 1 ~ lubridate::ymd(20180701), #united is actually a sharrow
    project_united_bike_lane_conv_old == 1 ~ lubridate::ymd(20120701),#these bike lanes look old
    project_bitsy_grant_trail==1 ~ lubridate::ymd(20181001), #northwest beltline connector (after aim 1)
    #Confirmed this Marietta project on Street View June 1, 2023.
    #It's after the aim 3 study ended, though.
    #after aim 1 as well
    project_marietta_st_bike_lane_conv_2018Dec ==1 ~lubridate::ymd(20181201),
    
    #Clifton Corridor - adding this June 1, 2023
    project_clifton_n_decatur_to_e_row_before_201708==1~lubridate::ymd(20170801),

    #a discovery Oakview and Cottage Grove - June 3rd, 2023
    #rough guess - June 3rd 2023
    project_sharrows_cottage_grove_oakview_se==1~lubridate::ymd(20171001)
    
  ),#close the case when above

    #12/17/21 this is a simple indicator variable so I can set my ribbon_study_month accordingly
    #for some of them, it makes sense to set the first month they were open the month following
    #the date they were officially open. for others, based on experience on the ground, 
      #it was clear they were
    #very open and rideable on the day they were open, or even before. so keep track of that here.
    #this will be added on, so a zero indicates same month, 
    #and a 1 (or more) indicates push the rideable
    #opening farther in the future
    ribbon_month_after_or_same = case_when(
      project_eastside_trail_wylie_extension == 1 ~ 0,
      project_eastside_trail_krog_tunnel == 1 ~ 0,
      project_eastside_trail_wylie_to_fulton_terr == 1 ~ 0, #very rideable day of.
      project_eastside_trail_irwin_to_dekalb == 1 ~ 0, #open date corresponds to edgewood
      project_eastside_trail_bk_to_i20 == 1 ~ 0, #guess, but def after march 2019
      project_eastside_trail_mauldin_to_memorial == 1 ~ 0, #my birthday 2019
      
      #note following month for most of these
      project_s_peachtree_creek_mm_to_ndh==1 ~  1, 
      project_s_peachtree_creek_bridge_under_clairmont == 1 ~ 1,
      project_s_peachtree_creek_int_to_clairmont_lake == 1 ~  1, #guess.
      project_s_peachtree_creek_clairmont_lake == 1 ~ 1, #definitely latest. after aim 1
      
      project_westside_trail_paved == 1 ~ 1, #late in the month, although I rode on it earlier.
      project_proctor_creek_greenway == 1  ~ 0, #early in the month
      
      project_tech_parkway == 1 ~ 0, #note, it was quite rideable before this time.
      project_luckie_st_lane_protected == 1 ~ 0,
      
      project_n_mcdonough_st_lane_protected == 1 ~ 0,
      
      #rough guess in december 1, 2017
      project_rda_cascade_mlk_bike_lane_dec2018==1 ~ 0 ,
      
      project_milton_ave_bike_lane_conv == 1 ~ 0,
      project_sylvan_bike_lane_buff_conv==1 ~ 0, 
      project_ivan_allen_gateway == 1 ~ 0,
      project_lawton_bike_lane_conv == 1 ~ 0,
      project_college_ave_bike_lane_conv == 1 ~ 0, 
      project_ormewood_bike_lane_buff == 1  ~ 0,
      project_ormewood_sharrow == 1  ~ 0, 
      project_united_sharrow_2018summer == 1 ~ 0, 
      project_united_bike_lane_conv_old == 1 ~ 0,
      project_bitsy_grant_trail==1 ~ 0, 
      project_marietta_st_bike_lane_conv_2018Dec ==1 ~0 ,
      project_clifton_n_decatur_to_e_row_before_201708==1~0, 
      TRUE ~ 0 #else set it to 0 just in case I missed some.
    ),

    #to align better with the data that you had in your diss_infra_dataprep file,
    #which indicates the month in which it was RIDEABLE, say the following, which just
    #takes it to the closet 15th of that month. note in the other diss code,
    #you called this date_ribbon_section_approx, but I'm going to be more explicit and call
    #it RIDE

  #12/17/21 decision: it's confusing to have separate dates both for "rideable" and official opening
  #just assume ribbon_study_month is when it was rideable, and make adjustments to that if necessary.

    #none swapped into a different year. here's where you push it into the next month or not based
    #on rideability. no need for that other  variable
    ribbon_year = lubridate::year(ribbon_date), 
    ribbon_month = lubridate::month(ribbon_date) + ribbon_month_after_or_same,

  #a numeric variable that lines up with study month for easy subtraction
    ribbon_study_month = case_when(
      ribbon_year < 2016 ~ -1, #this just codes anything that opened before 2016 as negative one
                              #it should work in inequality statements as 
      ribbon_year == 2016 ~ ribbon_month - 9, #this should convert 10 to 1, 11 to 2, and 12 to 3
      ribbon_year == 2017 ~ ribbon_month + 3, #this will make january 2017 the fourth month
      ribbon_year == 2018 ~ ribbon_month + 3+12, #and this should make january 2018 the 16th month
      ribbon_year == 2019 ~ ribbon_month + 3+12+12, #these should all be outside of the study
      ribbon_year == 2020 ~ ribbon_month + 3+12+12+12  #these should all be outside of the study
              ),

    ribbon_date_source = case_when(
      project_ormewood_bike_lane_buff == 1 ~ "City Report, 2018",
      project_milton_ave_bike_lane_conv == 1 ~ "City Report, 2018",
      project_ivan_allen_gateway == 1 ~ "City Report, 2018",
      project_milton_ave_bike_lane_conv == 1 ~ "City Report, 2018",
      project_sylvan_bike_lane_buff_conv==1 ~ "City Report, 2018",
      project_ivan_allen_gateway == 1 ~ "City Report, 2018",
      # project_rda_bike_lane_buff_2018mar == 1 ~ "City Report, 2018",
      project_lawton_bike_lane_conv == 1 ~ "City Report, 2018",
      project_proctor_creek_greenway == 1  ~ "City Report, 2018", #also dissertation aim 1
      project_college_ave_bike_lane_conv == 1 ~ "City Report, 2018",
      project_ormewood_bike_lane_buff == 1 ~ "City Report, 2018",
      project_ormewood_sharrow == 1  ~ "City Report, 2018",
      project_united_sharrow_2018summer == 1 ~ "City Report, 2018",
      project_united_bike_lane_conv_old == 1 ~ "educated guess", #they looked really old
      project_bitsy_grant_trail==1 ~ "City Report, 2018",
      project_marietta_st_bike_lane_conv_2018Dec ==1 ~ "City Report, 2018",
      
      project_eastside_trail_wylie_extension == 1 ~ "personal knowledge",
      project_eastside_trail_krog_tunnel == 1 ~ "personal knowledge",
      project_eastside_trail_wylie_to_fulton_terr == 1 ~ "personal knowledge", #but see note above
      project_eastside_trail_irwin_to_dekalb == 1 ~ "personal knowledge", #but see note above
      project_eastside_trail_bk_to_i20 == 1 ~ "educated guess", #it's definitely after March 2019
      
      project_westside_trail_paved == 1 ~ "City Report, 2017 & personal knowledge",
      project_tech_parkway == 1 ~ "City Report, 2017 & personal knowledge",
      project_luckie_st_lane_protected == 1 ~ "City Report, 2017 & personal knowledge",
      
      project_eastside_trail_2012 == 1 ~ "personal knowledge",
      project_path_400 == 1 ~ "news media",
      
      project_s_peachtree_creek_mm_to_ndh==1 ~  "PATH Foundation webpage & personal knowledge",
      project_s_peachtree_creek_bridge_under_clairmont == 1 ~ "PATH Foundation webpage & personal knowledge",
      project_s_peachtree_creek_int_to_clairmont_lake == 1 ~  "PATH Foundation webpage & personal knowledge",  
      project_s_peachtree_creek_clairmont_lake == 1 ~ "PATH Foundation webpage & personal knowledge",
      
      project_rda_cascade_mlk_bike_lane_dec2018 == 1 ~ "City Report, 2017",
      
      project_clifton_n_decatur_to_e_row_before_201708==1~"Emory new",
      
      #leave this one as last so you stop messing up your code
      project_n_mcdonough_st_lane_protected == 1 ~ "news media"
        ) #close the case when
  ) %>%  #close the mutate

    #exclude the crossings
    filter(footway_crossing == 0) %>% 

    ## organize and keep variables-------------------------
    dplyr::select(
      starts_with("osm_"), #this gets the IDs, the names, the indicator, 
      name_1, #fine, I'll keep.
      old_name,
      alt_name,
      starts_with("infra"),
      beltline,
      starts_with("cycleway"), 
      starts_with("ribbon"), 
      starts_with("project"),
      starts_with("diss_a1"), #this will bring in all of your dissertation variables created 12/5/21
      start_date,
      starts_with("highway"), #this grabs highway_1
      starts_with("major_or_res"), #in case there are any descendents
      contains("bicycle"),
      motor_vehicle,
      HFCS, 
      contains("surface"),
      contains("foot"), #this will grab footway_crossing
      paved,
      tracktype,
      contains("sidewalk"),
      hiking,
      starts_with("mtb"),
      # construction,
      # route,
      service,
      traffic_calming,
#      delivery,   driveway,
 #     buses,       moped,       minibus,       golf_cart,
      proposed,
      starts_with("length") ,#this will grab the length variable I made, too.
#      starts_with("tiger"),
      starts_with("tiger.name_base"), #this will include the names but not the suffixes, etc.
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
    )  %>% 
  
  #remove some
  dplyr::select(-mtb.scale.uphill, - mtb.scale, 
                -source.sidewalk, -mtb.scale.imba ,
                #drop the tiger stuff. you never use it. it's just distracting.
                -starts_with("tiger."),
                #remove the length variable that was already there.
                #we made another one called length_osm_m
                -length #this isi the original length field imported from OSM. get rid of it.
                ) 

## save---------------------------------
setwd(here("data-processed"))
save(all_h_osm_wrangle_geo, file = "all_h_osm_wrangle_geo.RData")
#load("all_h_osm_wrangle_geo.RData")
#a version without geometry
all_h_osm_wrangle_nogeo = all_h_osm_wrangle_geo %>% 
  st_set_geometry(NULL)
save(all_h_osm_wrangle_nogeo, file = "all_h_osm_wrangle_nogeo.RData")

names(all_h_osm_wrangle_nogeo)
#look-up table
lookup_osm_wrangle_geo = all_h_osm_wrangle_geo %>%
  dplyr::select(osm_id_osm, geometry)
save(lookup_osm_wrangle_geo, file = "lookup_osm_wrangle_geo.RData")


# Create a dataset of infra that WAS NOT classified above in the OSM-specific coding------------
#May 31, 2023 - okay this makes sense. I'm limiting to the observations without infra
#per the above and I will link in the infra data I used for my summer 2016 validation study,
#which I'm working on making sure is 100% accurate.
all_h_osm_noinfra = all_h_osm_wrangle_geo %>% 
  filter(infra_pre_merge_none==1)
save(all_h_osm_noinfra, file = "all_h_osm_noinfra.RData")
names(all_h_osm_noinfra)

#and the complement. create it here.
all_h_osm_anyinfra = all_h_osm_wrangle_geo %>% 
  filter(infra_pre_merge_none==0) %>% 
  mutate( 
    #Define infra 6 cat. It's simply how it's defined by the OSM coding above.
    infra_6cat_none = infra_6cat_pre_merge 
  )

table(all_h_osm_anyinfra$infra_6cat_none) #there should be no missings by definition


## checks and exploring------------
#confirm your ribbon date...
table(all_h_osm_wrangle_nogeo$ribbon_study_month)
set2_6 = RColorBrewer::brewer.pal(n=6, name = "Set2")
set1_6 = RColorBrewer::brewer.pal(n=6, name = "Set1")
# all_h_osm_wrangle_geo %>%
#   filter(
#     grepl("Ira B", osm_name_osm) |
#       grepl("Ira b", osm_name_osm) |
#       grepl("ira b", osm_name_osm)
#   ) %>% mapview()
# 

#May 30 2023,
#This is where I realized that the issues with the phantom bike lanes
#ocurred in the coa/arc code from June 2016
#also checking Clifton June 1, 2023. Looks good.
all_h_osm_wrangle_geo %>%
#  filter(beltline==1) %>%
  filter(infra_6cat_pre_merge!="none") %>% 
  mapview(
    zcol="infra_6cat_pre_merge",
    color = turbo(n=n_distinct(all_h_osm_wrangle_geo$infra_6cat_pre_merge))
  )


## Gather infrastructure that was previously classified by ARC or City of  Atlanta-------------
# link in the infra with the work you did in 2016.
#May 30th, 2023:
#I"m trying to figure out where this came from.
#See comments in Evernote.
#I am modifying this code that I wrote a long time ago.
source(here("scripts","summer-2016-bike-study","wrangle-bike-infra-summer-2016.R"))
#In its original iteration, that code was called:
#1_1_Import_manage_bike_layers_data_20201009

#Unfortunately, this will mean that it will be less easy to simply load OSM data and code 
#it all that way
#but that's alright for now. you can still post it online - just post the data


setwd(here("data-processed"))
load( file = "bike_inf_5_i285_4326.RData")
#load( file = "all_h_osm_wrangle_geo.RData")
# load( file = "monpon_sf_1mi.RData")
# load( file = "monpon_sf_2mi.RData")
set2_6 = RColorBrewer::brewer.pal(n=6, name = "Set2")
set2_5 = RColorBrewer::brewer.pal(n=5, name = "Set2")
set1_6 = RColorBrewer::brewer.pal(n=6, name = "Set1")

names(bike_inf_5_i285_4326)
bike_inf_5_i285_4326 %>% mapview(zcol="infra_6cat_arc_coa")


# Create a buffer around your previous (circa 2016) infrastructure-------#
# May 31, 2023 here again.
#above, I used a 20-foot buffer, so try that here, too. 
#Filter to where the main OSM file is infra_6cat_pre_merge = none, and then st_intersection() it
#against the buffered version of bike_inf_5

# names(bike_inf_5_i285_4326)
bike_inf_5_i285_4326
bike_inf_5_i285_4326_buff = bike_inf_5_i285_4326 %>%
  #convert to feet
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  #a 15 foot buffer might prevent some of the weird stuff but honestly this is fine.
  #Note, the 18 is a happy medium. I got one bike lane back that I had missed with 15.
  st_buffer(18) %>%  #brought it down to 15. I lost some. go back up to 18
  st_transform(4326) %>%
  st_make_valid() %>% #make sure it's valid.
  mutate(coa_arc = 1)
#
#Intermediate step: save some things because the step to create
# all_h_osm_coa_arc_int is taking forever and exhausting memory (12/4/21)
save(bike_inf_5_i285_4326_buff, file = "bike_inf_5_i285_4326_buff.RData")

library(tidyverse)
library(sf)
library(mapview)
names(bike_inf_5_i285_4326_buff)
names(all_h_osm_noinfra)
#
# #12/4/21 paring down to fewer variables because I was reaching the memory limit of R
#May 31, 2023 - it's more about number of rows than number of vars. Remove
#service or unclassified, as there's no infra on those raods.
table(all_h_osm_noinfra$highway_6cat)
# all_h_osm_noinfra %>% 
#   filter(highway_6cat=="unclassified or service") %>% 
#   mapview()
#Yup, let's get rid of all of this - May 31, 2023 - no infra of note
all_h_osm_noinfra_fewervars = all_h_osm_noinfra %>%
  dplyr::select(osm_id_osm, geometry, highway_6cat) %>%
  mutate(row_for_chop = row_number())

names(all_h_osm_noinfra_fewervars)
all_h_osm_noinfra_fewervars_yes_unclass_service=all_h_osm_noinfra_fewervars %>% 
  filter(highway_6cat=="unclassified or service") %>% 
  dplyr::select(-starts_with("highway"))#drop it to avoid conflicts.

all_h_osm_noinfra_fewervars_no_unclass_service=all_h_osm_noinfra_fewervars %>% 
  filter(highway_6cat!="unclassified or service") %>% 
  dplyr::select(-starts_with("highway"))#drop it to avoid conflicts.

#all_h_osm_noinfra_fewervars_no_unclass_service %>% mapview()

#May 31, 2023 To speed things up, I can remove all serviceo or unclassified from
#this intersection.
nrow(all_h_osm_noinfra_fewervars)

bike_inf_5_i285_4326_buff_fewervars = bike_inf_5_i285_4326_buff %>%
  dplyr::select(starts_with("data"), starts_with("infra"), geometry)


mapview(bike_inf_5_i285_4326_buff_fewervars) #leave this one as is.
nrow(all_h_osm_noinfra_fewervars)
nrow(all_h_osm_noinfra_fewervars)/5

## Spatial intersection between old 2016 data and this OSM no infra data------
#Run spatial intersection. I had originally broken it up into smaller chunks,
#but I'm hoping I don't need to do that now with my Macbook.
#Yup, this took about 10 mins. Not too terrible.
all_h_osm_coa_arc_int = all_h_osm_noinfra_fewervars_no_unclass_service %>%
  st_intersection(bike_inf_5_i285_4326_buff_fewervars)


names(all_h_osm_coa_arc_int)
#Pick one meaning take the longest one down there.
all_h_coa_arc_int_pick_one = all_h_osm_coa_arc_int %>%
  #this is just to create an indicator, and thisi is by definition
   #of the intersection above going to be 1 for all, so set all to 1.
   rename(data_source_coa_arc = data_source) %>%
    mutate(
    join_arc_coa = 1,
    length_chopped_m = as.numeric(st_length(geometry)),
    osm_chopped_no = row_number()
  ) %>%
  #load those other service-road obs back in here, May 31, 2023
  bind_rows(all_h_osm_noinfra_fewervars_yes_unclass_service) %>% 
#   #great, now just pick the longest of that osm if there are duplicates.
  st_set_geometry(NULL) %>% #turn geometry on or off
   group_by(osm_id_osm) %>%
#   #the default is to sort ascending, so this grabs the biggest one
   arrange(desc(length_chopped_m)) %>%
   slice(1) %>%
   ungroup() %>%
  dplyr::select(osm_id_osm, #from all_h_osm_noinfra
                infra_6cat_arc_coa, #from bike_inf_5_i285_4326_buff
                join_arc_coa, #created just above
                length_chopped_m, #created just above
                osm_chopped_no #created just above
                )
 save(all_h_coa_arc_int_pick_one, file = "all_h_coa_arc_int_pick_one.RData")


# set2_5 = RColorBrewer::brewer.pal(n=5, name = "Set2")
all_h_coa_arc_int_pick_one %>%
  filter(is.na(infra_6cat_arc_coa)==FALSE) %>%
  left_join(lookup_osm_wrangle_geo,by="osm_id_osm") %>% 
  st_as_sf() %>% 
  mapview(
    zcol="infra_6cat_arc_coa",
    color = set2_5
  )



# all_h_osm_coa_arc_int_2mi %>% 
#   filter(is.na(infra_6cat_arc_coa)==FALSE) %>% 
#   mapview(
#     zcol="infra_6cat_arc_coa",
#     color = set2_5
#   ) #this pretty much works!!

# summary(all_h_osm_noinfra_2mi$length_osm_m)
# summary(all_h_coa_arc_int_pick_one$length_chopped_m)


## Link that back with the main one that DID not have any infra classified by OSM------------------
#load it again so that it can be deleted above.
setwd(here("data-processed"))
# load(file = "all_h_osm_noinfra.RData")
# load(file = "all_h_coa_arc_int_pick_one.RData")
names(all_h_coa_arc_int_pick_one)
table(all_h_coa_arc_int_pick_one$infra_6cat_arc_coa)

class(all_h_osm_noinfra)
all_h_infra_join_coa_arc = all_h_osm_noinfra %>%
  left_join(all_h_coa_arc_int_pick_one, by = "osm_id_osm") %>%
  mutate(
    length_proportion_chopped = length_chopped_m/length_osm_m,
    length_proportion_above_cutoff = case_when(
      
      #change for sensitivity/specificity. leaving at .5 was actually too high (specific)
      #moving down to 0.3 got a few in piedmont park that weren't otherwise showing up
      
      #the problem is this also picks up a few little pieces of other streets. 
      #how to remove those... how about length has to be above a certain threshold AND
      #proportion above a certain threshold.
      #Don't worry about it too much. Can clean up below.
      length_proportion_chopped > 0.3 & length_chopped_m > 10 ~ 1,   
      TRUE ~ 0),
      
    #Define infra 6 cat
    infra_6cat_none = case_when(
      length_proportion_above_cutoff == 1 ~ infra_6cat_arc_coa,
      TRUE ~ "none" 
    ))   

save(all_h_infra_join_coa_arc, file = "all_h_infra_join_coa_arc.RData")
names(all_h_infra_join_coa_arc)
summary(all_h_infra_join_coa_arc$length_osm_remeasure_m)
summary(all_h_infra_join_coa_arc$length_osm_m)
table(all_h_infra_join_coa_arc$join_arc_coa)
#Mapview May 30, 2023
#Ugh, okay, here's the issue. 
all_h_infra_join_coa_arc %>% 
  filter(infra_6cat_none!="none") %>% 
  filter(join_arc_coa==1) %>% 
  mapview(zcol="infra_6cat_none")

table(all_h_infra_join_coa_arc$infra_6cat_pre_merge)
table(all_h_osm_wrangle_nogeo$infra_pre_merge_none)
nrow(all_h_osm_noinfra)
# all_h_infra_join_coa_arc %>% st_set_geometry(NULL) %>% 
#   dplyr::select(osm_id_osm, starts_with("infra") ) %>% View()

# all_h_infra_join_coa_arc %>% 
#   filter(is.na(infra_6cat_none)==FALSE) %>% 
#   mapview(
#     zcol="infra_6cat_none",
#     color = set2_6
#   )
# - GREAT. that seems to have worked. now link in with the file THAT DID have infra coded by me.

#load(file = "all_h_osm_wrangle_geo.RData")


#Okay, the issue is not in all_h_osm_anyinfra,
#but in all_h_infra_join_coa_arc
# all_h_osm_anyinfra %>% 
#   mapview(zcol="infra_6cat_none")
#all_h_osm_anyinfra %>% dplyr::select(osm_id_osm, starts_with("infra") ) %>% View()

nrow(all_h_osm_anyinfra)
nrow(all_h_osm_noinfra)
nrow(all_h_osm_wrangle_nogeo)
nrow(all_h_osm_anyinfra) + nrow(all_h_osm_noinfra) #check that they add up

## and bind rows them together-----------
table(all_h_infra_join_coa_arc$infra_6cat_none)
table(all_h_osm_anyinfra$infra_6cat_none)
#call it both because it includes data from both OSM and from ARC/COA
all_h_osm_wrangle_both_geo = all_h_infra_join_coa_arc %>% 
  bind_rows(all_h_osm_anyinfra) %>% #created above.
  #remake the infra_6cat variable, too, wherein the none is missings
  #for posterity to make sure the code works in other places
  mutate(
    
    #should I remeasure length_osm_m? sure, just to be sure
    #since you did all of that merging, etc.
    length_osm_remeasure_m = as.numeric(st_length(geometry)),
    
    infra_6cat = case_when(
      infra_6cat_none == "none" ~ NA_character_,
      TRUE  ~ infra_6cat_none
  ),
  # a shorter version that might be used later
  infra_6cat_none_abbrev = case_when(
    infra_6cat_none == "off_street_trail_paved" ~ "trail_p", 
    infra_6cat_none == "off_street_trail_dirt" ~ "trail_d", 
    infra_6cat_none == "bike_lane_protected" ~ "lane_p", 
    infra_6cat_none == "bike_lane_buffered" ~ "lane_b", 
    infra_6cat_none == "bike_lane_conventional" ~ "lane_c", 
    infra_6cat_none == "sharrow" ~ "sharrow", 
    infra_6cat_none == "none" ~ "none" 
        ),
  
  #fix some things systematically. for example, a georgia tech sharrow got coded as a sidewalk.
  infra_implausible = case_when(
    infra_6cat == "sharrow" & highway == "footway" ~ 1,
    osm_id_osm == 395875912  ~  1, #a sharrow / pedstrian at L5P. would prefer not to hard code
    infra_6cat == "sharrow" & highway == "pedestrian" ~ 1, #one at L5P
    infra_6cat == "bike_lane_buffered" & highway == "footway" ~ 1,
    
    #this is a bridge footway in the middle of the portman path. don't want to hard code.
    osm_id_osm == 39448233 ~ 1, 
    
    #June 3rd, 2023
    #remove the square coded as an off-street path at Peachtree St NE and 3rd St NE
    osm_id_osm==468024023~1,

    #June 3rd, 2023    
    #a similar square in Home Park - not really an off-street path
    #Home Park Ave and State St. nW
    osm_id_osm==468745079~1,
    
    #a similar thing on Georgia Tech's campus
    osm_id_osm==177014164~1,
    
    #a similar little path just s of Ralph McGill
    #and west of Parkway Dr NE.
    #don't call this an off-street path.
    osm_id_osm==243478849~1,
    
    #remove the little chopped pieces of infrastructure----######
    #update 10/11 - changed to 35 from 30 to remove a few more.
    infra_6cat_none_abbrev == "lane_c" & length_osm_m < 35 & highway == "service" ~ 1,
    infra_6cat_none_abbrev == "lane_c" & length_osm_m < 35 & highway == "footway" ~ 1,
    
    infra_6cat_none_abbrev == "lane_b" & length_osm_m < 35 & highway == "service" ~ 1,
    infra_6cat_none_abbrev == "lane_b" & length_osm_m < 35 & highway == "footway" ~ 1,
    
    infra_6cat_none_abbrev == "lane_p" & length_osm_m < 35 & highway == "service" ~ 1,
    infra_6cat_none_abbrev == "lane_p" & length_osm_m < 35 & highway == "footway" ~ 1,

    infra_6cat_none_abbrev == "sharrow" & length_osm_m < 35 & highway == "service" ~ 1,
    infra_6cat_none_abbrev == "sharrow" & length_osm_m < 35 & highway == "footway" ~ 1,
    infra_6cat_none_abbrev == "sharrow" & length_osm_m < 20 & highway == "unclassified" ~ 1,

    
    infra_6cat_none_abbrev == "trail_p" & length_osm_m < 35 & highway == "service" ~ 1,
    infra_6cat_none_abbrev == "trail_p" & length_osm_m < 35 & highway == "footway" ~ 1,
    infra_6cat_none_abbrev == "trail_p" & length_osm_m < 35 & highway == "residential" ~ 1,
    
    infra_6cat_none_abbrev == "trail_d" & length_osm_m < 35 & highway == "service" ~ 1,
    infra_6cat_none_abbrev == "trail_d" & length_osm_m < 35 & highway == "footway" ~ 1,
    infra_6cat_none_abbrev == "trail_d" & length_osm_m < 35 & highway == "residential" ~ 1,

    TRUE ~0),
    
  #Set their highway back to what they were (usually just a sidewalk)
  infra_6cat = case_when(
    infra_implausible == 1 ~ NA_character_,
    TRUE ~infra_6cat),
  
  infra_6cat_none = case_when(
    infra_implausible == 1 ~ "none",
    TRUE ~infra_6cat_none),

  infra_6cat_none_abbrev = case_when(
    infra_implausible == 1 ~ "none",
    TRUE ~infra_6cat_none_abbrev
        )
  ) 


# save----------------
save(all_h_osm_wrangle_both_geo, file = "all_h_osm_wrangle_both_geo.RData")
all_h_osm_wrangle_both_nogeo = all_h_osm_wrangle_both_geo %>%  
  st_set_geometry(NULL) %>% 
  as_tibble()
save(all_h_osm_wrangle_both_nogeo, file = "all_h_osm_wrangle_both_nogeo.RData")

# done!----------------------
#check it
table(all_h_osm_wrangle_both_nogeo$infra_6cat_none)
table(all_h_osm_wrangle_both_nogeo$infra_6cat_none_abbrev)
table(all_h_osm_wrangle_both_nogeo$infra_6cat)
table(all_h_osm_wrangle_both_nogeo$diss_a1_any)
table(all_h_osm_wrangle_both_nogeo$diss_a1_est)

table(all_h_osm_wrangle_both_nogeo$diss_a1_est, all_h_osm_wrangle_both_nogeo$diss_a1_est1)

names(all_h_osm_wrangle_both_geo)


#all_h_osm_wrangle_both_geo %>% dplyr::select(osm_id_osm, starts_with("infra") ,join_arc_coa ) %>% View()

# all_h_osm_wrangle_both_geo %>% 
#   filter(is.na(infra_6cat_none)==FALSE) %>% 
#   mapview(
#     zcol="infra_6cat_none",
#     color = set2_6
#   )
# 
# all_h_osm_wrangle_both_geo
#Here May 30 2023:
#Okay, the conventional bike lane on Peachtree is here, and it
#shouldn't be
#as is the sharrow on Peachtree St NE
all_h_osm_wrangle_both_geo %>%
  dplyr::select(starts_with("osm"),starts_with("infra")) %>% 
  filter(infra_6cat_none!="none") %>%
  mapview(
    zcol="infra_6cat_none_abbrev",
    color=turbo(n=n_distinct(all_h_osm_wrangle_both_geo$infra_6cat_none_abbrev))
  )



# Make a version with a small buffer around it for easier spatial merging------------------
load(file = "all_h_osm_wrangle_both_geo.RData")
library(tidyverse)
library(sf)
all_h_osm_wrangle_buff_20ft_geo = all_h_osm_wrangle_both_geo %>% 
  #convert to feet
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
               +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_buffer(20) %>% #20 foot buffer..
  st_transform(4326)
setwd(here("data-processed"))
save(all_h_osm_wrangle_buff_20ft_geo, file = "all_h_osm_wrangle_buff_20ft_geo.RData")


# look-up tables--------------------
names(all_h_osm_wrangle_both_nogeo)

lookup_all_h_osm_wrangle_buff_20ft_geo = all_h_osm_wrangle_buff_20ft_geo %>% 
  dplyr::select(osm_id_osm, geometry)
save(lookup_all_h_osm_wrangle_buff_20ft_geo, file = "lookup_all_h_osm_wrangle_buff_20ft_geo.RData")

#look up osm_id_osm and geometry
lookup_all_h_osm_wrangle_both_geo = all_h_osm_wrangle_both_geo %>% 
  dplyr::select(osm_id_osm, geometry)
save(lookup_all_h_osm_wrangle_both_geo, file = "lookup_all_h_osm_wrangle_both_geo.RData")

#I need to look-up this new measurement variable so I can sort by it
#(12/5/21 I didn't end up using this but oh well)
lookup_osm_id_length_osm_remeasure_m = all_h_osm_wrangle_both_nogeo %>% 
  dplyr::select(osm_id_osm, length_osm_remeasure_m)
summary(all_h_osm_wrangle_both_nogeo$length_osm_remeasure_m)
summary(all_h_osm_wrangle_both_nogeo$length_osm_m)
save(lookup_osm_id_length_osm_remeasure_m, file = "lookup_osm_id_length_osm_remeasure_m.RData")

#the osm indicator and the osm_id_osm (no geo)
names(all_h_osm_wrangle_both_nogeo)
lookup_osm_indicator = all_h_osm_wrangle_both_nogeo %>% 
  dplyr::select(osm_id_osm, osm_indicator)
save(lookup_osm_indicator, file = "lookup_osm_indicator.RData")


# checks and visualizations----------------------------------------
#is osm_id unique?
n_distinct(all_h_osm_wrangle_buff_20ft_geo$osm_id_osm)
nrow(all_h_osm_wrangle_buff_20ft_geo) #yes, apparently it is.
all_h_osm_wrangle_buff_20ft_geo %>% 
  st_set_geometry(NULL) %>% 
  group_by(osm_id_osm) %>% 
  summarise(n=n()) %>% 
  filter(n>1) #excellent. none

# all_h_osm_wrangle_buff_20ft_geo %>% 
#   slice(1:100) %>% 
#   mapview()
# #see the other code for more complete checks and visualizations and mapview
# load( file = "all_h_osm_wrangle_geo.RData")
# set2_6 = RColorBrewer::brewer.pal(n=6, name = "Set2")
# set2_5 = RColorBrewer::brewer.pal(n=5, name = "Set2")
# set1_6 = RColorBrewer::brewer.pal(n=6, name = "Set1")
# table(all_h_osm_wrangle_geo$infra_6cat_pre_merge)
# options(viewer = NULL) #send viewer to the browser
# options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
# 
# library(mapview)
# set2_5 = RColorBrewer::brewer.pal(n=5, name = "Set2")
#load("bike_inf_5_i285_4326.RData")
bike_inf_5_i285_4326 %>%
  filter(is.na(infra_6cat_arc_coa)==FALSE) %>%
  dplyr::select(starts_with("osm"),starts_with("infra"),
                starts_with("project")) %>% 
  mapview(
    zcol="infra_6cat_arc_coa",
    color=turbo(n=n_distinct(bike_inf_5_i285_4326$infra_6cat_arc_coa))
  )

table(all_h_osm_wrangle_both_geo$infra_6cat_none)
#load("all_h_osm_wrangle_both_geo.RData")
all_h_osm_wrangle_both_geo %>%
  filter(infra_6cat_none!="none") %>%
  dplyr::select(starts_with("osm"),starts_with("infra"),
                starts_with("project")) %>% 
  mapview(
    zcol="infra_6cat_none",
    color=turbo(n=n_distinct(all_h_osm_wrangle_both_geo$infra_6cat_none))
  )

#check the peachtree center cycle track project
table(all_h_osm_wrangle_both_geo$project_peachtree_center_pbl_east)
table(all_h_osm_wrangle_both_geo$infra_6cat_none)
all_h_osm_wrangle_both_geo %>% 
  dplyr::select(starts_with("osm"),starts_with("infra"),
                starts_with("project")) %>% 
  filter(infra_6cat_none=="bike_lane_protected") %>%
  mapview(zcol="project_peachtree_center_pbl_east")
# 
# #a vis that omits the dirt so it aligns with the infra file you made in 2016/17
# table(all_h_osm_wrangle_geo$infra_6cat_pre_merge)
# all_h_osm_wrangle_geo %>% 
#   filter(is.na(infra_6cat_pre_merge)==FALSE) %>% 
#   filter(infra_6cat_pre_merge !="off_street_trail_dirt") %>% 
#   mapview(
#     zcol="infra_6cat_pre_merge",
#     color = set2_6
#   )

#June 3rd, 2023
#For feasibility purposes, I have to stop mapping.
#out of curiosity, how many miles are in the study area?
# study_area_osm=all_h_osm_wrangle_both_geo %>% 
#   st_intersection(mp_sf_5halfmi) 
# 
# summary(study_area_osm$length_osm_m)
# table(study_area_osm$highway_6cat)
# study_area_osm %>% 
#   st_set_geometry(NULL) %>% 
#   mutate(dummy=1) %>% 
#   filter(highway_6cat!="unclassified or service") %>% 
#   group_by(dummy) %>% 
#   summarise(length_osm_m=sum(length_osm_m,na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   mutate(
#     length_osm_km=length_osm_m/1000,
#     length_osm_m=0.000621371*length_osm_m
#          )
#say we exclude the service roads from the calculation
# 1837 miles of total OSM segments in the study area.

#It's simply not feasible for us to get everything.



