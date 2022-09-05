############################################-
#-----Final basemap wrangling--------------#
############################################-

#12/5/21
#I separated this code from the other code which merges the bmap data with the OSM data.
#that code is now titled filename:2_0_wrangle_bmap_merge_osm_edge
#April 12 2022 - you should go through this code and organize it using the rmarkdown-style headers

library(tidyverse)
library(sf)
library(mapview)
library(here)#12/16/21 starting to use here() per Jenny Brian rec.

setwd(here("data-processed"))
getwd()
load("bmap_edge_join_geo.RData" )
load("lookup_edge_id_osm_id_osm.RData")
load("all_h_osm_wrangle_both_nogeo.RData")
load("lookup_edge_id_xy_coord.RData")
names(bmap_edge_join_geo) #note that this hasn't actually been run since june 2021; it takes a while
#[1] "edge_id"         "osm_id_osm"      "osm_id_strava"   "osm_name_strava" "join_aspatial"   "join_spatial"   
#"osm_indicator"   "geometry" 

nrow(bmap_edge_join_geo)
names(lookup_edge_id_osm_id_osm)
names(all_h_osm_wrangle_both_nogeo)
# Begin wrangling of edge-level (i.e., not longitudinal) basemap------------------------
bmap_edge_join_wrangle0 = bmap_edge_join_geo %>%
  #update 12/5/21 we should re-link the OSM vars
  #links in all of the OSM variables defined in 1_wrangle_osm.
  #first link each edge to a corresponding osm id
  #12/5/21 note this allows us to skip running all these OSM variables through the spatial join, 
  #which is very slow.
  left_join(all_h_osm_wrangle_both_nogeo, by = "osm_id_osm") %>%
  #the only variables that came from the initial strava basemap. all other are either from OSM or are derived.
  left_join(lookup_edge_id_xy_coord, by = "edge_id") %>% 
  
  #12/7/21 link in the diss infra data that you've updated.
  #the main purpose of this is to get the ribbon date approx so you can define open date for the aim 1 infrastructure
  mutate(
    #measure length of each feature
    #note, we also have a length measurement for the osm original file
    length_m = as.numeric(st_length(geometry)),
    
    #10/5/ decision to get rid of the stupid mdg code. Just note throughout that
    #all of the geometry have been re-measured by me
    length_km = as.numeric(length_m)/1000, 
    length_mi = length_km*0.621371     ,
    length_cm = as.integer(length_km*100000), #integer for speed
    length_mm = as.integer(length_km*1000000) ,
    
    #a dichotomous indicator for short segments
    length_below_15m = case_when(
      length_m < 15 ~ 1,
      TRUE ~ 0
    ),
    
   #Luckie St below Ivan Allen is called a residential road. That seems weird. Consider
    #recoding to tertiary
    project_luckie_st_residential =case_when( 
      osm_id_osm == 41374791 ~ 1,
      osm_id_osm == 561083964 ~ 1),
    
    highway = case_when(
      
      #fix the one at trinity that was in strava osm and was not recoded in the osm data
      osm_id_strava ==  414497329 ~  "tertiary", #this is coded as a cycleway but link it to trinity's highway
      osm_id_strava ==  414497333 ~  "tertiary",
      
      #you know what, Mike. it might be perceived as dishonest. just leave it as is.
      #      project_luckie_st_residential==1 ~ "tertiary",
      TRUE ~ highway,
    ),
    ## fix the highway categories----------------
    #Some that were joined spatially did not link to the original basemap--
    #PATH400 (In Development)
    highway_6cat = case_when(
      grepl("PATH400 (In Development)", osm_name_strava) ~ "path - paved or not",
      grepl("tertiary", highway) ~ "primary-tertiary road",  #to pick up trinity changes
      #else, set it to the value that it was. this will mean that some remain missing.
      TRUE ~highway_6cat), 
    
    highway_9cat = case_when(
      grepl("PATH400 (In Development)", osm_name_strava) ~ "path - paved or not",
      grepl("tertiary", highway) ~ "tertiary road", #to pick up trinity changes
      #else, set it to the value that it was. this will mean that some remain missing.
      TRUE ~highway_9cat), 
    
    #I want a numeric highway variable - in decreasing order of danger
    highway_9cat_num  = case_when(
      highway_9cat == "trunk road" ~ 0,
      highway_9cat == "primary road" ~ 1,
      highway_9cat == "secondary road" ~ 2,
      highway_9cat == "tertiary road" ~ 3,
      highway_9cat == "residential road" ~ 4,
      highway_9cat == "unclassified or service" ~ 5,
      highway_9cat == "living street" ~ 6,
      highway_9cat == "path - paved or not" ~ 7,
      TRUE ~ 8),
    
    #adding this 10/26/2020 - also including in table 1 results aim 3
    #same: remove the word "road". see below.
    highway_6cat_ordered_with_trunk = case_when(
      highway_9cat == "trunk road" ~ "0-trunk",
      highway_9cat == "primary road" ~ "1-primary",
      highway_9cat == "secondary road" ~ "2-secondary",
      highway_9cat == "tertiary road" ~ "3-tertiary",
      highway_9cat == "residential road" ~ "4-residential",
      highway_9cat == "unclassified or service" ~ "5-unclassified or service",
      #I'm lumping living street in with unclassified
      highway_9cat == "living street" ~"5-unclassified or service"
    ),
    
    #another one for easier table making, where trunk is included in primary
    #Update July 26, 2022: to make consistent with the intersection version created in 2_1_base...,
    #remove "road" from the category name
    highway_5cat_ordered_collapse_trunk = case_when(
      highway_9cat == "trunk road" ~ "1-trunk or primary", #i.e., it used to be 1-trunk or primary road
      highway_9cat == "primary road" ~ "1-trunk or primary",
      highway_9cat == "secondary road" ~ "2-secondary",
      highway_9cat == "tertiary road" ~ "3-tertiary",
      highway_9cat == "residential road" ~ "4-residential",
      highway_9cat == "unclassified or service" ~ "5-unclassified or service",
      #I'm lumping living street in with unclassified
      highway_9cat == "living street" ~"5-unclassified or service"
    ),
    
    #update 11/2/2020 - a category that collapsed primary w secondary and tertiary with residential
    #for aim 3 stratified analysis
    highway_collapsed_2_2_ordered =
      case_when(
        highway_9cat == "trunk road" ~ "1-prim-sec",
        highway_9cat == "primary road" ~ "1-prim-sec",
        highway_9cat == "secondary road" ~ "1-prim-sec",
        highway_9cat == "tertiary road" ~ "3-tert-res",
        highway_9cat == "residential road" ~ "3-tert-res",
        TRUE ~ "5-other" #weird that this is 5-..but whatevs.
      ),
    
   #comment July 26, 2022
   #I had defined hwy_2, hwy_4, and hwy_6 here, but I'm going to reserve
   #those variable names for a slightly distinct highway classification
   #where I classify the roadway segment as its highway category but 
   #any intersections with off-street paved trails as the most major crossing
   #street.

    
    #update 10/26/2020 - I'm including freedom parkway (the trunk road) as a 
   #part of primary through tertiary. this major_or_res is used in aim 2, so keep
    major_or_res = case_when(
      highway_9cat == "trunk road" ~ 1,
      highway_6cat == "primary-tertiary road" ~ 1,
      highway_6cat == "residential road" ~ 0
    ),
    #convert to integer for speed
    major_or_res = as.integer(major_or_res),
    
    #these should also change since they depend on those above
    major_or_res_highway_6cat = case_when(
      highway_9cat == "trunk road" ~ "primary-tertiary road", #(I'm still calling it primary-tertiary for a2 posterity)
      highway_6cat == "primary-tertiary road" ~ "primary-tertiary road",
      highway_6cat == "residential road"~ "residential road",
      TRUE ~ highway_6cat),
    
    #this one will make all the missing other, which should be easier to work with.
    major_or_res_other = case_when(
      highway_9cat == "trunk road" ~ "primary-tertiary road",
      highway_6cat == "primary-tertiary road" ~ "primary-tertiary road",
      highway_6cat == "residential road"~ "residential road",
      TRUE ~ "other"),
    
    # a version for a table where I include the numbers like this
    major_or_res_other_for_table= case_when(
      major_or_res_other == "primary-tertiary road" ~ "roadway-type-1",
      major_or_res_other == "residential road" ~ "roadway-type-2",
      major_or_res_other == "other" ~ "roadway-type-3"),
    
    ## manually recode infrastructure that was wrong based on the merge with OSM----------
      #or was wrong in OSM because the osm segments aren't short enough.
    
    ### westview drive sections that were converted back from protected bike lane to conventional--------
    #Note, I'm decidi`ng to do this by edge_id in this code rather than by osm_id
    #in the 1_wrangle_osm code because the edge ids are more precise. the osm-id doesn't stop/start where I need it to
    #so this will need to be changed in the longitudinal definition
    project_westview_protected_temporary = case_when(
      edge_id %in% c( 1183781, 1183780, 1183779, 1183778, 1183776, 1183775, 1183777 ) ~ 1),
    
    ### Path 400 fixes-------
    #12/4/21 have to define based on edge id rather than OSM id
    project_path_400_old_ivy_to_wieuca = case_when(
      edge_id %in% c(1023660)~ 1,
      TRUE ~ 0),
    
    project_path_400_sidney_marcus_to_miami_cir = case_when(
      edge_id %in% c(1023657, 1023658) ~ 1,
      TRUE ~ 0),
    
    #make a path-400 dissertation aim 1 variable in the same format as your 
   #others in the wrangle_osm code
    diss_a1_p4h = case_when(
      project_path_400_old_ivy_to_wieuca==1 ~ 1,
      project_path_400_sidney_marcus_to_miami_cir==1 ~ 1,
      TRUE ~0
    ),
    
    #everything else that says it's path 400 will just get classified as something else
    project_path_400_after_a1 = case_when(
      
      #if it's this pre-diss spot, then set it to zero
      project_path_400_old_ivy_to_wieuca == 1 ~ 0,
      project_path_400_sidney_marcus_to_miami_cir == 1 ~ 0 ,
      #else set it to one if path 400 is 1
      project_path_400 == 1 ~ 1, 
      TRUE ~ 0  
    ),

    
    ### ponce buffered bike lane fixes---------
    # it should stop at myrtle and begin at whole foods/ home depot parking lot. west of piedmont to juniper, 
    #it's a normal conventional bike lane. it's a sharrow west of juniper to peachtree.
    project_ponce_buffered_to_none = case_when(
      edge_id %in% 
        c(792255, 792254, 792253, 792252,792251, 792250,
          792233, 792234, 792236, 792237, 792238, 792239, 792243, 792244, 792245,
          792249, 792242, 792235, 792241, 792240, 792246, 792247,
          792248 )  ~ 1),
    
    project_ponce_buffered_to_conventional = case_when(
      edge_id %in% 
        c(985938, 1129039, 985937, 749298, 749297)  ~ 1),
    
    ### ralph david abernathy (rda fixes)--------------
    #both of the OSM-id-based indicator vars need fixing.
    #first, make the one near turner field much smaller. simply re-classify it.
    #this completely overwrites the osm-based version
    project_rda_bike_lane_buff_2018mar = case_when(
      edge_id %in% 
        c(1145925, 360660)  ~ 1,
      TRUE ~ 0),
    
    #### rda december 2018 lanes--------------------
    #also need to split up that section of RDA that we called buffered
    #when it's actually conventional. the ribbon date will be the same.
    #12/17/21 somehow through the merge, the infra didn't transfer as expected
    #hsve to hard recode.
    #until lucile, it's conventional
    project_rda_cascade_mlk_bike_lane_conv = case_when(
      edge_id %in% c(
        806025, 
        806024,
        806023, 
        806022, 
        806021, 
        806020,
        806033 #this is a long section near the cemetery that's conventional
        ) ~1, 
      #there's a small part that was coded correctly.
      project_rda_cascade_mlk_bike_lane_conv==1 ~
        project_rda_cascade_mlk_bike_lane_conv,
      TRUE ~0 
      ),
    #now we can reclassify this as buffered wherever the main project
    #is still 1 and it's not conventinonal
    project_rda_cascade_mlk_bike_lane_buff = case_when(
      project_rda_cascade_mlk_bike_lane_dec2018==1 &
        project_rda_cascade_mlk_bike_lane_conv==0 ~1,
      TRUE ~0
    ),

    ### Decatur-area work-------------------
    #doing this part 6/2/21
   
    #### east lake buffered bike lanes------
    #Per Google Streetview 
    #https://www.google.com/maps/@33.7657443,-84.3116656,3a,75y,179.11h,79.36t/data=!3m7!1e1!3m5!1s46XuDRnTzQwpZlWHsX9zjw!2e0!5s20171001T000000!7i13312!8i6656
    #the buffered lanes were there October 2017
    #In July 2015, they were conventional bike lanes (not applicable to this study)
    project_east_lake_buffered = case_when(
      edge_id %in% c(768756, 735117, 256181) ~ 1),
    
    #### missing conventional bike lanes in Decatur----
    #Park Place near East Lake MARTA
    project_east_lake_park_place_conventional = case_when(
      edge_id %in% c(243018, 257082, 768437, 243018, 1127928) ~ 1),
   
   ### Piedmont Park work-------
   #Here September 2, 2022. Some trails in Piedmont Park have not been
   #classified yet, somehow.
   #this is the beltline extension through piedmont, dirt for the duration
   #of this study
   project_piedmont_park_dirt = case_when(
     edge_id %in% c(
       827843, 828156, #eastside trail gravel north of monroe
       860884, 860878, 828406, 763720 #active oval
       ) ~1),
   
   project_piedmont_park_paved = case_when(
     edge_id %in% c(
       805104, 763707,
       805086, 805087, 805085,805091, 763708,763715,783179, 
       805097,783174,763706,763711,763719,763716,783172,763703,
       828170,828409,
       805089,
       828172
       )~1),

   
   
   ### re-create infra_6cat var-----------
  
    infra_6cat = case_when(
      
      project_east_lake_park_place_conventional ==1 ~ "bike_lane_conventional",
      project_east_lake_buffered == 1 ~ "bike_lane_buffered",
      project_ponce_buffered_to_none==1 ~ NA_character_,
      project_ponce_buffered_to_conventional==1 ~"bike_lane_conventional",
      
      #note this is as of 7/14/2017
      project_westview_protected_temporary  == 1 ~ "bike_lane_conventional",       
      
      #re-classify the ralph david abernathy lanes
      project_rda_bike_lane_buff_2018mar==1 ~"bike_lane_buffered",
      project_rda_cascade_mlk_bike_lane_buff==1 ~ "bike_lane_buffered",
      project_rda_cascade_mlk_bike_lane_conv==1 ~ "bike_lane_conventional",
      
      #Piedmont Park stuff
      project_piedmont_park_dirt == 1 ~ "off_street_trail_dirt",
      project_piedmont_park_paved == 1 ~ "off_street_trail_paved",

      #use the %in% coding. it will be faster.
      
      #### set the following to missing------
      osm_id_strava %in% c(
        525613028, #somehow highland got coded as a bike lane. it's not. it's nothing.
        #it got coded as a bike lane during the merge between the arc-coa data somehow (1_wrangle_osm)
        79391905, #stray sharrow that's just a parking lot
        96433108,#stray segment on peachtree. set to missing
        #mclendon and lake claire
        395875935,
        395875929,
        392497220,
        392357109 #sinclaire
      ) ~ NA_character_ ,
      
      #### exclude by edge_id----
      edge_id %in% c(
        1271020,
        
        #10th and juniper,
        1128977,
        
        #donald lee and beltline - extraneous thing to get rid of
        762293,
        
        #random bike lane at tech miscoded
        752241,
        1224047,
        
        830753, #between dekalb and edgewood
        
        
        #bike lane vs trail on trinity
        1128002,
        #clifton and haygood intersection crossing
        1129304,
        1129303 
      )   ~ NA_character_,

      edge_id == "825556" ~ "off_street_trail_paved", 
      edge_id %in% c(
        1093486,  #euclid and moreland walkway at L5P - it's a path
        
        #these are tech parkway north of north - not technically a protected bike lane
        825556,        
        825555
      ) ~ "off_street_trail_paved", 
      
      edge_id %in% c(
        1224093 #code das a sharrow at hemphill; it's a sidewalk that should be a bike lane
      )  ~ "bike_lane_conventional", 
      
      #a few edges of sylvan are actually buffered, not conventional
      edge_id %in% c(
        1325934, 1325935, 1325936, 1325937,1325938
      ) ~"bike_lane_buffered",
      
      #exclude asbury circle at Emory because paths aren't coded consistently at 
      #Emory, even though it is a path
      grepl("Asbury Circle", 	osm_name_osm) ~  NA_character_,
      
      TRUE ~infra_6cat
    ),
    
    
    #now classify the others according to how you just did that one, above, following the same code
    infra_6cat_none = case_when(
      is.na(infra_6cat)==TRUE ~ "none",
      TRUE ~ infra_6cat
    ),
    
    ### re-define infra_6cat_none_abbrev (repeating from 1_wrangle_osm code)-----------
    infra_6cat_none_abbrev = case_when(
      infra_6cat_none == "off_street_trail_paved" ~ "trail_p", 
      infra_6cat_none == "off_street_trail_dirt" ~ "trail_d", 
      infra_6cat_none == "bike_lane_protected" ~ "lane_p", 
      infra_6cat_none == "bike_lane_buffered" ~ "lane_b", 
      infra_6cat_none == "bike_lane_conventional" ~ "lane_c", 
      infra_6cat_none == "sharrow" ~ "sharrow", 
      infra_6cat_none == "none" ~ "none" 
    ),
   
   #1/3/22
   #make dichotomous versions of each so that you can include as a dummy var
   #in sf model, #dich for dichotomous
   infra_dich_trail_p=case_when(
     infra_6cat_none_abbrev == "trail_p" ~ 1,
     TRUE ~ 0),
   infra_dich_trail_d=case_when(
     infra_6cat_none_abbrev == "trail_d" ~ 1,
     TRUE ~ 0),
   infra_dich_lane_p=case_when(
     infra_6cat_none_abbrev == "lane_p" ~ 1,
     TRUE ~ 0),
   infra_dich_lane_b=case_when(
     infra_6cat_none_abbrev == "lane_b" ~ 1,
     TRUE ~ 0),
   infra_dich_lane_c=case_when(
     infra_6cat_none_abbrev == "lane_c" ~ 1,
     TRUE ~ 0),
   infra_dich_sharrow=case_when(
     infra_6cat_none_abbrev == "sharrow" ~ 1,
     TRUE ~ 0),
   

    #make this to get the order right for mapviews
    infra_6cat_legend_none = case_when(
      infra_6cat_none_abbrev == "trail_d" ~ "1-Trail-Dirt",
      infra_6cat_none_abbrev == "trail_p" ~ "2-Trail-Paved",
      infra_6cat_none_abbrev == "lane_p" ~ "3-Lane-Protected",
      infra_6cat_none_abbrev == "lane_b" ~ "4-Lane-Buffered",
      infra_6cat_none_abbrev == "lane_c" ~ "5-Lane-Conventional",
      infra_6cat_none_abbrev == "sharrow" ~ "6-Sharrow",
      infra_6cat_none_abbrev == "none" ~ "7-None"),
    
    #make this to get the order right for mapviews
    infra_6cat_legend_nodirt = case_when(
      infra_6cat_none_abbrev == "trail_p" ~ "1-Trail-Paved",
      infra_6cat_none_abbrev == "lane_p" ~ "2-Lane-Protected",
      infra_6cat_none_abbrev == "lane_b" ~ "3-Lane-Buffered",
      infra_6cat_none_abbrev == "lane_c" ~ "4-Lane-Conventional",
      infra_6cat_none_abbrev == "sharrow" ~ "5-Sharrow",
      TRUE ~ "none" ),
    
    
    #an indicator for a trail or not that is used in subsequent code
    infra_trail_dirt_or_paved = case_when(
      infra_6cat == "off_street_trail_dirt" ~ 1,
      infra_6cat == "off_street_trail_paved" ~ 1,
      TRUE ~ 0
    ),
    
    
    ##---finish the diss_a1_any vars (12/5/21)-------
    #update 12/17/21 calling these _prelim as the final versions will be created farther below.
    #dissertation aim 1 variables (preliminary; before you add more below)
    diss_a1_any_prelim = case_when(
      diss_a1_p4h==1 ~ 1, #add path 400 as a possibility here
      diss_a1_any==1 ~ 1, #otherwise, if the old any var is there
      TRUE ~ 0
    ),
    
    #let's also make one that's called diss_a1_eval
    #similar to our other notation for those that we're actuallye evaluating
    #note this one doesn't have path 400
    diss_a1_eval_prelim = case_when(
      diss_a1_wst_pro==1 ~ 1,
      diss_a1_lsl_gtp==1 ~ 1,
      diss_a1_est==1 ~ 1,
      diss_a1_pct==1 ~ 1,
      diss_a1_mcd==1 ~ 1,
      TRUE ~ 0
    ),
    
    #### make an analogous name_section_short variable like I've already------
    #created in the 0_miss_inf_dataprep
    #a long name but I'm specifying that it's any so I 
   #can just restrict to diss_a1_eval if needed.
    diss_a1_any_name_section_short = case_when(
      diss_a1_wst== 1 ~ "wst",
      diss_a1_pro== 1 ~ "pro",
      diss_a1_lsl== 1 ~ "lsl",
      diss_a1_gtp== 1 ~ "gtp",
      diss_a1_est1== 1 ~ "est1",
      diss_a1_est2== 1 ~ "est2",
      diss_a1_est3== 1 ~ "est3",
      diss_a1_mcd== 1 ~ "mcd",
      diss_a1_pct1== 1 ~ "pct1",
      diss_a1_pct2== 1 ~ "pct2",
      
      #might as well include these two as well
      diss_a1_p4h== 1 ~ "p4h" ,
      diss_a1_rda_cas == 1 ~ "rda_cas", #again, this rda is just that west of westside trail
      
    ),
    
    #---a short name for the group------------#
    #note I'm calling this eval because I use it more for actual reporting
    #where I won't be using p4h and rda
    #Decision 12/16/21 decision to omit the "eval" from the name variable
    #Can classify eval vs any instead by the indicator
    #used to be here: diss_a1_eval_name_group_short
    
    #to be consistent with below wrangling for some new groups, I'm also creating this here:
    diss_a1_any_name_group_short = case_when(
      diss_a1_lsl== 1 ~ "lsl_gtp",
      diss_a1_gtp== 1 ~ "lsl_gtp",
      diss_a1_wst== 1 ~ "wst_pro",
      diss_a1_pro== 1 ~ "wst_pro",
      diss_a1_est1== 1 ~ "est",
      diss_a1_est2== 1 ~ "est",
      diss_a1_est3== 1 ~ "est",
      diss_a1_pct1== 1 ~ "pct",
      diss_a1_pct2== 1 ~ "pct",
      diss_a1_mcd== 1 ~ "mcd",
      diss_a1_p4h== 1 ~ "p4h" ,
      diss_a1_rda_cas == 1 ~ "rda_cas",
      TRUE ~ "else" #setting it to else instead of missing
    ),
    
    #for when you want to enumerate the total amount of mileage, create
    #a variable that eliminates the parallel duplicates.
    #(you won't remove these for good because you want to capture rides that were assigned
    #to these infarstructures in Strava, but for descriptive purposes, it's useful)
    
    
    #### ribbon date finish and fix---------
    #if ribbon date is missing, set it to before the study. (Jan 1, 2017)
    #This might turn out to be critical 6/2/21
    ribbon_date_missing = case_when(
      is.na(ribbon_date)==TRUE & is.na(infra_6cat==FALSE) ~ 1),
    
    #fix the one westview drive section. call the ribbon date the date it changed from
    #protected bike lane to conventional bike lane (or nothing?)
    ribbon_date = case_when(
      ribbon_date_missing == 1 ~lubridate::ymd(20170101), #so it works with the rest of the code. pick jan 1, 2017
      
      #Per review of 2017 report or consistent with dissertation infrastructure
      #12/17/21 this is  where  the error came from...
      #not sure why I coded this again here but must have had a reason
      project_westside_trail_paved == 1 ~ lubridate::ymd(20170929), 

      #12/4/21 here to update path 400 based on edge id.
      project_path_400_old_ivy_to_wieuca ==1 ~ lubridate::ymd(20161215),
      project_path_400_sidney_marcus_to_miami_cir ==1 ~ lubridate::ymd(20161215),
      project_path_400_after_a1 ==1 ~ lubridate::ymd(20190528),
      #everything else, just pick a date later in the future:
      #can say 2019/5/28, which is about right per the youtube video cited here
      #https://atlanta.curbed.com/2019/5/28/18641829/  
      
      project_rda_bike_lane_buff_2018mar == 1 ~ lubridate::ymd(20180301),

      #else, use the other data in the ribbon_date column.
      TRUE ~ ribbon_date),
    
    #see 1_wrangle_osm for explanation of this variable.
    ribbon_month_after_or_same = case_when(
      project_westside_trail_paved == 1 ~ 1, #late in the month, although I rode on it earlier.
      project_path_400_old_ivy_to_wieuca == 1 ~ 0,
      project_path_400_sidney_marcus_to_miami_cir==1 ~0,
      project_path_400_after_a1==1 ~0,
    TRUE ~ ribbon_month_after_or_same), #else set it to what it was in the first code
      
    #simply make these again
    ribbon_year = lubridate::year(ribbon_date),
    ribbon_month = lubridate::month(ribbon_date) + ribbon_month_after_or_same,

    
    #and these - also repeated from the other code
    #a numeric variable that lines up with study month for easy subtraction
    ribbon_study_month = case_when(
      ribbon_year < 2016 ~ -1,  
      ribbon_year == 2016 ~ ribbon_month - 9, 
      ribbon_year == 2017 ~ ribbon_month + 3,  
      ribbon_year == 2018 ~ ribbon_month + 3+12, 
      ribbon_year == 2019 ~ ribbon_month + 3+12+12,  
      ribbon_year == 2020 ~ ribbon_month + 3+12+12+12),  

    #ribbon date sources as well
    ribbon_date_source = case_when(
      project_rda_bike_lane_buff_2018mar == 1 ~ "City Report, 2018",
      TRUE ~ribbon_date_source), #from the osm code

    
    ### maximum study month-------
    #speaking of the ribbon date, you need a maximum study month so that you
    #can exclude stuff that was open after your study ended
    #without having to summarize the longitudinal infrastructure (defined later)
    #I'd prefer to rename this to specify aims 2 and 3, but not doing so in case
    #it messes up code elsewhere.
    study_month_max = 23, #note this is only valid for aims 2/3
    
    ribbon_after_study = case_when(
      ribbon_study_month > study_month_max ~1 ,
      ribbon_study_month <= study_month_max ~0 ,
      TRUE ~0 #if it's missing, it means it's earlier. anything later should have a date.
      
    ),
    #do the same thing but for aim 1
    study_month_max_aim1 = 24,
    ribbon_after_study_aim1 = case_when(
      ribbon_study_month > study_month_max_aim1 ~1 ,
      ribbon_study_month <= study_month_max_aim1 ~0 ,
      TRUE ~0 #if it's missing, it means it's earlier. anything later should have a date.
    ),
      
    ## Indicators for excluding for length, e.g., sidewalks & boulevards---------
    ### exclude sidewalks-----
    infra_exclude_for_length_sidewalks =
      
      #note, this might also be driveways or crosswalks
      case_when(
        edge_id %in% c(
          
          #georgia tech sharrow exclusions because they're actually a sidewalk
          1224241,
          1224242,
          1224232,
          1224235,
          1224166,
          1224233,
          1224187,
          1224188,
          1224190,
          1224192,
          1224197,
          
          1224117,
          1224131,
          1224121,
          1224210,
          1224276,
          1224209,
          1224274,
          1224248,
          1224253,
          1224256,
          871241,
          1224272,
          
          1148621,
          1148622,
          1148623,
          1148602,
          1148624,
          
          1148612,
          1148606,
          844066,
          1224252,
          1224254,
          1224247,
          1148620,
          1148619,
          1148608,
          844065,
          
          1148607,
          1011369,
          
          #georgia tech bike lane sidewalk exclusions
          1224262,
          1224263,
          1224093, #double coded as a sharrow/ bike lane. recode as nothing here for the double-counting
          
          #georgia tech path sidewalk exclusions
          1224245,
          1224265,
          
          #georgia tech coded as a residential road. should be an excluded sidewalk
          1224270,
          1224134,
          1224295,
          1224125,
          1214208,
          1224179,
          1224251,
          
          #georgia tech 5th st
          1224199,
          
          #georgia tech 6th st
          1057698, 
          
          #intersections near ponce
          1129042,
          
          #park place buffered bike lane - sidewalk
          1018426,
          
          #ralph mcgill sidewalk
          1177834,
          
          #Mclendon sidewalks - sharrow
          1080145,
          1080146,
          1085036,
          1085041,
          1085402,
          1093487,
          1085399,
          1085400,
          1093496,
          1085332,
          1093505,
          1085327,
          1085325,
          1080110,
          1080116,
          1080118,
          1085303,
          1093529,
          1093522,
          1085330,
          1080117,
          1085292,
          1085403,
          1093498,
          1085328,
          
          #seminole at little five points - sidewalk coded as sharrow
          1085066,
          
          #mclendon sidewalks - conventional bike lane
          1122854,
          1127902,
          1127902,
          1127871,
          1127904,
          1127872,
          1127913,
          1127878,
          1127870,
          1127909,
          
          #clifton in candler park
          1139704,
          
          #other candler park
          
          
          #peachtree circle in ansley park sidewalks
          1128929,
          1128931,
          1128932,
          1128933,
          1128939,
          1128938,
          1128940,
          1128948,
          1128930,
          1128928,
          
          #west ponce and trinity sidewalks (decatur) and double coding of bike lane
          1127987,
          1127996,
          1127943,
          1127985,
          1128004,
          1127961,
          1128006,
          1128007,
          1128016,
          1128020,
          1127986,
          1127948,
          1127984,
          1128009,
          1128005,
          1127957,
          1127950,
          1127960,
          1127959,
          1128003,
          1127983,
          1127958,
          1084494,
          1084503,
          1128012,
          
          #bill kennedy west of roadway
          1136613,
          
          #bill kennedy - the new trail east of roadway that shouldn't be double counted during the study period
          1136604,
          
          #bill kennedy - beltline trail segment (did I make this?) across i-20
          1136604,
          
          #glenwood south of roadway
          1136908,
          1136907,
          1136905,
          1136911,
          1136915,
          
          1136927, 
          1136922,
          
          #a driveway on ponce in the buffered bike lane
          1177871,
          
          #crosswalk on ponce and juniper
          1129039,
          
          #intersection sidewalks/crosswalks at ivan allen and peachtree - should be rid
          1129095,
          1129096,
          1129097,
          1129098,
          
          1129074, #peachtree and pine crosswalk
          
          #conventional bike lanes at tech (make this the end to avoid the comma issue)
          1224066
        ) ~ 1
      ),
    
    ### double-counting boulevards------------
    infra_exclude_for_length_blvd = case_when(
      edge_id %in% c(
        
        #marietta - pick one side of the boulevard - exclude north side
        800158,
        800159,
        817627,
        817630,
        817631,
        1313672,
        1313671,
        
        #macon dr southwest
        805758,
        805757,
        805759,
        
        #10th st exclude north side
        1195938,
        1195939,
        
        #charles allen exclude east side
        855349,
        855348,
        855347,
        855346,
        855344,
        
        #	Donald Lee Hollowell Parkway Northwest
        1127184,
        758628,
        1127193,
        975904,
        975905,
        
        #state st at tech
        765341,
        765342,
        765343,
        765344,
        765345,
        1224071,
        
        #ralph david abernathy - north side -across connector - buffered
        360660,
        1145948,
        360665,
        360667,
        360662,
        360666,
        360663,
        360664,
        360661,
        
        #ralph david abernathy - north side -west of connector - conventional
        791776,
        791777,
        791783,
        1184055,
        1145940,
        1184057,
        360666,
        791780,
        791782,
        791781,
        791778,
        1184056,
        
        #ivan alley at world of coca-cola
        960997,
        960998,
        960999,
        
        
        #bike lanes on 17th st
        736530,
        736531,
        736533,
        736534,
        763457,
        763460,
        764124,
        1206902,
        1206904,
        736528,
        735655,
        735656,
        735657,
        763458,
        763461,
        736532,
        1206903,
        1206905,
        764125,
        735654,
        763455,
        763459,
        
        #howell mill exclude east side
        1038102,
        764142,
        764141,
        1038103,
        
        #peachtree rd - buckhead - conventional - southeast side
        1203642,
        1203641,
        1203640,
        1421167,
        779159,
        779157,
        779112,
        779162,
        779156,
        1203639,
        1421169,
        779155,
        779154,
        779152,
        779151,
        755885,
        779153,
        
        #roxboro road buckhead - north side
        1184393,
        1184396,
        1184394,
        1184390,
        1184392,
        1184395,
        
        #joseph e lowry over i-20
        1195948,
        1195957,
        1195958,
        1195946
      ) ~ 1,
      TRUE ~ 0
    ),
    
    ### double counting cycletracks----
    infra_exclude_for_length_cycletrack = case_when(
      edge_id %in% c(
        #keeping the roadway file and excluding the one that is specifically coded at the cycletrack
        1018455,
        1018456,
        1018459,
        1129129,
        1018454,
        1018453,
        1018451,
        1018449,
        1018448,
        1018446,
        1018445,
        1018444,
        1018443,
        1018447,
        
        1018457,
        1018460,
        1018461,
        1018462,
        
        #luckie st protected bike lane
        825557,
        
        #intersections and perpendicular segments
        1129137,
        1129136,
        1129131,
        1129134,

        794357,
        766736 #n mcdonough and school
      ) ~ 1
    ),
    
    ### double counting off-street paths------
    infra_exclude_for_length_trail_paved = case_when(
      edge_id %in% c(
        #duplicate atlanta beltline westside trail (excluding the one without an osm_id_osm)
        327174, #32174 does NOT go all the way to lena st, as it should.
        #327174 also goes too far down to the beltline southside trail,
        #so exclude 
        
        #what to keep?
        
        # 1423727, #keep
        # 1423730, #keep these
        
        #an extraneous eastside trail segment at dekalb avenue south of edgewood
        316159,
        981386,
        1047668,
        
        #westview drive double counting
        749623,
        
        #remove howard st where it parallels stone mountain trail
        257740,
        257739,
        257737,
        257735,
        257742,
        217583,
        257744,
        257738,
        257745,
        794334,
        794355,
        257736,
        257738,
        
        257746
        
        #Here again 12/4/21 we should be excluding path 400 as well
        #oh, actually it will be removed based on the ribbon date
      ) ~ 1
      
    ),
    
    infra_exclude_for_length = case_when(
      infra_exclude_for_length_blvd == 1 ~ 1,
      infra_exclude_for_length_sidewalks == 1 ~ 1,
      infra_exclude_for_length_cycletrack ==1 ~ 1,
      infra_exclude_for_length_trail_paved == 1 ~ 1,
      TRUE ~ 0
    ),
    
    infra_exclude_for_length_except_blvd = case_when(
      infra_exclude_for_length_sidewalks == 1 ~ 1,
      infra_exclude_for_length_cycletrack ==1 ~ 1,
      infra_exclude_for_length_trail_paved == 1 ~ 1,
      TRUE ~ 0
    ),
    
    ## a variable that is used in your tables and exclusions------------
    #(only minor concern is these are not time -dependent but that shouldn't be an issue)
    #see my mapview in the basemap checks code. there are quite a few dirt paths and sidewalks
    #and golf cart tracks, etc., that are not coded as infrastructure nor are they passable by a car
    #for the moment, I'm getting rid of them. it'd be nice to classify them as off-street paved trails
    #when possible but that HAS TO BE LATER.
    #3/10/21 update - I said the HAS TO BE LATER during the dissertation phase. I could include now?
    
    #the purpose of this indicator variable is to exclude missing highway variables unless infra is a trail
    highway_missing_no_trail_exclude = case_when(
      #it's supposed to be missing if it's a trail..
      infra_6cat_none_abbrev == "trail_p" ~0,  
      infra_6cat_none_abbrev == "trail_d" ~ 0,
      
      is.na(highway_6cat_ordered_with_trunk)==TRUE ~ 1, #else if it's missing but not a trail
      
      TRUE ~ 0),
    
    #    table(edge_mo_all_with_et_aim3_gears_nogeo$highway_6cat_ordered_with_trunk)
    #in the same vein....
    #table(edge_mo_all_with_et_aim3_gears_nogeo$major_or_res)
    #exclude trails on roadway. it's confusing. there aren't that many.
    
    infra_trail_p_on_roadway = case_when(
      infra_6cat_none_abbrev== "trail_p" & 
        is.na(major_or_res)==FALSE ~ 1,
      infra_6cat_none_abbrev == "trail_p" & 
        highway_6cat_ordered_with_trunk == "5-unclassified or service" ~ 1,
      infra_6cat_none_abbrev == "trail_p" & 
        highway_6cat_ordered_with_trunk == " 0-trunk" ~ 1,
      TRUE ~0),
    
    #bikeable or not (i.e., exclude interstates and trunk roads, e.g. freedom pkwy)
    nobikes = case_when(
      grepl("motorway", highway) ~ 1, #motorway and motorway_link
      TRUE ~ 0),
    
    #i'm including freedom parkway here.
    highway_trunk = case_when(
      grepl("trunk", highway) ~ 1, #trunk and trunk link
    )) %>%  
  
  #Mike, you're already excluding no bikes, so your no bikes exclusion later are pointless
  filter(nobikes == 0) %>%
  #remove the stray jonesboro rd se to the airport strand
  filter(osm_id_strava != "164879063") %>% 
  
  mutate(edge_id_def_unique  = row_number()) %>% 
  
    ## organize the variables that you care about and then everything else after-----
  dplyr::select(
    starts_with("edge_id"),
    starts_with("osm_"), #this gets the IDs, the names, the indicator, the chopped_no
    starts_with("join"),
    starts_with("length"),
    starts_with("infra"),
    beltline,
    starts_with("cycleway"), 
    starts_with("ribbon"), 
    starts_with("study_month"), 
    starts_with("project"),
    starts_with("diss"), #this picks up the dissertation aim 1 indicators created in osm wrangle
    starts_with("highway"), #this grabs highway_1
    starts_with("hwy"), #grab the small highway variables
    starts_with("major_or_res"), #in case there are any descendents
    contains("bicycle"),
    starts_with("length_") ,#this will grab the length variable I made, too.
    motor_vehicle,
    contains("surface"),
    paved,
    tracktype,
    contains("sidewalk"),
    starts_with("mtb"),
    contains("foot"),
    proposed  ,
    service, #this says what type of roadway it is if it's service, like driveway, which you may exclude

    name_1 ,

    x_coord_1, y_coord_1, x_coord_2, y_coord_2 ,  
    contains("_diff"), #for x abs diff and y abs diff
    north_south
    )

## save first main one-----------
save(bmap_edge_join_wrangle0, file = "bmap_edge_join_wrangle0.RData")
## checks-----------
nrow(bmap_edge_join_wrangle0)
table(bmap_edge_join_wrangle0$nobikes) #yep
names(bmap_edge_join_wrangle0)
#update 6/2/21 how many are missing ribbon. and if they're missing ribbon, they may not get processed later...

## check on Piedmont Park. It seems I'm not picking up some paths:
load("monpon_sf_1mi.RData")
load("monpon_sf_2mi.RData")
mv_piedmont_park_check = bmap_edge_join_wrangle0 %>% 
  dplyr::select(edge_id, starts_with("infra") ) %>% 
  st_intersection(monpon_sf_2mi) %>% 
  mapview(zcol = "infra_6cat")
mv_piedmont_park_check

#table(bmap_edge_join_wrangle0$infra_6cat_none_abbrev)
table(bmap_edge_join_wrangle0$project_path_400_after_a1) #got it.
table(bmap_edge_join_wrangle0$project_path_400_old_ivy_to_wieuca)
table(bmap_edge_join_wrangle0$project_path_400_sidney_marcus_to_miami_cir)
table(bmap_edge_join_wrangle0$project_path_400)

#September 2022:
### check on highway category------------
#as I'm considering including unclassified / service in aim 3 analysis
table(bmap_edge_join_wrangle0$highway_9cat)
bmap_edge_join_wrangle0 %>% 
  filter(highway_9cat=="unclassified or service") %>% 
  mapview()

table(bmap_edge_join_wrangle0$highway)
#what's the difference between unclassified and service?
mv_service = bmap_edge_join_wrangle0 %>% 
  filter(highway =="service") %>% 
  mapview(layer.name = "service", color = "red")
mv_unclassified = bmap_edge_join_wrangle0 %>% 
  filter(highway =="unclassified") %>% 
  mapview(layer.name = "unclassified", color = "green")
mv_service+mv_unclassified
#9/2/22 okay, so there's a lot of driveways and other weird stuff. hmm.
#note there is a service designation for lots of the driveways.

#update 10/26/2020 - there are no duplicate edge ids. so remove this superfluous code
#distinct_edge = bmap_edge_join_wrangle0 %>% distinct(edge_id)

#curious about some of the paved-trail intersections
table(bmap_edge_join_wrangle0$infra_6cat)
# bmap_edge_join_wrangle0e %>% 
#   filter(infra_6cat == "off_street_trail_paved") %>% 
#   mapview()
# First set of look-ups----------------
## a row number for each osm name-highway combination--------

#---Update 10/19/2020 - limit to ROADWAYS ONLY -----#
#Then link the trails in as intersections only. 
#The idea is just to generate intersections where cars could be---

bmap_for_osm_name_lookup = bmap_edge_join_wrangle0 %>% 
  st_set_geometry(NULL) %>% 
  filter(infra_6cat_none_abbrev != "trail_p") %>% 
  filter(infra_6cat_none_abbrev != "trail_d") %>% 
  filter(is.na(highway)==FALSE) 

lookup_name_number_osm_name_present = bmap_for_osm_name_lookup %>% 
  filter(is.na(osm_name_osm)==FALSE) %>% 
  group_by(osm_name_osm, highway ) %>% 
  summarise(n_segments = n()) %>% 
  ungroup() %>% 
  #begin it with osm so that your other dplyr::select code works
  mutate(osm_name_highway_combo_id = row_number())

# lookup_name_number_osm_name_present %>%
#   dplyr::select(
#     starts_with("name_number"), highway, starts_with("osm_name")
#     ) %>%
#   View()

bmap_osm_present =  bmap_edge_join_wrangle0  %>% 
  filter(is.na(osm_name_osm)==FALSE) %>% 
  left_join(lookup_name_number_osm_name_present, by = c("osm_name_osm", "highway"))

nrow(bmap_osm_present)
add_to = n_distinct(lookup_name_number_osm_name_present)

lookup_name_number_osm_name_absent =  bmap_for_osm_name_lookup %>% 
  filter(is.na(osm_name_osm)==TRUE) %>% 
  filter(is.na(osm_name_strava)==FALSE) %>% 
  group_by(osm_name_strava, highway ) %>% 
  summarise(n_segments = n()) %>% 
  ungroup() %>% 
  mutate(osm_name_highway_combo_id = row_number()+add_to)

bmap_osm_absent = bmap_edge_join_wrangle0 %>% 
  filter(is.na(osm_name_osm)==TRUE) %>% 
  filter(is.na(osm_name_strava)==FALSE) %>% 
  filter(is.na(highway)==FALSE) %>% 
  left_join(lookup_name_number_osm_name_absent, by = c("osm_name_strava", "highway"))

nrow(bmap_osm_absent)
# 
# lookup_name_number_osm_name_absent %>% 
#   dplyr::select(
#     starts_with("name_number"), highway, starts_with("osm_name")
#   ) %>% 
#   View()

#need this for aim-3 analytic sample
lookup_infra_trail_p_on_roadway = bmap_edge_join_wrangle0 %>% 
  st_set_geometry(NULL) %>% 
  distinct(edge_id, infra_trail_p_on_roadway) %>% 
  as_tibble()

save(lookup_infra_trail_p_on_roadway, file = "lookup_infra_trail_p_on_roadway.RData")  

bmap_osm_highway_both = bmap_osm_present %>% 
  bind_rows(bmap_osm_absent) %>% 
  mutate(lookup_name_number_indicator =1)



## combo-id - highway - lookup-----

### Updated look-up code for highway combo 10/22/20. See the distinct argument----------
lookup_osm_name_highway_combo_id = bmap_osm_highway_both %>% 
  st_set_geometry(NULL) %>% 
  distinct(osm_name_highway_combo_id, highway, osm_name_osm, osm_name_strava)

#the above is generating problems. what if I want one that only has the ID and the highway type?
lookup_osm_name_highway_9cat_combo_id = bmap_osm_highway_both %>% 
  st_set_geometry(NULL) %>% 
  #explicit choice to use _9cat. also no strava version.
  distinct(osm_name_highway_combo_id, 	highway_9cat, osm_name_osm ) 


lookup_infra_6cat_legend_none = bmap_osm_highway_both %>% 
  st_set_geometry(NULL) %>% 
  distinct(infra_6cat_none_abbrev, infra_6cat_legend_none)
save(lookup_infra_6cat_legend_none, file = "lookup_infra_6cat_legend_none.RData")


lookup_osm_name_infra_combo_id = bmap_osm_highway_both %>% 
  st_set_geometry(NULL) %>% 
  filter( is.na(osm_name_highway_combo_id)==FALSE) %>% 
  #this isn't 1 to 1 so pick your favorite
  distinct(osm_name_highway_combo_id, infra_6cat_none_abbrev) %>% 
  left_join(lookup_infra_6cat_legend_none, by = "infra_6cat_none_abbrev") %>%  
  group_by(osm_name_highway_combo_id) %>% 
  arrange(infra_6cat_legend_none) %>% 
  slice(1)
nrow(lookup_osm_name_infra_combo_id)
nrow(lookup_osm_name_highway_9cat_combo_id)


lookup_osm_name_highway_combo_id_edge_id = bmap_osm_highway_both %>% 
  st_set_geometry(NULL) %>% 
  distinct( edge_id, lookup_name_number_indicator, osm_name_highway_combo_id)




setwd(here("data-processed"))
save(lookup_osm_name_highway_combo_id, file = "lookup_osm_name_highway_combo_id.RData")
save(lookup_osm_name_highway_9cat_combo_id, file = "lookup_osm_name_highway_9cat_combo_id.RData")
save(lookup_osm_name_infra_combo_id, file = "lookup_osm_name_infra_combo_id.RData")
save(lookup_osm_name_highway_combo_id_edge_id, file = "lookup_osm_name_highway_combo_id_edge_id.RData")
load("lookup_osm_name_highway_combo_id_edge_id.RData")

# Final bmap wrangle before longitudinal----------
#9/30 - note here I am saving it to both locations. I'd like the "analysis data" location to be used
#moving forward where I remember to change it, but by saving it to both places, it will make sure
#that the Strava 2018 folder also has an updated copy in case the working directory is not changed in a code.

#update 11/15/2020
#hmm, okay, so what I think I'll do to keep all of these missings is consider the individual
#edge IDs to be part of the osm_name_highway_combo BUT just representing themselves.
#that way, they get picked up and can be sampled.
#Or, you could just group them randomly to be about the same size as the real groups.
#that way, it wouldn't inflate the sample size in a weird way.

bmap_edge_join_wrangle1 = bmap_edge_join_wrangle0 %>% 
  left_join(
    lookup_osm_name_highway_combo_id_edge_id, by = "edge_id"
  ) %>% 
  mutate(
    osm_name_highway_combo_id_miss = case_when(
      is.na(osm_name_highway_combo_id)==TRUE ~ 1,
      TRUE ~0)  )

bmap_edge_join_group_miss_no = bmap_edge_join_wrangle1 %>% 
  filter(osm_name_highway_combo_id_miss==0) %>% 
  mutate(group_id = osm_name_highway_combo_id)

group_id_max = bmap_edge_join_wrangle1 %>% 
  st_set_geometry(NULL) %>% 
  mutate( dummy=1) %>% 
  group_by(dummy) %>%
  summarise(group_id_max = max(osm_name_highway_combo_id, na.rm=TRUE)) %>% ungroup() %>% 
  dplyr::select(group_id_max) %>% pull()

n_groups = n_distinct(bmap_edge_join_group_miss_no$osm_name_highway_combo_id)
n_edges = n_distinct(bmap_edge_join_group_miss_no$edge_id)
edge_per_group = as.integer(n_edges/n_groups) %>%  as_tibble() %>% pull()

#so how many groups total among those that are missing?
bmap_edge_join_group_miss_yes = bmap_edge_join_wrangle1 %>% 
  filter(osm_name_highway_combo_id_miss==1) %>%
  mutate(
    n_rows_no_osm_group = n() ,
    n_groups_no_osm_group = as.integer(n_rows_no_osm_group/edge_per_group),
    #now simply assign each edge randomly to one of these groups
    group_id_synthetic = as.integer(
      #uniform distribution.
      runif(n=n(), min=1, max=n_groups_no_osm_group)
    ),
    group_id=group_id_synthetic+group_id_max
  )

summary(bmap_edge_join_group_miss_yes$n_groups_no_osm_group)
summary(bmap_edge_join_group_miss_yes$group_id_synthetic)
summary(bmap_edge_join_group_miss_yes$group_id)

n_distinct(bmap_edge_join_group_miss_yes$group_id)
n_distinct(bmap_edge_join_group_miss_yes$edge_id)

## ALMOST FINAL bmap_edge_join_wrangle BEFORE LONGITUDINAL-------
#12/12/21 I changed this so I could define the final final one after I 
#define the longitudinal one.
#I used to define bmap_edge_join_wrangle here but doing that down below isntead.
bmap_edge_join_wrangle_pre_long_geo = bmap_edge_join_group_miss_yes %>% 
  bind_rows(bmap_edge_join_group_miss_no) 


table(bmap_edge_join_wrangle_pre_long_geo$group_id)

save(bmap_edge_join_wrangle_pre_long_geo, file = "bmap_edge_join_wrangle_pre_long_geo.RData")
bmap_edge_join_wrangle_pre_long_nogeo = bmap_edge_join_wrangle_pre_long_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(bmap_edge_join_wrangle_pre_long_nogeo, file = "bmap_edge_join_wrangle_pre_long_nogeo.RData")


## lookups from bmap_edge_join before longitudinal----
### lookup main geometry----------------------
lookup_bmap_edge_geo = bmap_edge_join_wrangle_pre_long_geo %>% 
  dplyr::select(edge_id, geometry)
save(lookup_bmap_edge_geo, file = "lookup_bmap_edge_geo.RData")
nrow(lookup_bmap_edge_geo)

#### lookup geometry restricted to 5.5.-half-mile buffer for certain aim-2-3-related subsets----------
load("mp_sf_5halfmi.RData")
lookup_bmap_edge_aim3_geo = lookup_bmap_edge_geo %>% 
  st_intersection(mp_sf_5halfmi) %>% 
  dplyr::select(edge_id, geometry)

save(lookup_bmap_edge_aim3_geo, file = "lookup_bmap_edge_aim3_geo.RData")  
lookup_bmap_edge_aim3_geo %>% mapview()

lookup_edge_XY = bmap_edge_join_wrangle_pre_long_geo %>% 
  dplyr::select(edge_id, x_coord_1, y_coord_1, x_coord_2, y_coord_2  ) %>% 
  st_set_geometry(NULL)
save(lookup_edge_XY, file = "lookup_edge_XY.RData")

### look up length variables--------------
lookup_bmap_edge_length = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(edge_id,  starts_with("length")) %>% 
  mutate(lookup_bmap_edge_length = 1)
save(lookup_bmap_edge_length, file = "lookup_bmap_edge_length.RData")

lookup_bmap_edge_length_geo = bmap_edge_join_wrangle_pre_long_geo %>% 
  dplyr::select(edge_id,  starts_with("length")) %>% 
  mutate(lookup_bmap_edge_length = 1)
save(lookup_bmap_edge_length_geo, file = "lookup_bmap_edge_length_geo.RData")


lookup_bmap_edge_length_except_length_m = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(edge_id, 
                starts_with("length")) %>% 
  dplyr::select(-length_m) #I forget why i need this.
save(lookup_bmap_edge_length_except_length_m, file = "lookup_bmap_edge_length_except_length_m.RData")

lookup_group_id  = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  distinct(edge_id, group_id)
save(lookup_group_id, file = "lookup_group_id.RData")

table(bmap_edge_join_wrangle_pre_long_geo$highway_9cat_num)
table(bmap_edge_join_wrangle_pre_long_geo$highway_6cat_ordered_with_trunk)

### lookup exclude for length------------------
lookup_infra_exclude_for_length_nogeo = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(edge_id, infra_exclude_for_length)
save(lookup_infra_exclude_for_length_nogeo, file = "lookup_infra_exclude_for_length_nogeo.RData")

lookup_infra_exclude_for_length_nogeo = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(edge_id, infra_exclude_for_length)
save(lookup_infra_exclude_for_length_nogeo, file = "lookup_infra_exclude_for_length_nogeo.RData")

### look-up distinct values for highway-----------
lookup_highway  =  bmap_edge_join_wrangle_pre_long_nogeo %>% 
  distinct(highway, highway_9cat, highway_9cat_num, highway_6cat, 
           highway_6cat_ordered_with_trunk,
           major_or_res, major_or_res_highway_all, 
           major_or_res_highway_6cat, major_or_res_other)

save(lookup_highway, file = "lookup_highway.RData")

### look-up edge-highway values----------
#I'm using this for aim 1 to calculate roadway density in each hexagon
load("bmap_edge_join_wrangle_pre_long_geo.RData")
lookup_bmap_edge_highway_geo = bmap_edge_join_wrangle_pre_long_geo %>% 
  dplyr::select(
    edge_id,
    starts_with("highway"), starts_with("hwy"), starts_with("major_or_") )
dim(lookup_bmap_edge_highway_geo)
#note I'm just using one of these, but pull this and then filter. will just be less data to work with.
save(lookup_bmap_edge_highway_geo, file = "lookup_bmap_edge_highway_geo.RData")

lookup_bmap_edge_highway_nogeo =lookup_bmap_edge_highway_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(lookup_bmap_edge_highway_nogeo, file = "lookup_bmap_edge_highway_nogeo.RData")

### look up OSM name--------------------------------------------
lookup_edge_id_osm_id_name = bmap_edge_join_wrangle_pre_long_nogeo %>%
  dplyr::select(edge_id,  osm_id_osm, osm_name_osm ) %>% 
  as_tibble()
save(lookup_edge_id_osm_id_name, file = "lookup_edge_id_osm_id_name.RData")



#using this 12/12/21 below as a convenience lookup
lookup_edge_osm_name =bmap_edge_join_wrangle_pre_long_nogeo %>% 
  distinct(edge_id, osm_name_osm, osm_name_strava) %>% 
  as_tibble()
save(lookup_edge_osm_name, file = "lookup_edge_osm_name.RData")

### look up dissertation aim 1 variables----------
#Note that these NEED TO BE UPDATED at the end of the code, but they also need to be defined here.
#so call them prelim here .
table(bmap_edge_join_wrangle_pre_long_nogeo$diss_a1_eval_prelim)
lookup_edge_diss_a1_eval_prelim = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(edge_id, diss_a1_eval_prelim)

save(lookup_edge_diss_a1_eval_prelim, file = "lookup_edge_diss_a1_eval_prelim.RData")

#make one for any as well so I can exclude thoes that I've already considered
lookup_edge_diss_a1_any_prelim = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(edge_id, diss_a1_any_prelim)
save(lookup_edge_diss_a1_any_prelim, file = "lookup_edge_diss_a1_any_prelim.RData")

### look up the project names--------------
#this will be quite a few. useful. 
lookup_edge_project = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(edge_id, starts_with("project_"))

save(lookup_edge_project, file = "lookup_edge_project.RData")


# 12/5/21 the below code does not need to be repeated....------------

#load(file = "mp_sf_5halfmi.RData")
# load(file = "bmap_edge_join_wrangle_pre_long_geo.RData")
# #check on 5th st
# # bmap_edge_join_wrangle_pre_long_geo %>% filter(osm_name_osm == "5th Street Northwest") %>% mapview()
# # bmap_edge_join_wrangle_pre_long_geo %>% dplyr::select(starts_with("infra")) %>% names()
# table(bmap_edge_join_wrangle_pre_long_geo$infra_6cat_none_abbrev)
# bmap_edge_join_wrangle_pre_long_geo %>% 
#   filter(infra_exclude_for_length == 0) %>% #0 for keep the non-exclusions 
#   st_intersection(mp_sf_5halfmi) %>% 
#   st_set_geometry(NULL) %>% 
#   group_by(infra_6cat_none_abbrev) %>% 
#   summarise(length_km = sum(length_km, na.rm=TRUE))


# a unary union of the basemap---

#12/5/21 note you really don't need to repeat this unless you 
#change the extent of the map itself (which you won't)
#so comment it out.
# library(tidyverse)
# library(sf)
# library(concaveman)
# load(file = "lookup_bmap_edge_geo.RData")
# bmap_unary_union = lookup_bmap_edge_geo %>%
#   concaveman() %>%  #very fast
#   st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
#                +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
#   st_zm() %>%
#   st_buffer(5) %>%
#   st_transform(4326) %>% 
#   st_convex_hull()
#   st_sf()
# 
# plot(bmap_unary_union)
# library(mapview)
# mapview(bmap_unary_union) +mapview(lookup_bmap_edge_geo) #looks good. go with this one.
# mapview(bmap_unary_union)
# 

# save(bmap_unary_union, file = "bmap_unary_union.RData")

#----LONGITUDINAL BASEMAP (first time) --------
setwd(here("data-processed"))
#12/21/21 changing the file this is based off of because I want to add some indicators
#of change to the bmap_edge_join_wrangle_nogeo data
lookup_bmap_edge_mo = bmap_edge_join_wrangle_pre_long_nogeo %>% 
  mutate(n_mo=24) %>% 
  dplyr::select(edge_id, n_mo) %>% 
  uncount(n_mo) %>% #automatically removes this
  group_by(edge_id) %>% 
  mutate(study_month = row_number()) %>% 
  ungroup() #here's the culprit. no group here. sheesh.

save(lookup_bmap_edge_mo, file = "lookup_bmap_edge_mo.RData")
names(lookup_bmap_edge_mo)


## Longitudinal  infrastructure (exposure)--------
#hmmm, does it make sense to define the longitudinal basemap earlier? That way I can include 
#If the study month (date) is less than the ribbon date, then set it to something else.
#Here again 6/2/21. What if ribbon date is missing? Can we assume it was there before study began...
library(tidyverse)
library(sf)
# load("lookup_bmap_edge_mo.RData")
# load("bmap_edge_join_wrangle_pre_long_nogeo.RData")
#added the suffix as the final one will be created below pre_f_ for pre_final
#It used to be called bmap_edge_mo_nogeo 
#note this step takes about 20 min to run

#-------separate these out for speed before longitudinal processing----------##########
names(bmap_edge_join_wrangle_pre_long_nogeo)
lookup_bmap_edge_keep_for_long= bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(
    edge_id, 
    osm_name_osm, 
    starts_with("infra"), 
    starts_with("length"), 
    starts_with("diss_a1"),
    starts_with("ribbon_study_mo"),
    project_westview_protected_temporary,
    study_month_max_aim1
    )

lookup_bmap_edge_exclude_for_long =bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(
    -osm_name_osm,
    -starts_with("infra"), 
    -starts_with("length"), 
    -starts_with("diss_a1"),
    -starts_with("ribbon_study_mo"), 
    -project_westview_protected_temporary,
    -study_month_max_aim1
    ) 

ncol(lookup_bmap_edge_keep_for_long)
ncol(lookup_bmap_edge_exclude_for_long)
ncol(bmap_edge_join_wrangle_pre_long_nogeo)  

bmap_edge_mo_pre_f_nogeo =  lookup_bmap_edge_mo %>%
  left_join(lookup_bmap_edge_keep_for_long, by = "edge_id") %>% 
  mutate( 
    infra_6cat_long = case_when(
      ### deal with westview drive---- 
      #For this unusual one, we have to set it to a bike lane AFTER the ribbon date, 
      #but before this date, it should be protected
      
      project_westview_protected_temporary  == 1 &
        study_month < ribbon_study_month ~ "bike_lane_protected",
      project_westview_protected_temporary  == 1 &
        study_month >= ribbon_study_month ~ "bike_lane_conventional",
      
      ### First, those that do not vary over time. Set them to what they are.--------------------
      #I coded a few as having a negative ribbon_study_month, intended to simply mean that it opened
      #before the study
      
      ribbon_study_month < 3 ~ infra_6cat_none, #the earliest is 3, so this will set it to what it is.
      #if infrastructure is present, but there is NO opening date,
      #then I must assume that the opening date preceded the study period, so
      #Update 6/2/21 I thought this was giving me an issue, but this would have captured it here.
      is.na(ribbon_study_month)==TRUE ~ infra_6cat_none, #it just is what it if no noted date. otherwise.
      
      #--update 10/26 be more specific here to avoid miscoding the eastside trail----###
      #if it's the beltline, allow for it to be unpaved before the ribbon date.
      #for other things, it will just be none.
      
      grepl("Atlanta BeltLine West", osm_name_osm) &  
        study_month < ribbon_study_month ~ "off_street_trail_dirt",
      grepl("Atlanta BeltLine South", osm_name_osm) &  
        study_month < ribbon_study_month ~ "off_street_trail_dirt",
      
      ### Second, if it's not these, then set it as none---------
      study_month < ribbon_study_month ~ "none", 
      
      ### Third, set it to the infrastructure category if the 
      #tudy month is AFTER the ribbon date
      study_month >=  ribbon_study_month ~ infra_6cat_none,
      
      #if the ribbon study month is after 24, that means it's after our study ended, so that should also be ended
      ribbon_study_month > 24 ~"none", #critical! updated 10/26/2020. this should be redundant.
      
      TRUE ~ "none"  #for all other conditions, it's none.
    ),
    
    #update 10/11/2020 - some seem to have still been coded as missing. force them to be none
    #note, many of these are sidewalks. Keep an indicator around to keep track of them.
    infra_6cat_long_coded_as_miss = case_when(
      is.na(infra_6cat_long)==TRUE ~ 1,
      TRUE ~ 0
    ),
    
    #and recode those NAs as none
    infra_6cat_long = case_when(
      infra_6cat_long_coded_as_miss == 1 ~ "none",
      TRUE ~infra_6cat_long
    ),
    
    #    table(edge_mo_all_with_et_aim3_gears_nogeo$infra_6cat_long)
    #a version that's easier for the legend
    infra_6cat_long_legend_nodirt = case_when(
      infra_6cat_long == "off_street_trail_paved" ~ "1-Trail-Paved",
      infra_6cat_long == "bike_lane_protected" ~ "2-Lane-Protected",
      infra_6cat_long == "bike_lane_buffered" ~ "3-Lane-Buffered",
      infra_6cat_long == "bike_lane_conventional" ~ "4-Lane-Conventional",
      infra_6cat_long == "sharrow" ~ "5-Sharrow",
      TRUE ~ "none" ),
    
    #this could perhaps be more precise, e.g., some infra going from sharrow to bike lane
    #or from bike lane to buffered bike lane, but that will be another day  - MDG 9/29/2020
    
    #dummy variables for easier multiplication in my aim 1 hex roll-up
    #use consistent notation as you have where you've abbreviated them elsewhere
    infra_6_dummy_1_trail_p = case_when(
      infra_6cat_long =="off_street_trail_paved" ~ 1,
      TRUE ~ 0),
    infra_6_dummy_2_lane_p = case_when(
      infra_6cat_long =="bike_lane_protected" ~ 1,
      TRUE ~ 0),
    infra_6_dummy_3_lane_b = case_when(
      infra_6cat_long =="bike_lane_buffered" ~ 1,
      TRUE ~ 0),
    infra_6_dummy_4_lane_c = case_when(
      infra_6cat_long =="bike_lane_conventional" ~ 1,
      TRUE ~ 0),
    infra_6_dummy_5_sharrow = case_when(   
      infra_6cat_long =="sharrow" ~ 1,
      TRUE ~ 0) ,
    #I added the dirt category July 25, 2022 in case needed for aim 3
    infra_6_dummy_6_trail_d = case_when(   
      infra_6cat_long =="off_street_trail_dirt" ~ 1,
      TRUE ~ 0) ,
    
    #can omit the none category...not necessary
    #do I use the dirt trail category?
    
    ###---length of infrastructure in each category-------------
    #the reason for this is so you can take a min and a max on a given
    #edge and assess the change. and then those areas that changed by a
    #certain amount but were not incldued as the main exposure can be
    #systematically included 12/12/21 2:30 pm
    infra_length_mi_1_trail_p =infra_6_dummy_1_trail_p*length_mi, #paved trail
    infra_length_mi_2_lane_p =infra_6_dummy_2_lane_p*length_mi, #protected
    infra_length_mi_3_lane_b =infra_6_dummy_3_lane_b*length_mi, #buffered
    infra_length_mi_4_lane_c =infra_6_dummy_4_lane_c*length_mi, #conventional
    infra_length_mi_5_sharrow =infra_6_dummy_5_sharrow*length_mi, #sharrow

    ###----whether the dissertation infra was rideable (open)------
    #I like the _expo_ syntax to match my hex earlier.
    #Update 12/13/21 2pm I'm calling these expo_line_ to make an easy comparison
    #with expo_buff in the other code. go
    expo_line_wst = case_when(
      diss_a1_wst== 1 &
      study_month >= ribbon_study_month ~ 1,
      TRUE ~ 0 ),
    expo_line_pro = case_when(
      diss_a1_pro == 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0 ),
    expo_line_lsl = case_when(
      diss_a1_lsl== 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0  ),
    expo_line_gtp = case_when(
      diss_a1_gtp== 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0 ),
    expo_line_est1 = case_when(
      diss_a1_est1 == 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0  ),
    expo_line_est2 = case_when(
      diss_a1_est2 == 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0  ),
    expo_line_est3 = case_when(
      diss_a1_est3 == 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0 ),
    expo_line_est = case_when( #combo est
      expo_line_est1 == 1 ~ 1,
      expo_line_est2 == 1 ~ 1,
      expo_line_est3 == 1 ~ 1,
      TRUE ~0
    ),
    expo_line_mcd = case_when(
      diss_a1_mcd == 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0  ),
    expo_line_pct1 = case_when(
      diss_a1_pct1 == 1 &
        study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0  ),
    expo_line_pct2 = case_when(
      diss_a1_pct2 == 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0  ),
    expo_line_pct = case_when( #combo pct
      expo_line_pct1 == 1 ~ 1,
      expo_line_pct2 == 1 ~ 1,
      TRUE ~ 0   ),
    expo_line_p4h = case_when(
      diss_a1_p4h == 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0  ),
            #note that there's another section of RDA we're now considering
    expo_line_rda_cas = case_when(
      diss_a1_rda_cas == 1 &
      study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0  ),
    #the combos (joint exposure)
    expo_line_wst_pro = case_when(
      expo_line_wst == 1 ~ 1,
      expo_line_pro == 1 ~ 1,
      TRUE ~0
    ),
    
    expo_line_lsl_gtp = case_when(
      expo_line_lsl == 1 ~ 1,
      expo_line_gtp == 1 ~ 1,
      TRUE ~0
    ),
    
    #lastly, any infra open? I'm specifying the "eval" to exclude p4h and rda
    #this should be updated below as well
    #in the other code, it's expo_eval_buff 
    expo_line_eval = case_when(
      expo_line_wst==1~1,
      expo_line_pro==1~1,
      expo_line_gtp==1~1,
      expo_line_lsl==1~1,
      expo_line_est1==1~1,
      expo_line_est2==1~1,
      expo_line_est3==1~1,
      expo_line_pct1==1~1,
      expo_line_pct2==1~1,
      expo_line_mcd==1~1,
      TRUE ~ 0 
    ),
    
    #update 12/30/21 I'm adding a variable
  ) %>% 
  #lastly, bring in the time-invariant variables again. should be faster this way
  left_join(lookup_bmap_edge_exclude_for_long, by = "edge_id")

save(bmap_edge_mo_pre_f_nogeo, file = "bmap_edge_mo_pre_f_nogeo.RData")

## look-ups from longitudinal basemap--------------
#names(edge_mo_wzeros_nogeo)


#12/7/21 note this is used elsewhere so I'm not adding the nogeo suffix
#Actually I am going to add the _nogeo suffix. If it breaks, fix it later.
lookup_edge_mo_infra_nogeo = bmap_edge_mo_pre_f_nogeo %>% 
  dplyr::select(edge_id, infra_6cat_long, infra_6cat_long_legend_nodirt, study_month) %>% 
  as_tibble()

save(lookup_edge_mo_infra_nogeo, file = "lookup_edge_mo_infra_nogeo.RData")

# a version with geometry and ribbon_after_study needed for code in aim1_1_hex_covariates
#changed because it was so similar to above.
lookup_edge_mo_infra_6_dummy_geo = bmap_edge_mo_pre_f_nogeo %>% 
  dplyr::select(edge_id,study_month, starts_with("infra_6_dum")) %>% 
  left_join(lookup_bmap_edge_geo, by = "edge_id") %>% 
  st_sf()

save(lookup_edge_mo_infra_6_dummy_geo, file = "lookup_edge_mo_infra_6_dummy_geo.RData")

#a version without geometry since you just need to run the geometry through once.
#this is extraordinarily confusing since it's so similar to above...whatever.
lookup_edge_mo_infra_6_dummy_nogeo = lookup_edge_mo_infra_6_dummy_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(lookup_edge_mo_infra_6_dummy_nogeo, file = "lookup_edge_mo_infra_6_dummy_nogeo.RData")

### lookup infra edge - last study month----------
#A static look-up table to check what edges ever have infra. Use 23
lookup_edge_infra_study_mo_23 = lookup_edge_mo_infra_nogeo %>% 
  filter(study_month==23) %>% 
  distinct(edge_id, infra_6cat_long) %>% 
  as_tibble()

setwd(here("data-processed"))
lookup_edge_infra_study_mo_23
save(lookup_edge_infra_study_mo_23, file = "lookup_edge_infra_study_mo_23.RData")


### lookup with infra and ribbon date and exclude for length-----------
lookup_edge_mo_infra_ribbon_exclude_geo = bmap_edge_mo_pre_f_nogeo %>% 
  dplyr::select(edge_id, study_month,
                starts_with("infra"), starts_with("ribbon"), contains("exclude_for")) %>% 
  left_join(lookup_bmap_edge_geo, by = "edge_id") %>% 
  st_sf()

save(lookup_edge_mo_infra_ribbon_exclude_geo, file = "lookup_edge_mo_infra_ribbon_exclude_geo.RData")
### lookup: infra_6cat with infra_most_protection_ordered----------
lookup_infra_abbrev_ordered = bmap_edge_mo_pre_f_nogeo %>% 
  mutate(
    infra_most_protection_ordered =
      case_when(
        infra_6cat_long == "off_street_trail_paved" ~ "1-trail_p",
        infra_6cat_long == "bike_lane_protected" ~ "2-lane_p",
        infra_6cat_long == "bike_lane_buffered" ~ "3-lane_b",
        infra_6cat_long == "bike_lane_conventional" ~ "4-lane_c",
        infra_6cat_long == "sharrow" ~ "5-sharrow",
        infra_6cat_long == "none" ~ "6-none" 
      ),
    
    infra_ordered =
      case_when(
        infra_6cat_long == "off_street_trail_paved" ~ "1-trail_p",
        infra_6cat_long == "bike_lane_protected" ~ "2-lane_p",
        infra_6cat_long == "bike_lane_buffered" ~ "3-lane_b",
        infra_6cat_long == "bike_lane_conventional" ~ "4-lane_c",
        infra_6cat_long == "sharrow" ~ "5-sharrow",
        infra_6cat_long == "none" ~ "6-none" 
      )
  ) %>% 
  
  dplyr::select(infra_most_protection_ordered, infra_6cat_long, infra_ordered) %>% 
  distinct(infra_most_protection_ordered, infra_6cat_long, infra_ordered) %>% 
  arrange(infra_most_protection_ordered) %>% 
  as_tibble()
setwd(here("data-processed"))
save(lookup_infra_abbrev_ordered, file = "lookup_infra_abbrev_ordered.RData")

lookup_infra_ordered = lookup_infra_abbrev_ordered %>% 
  distinct(infra_6cat_long, infra_ordered) %>% 
  as_tibble() %>% 
  arrange(infra_ordered)

save(lookup_infra_ordered, file = "lookup_infra_ordered.RData")
### lookup for table 1 of infra ------
lookup_infra_table_order  = bmap_edge_mo_pre_f_nogeo %>% 
  group_by(infra_6cat_long) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(
    infra_table_no = case_when(
      #      infra_6cat_long=="off_street_trail_dirt" ~ 1,
      infra_6cat_long=="off_street_trail_paved" ~ 1,
      infra_6cat_long=="bike_lane_protected" ~ 2,
      infra_6cat_long=="bike_lane_buffered" ~ 3,
      infra_6cat_long=="bike_lane_conventional" ~ 4,
      infra_6cat_long=="sharrow" ~ 5,
      infra_6cat_long=="none" ~ 6
    )
  ) %>% 
  dplyr::select(-n)


save(lookup_infra_table_order, file = "lookup_infra_table_order.RData")


## CHANGE IN INFRASTRUCTURE LENGTH over the study period (aim 1)--------
#again, we can use this to see where infra changed the most and
#include as a possible confounder in aim 1

load("bmap_edge_mo_pre_f_nogeo.RData")
load("lookup_edge_diss_a1_eval_prelim.RData")
load("lookup_edge_diss_a1_any_prelim.RData")
load("lookup_edge_osm_name.RData")
load("lookup_edge_project.RData")
edge_infra_change_a1_nogeo = bmap_edge_mo_pre_f_nogeo %>% 
  #restrict to study month before 24. basically excludes infra that I included in my basemap
  #but that was installed later.
  #to make this measurement accurate, you should filter to infra_exclude_for_length=0
  filter(study_month <= study_month_max_aim1) %>% 
  group_by(edge_id) %>% 
  summarise(
    infra_length_mi_1_trail_p_min = min(infra_length_mi_1_trail_p, na.rm=TRUE),
    infra_length_mi_2_lane_p_min = min(infra_length_mi_2_lane_p, na.rm = TRUE),
    infra_length_mi_3_lane_b_min = min(infra_length_mi_3_lane_b, na.rm = TRUE),
    infra_length_mi_4_lane_c_min = min(infra_length_mi_4_lane_c, na.rm = TRUE),
    infra_length_mi_5_sharrow_min = min(infra_length_mi_5_sharrow, na.rm = TRUE),
    
    infra_length_mi_1_trail_p_max = max(infra_length_mi_1_trail_p, na.rm=TRUE),
    infra_length_mi_2_lane_p_max = max(infra_length_mi_2_lane_p, na.rm = TRUE),
    infra_length_mi_3_lane_b_max = max(infra_length_mi_3_lane_b, na.rm = TRUE),
    infra_length_mi_4_lane_c_max = max(infra_length_mi_4_lane_c, na.rm = TRUE),
    infra_length_mi_5_sharrow_max = max(infra_length_mi_5_sharrow, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    #calculate the difference over time (max-min)
    infra_length_mi_1_trail_p_diff = infra_length_mi_1_trail_p_max - infra_length_mi_1_trail_p_min,
    infra_length_mi_2_lane_p_diff = infra_length_mi_2_lane_p_max - infra_length_mi_2_lane_p_min,
    infra_length_mi_3_lane_b_diff = infra_length_mi_3_lane_b_max - infra_length_mi_3_lane_b_min,
    infra_length_mi_4_lane_c_diff = infra_length_mi_4_lane_c_max - infra_length_mi_4_lane_c_min,
    infra_length_mi_5_sharrow_diff = infra_length_mi_5_sharrow_max - infra_length_mi_5_sharrow_min,
    #make an indicator for any difference so can restrict to this
    infra_length_mi_any_diff = case_when(
      infra_length_mi_1_trail_p_diff>0~infra_length_mi_1_trail_p_diff,
      infra_length_mi_2_lane_p_diff>0~infra_length_mi_2_lane_p_diff,
      infra_length_mi_3_lane_b_diff>0~infra_length_mi_3_lane_b_diff,
      infra_length_mi_4_lane_c_diff>0~infra_length_mi_4_lane_c_diff,
      infra_length_mi_5_sharrow_diff>0~infra_length_mi_5_sharrow_diff,
      TRUE ~ 0),
    infra_length_mi_any_diff_ind = case_when(
      infra_length_mi_any_diff >0 ~ 1,
      TRUE ~ 0),
    #an indicator that excludes sharrows
    infra_length_mi_any_no_sharrow_diff = case_when(
      infra_length_mi_1_trail_p_diff>0~infra_length_mi_1_trail_p_diff,
      infra_length_mi_2_lane_p_diff>0~infra_length_mi_2_lane_p_diff,
      infra_length_mi_3_lane_b_diff>0~infra_length_mi_3_lane_b_diff,
      infra_length_mi_4_lane_c_diff>0~infra_length_mi_4_lane_c_diff,
      TRUE ~ 0),
    infra_length_mi_any_no_sharrow_diff_ind = case_when(
      infra_length_mi_any_no_sharrow_diff >0 ~ 1,
      TRUE ~ 0)
    ) %>% 
  #link in the diss_a1_eval indicator here.
  #if it's not that, then call it a possible confounder (say, covariate)
  #and then drop it.
  filter(infra_length_mi_any_diff_ind==1) %>% #now filter out the rest
#  left_join(lookup_edge_diss_a1_eval_prelim, by = "edge_id") %>% #for reference in mapviews only. don't need
  left_join(lookup_edge_diss_a1_any_prelim, by = "edge_id") %>%  #linked here based on 1st definition
  #link in the osm names so I can classify them with some more detail
  left_join(lookup_edge_osm_name, by = "edge_id") %>% 
#  left_join(lookup_edge_project, by = "edge_id") %>% #add the project indicators. actually just use osm_name_osm
  ### add new diss aim 1 infrastructure (covariates)---------------------------
  mutate(
    #this is infrastructure that to date we're not considering a main exposure
    #but we could in some cases
    diss_a1_late_add = case_when(
      #12/16/21 of course this code coudl be more streamlined by adding these above,
      #but this reflects the order in which these were decided to be included.
      #the nice thing about this, though, is it automatically brings non-sharrows that changed in
      #12/17/21 allow sharrows (uni) to be included but then don't include in final analysis
      diss_a1_any_prelim==0 & infra_length_mi_any_diff_ind==1 ~ 1,
      TRUE ~ 0 ),
    #label each of these so I can create little buffers using the same hex-wrangle syntax
        # - a conventional bike lane on Milton (2017-12) - included as a possible confounder; 
              #not a main analysis because
        # - a paved trail on Ivan Allen west of Luckie St (2018-02) - 
                #very close to the Luckie St Lane an Tech Parkway's buffer area;
                #considered a main analysis because it's a paved trail, 
                #but it's very difficult to tell its effect apart from that of the 
                #Tech Parkway and Luckie St Lane
        # - a conventional bike lane on Sylvan (2018-02) - included as possible confounder now;
                #note it's actually buffered between murphy and warner (north side of the lane)
        # - a section of buffered bike lane on RDA (2018-03) as it passes under the connector near the 
                #baseball stadium  - included as possible confounder now
        # - a conventional bike lane on Lawton (2018-04) - 
                #would  have included as apossible confounder now; 
                #no difference in terms of hexagons with Westside Trail, though
        # - a buffered bike lane on Ormewood (2018-07),  
              #- included as possible confounder now; 
              #I thought about including as a main treatment area but decided it opened too 
              #late to be meaningfully studied (our study ended 2018-09)
        # - conventional bike lane on college (august 2018)
    #ridiculously long name. oh well.
    diss_a1_any_name_section_short_late_add = case_when(
      diss_a1_late_add==1 &       grepl("Milton", osm_name_osm) ~ "mil",
      diss_a1_late_add==1 &       grepl("Lawton", osm_name_osm) ~ "law",
      diss_a1_late_add==1 &       grepl("Sylvan", osm_name_osm) ~ "syl",
      diss_a1_late_add==1 &       grepl("Ralph David", osm_name_osm) ~ "rda_con", #for RDA at connector...
      diss_a1_late_add==1 &       grepl("Ivan Allen", osm_name_osm) ~ "iva",  #Ivan Allen Gateway 
      #note! 12/16/21 uni won't get picked up because it's classified as a sharrow per previous code.
      #keep it but don't use it.
      diss_a1_late_add==1 &       grepl("United", osm_name_osm) ~ "uni",  #keep but don't use 12/16/21
      diss_a1_late_add==1 &       grepl("Ormewood", osm_name_osm) ~ "orm",  
      diss_a1_late_add==1 &       grepl("Colleg", osm_name_osm) ~ "col"  #adding college 12/16/21
    ),
    diss_a1_any_name_group_short_late_add = case_when(
      #note that orme and united are really the same...so make them a group
      #I had initially grouped ormewood and united but I've actually ecided to leave them separate
      #since united between boulevard and ormewood is just a sharrow so for aim 1 purposes, not worth it
      diss_a1_late_add==1 &       grepl("Ivan Allen", osm_name_osm) ~ "lsl_gtp_iva", #adjacent to luckie st
      #what about sylvan and lawton...very close to one another...
      TRUE ~ diss_a1_any_name_section_short_late_add #else it just takes the names above
    ),
    
    #now for a little explanation for why we didn't include these as the main analysis
    #you should 
    diss_a1_why_not_eval = case_when(
      diss_a1_any_name_section_short_late_add == "mil" ~ "conventional bike lane",
      diss_a1_any_name_section_short_late_add == "syl" ~ "conventional bike lane",
      diss_a1_any_name_section_short_late_add == "law" ~ "conventional bike lane",
      diss_a1_any_name_section_short_late_add == "col" ~ "conventional bike lane, and aug 2018",
      diss_a1_any_name_section_short_late_add == "iva" ~ "a trail and early enough...should assess",
       diss_a1_any_name_section_short_late_add == "uni" ~ "buffered bike lanes and summer 2018..pretty late",
      diss_a1_any_name_section_short_late_add == "orm" ~ "buffered bike lanes and summer 2018..pretty late"
    )
  ) %>% 
  #drop a few variables for easier linking to future datasets
  dplyr::select(
    #drop anything that ends in prelim
    -ends_with("prelim"),
    -starts_with("osm_name"), 
    -starts_with("project_")
    )

save(edge_infra_change_a1_nogeo, file = "edge_infra_change_a1_nogeo.RData")
#make a geometry version to visualize
edge_infra_change_a1_geo = edge_infra_change_a1_nogeo %>% 
  left_join(lookup_bmap_edge_geo, by = "edge_id") %>% 
  st_sf()
save(edge_infra_change_a1_geo, file = "edge_infra_change_a1_geo.RData")
names(edge_infra_change_a1_nogeo)

### visualize infra that changes by whether or not it's evaluated in diss a1------

# edge_infra_change_a1_nogeo %>% left_join(lookup_bmap_edge_geo, by = "edge_id") %>% mapview()
# table(bmap_edge_join_wrangle_pre_long_nogeo$diss_a1_eval)
# table(bmap_edge_join_wrangle_pre_long_nogeo$infra_exclude_for_length)
# names(lookup_infra_exclude_for_length_nogeo)
# names(lookup_edge_diss_a1_eval_prelim)
# table(edge_infra_change_a1_geo$infra_length_mi_any_diff_ind,
#       edge_infra_change_a1_geo$infra_length_mi_any_no_sharrow_diff_ind)
# edge_infra_change_a1_geo %>%
#   left_join(lookup_edge_diss_a1_eval_prelim, "edge_id") %>%
#   left_join(lookup_edge_diss_a1_any, by = "edge_id") %>%
# #  left_join(lookup_edge_ribbon_date, by = "edge_id") %>%
#   left_join(lookup_edge_osm_name, by = "edge_id") %>%
#   left_join(lookup_infra_exclude_for_length_nogeo, "edge_id") %>% #exclude
#   filter(infra_length_mi_any_diff_ind==1) %>% #include sharrows
# #  filter(infra_length_mi_any_no_sharrow_diff_ind==1) %>% #exclude sharrows
#   filter(infra_exclude_for_length==0) %>%
#   mapview(zcol = "diss_a1_eval")
# edge_infra_change_a1_geo %>% 
#   filter(infra_length_mi_1_trail_p_diff>0) %>% 
#   mapview(zcol ="infra_length_mi_1_trail_p_diff")
# edge_infra_change_a1_geo %>% 
#   filter(infra_length_mi_2_lane_p_diff>0) %>% 
#   mapview(zcol ="infra_length_mi_2_lane_p_diff")
# edge_infra_change_a1_geo %>% 
#   filter(infra_length_mi_3_lane_b_diff>0) %>% 
#   mapview(zcol ="infra_length_mi_3_lane_b_diff")
# edge_infra_change_a1_geo %>%
#   filter(infra_length_mi_4_lane_c_diff>0) %>%
#   mapview(zcol ="infra_length_mi_4_lane_c_diff")
# edge_infra_change_a1_geo %>% 
#   filter(infra_length_mi_5_sharrow_diff>0) %>% 
#   mapview(zcol ="infra_length_mi_5_sharrow_diff")


# load("edge_infra_change_a1_nogeo.RData")



# FINAL FINAL BMAP_EDGE_JOIN_WRANGLE-----------------------------------------------
#12/12/21 we're now considering some indicators that summarize time-varying stuff
#from above

#load("edge_infra_change_a1_nogeo.RData")
load("bmap_edge_join_wrangle_pre_long_geo.RData")
#adding the time-varying infra summaries from above
names(edge_infra_change_a1_nogeo)
bmap_edge_join_wrangle_pre_long_nogeo %>% 
  dplyr::select(starts_with("diss_a1_")) %>% 
  names()
bmap_edge_join_wrangle = bmap_edge_join_wrangle_pre_long_geo %>% 
  st_transform(4326) %>% 
  left_join(edge_infra_change_a1_nogeo, by = "edge_id") %>% 
  #redefine the infra add-ons
  mutate(
    diss_a1_any_name_section_short = case_when(
      diss_a1_late_add==1 ~ diss_a1_any_name_section_short_late_add,
      TRUE ~ diss_a1_any_name_section_short
    ),
    #rename the grouped lsl-gtp-ivan allen
    diss_a1_any_name_group_short = case_when(
      diss_a1_any_name_group_short == "lsl_gtp" ~ "lsl_gtp_iva", #if it was lsl-gtp before, set it to this.
      diss_a1_any_name_group_short_late_add == "lsl_gtp_iva" ~ "lsl_gtp_iva",
      diss_a1_any_name_group_short_late_add == "orm" ~ "orm",
      diss_a1_any_name_group_short_late_add == "uni" ~ "uni",
      diss_a1_any_name_group_short_late_add == "col" ~ "col",
      diss_a1_any_name_group_short_late_add == "rda_con" ~ "rda_con", #march 2018
      diss_a1_any_name_group_short_late_add == "syl" ~ "syl",
      diss_a1_any_name_group_short_late_add == "mil" ~ "mil",
      diss_a1_any_name_group_short_late_add == "law" ~ "law",
      TRUE ~ diss_a1_any_name_group_short #otherwise set it to its old value.
        #I think some elses will be overwitten (old 88494)
    ),
    
    #make an ordered version
    #------an ordered short name for the group for easier table printing----------#
    #so I can make a look-up out of this like I used to have in my other 1_1_misc_diss_infr_data
    #code
    #12/16/21 leave the eval here, though, for the ordered version
    diss_a1_eval_name_group_short_ordered = case_when(
      diss_a1_any_name_group_short == "pct" ~ "1-pct",
      diss_a1_any_name_group_short == "est" ~ "2-est",
      diss_a1_any_name_group_short == "wst_pro" ~ "3-wst_pro",
      diss_a1_any_name_group_short == "lsl_gtp_iva" ~ "4-lsl_gtp_iva",
      diss_a1_any_name_group_short == "mcd" ~ "5-mcd",
      TRUE ~ "6-else" #not calling it none because some do have some infra..
    ),
    
    #also fix diss_a1_eval to make sure iva is included.
    diss_a1_eval = case_when(
      diss_a1_any_name_group_short=="lsl_gtp_iva" ~ 1,
      TRUE ~ diss_a1_eval_prelim ),
    #and fix diss_a1_any
    diss_a1_any = case_when(
      diss_a1_late_add==1 ~ 1,
      TRUE ~ diss_a1_any_prelim ), #make room for these new ones; otherwise same as before
    
    #and add indicators for these new ones like you did with the other ones, e.g., diss_a1_p4h 
    diss_a1_lsl_gtp_iva = case_when(
      diss_a1_any_name_group_short== "lsl_gtp_iva" ~ 1,
      TRUE ~ 0 ),
    diss_a1_iva = case_when(
      diss_a1_any_name_section_short== "iva" ~ 1,
      TRUE ~ 0 ),
    diss_a1_orm = case_when(
      diss_a1_any_name_section_short== "orm" ~ 1,
      TRUE ~ 0 ),
    diss_a1_uni = case_when( #12/17/21 leaving this here but probably won't use.
      diss_a1_any_name_section_short== "uni" ~ 1,
      TRUE ~ 0 ),
    diss_a1_law = case_when(
      diss_a1_any_name_section_short== "law" ~ 1,
      TRUE ~ 0 ),
    diss_a1_col = case_when(
      diss_a1_any_name_section_short== "col" ~ 1,
      TRUE ~ 0 ),
    diss_a1_mil = case_when(
      diss_a1_any_name_section_short== "mil" ~ 1,
      TRUE ~ 0 ),
    diss_a1_syl = case_when(
      diss_a1_any_name_section_short== "syl" ~ 1,
      TRUE ~ 0 ),
    diss_a1_rda_con = case_when(
      diss_a1_any_name_section_short== "rda_con" ~ 1,
      TRUE ~ 0 )
  )

save(bmap_edge_join_wrangle, file = "bmap_edge_join_wrangle.RData")
bmap_edge_join_wrangle_nogeo = bmap_edge_join_wrangle %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(bmap_edge_join_wrangle_nogeo, file = "bmap_edge_join_wrangle_nogeo.RData")
st_crs(bmap_edge_join_wrangle)
table(bmap_edge_join_wrangle$diss_a1_lsl_gtp_iva)
table(bmap_edge_join_wrangle$diss_a1_ws)
table(bmap_edge_join_wrangle$diss_a1_any_name_section_short) #good. worked.
table(bmap_edge_join_wrangle$diss_a1_any_name_group_short) #good. worked.
table(bmap_edge_join_wrangle$diss_a1_eval)
# bmap_edge_join_wrangle %>%
#   filter(diss_a1_rda_con==1) %>%
#   filter(infra_exclude_for_length==0) %>% #amazing. great job mike
#   mapview()
# bmap_edge_join_wrangle %>%
#   filter(diss_a1_orm==1) %>%
#   filter(infra_exclude_for_length==0) %>% 
#   mapview()
# bmap_edge_join_wrangle %>%
#   filter(diss_a1_col==1) %>%
#   filter(infra_exclude_for_length==0) %>% 
#   mapview()
# 
# bmap_edge_join_wrangle %>%
#   filter(diss_a1_law==1) %>%
#   filter(infra_exclude_for_length==0) %>% 
#   mapview()
# bmap_edge_join_wrangle %>%
#   filter(diss_a1_syl==1) %>%
#   filter(infra_exclude_for_length==0) %>% 
#   mapview()
# bmap_edge_join_wrangle %>%
#   filter(diss_a1_rda_cas==1) %>%
#   filter(infra_exclude_for_length==0) %>% 
#   mapview(zcol = "infra_6cat")




## lookups----------------------------------------------------
#-----------------------new indicators-------------------------#
lookup_diss_a1_new_indicators = bmap_edge_join_wrangle_nogeo %>% 
  dplyr::select(edge_id, 
                diss_a1_lsl_gtp_iva ,diss_a1_iva,  diss_a1_orm,  diss_a1_uni, diss_a1_col,
                diss_a1_law, diss_a1_mil, diss_a1_syl, diss_a1_rda_con)
save(lookup_diss_a1_new_indicators, file = "lookup_diss_a1_new_indicators.RData")

#---------other ones that need to be updated-------------------#
#diss_a1_any_name_section_short
#diss_a1_any_name_group_short
#diss_a1_eval
#diss_a1_any
#not calling this one prelim
lookup_edge_diss_a1_eval = bmap_edge_join_wrangle_nogeo %>% 
  dplyr::select(edge_id, diss_a1_eval)
save(lookup_edge_diss_a1_eval, file = "lookup_edge_diss_a1_eval.RData")
#make one for any as well so I can exclude those that I've already considered
lookup_edge_diss_a1_any = bmap_edge_join_wrangle_nogeo %>% 
  dplyr::select(edge_id, diss_a1_any)
save(lookup_edge_diss_a1_any, file = "lookup_edge_diss_a1_any.RData")

#load(file = "bmap_edge_join_wrangle_nogeo.RData")
#this one wasn't created above (good)
lookup_diss_a1_any_name_group_short = bmap_edge_join_wrangle_nogeo %>% 
  distinct(diss_a1_any_name_group_short, diss_a1_eval_name_group_short_ordered)

save(lookup_diss_a1_any_name_group_short, file = "lookup_diss_a1_any_name_group_short.RData")
table(lookup_diss_a1_any_name_group_short$diss_a1_eval_name_group_short_ordered)
## other miscellaneous lookups-------------------------------------------------
### look up ribbon dates------------------------
#both the ride version and the actual version...not sure if different
lookup_edge_ribbon_date = bmap_edge_join_wrangle_nogeo %>% 
  dplyr::select(edge_id, ribbon_date) 
names(lookup_edge_ribbon_date)
save(lookup_edge_ribbon_date, file = "lookup_edge_ribbon_date.RData")

### lookup infra6cat vs infra6cat abbreviated------------------------

lookup_infra_6cat_none_abbrev = bmap_edge_join_wrangle_nogeo %>% 
  distinct(infra_6cat_none, infra_6cat_none_abbrev)

save(lookup_infra_6cat_none_abbrev, file = "lookup_infra_6cat_none_abbrev.RData")


### lookup important stuff but length------------------------
#load(file = "bmap_edge_join_wrangle_pre_long_geo.RData")
#note this is a little confusing because it actually includes some variables from the
#initial strava basemap, so let's call this something else
#lookup_edge_id_import_vars_but_length
#used to be called lookup_edge_id_import_vars_but_length
lookup_edge_id_import_vars_but_length = bmap_edge_join_wrangle_nogeo %>%
  dplyr::select(
    starts_with("edge_id"), #including edge_id_def_unique
    starts_with("osm_"), #this gets the IDs, the names, the indicator, 
    starts_with("group_id"), #a recent addition (november)
    starts_with("join"), #join_coa_arc, join_spatial, join_aspatial
    starts_with("infra"),
    starts_with("project"),
    beltline,
    starts_with("ribbon"), #important to save this down here because ribbon dates will be better.
    starts_with("highway"), #this grabs highway_1
    starts_with("hwy"), #7/25/22 - should pick up none.
    starts_with("major_or_res")  #in case there are any descendents
    #    maxspeed ,
    
    # X1, Y1, X2, Y2,  #I use these for something
    # x_abs_diff,
    # y_abs_diff,
    # north_south
    
  ) 

save(lookup_edge_id_import_vars_but_length, file = "lookup_edge_id_import_vars_but_length.RData")

# FINAL FINAL LONGITUDINAL BASEMAP ---------------------------------------
library(tidyverse)
library(sf)
library(here)
library(mapview)
setwd(here("data-processed"))
# load(file = "bmap_edge_mo_pre_f_nogeo.RData")
# load(file = "lookup_diss_a1_new_indicators.RData")

bmap_edge_mo_nogeo = bmap_edge_mo_pre_f_nogeo %>% 
  left_join(lookup_diss_a1_new_indicators, by = "edge_id") %>% 
  ungroup() %>% 
  mutate(
    #exposure indicators over time for theses new possible confounders
    #for the ribbon dates, just use their ribbon date (i.e., don't worry about the approximation)
    expo_line_iva = case_when(
      diss_a1_iva ==1 &
        study_month >=ribbon_study_month ~ 1,
      TRUE ~0
    ),
    #redefine this one with the broader definition including ivan allen
    expo_line_lsl_gtp_iva = case_when(
      expo_line_iva ==1 ~ 1, 
      expo_line_lsl_gtp ==1 ~ 1,
      TRUE ~0
    ),
    #these opened all at the same time so this works fine 
    #(i.e., don't worry about _orm and _uni separately)
    #update 12/16/21 actually do worry about them separately
    expo_line_orm = case_when(
      diss_a1_orm==1 &
        study_month >=ribbon_study_month ~ 1,
        TRUE ~ 0 ),
    expo_line_uni = case_when(
      diss_a1_uni==1 &
        study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0 ),
    expo_line_orm = case_when(
      diss_a1_orm==1 &
        study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0 ),
    expo_line_orm_uni = case_when(
      expo_line_orm==1 ~ 1,
      expo_line_uni==1 ~ 1,
      TRUE ~ 0 ),
    expo_line_law = case_when(
      diss_a1_law==1 &
        study_month >=ribbon_study_month ~ 1,
        TRUE ~ 0 ),
    expo_line_syl = case_when(
      diss_a1_syl==1 &
        study_month >=ribbon_study_month ~ 1,
        TRUE ~ 0 ),
    expo_line_mil = case_when(
      diss_a1_mil==1 &
        study_month >=ribbon_study_month ~ 1,
        TRUE ~ 0 ),
    expo_line_col = case_when(
      diss_a1_col==1 &
        study_month >=ribbon_study_month ~ 1,
      TRUE ~ 0 ),
    expo_line_rda_con = case_when(
      diss_a1_rda_con==1 &
        study_month >=ribbon_study_month ~ 1,
        TRUE ~ 0 ),
    
    #could add a few more bundles to be consistent with other like combined wst areas
    #and the possible confounders
    expo_line_wst_rda_syl_pro = case_when(
      expo_line_wst==1 ~ 1,
      expo_line_rda_cas==1 ~ 1,
      expo_line_syl==1 ~ 1,
      expo_line_pro==1 ~ 1,
      TRUE ~ 0
      ),
    
    #okay, now redefine the expo_line_eval
    #note here we're just worried about those that we're assessing as a main exposure
    expo_line_eval = case_when(
      expo_line_lsl_gtp_iva==1~1,
      TRUE ~expo_line_eval) ,
    
    #for consistency with the hex code, make a possible confounder one and an any one
    #all possible confounders together
    expo_line_poss_conf = case_when(
      expo_line_rda_cas==1 ~ 1,
      expo_line_rda_con==1 ~ 1,
      expo_line_p4h==1 ~ 1,
      expo_line_syl==1 ~ 1,
      expo_line_law==1 ~ 1, #could omit lawton because it's entirely contained within wst
      expo_line_mil==1 ~ 1,
      expo_line_uni==1 ~ 1, # just a sharrow. can probably ignore.
      expo_line_orm ==1 ~ 1,
      expo_line_col ==1 ~ 1,
      TRUE ~ 0
    ),
    
    expo_line_any = case_when(
      expo_line_eval==1~1,
      expo_line_poss_conf==1 ~ 1,
      TRUE ~expo_line_eval) ,
    
    #12/17/21 
    #considering the possibility of evaluating everything except for p4h
    #(also no united...just a sharrow)
    #adding for consistency with hex code
    expo_line_any_no_p4h = case_when(
      expo_line_eval== 1 ~ 1,
      expo_line_rda_cas==1 ~ 1,
      expo_line_rda_con==1 ~ 1,
      expo_line_syl==1 ~ 1,
      expo_line_law==1 ~ 1, #could omit lawton because it's entirely contained within wst
      expo_line_mil==1 ~ 1,
      expo_line_col==1 ~ 1,
      expo_line_orm ==1 ~ 1,
      TRUE ~0
    ),
    
    ## length of infrastructure excluding those we're evaluating----------------
    #calculating it this way will make it easy to sump up by hexagon.
    #to make it easier, flip your eval, so 1 is NOT evaluating
    expo_line_eval_flipped = abs(1-expo_line_eval),
    
    #how much infrastructure on this edge, excluding the evaluation infrastructure?
    infra_length_mi_1_trail_p_no_eval = infra_length_mi_1_trail_p*expo_line_eval_flipped,
    infra_length_mi_2_lane_p_no_eval = infra_length_mi_2_lane_p*expo_line_eval_flipped,
    infra_length_mi_3_lane_b_no_eval = infra_length_mi_3_lane_b*expo_line_eval_flipped,
    infra_length_mi_4_lane_c_no_eval = infra_length_mi_4_lane_c*expo_line_eval_flipped,
    infra_length_mi_5_sharrow_no_eval = infra_length_mi_5_sharrow*expo_line_eval_flipped
  )

save(bmap_edge_mo_nogeo, file = "bmap_edge_mo_nogeo.RData")

#------vars that need to be updated--------------##
# expo_line_eval
#expo_line_poss_conf
# expo_line_any

## checks---------
# There are a couple key beltline things I need to check for my aim 3.
#They should change from dirt to paved or none to paved
327174 #westside trail
1423722 #eastside trail extension
bmap_edge_join_wrangle$infra
names(bmap_edge_join_wrangle_nogeo)
bmap_edge_mo_nogeo %>% 
  filter(edge_id == "327174") %>% 
  dplyr::select(edge_id, starts_with("study_mo"), infra_6cat_long) %>% 
  print(n=25) #yes!

bmap_edge_mo_nogeo %>% 
  filter(edge_id == "1423722") %>% 
  dplyr::select(edge_id, starts_with("study_mo"), infra_6cat_long) %>% 
  print(n=25) #yes!


## look up dissertation variables by month-----------------
#load("bmap_edge_mo_nogeo.RData")
lookup_diss_a1_edge_mo= bmap_edge_mo_nogeo %>% 
  dplyr::select(edge_id, study_month, starts_with("diss_a1")) 
save(lookup_diss_a1_edge_mo, file = "lookup_diss_a1_edge_mo.RData")
#you're going to need a new exposure look-up table, by the way.
lookup_edge_mo_expo_line = bmap_edge_mo_nogeo %>% 
  dplyr::select(edge_id, study_month, starts_with("expo_line"))
save(lookup_edge_mo_expo_line, file = "lookup_edge_mo_expo_line.RData")
### dissertation aim 1 correct measurements and buffer area----------------
#hmm, we're still not excluding the WST dirt area. must be because of the spatial join?
#so for this we're going to bring in a couple of things to fix it, just for the 
#correct geometry.
#Bring in
#note that the basemap doesn't correctly exclude the westside trial dirt section.
#do so for this calculation
load("diss_inf_est_wst_fixes_pared.RData")
load("bmap_edge_join_wrangle.RData")
bmap_diss_a1_corrected_geo =  bmap_edge_join_wrangle %>% 
  filter(infra_exclude_for_length==0) %>%  
  filter(diss_a1_eval==1) %>% 
  filter(diss_a1_est==0) %>% 
  filter(diss_a1_wst==0) %>% #impoportant to keep wst straight
  dplyr::select(
    edge_id, starts_with("diss_a1")
  ) %>% 
  bind_rows(diss_inf_est_wst_fixes_pared) %>% 
  mutate(
    #re-calculate length
    length_m_diss_a1_correct = as.numeric(st_length(geometry)),
    length_km_diss_a1_correct = length_m_diss_a1_correct/1000,
    length_mi_diss_a1_correct = length_km_diss_a1_correct*0.621371,
    
    diss_a1_eval_name_section_short = case_when(
      name_section_short == "est1" ~ "est1",
      name_section_short == "est2" ~ "est2",
      name_section_short == "est3" ~ "est3",
      name_section_short == "wst1" ~ "wst",
      name_section_short == "wst3" ~ "wst",

      TRUE ~ diss_a1_any_name_section_short
    ),
    diss_a1_eval_name_group_short = case_when(
      name_section_short == "est1" ~ "est",
      name_section_short == "est2" ~ "est",
      name_section_short == "est3" ~ "est",
      name_section_short == "wst1" ~ "wst_pro",
      name_section_short == "wst3" ~ "wst_pro",
      
      diss_a1_any_name_section_short == "pro" ~ "wst_pro",
      diss_a1_any_name_section_short == "lsl" ~ "lsl_gtp_iva",
      diss_a1_any_name_section_short == "gtp" ~ "lsl_gtp_iva",
      diss_a1_any_name_section_short == "iva" ~ "lsl_gtp_iva",
      diss_a1_any_name_section_short == "pct1" ~ "pct",
      diss_a1_any_name_section_short == "pct2" ~ "pct",
      diss_a1_any_name_section_short == "mcd" ~ "mcd"
    ), 
  #name it this as well for easy linking elsewhere
  name_group_short = diss_a1_eval_name_group_short,
  #make a factor version of this for plotting in Figure 1
  #short name.
  diss_a1_eval_fac_t1 =
      factor(
        case_when(
          diss_a1_eval_name_section_short == "wst" ~ "WST",
          diss_a1_eval_name_section_short == "pro"  ~ "PCG",
          diss_a1_eval_name_section_short == "lsl" ~ "LS",
          diss_a1_eval_name_section_short == "gtp" ~ "TECH",
          diss_a1_eval_name_section_short == "iva" ~ "IAP",
          diss_a1_eval_name_section_short == "est1" ~ "EST-EXT1",
          diss_a1_eval_name_section_short == "est2" ~ "EST-EXT2",
          diss_a1_eval_name_section_short == "est3" ~ "EST-EXT3",
          diss_a1_eval_name_section_short == "pct1" ~ "PCT1",
          diss_a1_eval_name_section_short == "pct2" ~ "PCT2",
          diss_a1_eval_name_section_short == "mcd" ~ "MCD"
        ),
        levels = c(
          "WST",
          "PCG",
          "LS",
          "TECH",
          "IAP",
          "EST-EXT1",
          "EST-EXT2",
          "EST-EXT3",
          "PCT1",
          "PCT2",
          "MCD"
        ) #levels
        ) #close factor
  ) #close mutate


save(bmap_diss_a1_corrected_geo, file = "bmap_diss_a1_corrected_geo.RData")  
#12/21/21 I'm using this in my Figures, so I need this to be right
#and resolved.
# bmap_diss_a1_corrected_geo %>% 
#   mapview(zcol = "name_section_short")
# bmap_diss_a1_corrected_geo %>% 
#   mapview(zcol = "diss_a1_eval_name_group_short")
# bmap_diss_a1_corrected_geo %>% 
#   mapview(zcol = "diss_a1_eval_fac_t1")


#For the buffer areas, don't worry about it, as it's almost the exact same.

# Define buffers for aim 1----------------------
bmap_edge_join_wrangle_nogeo %>% 
  filter(diss_a1_eval==1) %>%   
  group_by(diss_a1_any_name_section_short) %>% 
  summarise(n=n())

## 1-mile buffer--------------
# library(tidyverse)
# library(sf)
# library(mapview)
# load("bmap_edge_join_wrangle.RData") #Load in case not running the whole thing
# table(bmap_edge_join_wrangle$diss_a1_any_name_section_short)
#4/12/22 I'm getting weird things where the st_buffer isn't working
#conver to feet, create buffer, and then convert back to 4326
bmap_diss_a1_any_buff_1_mi =  bmap_edge_join_wrangle %>% 
  filter(infra_exclude_for_length==0) %>%  #important to keep wst correct
#  filter(diss_a1_eval==1) %>%   
  filter(diss_a1_any==1) %>%
  group_by(diss_a1_any_name_section_short) %>% 
  summarise(length= sum(length_m, na.rm=TRUE)) %>% #doesn't matter what, just pick something
  st_union(by_feature = TRUE) %>% 
  ungroup() %>% 
  st_transform(2240) %>% #ft; see https://spatialreference.org/ref/?search=georgia
  st_buffer(5280) %>%  #1 mile in feet now; 4/12/22
  st_transform(4326) %>% 
  #to be consistent with your existing code:
  mutate(
    buffer_size_mi = 1,
    name_section_short=diss_a1_any_name_section_short
  )

save(bmap_diss_a1_any_buff_1_mi, file = "bmap_diss_a1_any_buff_1_mi.RData")
bmap_diss_a1_any_buff_1_mi %>% mapview(zcol = "diss_a1_any_name_section_short")

## 1/2-mile buffer------------------------------------
st_crs(bmap_edge_join_wrangle)
5280/2
bmap_diss_a1_any_buff_half_mi =  bmap_edge_join_wrangle %>% 
  filter(infra_exclude_for_length==0) %>%  #important to keep wst correct
  #  filter(diss_a1_eval==1) %>%   
  filter(diss_a1_any==1) %>%
  group_by(diss_a1_any_name_section_short) %>% 
  summarise(length= sum(length_m, na.rm=TRUE)) %>% #doesn't matter what, just pick something
  st_union(by_feature = TRUE) %>% 
  ungroup() %>% 
  st_transform(2240) %>% #ft; see https://spatialreference.org/ref/?search=georgia
  st_buffer(5280/2) %>%  #1/2 mile in feet now; 4/12/22
  st_transform(4326) %>% 
  #to be consistent with your existing code:
  mutate(
    buffer_size_mi = "half",
    name_section_short=diss_a1_any_name_section_short
  )

save(bmap_diss_a1_any_buff_half_mi, file = "bmap_diss_a1_any_buff_half_mi.RData")

#visualize and compare with what if I had done it using the basemap wrangle ONLY
mapview(
  bmap_diss_a1_any_buff_half_mi, 
  zcol = "diss_a1_any_name_section_short",
  col.regions = rainbow(n=n_distinct(bmap_diss_a1_any_buff_half_mi$diss_a1_any_name_section_short)))


