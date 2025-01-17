
#..............................................................................#
#   Script developed by Jen Cruz to calculate home ranges                      # 
#   We rely on amt vignette here:                                              #
#   https://cran.r-project.org/web/packages/amt/vignettes/p2_hr.html           #
#   as well as: Signer & Fieberg (2021) PeerJ9:e11031                          #
#   http://doi.org/10.7717/peerj.11031                                         #
#..............................................................................#

########################## DEFINING TRACKS SCRIPT ##############################
#.......................    For HR Estimation AMT  .............................

# load packages relevant to this script:

library( tidyverse ) #easy data manipulation
library( amt )
library( sp )
library( lubridate ) # easy date manipulation
library( sf )

# Load or create data
# Clean your workspace to reset your R environment.
rm( list = ls() )

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
# getwd()
# if so then:
workdir <- getwd()

# load workspace 
load( "cleaningPRFA21.RData" )
load( "TracksWorkspace_JEN.RData" )
load( "TracksWorkspace.RData" )


# Make Tracks using AMT
# Check sample size

table( datadf$territory )

# Data collection for each individual : histograms

# sampling dates

ggplot( datadf, aes( x = jday, group = territory ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  facet_wrap( ~ territory )

# speeds traveled (in knots)

ggplot( datadf, aes( x = speed, group = territory ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  facet_wrap( ~ territory )

#..............................................................................#
#......                                                                  ......#
#......   Creating tracks, calculating step lengths and turning angles   ......#
#......             for all individuals at the same time:                ......#
#......                                                                  ......#
#..............................................................................#

########################       CREATING TRACKS      ############################
#.......................    For HR Estimation AMT   ...........................#

# amt requires us to turn data into tracks for further analyses.

trks <- datadf %>% 
  # make track. Note you can add additional columns to it.
  amt::make_track(.y = lat, .x = lon, .t = ts, 
                  #define columns that you want to keep, relabel if you need:
                  id = id, sex = Sex, mth = mth,jday = jday, speed = speed, 
                  alt = alt, territory = territory, 
                  # assign collection crs (data collected in WGS 84)
                  crs = crsdata )
get_crs(trks) # check crs

# Reproject to UTM to convert lat lon to easting northing:

trks <- amt::transform_coords( trks, crs_to = crstracks ) 

# crstracks crs assigned from NCA_Shape, 
# which is in UTM Zone 11N

get_crs(trks) # check crs

# Turn into a tibble list by grouping and nest by individual IDs:

trks <- trks %>%  amt::nest( data = -"territory" )

# view

trks


# Remember we have multiple types of data including detailed data for flights:
# 3 times a week, 20min fixes during the day, then hourly fixes during migration.
# Focus on data during breeding season.
# Remove migration locations.
# How do we know when individuals started migrating North?
# Plot overall paths for each individual:

for( i in 1:dim(trks)[1]){
  a <- as_sf_points( trks$data[[i]] ) %>% 
    ggplot(.) + theme_bw(base_size = 17) +
    labs( title = paste0('individual =', trks$territory[i]) ) +
    geom_sf(data = NCA_Shape, inherit.aes = FALSE ) +
    geom_sf() 
  print(a)
}

# Use NCA polygon to remove records that exist East & North of the NCA.
# We can extract the extent of a polygon:

sf::st_bbox(NCA_Shape)

# Then use the Eastern & Northern coordinates to filter out data 

xmax <- as.numeric(st_bbox(NCA_Shape)$xmax) # 627081.5
ymax <- as.numeric(sf::st_bbox(NCA_Shape)$ymax)  + 10000

# subset those tracks less than as breeding and those > as migrating:
# jday 181 = July 1

trks <- trks %>% mutate(
  breeding = map( data, ~ filter(., x_ < xmax, y_ < ymax, jday < 181 ) ),
  migrating = map( data, ~ filter(., x_ >= xmax, y_ >= ymax, jday >= 181 ) ) )

# view

trks
trks[["breeding"]]

# Use info to report how many usable points we have during time of each inv @ NCA.
# Note we created two other groups of tibbles for the breeding season and migrating
# Plot step lengths

for( i in 1:dim(trks)[1]){
  a <-  steps( trks$breeding[[i]] ) %>% 
    # a <-  steps( trks$migrating[[i]] ) %>% 
    mutate( jday = lubridate::yday( t1_ ) ) %>% #what is t1_?
    group_by( jday ) %>% # daily
    summarise( sl_ = sum(sl_)/1000 ) %>% # sl = step lengths or distance traveled 
    ggplot(.) + theme_bw(base_size = 17) +
    geom_line( aes( y = sl_, x = jday), color = "dark green", lwd = 0.75) +
    ylim(NA,400) + 
    labs( title = paste0('individual =', trks$territory[i]),
          y = "Daily distance (km)", x = "Day of the year" )
  print(a)
}

# Focus on breeding season data:
# Estimate sampling rate for each individual by looping through data using 
# purrr function map( )

sumtrks <- trks %>%  summarize( 
  map( breeding, amt::summarize_sampling_rate ) )

# view

sumtrks[[1]] # sampling rate is seconds between fixes

#   facet_wrap( ~territory, scales = 'free' )

# Resample all tracks to 5 second intervals (with 3 second tolerance):

trks_resamp_5sec <- trks %>% mutate(
  red = map( breeding, function(x) 
    x %>%  track_resample( rate = seconds(5), 
                           tolerance = seconds(3)) %>% 
      steps_by_burst() ) )

view(trks_resamp_5sec[["data"]][5])

# red = resampled breeding tracks. Each burst contains 5-second gps fixes in sequence until 5-sec 
# interval ends, ending burst. New burst begins at next point where 5-second fixes resume. 
# All bursts are pieces of a track that only contain 5-sec fix data.

# split tibbles and create new objects for easier viewing: starting with breeding 
# season data only - which are still tracks not steps by bursts yet

trks_breed <- trks_resamp_5sec %>%
  # select only the tibbles we want to keep
  dplyr::select( territory, breeding ) %>%
  # unnest for easier visualization and processing
  unnest( cols = breeding )

# view

head( trks_breed)

# followed by steps by bursts breeding season data (5-sec gps fixes)

trks_5sec <- trks_resamp_5sec %>% dplyr::select( territory, red ) %>%
  unnest( cols = red )

# view

head(trks_5sec)

# Note that there are columns of interest in trks_breed that were not kept
# in the steps dataframe. We add those to our trks_5sec for a more complete picture

trks_5sec_complete <- trks_breed %>%
  # select columns of interest
  dplyr::select( id, territory, sex, mth, jday,
                 alt, speed, x2_ = x_, y2_ = y_, t2_ = t_  ) %>%
  # append to steps
  right_join( trks_5sec, by = c("territory", "x2_", "y2_", "t2_" ) )

# check
head( trks_5sec_complete)
trks_5sec_complete %>% filter( id == 1)

# steps = two sequential points, output tibble contains all seq steps that match your input criteria,
# and labels them as a burst each time there are 2+ steps matching your rate (3 seq gps points)

######################          PLOT STEP LENGTHS      #########################
#.....................         To Define Foraging      ........................#

# Plot step lengths:

trks_5sec_complete %>%   
  ggplot(.) +
  # geom_density( aes( x = sl_, fill = as.factor(burst_)), alpha = 0.4 ) +
  geom_histogram( aes( x = sl_ ) ) +
  xlab("Step length" ) + 
  # ylim( 0, 0.01 ) + xlim(0, 2000 ) +
  theme_bw( base_size = 19 )  +
  theme( legend.position = "none" ) +
  facet_wrap( ~territory, scales = 'free_y' )

# What is the unit on step length? meters?.....................................?

#######################         PLOT TURN ANGLES       #########################
#.....................         To Define Foraging      ........................#

# Plot turning angles:

trks_5sec_complete %>% # filter( id == 1 ) %>% 
  ggplot( .) +
  geom_histogram( aes( x = ta_ ) ) +
  #geom_histogram( aes( x = direction_p ) ) +
  #coord_polar() +
  ylab("Count") + xlab("TA") +
  xlim(-5,5) +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +
  theme_bw( base_size = 19 ) +
  facet_wrap( ~territory, scales = 'free_y' )

# Warning message:
#   Removed 8640 rows containing non-finite values (stat_bin). 
#   Missing TA values at start and end of burst maybe?

# Harsh turning angles <-0.5 and >0.5 .........................................?

########################    SAVING OBJECTS AND DATA    #########################
#..............................................................................#

# save breeding season data tracks (breeding, full data, not resampled)
 
write_rds( trks_breed, "trks_breed")

# save breeding season data tracks reduced to 5-second rate

write_rds( trks_5sec, "trks_5sec" )

# save complete/combined data including 5 second rate steps by burst and all 
# other associated info found in un-resampled breeding data

write_rds( trks_5sec_complete, "trks_5sec_complete" )

save.image( "TracksWorkspace.RData" )

########################         END OF SCRIPT       ###########################
