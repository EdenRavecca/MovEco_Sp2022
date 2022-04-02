

############################ DATA CLEANING SCRIPT ##############################
#.......................    Records/GPS/Projection   ...........................


# load packages relevant to this script:

library( sp )
library( tidyverse ) # easy data manipulation and plotting
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
#library( amt ) # creating tracks from location data
library(sf)
library(lubridate)

##############################  Load PRFA Data  ################################
#.................... PRFA data Shared Research Drive   ........................

# Clean your workspace to reset your R environment

rm( list = ls() )
load( "cleaningPRFA21.RData" )

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:

getwd()

# if so then:
workdir <- getwd()

# set path to where you can access your data

datapath <- "Z:/Common/PrairieFalcons/"

# Import GPS data 
# Fixes are stored as separate CSV files for each individual
# Create a function that imports multiple files at once:

load_data <- function( path ){
  # extract all file names in the folder
  myfiles <- dir( path, pattern = '\\.csv', full.names = TRUE )
  for( i in 1:length(myfiles) ){
    mydata <- read.csv( file = myfiles[i], 
                        # remove white spaces  
                        strip.white =TRUE, 
                        # include column headers
                        header = TRUE, 
                        # read the serial column as a character instead of number:
                        colClasses = c("serial" = "character") ) 
    # create df for first file and attach rows for other files
    ifelse( i == 1,
            df <- mydata, 
            df <- bind_rows(df, mydata) ) ## I'm not sure what's going on here
  } 
  #return df for all individuals
  return( df )
}

# apply function to import all files as list of databases:

dataraw <- load_data( paste0(datapath, 'allindvs/') )
head(dataraw)
# Note that the files are all in a subdirectory

# Import trapping records with details of when transmitters were deployed

records <- read.csv( file = paste0( datapath,"survey_0.csv" ),
                     # replace values with NA
                     na.strings = c(""," ","NA"), 
                     # include column headings
                     header = TRUE )
# check
head( records ); dim( records )

# import polygon of the NCA as sf spatial file:

NCA_Shape <- sf::st_read("Z:/Common/QCLData/Habitat/NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp")


############################ Cleaning Records ##################################
#.........................  Survey 123 PRFA data  ..............................

colnames( records )

# keep transmitter id, date and sex

records <- records %>% dplyr::select( Telemetry.Unit.ID, Sex, 
                                      Date.and.Time )
# view

records

# convert date to correct format using lubridate

records$StartDate <- lubridate::mdy_hms( records$Date.and.Time, 
                                         tz = "UTC")

# Add a day so that we can ignore records from the trapping day (or prior)
# and start only with those from the following day:

records$StartDate <- records$StartDate + lubridate::days(1)

# convert start date to Julian date

records$StartDay <- lubridate::yday( records$StartDate )

# unit IDs were recorded without the beginning of their serial number
# add beginning of CTT ID number, so we can match survey123 records
# to the GPS serial IDs:

records$serial <- paste0( '894608001201', records$Telemetry.Unit.ID )

# check 

head( records); dim( records)

############################ Cleaning GPS data ################################
#........................       CTT PRFA data      .............................

# GPS units often provide information on the quality of the fixes
# Units from CTT provide HDOP, VDOP and `time to fix` 
# Start by viewing what those look like in the dataset

hist( dataraw$vdop, breaks = 50 )
hist( dataraw$hdop, breaks = 50 )
hist( dataraw$time_to_fix )

# Remove 2D fixes and fixes where HDOP or VDOP ≥10 following
# D’eon and Delparte (2005); Poessel et al. (2016).
# Also those where time to fix > 20min or with 0 satellites:

# start by creating a new dataframe to store cleaned location records:

datadf <- dataraw 

colnames( datadf )
head( datadf )

# Filter to remove inaccurate locations

datadf <- datadf %>% dplyr::filter( hdop < 10 ) %>%
  dplyr::filter( vdop < 10 ) %>%
  dplyr::filter( time_to_fix <= 20 ) %>% 
  dplyr::filter( nsats > 0 ) %>%
  dplyr::filter( lat > 0 ) %>% 
  # remove superfluous columns
  dplyr::select( -inactivity, -geo, -data_voltage, -solar_current, 
                 -solar_charge )

# view

head( datadf ); dim( datadf )

# How many rows did we remove?
dim( dataraw ) - dim( datadf )

# We also need to create a time column containing date and time information
# in POSIX format (as required by amt)
# Data are stored in GPS_YYYY.MM.DD_HH.MM.SS format in our data 
# We define correct format with lubridate 

datadf$date <- lubridate::ymd_hms( datadf$GPS_YYYY.MM.DD_HH.MM.SS,
                                   tz = "UTC" )

# create new column where we convert it to posixct

datadf$ts <- as.POSIXct( datadf$date )

# view

head( datadf ); dim( datadf ) 
## Whats's the difference between date and ts? They look the same..

# check if any data are missing

all( complete.cases( datadf ) )
# none so we can move on

# add seperate month and day of year columns using lubridate

datadf <- datadf %>% 
  mutate( mth = lubridate::month(date),
          jday = lubridate::yday(date) )

# Remove records from prior to deployment 
# Keep relevant information from the records dataframe 
# Combine datadf to records df

datadf <- records %>%  dplyr::select( serial, Sex, StartDay ) %>% 
  right_join( datadf, by = "serial" )
# select serial, sex, StartDay from the records df and right_join to combine
# with datadf by `serial` ID

# view

head( datadf ); dim( datadf )

# Using StartDay to filter records, remove records that occurred prior to 
# StartDay (deployment)

datadf <- datadf %>% 
  group_by( serial ) %>% 
  dplyr::filter( jday > StartDay ) %>% 
  ungroup() 
# why group and ungroup?

# view

head( datadf ); dim( datadf )

# Create a new individual ID column:

datadf$id <- group_indices( datadf, serial )

# Using serial ID unique to each individual found in df, add territory column
# In Territory column each serial ID is linked to its corresponding territory

datadf <- datadf %>%
  mutate(territory = case_when(
    endsWith(serial, "47221") ~ "SG",
    endsWith(serial, "47775") ~ "CRW",
    endsWith(serial, "47874") ~ "SDTP",
    endsWith(serial, "48120") ~ "PR_II",
    endsWith(serial, "46751") ~ "HHGS_DS",
    endsWith(serial, "46983") ~ "HHGS_US",
    endsWith(serial, "47197") ~ "Mac",
    endsWith(serial, "48229") ~ "CRW_new",
    endsWith(serial, "48377") ~ "CFR",
  ))

unique(datadf$territory)
colnames(datadf)

SG <- datadf %>%
  dplyr::filter(., territory == "SG")
head(SG)
range(SG$jday)

#########################   Define CRS/Projection  #############################
#........................       CTT PRFA data      .............................

# Define coordinate system and projection for the data
# GPS data were recorded using WGS84 in lat lon
# Use the epsg code to define coordinate system
# https://spatialreference.org/ref/epsg/wgs-84/ 
# epgs = 4326 for this coordinate system

# For amt, crs need to be provided using sp package # why sp package?

# crsdata <- sp::CRS( "+init=epsg:4326" ) # old script, this line no longer works, deprecated
crsdata <- 4326 # sp::CRS( "+init=epsg:4326" ) # updated

# Transform the lat lon to eastings and northings using UTM
# http://www.dmap.co.uk/utmworld.htm
# zone 11:

# crstracks <- sp::CRS( "+proj=utm +zone=11" )
crstracks <- sf::st_crs( NCA_Shape )

# Convert the NCA shapefile to the same projection as GPS data and for the 
# tracks that will be created in next script

NCA_Shape <- sf::st_transform( NCA_Shape, crstracks ) # this line doesn't make sense with the previous line
# seems not needed

# Next Script: Defining Tracks

save.image( "cleaningPRFA21.RData" )

#########################    end of script  ####################################







