#........................................................................#
# Script developed by Jen Cruz to estimate SSFs and iSSFs                #
# approach derived from Fieberg et al. 2021 DOI: 10.1111/1365-2656.13441 #
# using code from Appendices B and C                                     #
# also Muff et al. 2019 DOI: 10.1111/1365-2656.13087                     #
# code here:                                                             #  
# https://conservancy.umn.edu/handle/11299/204737                        #
#                                                                        #
# We use landcover data from the National Geospatial Data Asset          #
# https://www.mrlc.gov                                                   #
# Habitat predictors include 2018 estimates of sagebrush cover           #
#........................................................................#

# Clean your workspace to reset your R environment

rm( list = ls() )

# load packages

library( tidyverse )
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt )
library( sf )
library( terra )
library( raster )
library( rasterVis ) # for raster visualization
library( glmmTMB ) # for analysis

###########################       LOAD DATA       ##############################
#.................... all necessary cleaned data objects .......................

# load 5 sec tracks

trks.fast <- read_rds( "trks.fast.breed" )

# load points also so that we can combine data
# Huh?.........................................................................?

trks.all.breed <- read_rds( "trks.breed" )

# view

head( trks.fast )

head( trks.all.breed )

# load range for all individuals estimated using ctmm 
# load( "../ctmm_akde_w.rda" ) 
# check that it loaded the object
# class( akde.w )

# import polygon of the NCA as sf spatial file:

NCA_Shape <- sf::st_read("Z:/Common/QCLData/Habitat/NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp")

# import NLCD data with raster:

habdata <- raster::raster( "Z:/Common/QCLData/Habitat/NLCD_new/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img" )

# # visualize with either rasterVis or terra::plot:
# 
# rasterVis::levelplot( habdata )
# 
# # view
# 
# habdata

############################     PREP DATA    ##################################

# we prepare the predictor data similarly to how we did it for RSFs:
# get coordinates from shapefile

crstracks <- sf::st_crs( NCA_Shape )

# checking outline of NCA

sf::st_bbox( NCA_Shape )

# We define available habitat as area of NCA with a small buffer
# around it and draw points from it
# create a buffer around the NCA using outline of NCA and sf package:
# we are more generous than with our RSF analyses

NCA_buf <- NCA_Shape %>% sf::st_buffer( dist =1e4 )

# create a version that matches coordinates of the predictor raster:
# coordinates supposed to be projection?

NCA_trans <- sf::st_transform( NCA_buf, st_crs( habdata ) ) 
NCA_shapereproj <- sf::st_transform( NCA_Shape, st_crs( habdata ) ) 

# crop raster to buffered NCA:

nlcd_crop <- raster::crop( habdata, NCA_trans )

# Now that we have cropped it to the appropriate area it should be faster to process 

# view

nlcd_crop
# Warning message:
# In matrix(ratvalues, nrow = length(ratvalues)/length(ratnames)) :
#  data length [1791] is not a sub-multiple or multiple of the number of rows [255]

# # values greater than 100 are empty so replace with missing
# 
# sage_cropped[ sage_cropped > 100 ] <- NA

# Plot habitat data

plot( nlcd_crop )
plot(NCA_shapereproj, col= NA, lwd= 2.5, add= TRUE)

# extract individual ids

ids <- unique(trks.all.breed$territory)

# to create random steps, we start by nesting our data using purr:

steps_all <- trks.fast %>% nest( data = -"territory" )

# view

steps_allx <- steps_all %>% unnest(cols = data)
head(trks.fast)
range(trks.fast$sl_)
which(steps_allx$sl_== 0)

# we then estimate random steps

steps_all <- steps_all %>% 
  dplyr::mutate( rnd = lapply( data, function(x){
    amt::random_steps( x ) } ) )

#......................................................?????????????????????????
# Steps with length 0 are present. This will lead to an error when fitting a gamma 
# distribution. 0 step lengths are replaced with the smallest non zero step length, 
# which is: 0.333185581903045

# Note that by default the random_steps() function fits a tentative
# gamma distribution to the observed step lengths and a tentative
# von Mises distribution to the observed turn angles.
# It then generates random available points by sampling step-lengths
# and turn angles from these fitted distributions and combining these
# random steps with the starting locations associated with each observed
# movement step.

# now unnest the new dataframes to make sure they worked

stepsdf <- steps_all %>% dplyr::select( territory, rnd ) %>% 
  unnest( cols = rnd ) 

# view

head( stepsdf );dim( stepsdf )

# Before extracting data we need to turn it to sf object and
# change projection to match the raster.

# We start by turning it to sf object, assigning the correct projection

steps_sf <- sf::st_as_sf( stepsdf, coords = c("x2_", "y2_"), 
                          crs = crstracks )

# Note that we use the end of the step coordinates, since we want to 
# focus on habitat selection, not movement.

steps_sf

# We then transform the crs:

steps_trans <- sf::st_transform( steps_sf, st_crs(nlcd_crop) )

# extracting with raster we can use the sf object directly, you also 
# have the choice to use a buffer around each point if you want to increase 
# your resolution:

nlcd_vals <- raster::extract( x = nlcd_crop, steps_trans,
                             method = "simple" )

# check

nlcd_vals

# Remember in our prior script we created the random steps, which were #
# fit using step-length and turning angle distributions. #
df <- dfraw
# we check sample sizes for each individual
table( df$id)
#select ids those with poor sample size
remids <- c( 1,3,7)
# use those to remove their data from dataframe:
df <- df %>%  
  dplyr::filter( !(id %in% remids) )
#check 
dim(df)

# We cannot have missing values for the predictors so we 
# need to remove steps with missing ta_ 
#step ids are not unique to individuals so we create a unique id:
df$id_step <- paste0( df$id, df$step_id_ )
head( df);dim(df)

#which steps have missing ta values:
df$id_step[ which( is.na(df$ta_) ) ]
#how many don't:
length( df$id_step[ which( !is.na(df$ta_) ) ] )

# which steps have missing sl values:
df$id_step[ which(is.na(df$sl_)) ] 
#none so we focus on ta only

# we record ids for those with missing ta values:
rem <- df$id_step[ which(is.na(df$ta_)) ] 

#remove them from dataframe
df <- df %>%  
  dplyr::filter( !(id_step %in%  rem ) )

dim(df)
#recheck sample size:
table( df$id)


habpath <- "Z:/Common/QCLData/Habitat/NLCD_new/"

# Import the legend for the 2019 NLCD data.
nlcdlegend <- read.csv( paste( habpath, "nlcd_2019_land_cover_l48_20210604/NLCD_Land_Cover_Legend.csv", sep = "") )

# Extract habitats found at our sites.
num.codes <- unique( unlist( nlcd_vals ) )

# Create reduced legend based on habitats present at our sites

update.legend <- nlcdlegend %>% dplyr::filter( Value %in% num.codes )  

# View updated legend.
print( update.legend )

# What proportion of our data are missing values

sum( is.na( nlcd_vals ))/ length( nlcd_vals )
# 0

# We append our predictor estimates to the original steps tibble:

df_all <- cbind( stepsdf, nlcd_vals )
head(df_all)
head(update.legend)
colnames(update.legend)
class(df_all)

table(df_all$nlcd_vals)

df_all <- dplyr::left_join(df_all, update.legend, by = c("nlcd_vals" = "Value"))
head(df_all_label)

# # Scale predictors 
# 
# df_all$nlcd_vals <- scale( df_all$nlcd_vals )
# 
# # replace missing values with mean, which is 0 after they have been scaled
# 
# df_all$sage_30m[ is.na(df_all$sage_30m) ] <- 0

# we also assign weights to available points to be much greater than #
# used points

df_all$weight <- 1000 ^( 1 - as.integer(df_all$case_ ) )

# check

head( df_all )

#######################           ANALYZE DATA           #######################

# amt function doesn't take random effects or weights so we move to a more
# flexible package. We move straight into a model #
# that includes random intercepts and slopes as well as a fixed #
# large variance for the random intercepts, as recommended by Muff #
# et al. 2019 and weights of 1000 for available points #

# we start by defining the model without running it, which let's us
# fit the large variance to the random ID intercepts

m1.struc <- glmmTMB( case_ ~ nlcd_vals +  
                       # define random effects
                       ( 1| step_id_ ) + 
                       ( 1| territory ) + ( 0 + nlcd_vals | territory ), 
                     family = poisson, data = df_all, 
                     weights = weight, doFit=FALSE ) 

# fix variance

m1.struc$parameters$theta[ 1 ] <- log( 1e3 ) 

# tell it not to change variance

m1.struc$mapArg <- list( theta = factor( c(NA, 1:2) ) )

# then fit the model

m1 <- glmmTMB::fitTMB( m1.struc )
summary( m1 )

######## how would we partition data into behavioral states prior to analyses
# for the used data we start by exploring it a bit more to refresh #
# our memory:
# we combine track dataframe to use information we have calculated 
# previously 
head(trks.all.breed)

trks_all <- trks.all.breed %>% 
  # select columns of interest
  dplyr::select( id, territory, sex, mth, jday,
                 alt, speed, x2_ = x_, y2_ = y_, t2_ = t_  )

trks_all <- trks_all %>% 
  # append to steps
  right_join( trks.fast, by = c("territory", "x2_", "y2_", "t2_" ) )

# check

head( trks_all )

# we add week for easier visualization and subsetting 

trks_all <- trks_all %>%  
  # remove those bursts that have too few points
  amt::filter_min_n_burst( min_n = 5 ) %>% 
  # add week and hour columns 
  mutate( wk = lubridate::week( t2_ ), 
          hr =  lubridate::hour( t2_ ) )  

# we remind ourselves about over which weeks our data overlap
ggplot( trks_all, aes( x = jday, fill = as.factor(wk) ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( alpha = 0.8 ) +
  facet_wrap( ~ territory )

# from this we can see that we have an uneven sample size that we 
# need to deal with 

# we visualise step lengths and turning angles for each individual 

trks_all %>%   
  ggplot(.) +
    geom_density( aes( x = sl_, fill = as.factor(wk) ), alpha = 0.6 ) +
  #  geom_density( aes( x = ta_, fill = as.factor(wk) ), alpha = 0.6 ) +
  #  geom_density( aes( x = hr, fill = as.factor(wk)), alpha = 0.6 ) +
  #  geom_density( aes( x = speed, fill = as.factor(wk) ), alpha = 0.6 ) +
  #geom_histogram( aes( x = sl_, fill = as.factor(wk) ) ) +
  #xlab("Step length" ) + 
  #ylim( 0, 0.01 ) + 
  #xlim(0, 300 ) +
  theme_bw( base_size = 19 )  +
  theme( legend.position = "none" ) +
  facet_wrap( ~territory, scales = 'free' )

# We plot alternative selections for step lengths and turning angles
# to choose parameters that may help to coarsely remove nesting 
# locations and travelling locations

ids <- unique( trks_all$id )
ters <- unique( trks_all$territory )

for( i in ters ){  
  # pull observations that you think belong to foraging
  a <- trks_all %>% dplyr::filter( territory == "CFR" ) %>% 
    # dplyr::filter( ta_ < 0.5 | ta_ > -0.5 ) %>% 
    dplyr::filter( sl_ < 5000 ) %>% 
    dplyr::filter( speed > 1 | speed < 30 ) %>% 
    # dplyr::filter( hr > 6 | hr < 20 ) %>% 
    sf::st_as_sf(., coords = c("x2_", "y2_"), crs = crstracks )
  # pull all observations for that individual
  b <- trks_all %>% filter( territory == "CFR" ) %>% 
    sf::st_as_sf(., coords = c("x2_", "y2_"), crs = crstracks )
  # compare
  c  <- ggplot( b ) +
    theme_bw( base_size = 15 ) +
    labs( title = "CFR" ) +
    theme( legend.position = "none" ) +
    geom_sf() +
    geom_sf( data = NCA_Shape, inherit.aes = FALSE, alpha = 0 ) +
    geom_sf( data = a, aes(col = as.factor( burst_ ) ), 
             inherit.aes = FALSE )# +
  # facet_wrap( ~wk )
  
  print( c )
}

# We can use these parameters to subset the data but we would have #
# to recalculate steps #

##########################################################################
### Save desired results                                  #
# Save the steps dataframe with extracted raster values so that I 
# don't have to recreate it when estimating issfs 
write_rds( df_all, "df_all" )

write_rds( trks_all, "trks_all" )

#Also save the unscaled raster values as a csv
write.csv( nlcd_vals, "nlcd_vals.csv", row.names = FALSE )
head(nlcd_vals)

#save workspace if in progress
save.image( 'SSF_PRFA.RData' )
############# end of script  ##################################

