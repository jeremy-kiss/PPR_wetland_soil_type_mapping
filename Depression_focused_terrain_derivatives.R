## Depression focused terrain derivatives 
## Author: Jeremy Kiss
## Date: 2022-01-25

#install whitebox
library(devtools)
devtools::install_github("giswqs/whiteboxR")
whitebox::wbt_init()

# whitebox tool descriptions: https://jblindsay.github.io/wbt_book/intro.html 
### Depression focused - DEM terrain derivatives 

#load library
library(raster)
library(rgdal)
library(dplyr)
library(spdplyr)
library(fasterize)
library(stars)
library(exactextractr)

####
#### USER INPUTS 
####

## Minimum depression depth size to remove in meters 
## 0.1 recommended 
min_dep_depth <- 0.1


#set wd 
wd <- "D:/wd/"
setwd(wd)

## input DEM filepath 
dem.filepath <- "D:/wd/DEM.tif"

####
#### 
####


#create directories for specific output file types
dir.create("./working/")
dir.create("./final/")

# load dem 
# script was written for DEM 2 - 10 m in resolution with smoothing 
dem <- raster(dem.filepath)

###
### CLOSED DEPRESSIONS 
###

# fill depressions using whitebox
# don't load whitebox library because it affects the raster::extract function
whitebox::wbt_fill_depressions(dem.filepath, "./working/filled_dem_nofix.tif", fix_flats = F  )

# subtract original dem from filled dem
whitebox::wbt_subtract( input1= "./working/filled_dem_nofix.tif", input2= dem.filepath, output = "./working/fill_diff.tif"  )

# load fill_difference raster
fill_diff <- raster("./working/fill_diff.tif"   )

# reclassify to binary 
# reclass rules 
reclass.list <- c(0,     0.0001, 0,
                  .0001, 1000,   1)
# create matrix
reclass.mat <- matrix(reclass.list, ncol = 3, byrow=T )

# reclassify 
fill_diff_bin <- reclassify(fill_diff, reclass.mat) 

# convert to polygon 
# FAST METHOD - faster than rasterToPolygons 
closed_dep_poly <- st_as_stars(fill_diff_bin) %>% st_as_sf(merge = T)

# select only depression polygons 
closed_dep_poly <- closed_dep_poly %>% filter(fill_diff == 1 )

# create unique poly ID 
closed_dep_poly$DEP_ID <- paste0(seq.int(nrow(closed_dep_poly)))

# remove fill_diff column 
closed_dep_poly  <- closed_dep_poly %>% select(DEP_ID )

# compute zonal statistics of polygons to determine the maximum depth for each depression
# this will create a df output of the max dep for each polygon
closed_dep_poly$fill_diff <- exact_extract(x = fill_diff, y = closed_dep_poly, fun = 'max')

#select only those with a depth greater than the minimum size specified above 
closed_dep_poly <- closed_dep_poly %>% filter(fill_diff > min_dep_depth )

# export the closed depressions
st_write(closed_dep_poly, dsn= "./working/closed_dep.shp", layer= "closed_dep" , driver= "ESRI Shapefile", append = F ) # append F is overwrite


###
### DEPRESSION WATERSHEDS
###

# determine the watersheds for depressions that are deeper than the min_dep_depth 

## fill the dem again, this time only filling the depressions shallower than the min dep depth defined. those depressions will be   fixed so that their flow is re-routed 

# create basins using whitebox. This will delineate watershed basins for all depressions, including very small depressions. Some of the previously identified closed depressions will contain multiple watersheds, which will be joined to replicate the output from the ArcGIS watershed tool, that can use polygons as input 
whitebox::wbt_fill_depressions(dem.filepath, "./working/fixed_dem.tif", fix_flats = T , max_depth = min_dep_depth  )

## create a d8 pointer surface 
whitebox::wbt_d8_pointer("./working/fixed_dem.tif", output = "./working/d8pointer.tif", esri_pntr = F )

# basins
whitebox::wbt_basins(d8_pntr = "./working/d8pointer.tif",  output ="./working/basins.tif"    )

# load in basins 
basins <- raster("./working/basins.tif" )

# Raster to polygon 
# faster method 
basins_poly <-  st_as_stars(basins) %>% st_as_sf(merge = T)

# Need to figure out which basins belong to each depression, then they can be dissolved 
# convert to sf 
closed_dep_sf <- st_as_sf(closed_dep_poly)

# make valid
basins_poly <- st_make_valid(basins_poly)
closed_dep_sf <- st_make_valid(closed_dep_sf)

# join the polygons. largest function associates watersheds with the depression they have the largest overlap with. This sorts out issues where depressions may touch other watersheds 
dep_wsheds <- st_join(x= basins_poly, y= closed_dep_sf, join= st_intersects, largest = T )

# pull out the watersheds that have no depressions associated with them 
orphan_wsheds <- dep_wsheds %>% filter(is.na(DEP_ID) )  %>% select(DEP_ID, fill_diff )

# delete orphaned watersheds that are less than 100m2
orphan_wsheds$area <- st_area(orphan_wsheds) 
orphan_wsheds$area <- as.vector(orphan_wsheds$area )
orphan_wsheds <- orphan_wsheds %>% filter(area >= 100)

# set fill_diff (depression depth) to 0.1 for all of these orphaned watersheds - depressions don't exist within them so a small value is given 
orphan_wsheds$fill_diff <- 0.1

# export orphaned watersheds 
write_sf(orphan_wsheds , dsn= "./working/orphan_wsheds.shp", layer= "dep_wsheds" , driver= "ESRI Shapefile", append = F )

# union - this will group all depression watersheds with common DEP_IDs
dep_wsheds <- dep_wsheds %>% group_by(DEP_ID) %>% summarise()

# delete watershed polygons without associated depression
dep_wsheds <- dep_wsheds %>% filter(!is.na(DEP_ID) )

# export
write_sf(dep_wsheds , dsn= "./working/dep_wsheds.shp", layer= "dep_wsheds" , driver= "ESRI Shapefile", append=F)



###
### ORPHANED WATERSHEDS MIN ELEVATION RASTER
###

# extract minimum elevation value from depressions
orphan_wsheds$dep_MIN <- exact_extract(x = dem, y = orphan_wsheds, fun = 'min')

# convert dep_MIN from data frame to list 
orphan_wsheds$dep_MIN <- as.vector(orphan_wsheds$dep_MIN)

# convert to raster 
orphan_wsheds_rst <- rasterize(x = orphan_wsheds, y = dem , field = orphan_wsheds$dep_MIN )

# export
writeRaster(orphan_wsheds_rst, 
            filename = "./working/orphan_wsheds_MIN.tif", 
            format = "GTiff",
            overwrite = T)


###
### DEPRESSION SPILLOVER ELEVATION RASTER
###

# need to generate points along the closed depression edge

# Convert closed depression polys to lines 
closed_dep_poly_sp <- as(closed_dep_poly, "Spatial")
closed_dep_outline <- as(closed_dep_poly_sp, "SpatialLinesDataFrame")

# can't use exact_extract for lines 
beginCluster()
closed_dep_outline$spill_MIN <- extract(x= dem, y=closed_dep_outline, fun=min, na.rm= T)
endCluster()

# convert dep_wsheds to SP 
dep_wsheds_sp <- as(dep_wsheds, "Spatial")

# merge by the DEP_ID
dep_wsheds_sp <- merge(x = dep_wsheds_sp, y = closed_dep_outline, by ="DEP_ID")

# convert back to sf 
dep_wsheds <- st_as_sf(dep_wsheds_sp)

# convert to raster 
spill_MIN_rst <- fasterize(sf = dep_wsheds, raster = dem, field = "spill_MIN"  )

# merge with orphaned raster min raster 
spill_MIN_rst_w_orphan <- cover(x = spill_MIN_rst, y = orphan_wsheds_rst  )

# export
writeRaster(spill_MIN_rst_w_orphan, 
            filename = "./working/spill_MIN.tif", 
            format = "GTiff",
            overwrite = T)


###
### DEPRESSION MINIMUM ELEVATION RASTER
###

# extract minimum elevation value from depressions
closed_dep_poly$dep_MIN <- exact_extract(x= dem, y=closed_dep_poly, fun = 'min')


# convert dep_MIN from data frame to list 
closed_dep_poly$dep_MIN <- as.vector(closed_dep_poly$dep_MIN)

# merge by DEP_ID
dep_wsheds_sp <- merge(x = dep_wsheds_sp, y = closed_dep_poly, by ="DEP_ID")

# convert to raster 
dep_MIN_rst <- rasterize(x = dep_wsheds_sp, y = dem , field = dep_wsheds_sp$dep_MIN )

# merge with orphaned raster min raster 
dep_MIN_rst_w_orphan <- cover(x = dep_MIN_rst, y = orphan_wsheds_rst  )

# export
writeRaster(dep_MIN_rst_w_orphan, 
            filename = "./working/dep_MIN.tif", 
            format = "GTiff",
            overwrite = T)


###
### DEPRESSION DEPTH RASTER
###


# create raster of orphaned dep depth 
# convert dep_MIN from data frame to list 
orphan_wsheds$dep_MIN <- as.vector(orphan_wsheds$dep_MIN)

# convert to raster 
orphan_wsheds_rst <- fasterize(sf = orphan_wsheds, raster = dem , field = 'dep_MIN' )

# merge orphaned wsheds back with main 
dep_wsheds <- dep_wsheds %>% select(DEP_ID, fill_diff, geometry )
orphan_wsheds <- orphan_wsheds %>% select(DEP_ID, fill_diff, geometry )
wsheds <- rbind(dep_wsheds, orphan_wsheds )

# convert to raster 
dep_depth_rst <- fasterize(sf = wsheds, raster = dem , field = "fill_diff" )

# export
writeRaster(dep_depth_rst, 
            filename = "./final/dep_depth.tif", 
            format = "GTiff",
            overwrite = T)


###
### CALCULATE ELEVATION ABOVE DEPRESSION MIN
###

# subtract depression minimum from elevation 
whitebox::wbt_subtract( input1= dem.filepath, input2= "./working/dep_MIN.tif", output = "./final/elev_from_dep_min.tif"  )

# define nodata value 
whitebox::wbt_set_nodata_value(input = "./final/elev_from_dep_min.tif", output = "./final/elev_from_dep_min.tif", back_value = 3.4e+38 )


###
### CALCULATE ELEVATION FROM SPILLOVER 
###

# subtract depression minimum from elevation 
whitebox::wbt_subtract( input1= dem.filepath, input2= "./working/spill_MIN.tif", output = "./final/elev_from_spill_min.tif"  )

# define nodata value 
whitebox::wbt_set_nodata_value(input = "./final/elev_from_spill_min.tif", output = "./final/elev_from_spill_min.tif", back_value = 3.4e+38 )


###
### CALCULATE ELEVATION ABOVE DEPRESSION MIN PER DEPRESSION DEPTH RATIO
###

# divide elevation from depression min by depression depth 
whitebox::wbt_divide( input1= "./final/elev_from_dep_min.tif", input2= "./final/dep_depth.tif", output = "./final/ratio_elev_from_min_per_depth.tif")



###
### CREATE RASTER OF MAX CATCHMENT AREA PER WATERSHED 
###

# calculate catchment area 
whitebox::wbt_fd8_flow_accumulation(dem= dem.filepath, output= "./working/catchment.tif", out_type = "catchment area" )

# load catchment area raster 
catchment.rst <- raster( "./working/catchment.tif")

# extract MAX catchment area per watershed poly 
wsheds$max_catch <- exact_extract(x= catchment.rst, y=wsheds, fun = 'max')

# convert to raster 
wshed_max_catch_rst <- fasterize(sf = wsheds, raster = dem , field = "max_catch" )

# export
writeRaster(wshed_max_catch_rst , 
            filename = "./final/wshed_max_catch.tif", 
            format = "GTiff",
            overwrite = T)




### SMOOTHING AT WATERSHED EDGES 

# convert watershed poly to line 
dep_wshed_outline <- as(dep_wsheds_sp, "SpatialLinesDataFrame")

# buffer along lines, width is in meters and on each side, so width of 10 m will be 20 m total. This should scale with the resolution 
# get resolution
res <- res(dem)

# set width equal to 2* resolution unless res is greater than 10, then just set to resolution 
width <- if (res >= 10) {res[1] } else {res[1]*2 }

# create the watershed buffer 
wshed_buffer <- buffer(dep_wshed_outline, width = width, dissolve =T)

# convert to sf 
wshed_buffer_sf <- st_as_sf(wshed_buffer )

# rasterize the wshed buffer 
wshed_buffer_rst <- fasterize(sf = wshed_buffer_sf, raster = dem )



###
### SMOOTH THE WATERSHED EDGES IN THE COVARIATES (EXCLUDING DEPRESSION DEPTH)
### AND FILLING NO DATA AREAS WITHIN EXTENT 
###

# Define the function to replace the covariate watershed edge with the smooth
# x is the buffer edge along watershed edges 
# y is the covariate raster 
replace_w_smth_fn <- function (x, y){ 
  ifelse(is.na(x), y, x ) }

# count number of NoData cells in dem 
dem_NoData <- sum(is.na(values(dem)))

# create list of the final covariates to have watershed edges smoothed 
final.list <- list.files(path = "./final/", pattern = "\\.tif$", full.names = T)

### loop to smooth at edges and fill no data areas 

for (z in 1:length(final.list)){
  # save path
  raster.path <- final.list[z]
  
  # load raster
  covar_raster <- raster(raster.path)
  
  ### SMOOTH Watershed EDGES - SKIP depression depth, max catchment, and wetland strahler rasters 
  
  if ( raster.path != "./final/dep_depth.tif" & raster.path != "./final/wshed_max_catch.tif"  ){
    
    #mask along buffer
    buffer <- mask(covar_raster, mask = wshed_buffer_rst  )
    
    #smooth along buffer 
    buffer_smth <- focal(buffer, w=matrix(1,3,3), fun = mean, na.rm = T   )
    
    #replace buffer area with smoothed 
    raster_smth <- overlay (x = buffer_smth , y = covar_raster, fun = replace_w_smth_fn  )
    
    covar_raster <- raster_smth
    
  }
  # end SMOOTH watershed EDGES 
  
  
  # count NoData for covar raster
  raster_NoData <- sum(is.na(values(covar_raster)))
  
  # check if the number of no data values matches the dem raster. if it does not, then it will fill the raster. It will doe so with a small smoothing window first, then a large if it is still required. Mask after filling. NAonly means hat only NoData are replaced with computed focal values 
  if (raster_NoData != dem_NoData){
    
    filled1.raster <- focal(covar_raster, w=matrix(1,3,3), fun=mean, na.rm=TRUE,NAonly=TRUE, pad=TRUE )
    
    covar_raster <- mask(x= filled1.raster, mask=dem)
    
    covar_raster_no.ofNA <- sum(is.na(values(covar_raster))) 
    
    
    
    if (covar_raster_no.ofNA != dem_NoData){
      
      filled2.raster <- focal(covar_raster, w=matrix(1,5,5), fun=mean, na.rm=TRUE,NAonly=TRUE, pad=TRUE )
      
      covar_raster <- mask(x= filled2.raster, mask=dem)
      
      covar_raster_no.ofNA <- sum(is.na(values(covar_raster))) 
      
      
      
      if (covar_raster_no.ofNA != dem_NoData){
        
        filled3.raster <- focal(covar_raster, w=matrix(1,99,99), fun=mean, na.rm=TRUE,NAonly=TRUE, pad=TRUE )
        
        covar_raster <- mask(x= filled3.raster, mask=dem)
        
      }}}
  
  
  # Export 
  # issue in exporting if raster has not been written to memory. do arbitrary calculation to bring into memory 
  covar_raster <- covar_raster * 1
  
  # export 
  writeRaster(covar_raster, 
              filename = raster.path, 
              format = "GTiff",
              overwrite = T)
  
  # end of for loop 
}



## delete working folder 
unlink('./working',recursive = TRUE)



