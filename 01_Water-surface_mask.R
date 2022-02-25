## Mask DEM flat water surfaces 
## Author: Jeremy Kiss
## Date: 2022-01-25

# a mask for the flat bottoms is created by calculating ConvI and calculating the standard deviation of that surface. 
# A reclassified surface of standard deviation is combined with a surface of closed depressions. Only the locations within closed depressions are classified as depression flat bottoms  

# work directory 
wd <- "D:/wd/sites/"
setwd(wd)

# site list 
# within the work directory, need a folder per site. Each site folder should have a dem directory, with the original dem in its own folder   named by its spatial resolution
# create a list of the site names matching the folder names 
site.list <- c("STDE", "SWCU", "SMCR")

# load libraries
library(raster)
library(whitebox)
# set connection b.etween RStudio and SAGA 
library(RSAGA)
work_env <- rsaga.env(path = "C:/Program Files (x86)/SAGA-GIS/")

# for each site
for(i in 1:length(site.list)){
  
  # get site name.
  site <- site.list[i]
  
  # specify site wd 
  setwd(wd)
  site.wd <- paste0("./", site )
  setwd(site.wd)
  
  # path to 1m DEM 
  dem1m.fp <- normalizePath(paste0("./dem/1m/", site, "_dem_1m.tif") )
  
  # create directory for flat bottom mask
  dir.create("./flat-btm_mask")
  dir.create("./flat-btm_mask/tmp")
  dir.create("./dem/working_vers/")
  
  # resample to 2m  using GDAL
  system(paste0("gdalwarp -tr 2 2 -r bilinear -overwrite   ", dem1m.fp, " ./dem/working_vers/dem2m.tif  " ) )
  
  # specify dem path for ConvI - want something with less processing and smoothing 
  dem2m.fp <- normalizePath("./dem/working_vers/dem2m.tif" )
  
  # import DEM to .sgrd_______________________________________________________________________________________________
  rsaga.geoprocessor("io_gdal", module = 0, env = work_env, 
                     param = list(
                       FILES= dem2m.fp,
                       GRIDS= "./flat-btm_mask/tmp/dem.sgrd" ,
                       RESAMPLING= 0 ))
  
  # calculate convergence index using SAGA 
  rsaga.geoprocessor("ta_morphometry", module = 1, env = work_env, 
                     param = list(
                       ELEVATION= "./flat-btm_mask/tmp/dem.sgrd" ,
                       RESULT = "./flat-btm_mask/tmp/ConvI.sgrd" ,
                       METHOD= 1,
                       NEIGHBOURS = 0
                     ))
  
  # export as tif 
  rsaga.geoprocessor("io_gdal", module = 2, env = work_env, 
                     param = list(
                       GRIDS=  "./flat-btm_mask/tmp/ConvI.sgrd" ,
                       FILE = "./flat-btm_mask/tmp/ConvI.tif"  
                     ))
  
  # calculate standard deviation of ConvI using a window of 19 x 19 
  wbt_standard_deviation_filter(i="./flat-btm_mask/tmp/ConvI.tif" ,
                                o = "./flat-btm_mask/tmp/ConvI_sd.tif",
                                filterx = 19 ,
                                filtery =19   
                                )
  
  # load raster into r 
  ConvI.sd <- raster("./flat-btm_mask/tmp/ConvI_sd.tif")
  
  # Reclassify the standard deviation 
  # reclassify to binary 
  # reclass rules 
  reclass.list <- c(0, 30, 0,
                    30, 200, 1)
  # create matrix
  reclass.mat <- matrix(reclass.list, ncol = 3, byrow=T )
  
  # reclassify 
  convI.class <- raster::reclassify(ConvI.sd, reclass.mat) 
  
  # export
  writeRaster(convI.class, filename = "./flat-btm_mask/tmp/convI_class.tif", format = "GTiff", overwrite = T)
  
  
  ## CLOSED DEPRESSIONS
  
  dem2m.sm.fp <- "./flat-btm_mask/tmp/dem2m_Sm.tif"
 
  
  # run smoothing on 2m DEM before calculating closed depressions 
  wbt_feature_preserving_smoothing( 
    dem = dem2m.fp  ,
    output = dem2m.sm.fp ,
    filter=11,
    norm_diff = 15.0,
    num_iter = 3) 
  
  # 3x3 simple mean filter 
  wbt_mean_filter(
    i = dem2m.sm.fp ,
    output = dem2m.sm.fp ,
    filterx = 3,
    filtery=3)
  
  # fill single cell pits 
  wbt_fill_single_cell_pits( 
    dem = dem2m.sm.fp ,
    output =  dem2m.sm.fp )
  
  # fill depressions using whitebox
  # don't load whitebox library because it affects the raster::extract function
  whitebox::wbt_fill_depressions(dem2m.sm.fp , "./flat-btm_mask/tmp/filled_dem_nofix.tif", fix_flats = F  )
  
  # subtract original dem from filled dem
  whitebox::wbt_subtract( input1= "./flat-btm_mask/tmp/filled_dem_nofix.tif", input2= dem2m.sm.fp , output = "./flat-btm_mask/tmp/fill_diff.tif"  )
  
  ## load fill_difference raster
  fill_diff <- raster("./flat-btm_mask/tmp/fill_diff.tif"   )
  
  #reclassify to binary 
  #reclass rules 
  reclass.list <- c(0,     0.0001, 0,
                    .0001, 1000,   1)
  #create matrix
  reclass.mat <- matrix(reclass.list, ncol = 3, byrow=T )
  
  #reclassify 
  fill_diff_class <- reclassify(fill_diff, reclass.mat) 
  
  # raster calculator 
  ifelse_fn <- function(x,y){
    ifelse(x==1 & y==1 , 1, 0)
    }
  
  # apply function 
  flat_mask <- overlay(x=convI.class, y= fill_diff_class, fun = ifelse_fn  )
  
  # export
  writeRaster(flat_mask , filename = "./flat-btm_mask/tmp/flat-btm_mask_2m.tif", format = "GTiff", overwrite = T)
  
  
  # focal modal filter 
  wbt_majority_filter( i=  "./flat-btm_mask/tmp/flat-btm_mask_2m.tif", 
                       o= "./flat-btm_mask/flat-btm_mask_2m.tif",
                       filterx = 5,
                       filtery = 5
                       )
  
  # load in orig dem
  dem1m <- raster( dem1m.fp )

  # get original extent
  extent <- c(dem1m@extent@xmin, dem1m@extent@ymin, dem1m@extent@xmax, dem1m@extent@ymax)
  extent <- paste(extent[1],extent[2],extent[3],extent[4])

  # resample to 1m
  system(paste0("gdalwarp -r near -tr 1 1  -te ", extent ,"  -overwrite ./flat-btm_mask/tmp/flat-btm_mask_2m.tif  ./flat-btm_mask/tmp/flat-btm_mask_1m.tif " ) )

  # load in flat mask
  flat_mask_1m <- raster("./flat-btm_mask/tmp/flat-btm_mask_1m.tif")

  # mask by the 1m DEM
  flat_mask_1m <- mask(x= flat_mask_1m, mask = dem1m )

  # export
  writeRaster(flat_mask_1m  , filename = "./flat-btm_mask/flat-btm_mask_1m.tif", format = "GTiff", overwrite = T)

}
