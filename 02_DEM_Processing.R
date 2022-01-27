## DEM PROCESSING 
## Author: Jeremy Kiss
## Date: 2022-01-25

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

# first, the 1m DEM is processed so that water surfaces are re-interpolated
# for each site
for(i in 1:length(site.list)){
  
  #get site name.
  site <- site.list[i]
  
  # specify site wd 
  setwd(wd)
  site.wd <- paste0("./", site )
  setwd(site.wd)

  # path to 1m DEM 
  dem1m.fp <- normalizePath(paste0("./dem/1m/", site, "_dem_1m.tif") )
  
  # load in 1m dem 
  dem1m <- raster(dem1m.fp)
 
  # load in 1m water surface mask 
  flat_mask <- raster("./flat-btm_mask/flat-btm_mask_1m.tif")
  
  # create dem without bottoms  
  dem_no_btm <- mask( x = dem1m, mask= flat_mask, maskvalue = 1)
  
  # export 
  writeRaster(dem_no_btm , filename = "./dem/working_vers/dem_no_btm.tif", format = "GTiff", overwrite = T )
  
  # create dem of only bottoms 
  btm_dem <- mask( x = dem1m, mask= flat_mask, maskvalue = 0)
  
  # xport 
  writeRaster(btm_dem, filename = "./dem/working_vers/btm_dem.tif", format = "GTiff", overwrite = T )
  
  # resample dem_no_btm and btm_dem to 5m 
  system("gdalwarp -tr 5 5 -r bilinear -overwrite ./dem/working_vers/dem_no_btm.tif ./dem/working_vers/dem_no_btm_5m.tif"   )
  system("gdalwarp -tr 5 5 -r bilinear  -overwrite ./dem/working_vers/btm_dem.tif ./dem/working_vers/btm_dem_5m.tif"   )  

  
  # import DEM to .sgrd
  rsaga.geoprocessor("io_gdal", module = 0, env = work_env, 
                     param = list(
                       FILES= "./dem/working_vers/dem_no_btm_5m.tif" ,
                       GRIDS= "./dem/working_vers/dem_no_btm_5m_spline.sgrd" ,
                       RESAMPLING= 0 
                       ))
  
  # close gaps with spline 
  rsaga.geoprocessor("grid_tools", module = 25, env = work_env,
                     param = list ( 
                       GRID="./dem/working_vers/dem_no_btm_5m_spline.sgrd", 
                       MAXPOINTS = 1000,
                       LOCALPOINTS= 50,
                       EXTENDED=1
                       ))
                     
  
  # export as tif 
  rsaga.geoprocessor("io_gdal", module = 2, env = work_env, 
                     param = list(
                      GRIDS=  "./dem/working_vers/dem_no_btm_5m_spline.sgrd" ,
                      FILE = "./dem/working_vers/5m_spline.tif"  
                      ))
  
  # get extent from 1m DEM 
  extent <- c(dem1m@extent@xmin, dem1m@extent@ymin, dem1m@extent@xmax, dem1m@extent@ymax)
  extent <- paste(extent[1],extent[2],extent[3],extent[4])
  
  # resample spline to 1m 
  system(paste0("gdalwarp -tr 1 1 -r near -te ", extent ,"  -overwrite  ./dem/working_vers/5m_spline.tif ./dem/working_vers/1m_spline.tif"))
  
  # smooth with 9x9 mean filter 
  whitebox::wbt_mean_filter(
    i = "./dem/working_vers/1m_spline.tif",
    output = "./dem/working_vers/1m_spline_sm.tif",
    filterx = 9,
    filtery=9)
  
  # load smoothed spline raster 
  spline_1m <- raster("./dem/working_vers/1m_spline_sm.tif")
  
  # mask spline to match original dem 
  spline_1m <- mask(x= spline_1m, mask = dem1m)
  
  # fill missing bottoms with splined version 
  dem1m_btm_filled <- merge(dem_no_btm , spline_1m, overlap = T , tolerance = 0.1)
  
  # export
  writeRaster(dem1m_btm_filled  , filename = "./dem/1m/dem1m_btm_filled.tif", format = "GTiff", overwrite = T)
    
}    


# Resample DEM using GDAL 
# DEM to be used has had water surface pond bottoms removed using 01_Water-surfac_mask.R script
site.list <- c("STDE", "SWCU", "SMCR")
 
# list of target resolutions  
res.list <- c(2,5,10)

# loop to resample to target resolutions.
for(i in 1:length(site.list)){
  
  site <- site.list[i]
  setwd(wd)
  site.wd <- paste0("./", site , "/dem/")
  setwd(site.wd)
  
  # create folder for processed DEM   
  dir.create(paste0("./processed_dem" ))
  
  # loop for each resolution 
      for(x in 1:length(res.list)){
        res <- res.list[x]
        
        
        # resample to target resolution 
        system( paste0("gdalwarp -tr ", res, " ", res, " -r bilinear  -overwrite ./1m/dem1m_btm_filled.tif ./working_vers/dem_",res,"m_res.tif" ))
        
        # SMOOTHING AND PIT FILLING #
        # Smooth using Feature Preserving Smoothing whitebox tool 
        wbt_feature_preserving_smoothing( 
          dem = paste0("./working_vers/dem_",res,"m_res.tif") ,
          output = paste0("./working_vers/dem_",res,"m_fps_3iter.tif"),
          filter=11,
          norm_diff = 15.0,
          num_iter = 3) 
        
        # 3x3 simple mean filter 
        wbt_mean_filter(
          i = paste0("./working_vers/dem_",res,"m_fps_3iter.tif"),
          output = paste0("./working_vers/dem_",res,"m_smooth.tif"),
          filterx = 3,
          filtery=3)
        
        # Fill single cell pits 
        wbt_fill_single_cell_pits( 
          dem = paste0("./working_vers/dem_",res,"m_smooth.tif"),
          output =  paste0("./processed_dem/", site, "_dem_",res,"m.tif"))
        
        }
  }
