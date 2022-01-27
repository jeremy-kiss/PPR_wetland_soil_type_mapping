## COVARIATE CALCULATIONS
## Author: Jeremy Kiss
## Date: 2022-01-25


#work directory 
wd <- "D:/sites/"
setwd(wd)

# site list 
# within the work directory, need a folder per site. Each site folder should have a dem directory, with the original dem in its own folder named by its spatial resolution
# create a list of the site names matching the folder names 
site.list <- c("STDE", "SWCU", "SMCR")

# list of target resolutions  
res.list <- c(2,5,10)

## SAGA COVAR____________________________________________________________

library(RSAGA)
library(raster)
# set connection between RStudio and SAGA 
work_env <- rsaga.env(path = "C:/Program Files (x86)/SAGA-GIS/")

# run loop per site and resolution
for(i in 1:length(site.list)){
  
  # get site name
  site <- site.list[i]
  
  # specify site wd 
  setwd(wd)
  site.wd <- paste0("./", site )
  setwd(site.wd)
  
  # create directory for covariates 
  dir.create("./covar")

  # loop to create SAGA covar for each resolution at each site 
  for(x in 1:length(res.list)){
    res <- res.list[x]
    
    setwd(wd)
    setwd(site.wd)
    # specify DEM to use, will change wd so as absolute path 
    dem.fp <- normalizePath(paste0("./dem/processed_dem/", site, "_dem_",res,"m.tif"))
    
    # create folder for each resolution 
    res.wd <- paste0("./covar/",res,"m" )
    dir.create(res.wd)
    
    # set wd to target res folder 
    setwd(res.wd)
    
    # create directories for specific output file types
    dir.create("SGRDS")
    dir.create("TIFFS")

    # dem sgrd path 
    dem.sgrd.fp <- "./SGRDS/dem.sgrd"

    # import DEM to .sgrd
    rsaga.geoprocessor("io_gdal", module = 0, env = work_env, 
                       param = list(
                         FILES= dem.fp,
                         GRIDS= dem.sgrd.fp ,
                         RESAMPLING= 0 ))
    
    # create inverse DEM
    rsaga.geoprocessor("grid_tools", module = 34, env = work_env, 
                       param = list(
                         GRID= dem.sgrd.fp ,
                         INVERSE= "SGRDS/inverse_dem.sgrd"))
    
    # slope, aspect, curvature
    
    # use Slope, Aspect, Curvature tool from SAGA on dem
    rsaga.slope.asp.curv(in.dem=dem.sgrd.fp, 
                         out.slope =  "SGRDS/slope.sgrd", 
                         out.aspect= "SGRDS/aspect.sgrd",
                         out.cprof =   "SGRDS/prof.sgrd", 
                         out.cplan =  "SGRDS/plan.sgrd", 
                         out.ctang = "SGRDS/tang_curve.sgrd",
                         method = "poly2zevenbergen", env=work_env)
    
    # fill gaps for aspect output 
    # use Close gaps with stepwise resampling **
    rsaga.geoprocessor("grid_tools", module = 29, env = work_env, 
                       param = list(
                         INPUT= "SGRDS/aspect.sgrd",
                         MASK = dem.sgrd.fp,
                         RESULT = "SGRDS/aspect.sgrd",
                         RESAMPLING= 3,
                         GROW= 1.2
                       ))
    
    # aspect is circular measurement - convert to northness
    # northness
    rsaga.grid.calculus(env=work_env, 
                        in.grids = "SGRDS/aspect.sgrd",
                        out.grid = "SGRDS/asp_northness.sgrd",
                        ~cos(a))

    # MFD_catch 
    # need to use rsaga.geoprocessor and identify the library and the module #
    rsaga.geoprocessor("garden_learn_to_program", module = 7, env = work_env, 
                       param = list(ELEVATION= dem.sgrd.fp,
                          AREA= "SGRDS/MFD_catch.sgrd",
                          METHOD= "MFD"))

    # SCA
    rsaga.geoprocessor("ta_hydrology", module = 19, env = work_env, 
                       param = list(
                         DEM=dem.sgrd.fp,
                         TCA= "SGRDS/MFD_catch.sgrd",
                         SCA= "SGRDS/SCA.sgrd",
                         METHOD=1))
    
    
    # use Close gaps with stepwise resampling ** Use grow factor of 1.4
    rsaga.geoprocessor("grid_tools", module = 29, env = work_env, 
                       param = list(
                         INPUT= "SGRDS/SCA.sgrd",
                         MASK = dem.sgrd.fp,
                         RESULT = "SGRDS/SCA.sgrd",
                         RESAMPLING= 3,
                         GROW= 1.2
                       ))
 
    # LS_factor
    rsaga.geoprocessor("ta_hydrology", module = 22, env = work_env, 
                       param = list(
                         SLOPE = "SGRDS/slope.sgrd",
                         AREA= "SGRDS/SCA.sgrd",
                         LS= "SGRDS/LS_factor.sgrd",
                         CONV=0,
                         METHOD=0,
                         STABILITY=0))
    
    # use Close gaps with stepwise resampling ** Use grow factor of 1.4
    rsaga.geoprocessor("grid_tools", module = 29, env = work_env, 
                       param = list(
                         INPUT= "SGRDS/LS_factor.sgrd",
                         MASK = dem.sgrd.fp,
                         RESULT = "SGRDS/LS_factor.sgrd",
                         RESAMPLING= 3,
                         GROW= 1.2
                       ))
  
    # SWI
    rsaga.geoprocessor("ta_hydrology", module = 15, env = work_env, 
                       param = list(
                         DEM=dem.sgrd.fp,
                         TWI= "SGRDS/SWI.sgrd",
                         AREA_TYPE=2,
                         SLOPE_TYPE=1,
                         SLOPE_WEIGHT=1
                       ))
    
    # Relative Heights and Slope Positions
    # running at 2 different "neighbourhoods", t10 and t1000 - which is better suited to SK landscapes 
    rsaga.geoprocessor("ta_morphometry", module = 14, env = work_env, 
                       param = list(
                         DEM=dem.sgrd.fp,
                         NH= "SGRDS/Norm_Height_t10.sgrd",
                         T=10
                       ))
    
    rsaga.geoprocessor("ta_morphometry", module = 14, env = work_env, 
                       param = list(
                         DEM=dem.sgrd.fp,
                         HO= "SGRDS/Slope_Height_t1000.sgrd",
                         HU= "SGRDS/Valley_Depth_t1000.sgrd",
                         NH= "SGRDS/Norm_Height_t1000.sgrd",
                         MS= "SGRDS/MidSlope_Pos_t1000.sgrd",
                         T=1000
                       ))
    
    # SDA
    # Inverse MFD_catch
    rsaga.geoprocessor("garden_learn_to_program", module = 7, env = work_env, 
                       param = list(ELEVATION= "SGRDS/inverse_dem.sgrd",
                          AREA=      "SGRDS/inverse_MFD_catch.sgrd",
                          METHOD= "MFD"))
    
    # Inverse Flow width and SDA
    rsaga.geoprocessor("ta_hydrology", module = 19, env = work_env, 
                       param = list(
                         DEM=dem.sgrd.fp,
                         TCA= "SGRDS/inverse_MFD_catch.sgrd",
                         SCA= "SGRDS/SDA.sgrd",
                         METHOD=1))
  
    # Use Close gaps with stepwise resampling ** Use grow factor of 1.4
    rsaga.geoprocessor("grid_tools", module = 29, env = work_env, 
                       param = list(
                         INPUT= "SGRDS/SDA.sgrd",
                         MASK = dem.sgrd.fp,
                         RESULT = "SGRDS/SDA.sgrd",
                         RESAMPLING= 3,
                         GROW= 1.2
                       ))
  
    # Terrain Ruggedness Index
    rsaga.geoprocessor("ta_morphometry", module = 16, env = work_env, 
                       param = list(
                         DEM=dem.sgrd.fp,
                         TRI= "SGRDS/TRI.sgrd",
                         DW_WEIGHTING=1
                       ))
    
    # MRVBF MRRTF 
    # Determine input for slope Threshold. 
    t_slope <- 116.57*res^-0.62
    
    #run
    rsaga.geoprocessor("ta_morphometry", module = 8, env = work_env, 
                       param = list(
                         DEM= dem.sgrd.fp,
                         MRVBF=  "SGRDS/MRVBF.sgrd",
                         MRRTF=  "SGRDS/MRRTF.sgrd",
                         T_SLOPE= t_slope,
                         MAX_RES=100
                       ))
  
    #  MRVBF and MRRTF need to be masked with the dem 
    # Mask MRVBF
    rsaga.geoprocessor("grid_tools", module = 24, env = work_env, 
                       param = list(
                         GRID= "SGRDS/MRVBF.sgrd",
                         MASK= dem.sgrd.fp,
                         MASKED= "SGRDS/MRVBF.sgrd"
                       ))
    # Mask MRRTF
    rsaga.geoprocessor("grid_tools", module = 24, env = work_env, 
                       param = list(
                         GRID= "SGRDS/MRRTF.sgrd",
                         MASK= dem.sgrd.fp,
                         MASKED= "SGRDS/MRRTF.sgrd"
                       ))
    
    # delete any SGRDs that are not to be used as covariates
    unlink("SGRDS/aspect.sdat")
    unlink("SGRDS/inverse_dem.sdat")
    unlink( "SGRDS/inverse_MFD_catch.sdat")

    # Open all .sgrd in SAGA and make sure they have the correct amount of NoData cells 
    
    # CONVERT ALL SGRDS TO TIFF  
    # CREATE A RASTER STACK OF THE COVARIATES WE JUST CREATED
    files <- list.files(path="SGRDS", pattern="*.sdat$", full.names=T, recursive=FALSE)
    sgrd_stack <- stack(files)
    sgrd_stack
    names(sgrd_stack)
    projection(sgrd_stack)
    
    # WRITE THE FILES IN THE RASTER STACK AS GEOTIFFS AND SAVE THEM TO THE TIFF FOLDER
    outName = paste("TIFFS/", names(sgrd_stack), '.tif', sep = "")
    outName
    writeRaster(sgrd_stack, filename = outName, format="GTiff", overwrite=TRUE, bylayer=TRUE)
    
    # SINCE RSAGA SAVES THE SGRDS TO THE COMPUTER, WE CAN NOW DELETE THEM SINCE THE TIFFS ARE CREATED
    unlink('SGRDS',recursive = TRUE)
   
  }
  setwd(wd)
}






# STUDY-SPECIFIC CUSTOM DERIVATIVES ____________________________________________________________

# Depression focused terrain derivatives 

# install whitebox - Uncomment if necessary 
# library(devtools)
# devtools::install_github("giswqs/whiteboxR")
# whitebox::wbt_init()

#load libraries
library(raster)
library(rgdal)
library(dplyr)
library(spdplyr)
library(fasterize)
library(stars)
library(exactextractr)


# Minimum depression depth size to remove in meters 
# 0.1 recommended 
min_dep_depth <- 0.1

# run loop per site and resolution
for(i in 1:length(site.list)){
  
  # get site name
  site <- site.list[i]
  
  # specify site wd 
  setwd(wd)
  site.wd <- paste0("./", site )
  setwd(site.wd)
  

  # loop to create SAGA covar for each resolution at each site 
  for(x in 1:length(res.list)){
    res <- res.list[x]
    
    setwd(wd)
    setwd(site.wd)
    # specify DEM to use, will change wd so as absolute path 
    dem.filepath <- normalizePath(paste0("./dem/processed_dem/", site, "_dem_",res,"m.tif"))
    
    
    # specify folder for each resolution 
    res.wd <- paste0("./covar/",res,"m" )

    # set wd to target res folder 
    setwd(res.wd)
    
    # create directories for specific output file types
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
    
    
    ###
    ### Rasterize wetland Strahler order 
    ###
    # these were generated following the methods described in  Kiss et al. 2021 paper DOI: 10.1007/s13157-021-01436-3
    # using min and max connectivity threshold levels 
    
    setwd(wd)
    setwd(site.wd)
    
    # load in wetland strahler order polygon
    w_strah_poly <- st_read(paste0("./wetland_strahler/",site,"_wetland_strahler_MaxPoly_2m.shp" ) )
   
    # convert to raster 
    # using the strahler order generated for the maximum depression outlines and using the minimum level of spill channel connectivity 
    # any location outside of the depression polygons is given a value of 0 
    w_strah_rst_minC <- fasterize(sf = w_strah_poly, raster = dem, field = "ArcOrd_50", background =0  )
    
    # 0 is added to no data areas, need to be masked 
    w_strah_rst_minC <- mask(x= w_strah_rst_minC, mask=dem)
   
    # export 
    setwd(res.wd)
    writeRaster(w_strah_rst_minC , 
                filename = paste0("./final/wetland_Strahler_minC.tif" ), 
                format = "GTiff",
                overwrite = T)
    
    # convert to raster 
    # using the strahler order generated for the maximum depression outlines and using the minimum level of spill channel connectivity 
    # any location outside of the depression polygons is given a value of 0 
    w_strah_rst_maxC <- fasterize(sf = w_strah_poly, raster = dem, field = "ArcOrd_fll", background =0  )
    
    # 0 is added to no data areas, need to be masked 
    w_strah_rst_maxC <- mask(x= w_strah_rst_maxC, mask=dem)
    
    # export 
    writeRaster(w_strah_rst_maxC , 
                filename = paste0("./final/wetland_Strahler_maxC.tif" ), 
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
      
      if ( raster.path != "./final/dep_depth.tif" & raster.path != "./final/wshed_max_catch.tif" & raster.path != "wetland_Strahler_minC.tif"  & raster.path != "wetland_Strahler_maxC.tif" ){
        
        #mask along buffer
        buffer <- mask(covar_raster, mask = wshed_buffer_rst  )
        
        #smooth along buffer 
        buffer_smth <- focal(buffer, w=matrix(1,3,3), fun = mean, na.rm = T   )
        
        #replace buffer area with smoothed 
        raster_smth <- overlay (x = buffer_smth , y = covar_raster, fun = replace_w_smth_fn  )
        
        covar_raster <- raster_smth
        
      }
      # end SMOOTH watershed EDGES 
      
      
      #count NoData for covar raster
      raster_NoData <- sum(is.na(values(covar_raster)))
      
      #check if the number of no data values matches the dem raster. if it does not, then it will fill the raster. It will doe so with a small smoothing window first, then a large if it is still required. Mask after filling. NAonly means hat only NoData are replaced with computed focal values 
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
    
    

    
    #copy files from final to TIFFS
    file.copy(from = final.list, to = "./TIFFS",  overwrite = T) 
    
    
    ## delete working folder 
    unlink('./working',recursive = TRUE)
    unlink('./final',recursive = TRUE)
    
  
  }
setwd(wd)
  }




### Euclidean distance - Script written by Daniel Saurette based on Behrens et al. 2018 ____________________________________________
library(raster)
library(rgdal)

# function:
# this commits the function to the Global Environments
# the required input is a RasterLayer
# expand line 13 to see the code
EDF<- function(d){
  
  # first we set up window and plot d
  par(mfrow=c(2,4), mar=c(0.2,0.2,1.5,0.2), oma=c(0.2,0.2,2,2))  
  plot(d,main = "Source", legend=FALSE,axes=FALSE)
  
  # next convert the raster to a dataframe for calculating the XDIST and YDIST
  d2<- as(d,"SpatialPointsDataFrame")
  d2<- as.data.frame(d2@coords)
  d2$xdist<- d@extent@xmax - d2$x
  d2$ydist<- d@extent@ymax - d2$y

  # now we need to generate vectors representing the 4 corners and the center of the raster
  nw  <- c(d@extent@xmin,d@extent@ymax)
  ne  <- c(d@extent@xmax,d@extent@ymax)

  # generate distance to NW corner grid
  NW<- distanceFromPoints(d,nw)
  NW<- mask(x = NW, mask = d)
  projection(NW)<-crs(d)
  plot(NW, main='Dist NW',legend=FALSE,axes=FALSE)
  writeRaster(NW,"DIST_NW.tif",overwrite = TRUE)
  print('DISTANCE FROM NW GRID COMPLETE')
  
  # generate distance to NE corner grid
  NE<- distanceFromPoints(d,ne)
  NE<- mask(x = NE, mask = d)
  projection(NE)<-crs(d)
  plot(NE, main='Dist from NE',legend=FALSE,axes=FALSE)
  writeRaster(NE,"DIST_NE.tif",overwrite = TRUE)
  print('DISTANCE FROM NE GRID COMPLETE')
  
}



# run loop per site and resolution
for(i in 1:length(site.list)){
  
  # get site name
  site <- site.list[i]
  
  # specify site wd 
  setwd(wd)
  site.wd <- paste0("./", site )
  setwd(site.wd)
  
  # loop to create SAGA covar for each resolution at each site 
  for(x in 1:length(res.list)){
    res <- res.list[x]
    
    setwd(wd)
    setwd(site.wd)
    
    # get dem path 
    dem.filepath <- normalizePath(paste0("./dem/processed_dem/", site, "_dem_",res,"m.tif"))
    
    # specify folder for each resolution 
    res.wd <- paste0("./covar/",res,"m" )
    
    # set wd to target res folder
    setwd(res.wd)
    
    # load a raster for a study area. Does not matter which one, but I usually add the DEM
    d<- raster(dem.filepath)
    
    # set wd
    setwd("./TIFFS")
    
    # and now we run the function with 6 characters!!
    EDF(d)
    
  }

}





# WHITEBOX DERIVATIVES ____________________________________________________________

## first determining the scales at which to calculate the MaxElevationDeviation
# created a shapefile for each site of points in key areas - along hillslopes - bottom, mid, top. Spanning different areas of each site 
# only using 2m DEM for determining scale windows 

# STDE
whitebox::wbt_max_elev_dev_signature(dem= "D:/GIS/LWSBF/RDU_3.0_2021/sites/STDE/dem/processed_dem/STDE_dem_2m.tif",
                                     points = "D:/GIS/LWSBF/RDU_3.0_2021/sites/STDE/wb_MaxElevDevSig/sig_test_pts.shp",
                                     output= "D:/GIS/LWSBF/RDU_3.0_2021/sites/STDE/wb_MaxElevDevSig/STDE_sig.html",
                                     min_scale = 10,
                                     max_scale = 200,
                                     step = 10)

# scale windows
# local = 70 cells
# meso = 300 cells
# broad = 5000 cells


# SWCU
whitebox::wbt_max_elev_dev_signature(dem= "D:/GIS/LWSBF/RDU_3.0_2021/sites/SWCU/dem/processed_dem/SWCU_dem_2m.tif",
                                     points = "D:/GIS/LWSBF/RDU_3.0_2021/sites/SWCU/wb_MaxElevDevSig/sig_test_pts.shp",
                                     output= "D:/GIS/LWSBF/RDU_3.0_2021/sites/SWCU/wb_MaxElevDevSig/SWCU_sig.html",
                                     min_scale = 10,
                                     max_scale = 1000,
                                     step = 10)

# scale windows
# local = 100 cells 
# meso = 600 cells 
# broad = 5000 cells 


# SMCR
whitebox::wbt_max_elev_dev_signature(dem= "D:/GIS/LWSBF/RDU_3.0_2021/sites/SMCR/dem/processed_dem/SMCR_dem_2m.tif",
                                     points = "D:/GIS/LWSBF/RDU_3.0_2021/sites/SMCR/wb_MaxElevDevSig/sig_test_pts.shp",
                                     output= "D:/GIS/LWSBF/RDU_3.0_2021/sites/SMCR/wb_MaxElevDevSig/SMCR_sig.html",
                                     min_scale = 10,
                                     max_scale = 500,
                                     step = 10)

# scale windows
#local = 100 cells 
# meso = 400 cells
# broad = 5000 cells 


## FINAL windows 
# local - 10 -50 cells, 20 - 100m 
# meso - 51 - 400 cells, 102m - 800m
# broad 401 - 2000 cells, 802- 4000m




# run loop per site and resolution
for(i in 1:length(site.list)){
  
  # get site name
  site <- site.list[i]
  
  # specify site wd 
  setwd(wd)
  site.wd <- paste0("./", site )
  setwd(site.wd)
  
  #loop to create SAGA covar for each resolution at each site 
  for(x in 1:length(res.list)){
    res <- res.list[x]
    
    setwd(wd)
    setwd(site.wd)
    
    # get dem path 
    dem.filepath <- normalizePath(paste0("./dem/processed_dem/", site, "_dem_",res,"m.tif"))
    
    # specify folder for each resolution 
    res.wd <- paste0("./covar/",res,"m" )
    
    # set wd to target res folder
    setwd(res.wd)
    
    
    # Stochastic depression analysis
    # RMSE was set based on the highest RMSE of all 3 sites, St. Denis 0.12m 
    # don't know distance over which individual error values are correlated- need DEM error semivariogram, using 3 times the resolution as is recommended in the tutorial video https://www.youtube.com/watch?v=pg8puYYbSzU in order to reduce run time 
    # LONG RUN TIME >10 min 
    whitebox::wbt_stochastic_depression_analysis(dem = dem.filepath, output="./TIFFS/stoch_dep_ana.tif",
                                                 rmse= 0.14,
                                                 range = res*3,
                                                 iterations = 300)
    
    
    dir.create("./maxElevDev" )
    
    
    # maxElevDev local
    whitebox::wbt_max_elevation_deviation(dem = dem.filepath, 
                                          out_mag = "./maxElevDev/maxElevDev_local.tif",
                                          out_scale = "./maxElevDev/maxElevDev_local_scale.tif",
                                          min_scale = 20/res,
                                          max_scale = 100/res ,
                                          step = 5)
    
    
    # maxElevDev meso
    whitebox::wbt_max_elevation_deviation(dem = dem.filepath, 
                                          out_mag = "./maxElevDev/maxElevDev_meso.tif",
                                          out_scale = "./maxElevDev/maxElevDev_meso_scale.tif",
                                          min_scale = 100/res,
                                          max_scale = 800/res ,
                                          step = 5)
    
    # maxElevDev broad
    whitebox::wbt_max_elevation_deviation(dem = dem.filepath, 
                                          out_mag = "./maxElevDev/maxElevDev_broad.tif",
                                          out_scale = "./maxElevDev/maxElevDev_broad_scale.tif",
                                          min_scale = 800/res,
                                          max_scale = 2000/res ,
                                          step = 5)
    
    
    MED.list <- (c("./maxElevDev/maxElevDev_local.tif","./maxElevDev/maxElevDev_meso.tif","./maxElevDev/maxElevDev_broad.tif") )
    
    # copy needed rasters to TIFFS 
    file.copy(
      from= MED.list,
      to= "./TIFFS",
      overwrite = T,
      copy.mode = T)

  
    }
}



## check that each site and resolution has all covariates calculated 

# run loop per site and resolution
for(i in 1:length(site.list)){
  
  # get site name
  site <- site.list[i]
  
  # specify site wd 
  setwd(wd)
  site.wd <- paste0("./", site )
  setwd(site.wd)
  
  
  # loop to create SAGA covar for each resolution at each site 
  
  for(x in 1:length(res.list)){
    res <- res.list[x]
    
    cov.path <- paste0("./covar/", res, "m/TIFFS/")

    files <- list.files(path = cov.path, pattern = "\\.tif$",
               full.names = TRUE)
    
    # check if each folder has 32 covar
    print(32 == length(files))
    
    # should print all TRUE 
    
  }}

