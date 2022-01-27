## EXTRACT COVARIATE VALUES TO POINTS
## Author: Jeremy Kiss
## Date: 2022-01-25

# load libraries 
library(raster)
library(dplyr)
library(spdplyr)
library(rgdal)
library(sf)

#load work directory 
wd <- "D:/wd/"
sites.wd <- "D:/wd/sites/"
setwd(sites.wd)


# Load in points shapefiles 

# Training data 
STDE_pts <- shapefile("./STDE/soil_pt_data/STDE_LWBSF_pts.shp")
SWCU_pts <- shapefile("./SWCU/soil_pt_data/SWCU_LWBSF_pts.shp")
SMCR_pts <- shapefile("./SMCR/soil_pt_data/SMCR_LWBSF_pts.shp")
STDE_center_pts <- shapefile("./STDE/soil_pt_data/wetland_centers_2000.shp")

# External testing data 
STDE_BIOCAP_pts <- shapefile("./STDE/soil_pt_data/STDE_BIOCAP_pts.shp")
SMCR_Brown_pts <- shapefile("./SMCR/soil_pt_data/SMCR_BrownDrainage_pts.shp")

# separate drained from undrained SMCR_Brown_pts
SMCR_Brown_drnd_pts <- SMCR_Brown_pts %>% filter(Drained =="Y" )
SMCR_Brown_undrnd_pts <- SMCR_Brown_pts %>% filter(Drained =="N" )

# clean data 
# rename ID to CoreCode
STDE_center_pts <- STDE_center_pts %>% rename(CoreCode = wetland_nu)
# select only code and RDU class columns 
STDE_pts <- STDE_pts %>% select(CoreCode, RDU)
SWCU_pts <- SWCU_pts %>% select(CoreCode, RDU)
SMCR_pts <- SMCR_pts %>% select(CoreCode, RDU)
STDE_BIOCAP_pts <- STDE_BIOCAP_pts %>% select(CoreCode, RDU)
SMCR_Brown_drnd_pts <- SMCR_Brown_drnd_pts %>% select(CoreCode, RDU)
SMCR_Brown_undrnd_pts <- SMCR_Brown_undrnd_pts %>% select(CoreCode, RDU)
STDE_center_pts <- STDE_center_pts %>%  select(CoreCode, RDU)

# create col to differentiate datasets
STDE_pts$set <- "STDE_LWBSF"
SWCU_pts$set <- "SWCU_LWBSF"
SMCR_pts$set <- "SMCR_LWBSF"
STDE_center_pts$set <- "STDE_CENTER"
STDE_BIOCAP_pts$set <- "STDE_BIOCAP"
SMCR_Brown_drnd_pts$set <- "SMCR_BROWN_DRAINED"
SMCR_Brown_undrnd_pts$set <- "SMCR_BROWN_UNDRAINED"

# create col to differentiate sites 
STDE_pts$site <- "STDE"
SWCU_pts$site <- "SWCU"
SMCR_pts$site <- "SMCR"
STDE_center_pts$site <- "STDE"
STDE_BIOCAP_pts$site <- "STDE"
SMCR_Brown_drnd_pts$site <- "SMCR"
SMCR_Brown_undrnd_pts$site <- "SMCR"

# merge to single sp df of points 
pts <- rbind(STDE_pts, SWCU_pts, SMCR_pts, STDE_center_pts, STDE_BIOCAP_pts, SMCR_Brown_drnd_pts, SMCR_Brown_undrnd_pts)

#reoder col
pts <- pts %>% select(CoreCode, set, site, RDU)


# Prepare classes 
# existing soils clases 
unique(pts$RDU)

# remove all D/U transitions classes - only 5 cases. 4 from BIOCAP, 1 from Brown
which(pts$RDU == "D/U")
pts <- pts %>% filter(RDU != "D/U" )

# classify soils to RDU to 3 class system - Upland, Calcareous Wetland, and Non-Calcareous Wetland 
# R = Recharge
# D = Discharge
# U = Upland
# D/U = could not be distinguished between discharge or upland
# R/D = transition between recharge and discharge soil. No carbonates in surface horizon so classified as non-calcareous
pts$class3 <- pts$RDU
pts <- pts %>%  mutate(class3 =  recode(class3, "R" = "NCW", "D" = "CW", "R/D" = "NCW", "U" = "U"))

# points grouped per site 
all_STDE_pts <- pts %>% filter(site == "STDE" )
all_SWCU_pts <- pts %>% filter(site == "SWCU")
all_SMCR_pts <- pts %>% filter(site == "SMCR")

# write each site data to disk 
writeOGR(all_STDE_pts, layer= "all_STDE_pts", dsn = "./STDE/soil_pt_data/all_STDE_pts.shp" , driver = "ESRI Shapefile", overwrite_layer = T)
writeOGR(all_SWCU_pts, layer= "all_SWCU_pts", dsn = "./SWCU/soil_pt_data/all_SWCU_pts.shp" , driver = "ESRI Shapefile", overwrite_layer = T)
writeOGR(all_SMCR_pts, layer= "all_SMCR_pts", dsn = "./SMCR/soil_pt_data/all_SMCR_pts.shp" , driver = "ESRI Shapefile", overwrite_layer = T)

# Extract covariate data to points 
site.list <- c("STDE", "SWCU", "SMCR")
res.list <- c(2,5,10)

# loop through each site and resolution 
for(i in 1:length(site.list)){
  # get site name
  site <- site.list[i]
  
  # specify site wd 
  setwd(sites.wd)
  site.wd <- paste0("./", site )
  setwd(site.wd)
  
  # create directory for pts w/ covariate data  
  dir.create("./soil_pt_data/pt_w_covar/")
  
  # loop to create SAGA covar for each resolution at each site 
  for(x in 1:length(res.list)){
    
    res <- res.list[x]
    
    # specify location of covariates for target site and resolution 
    cov.path <- paste0("./covar/",res,"m/TIFFS" )
    list.files(path = cov.path, pattern = "\\.tif$",
               full.names = TRUE)
    
    #Create raster stack without loading any into memory 
    files <- list.files(path = cov.path,
                        pattern = "\\.tif$", full.names = TRUE)
    
    # stack rasters
    Cov.Stack <- raster(files[1])
    
    for (i in 2:length(files)) {
      Cov.Stack <- stack(Cov.Stack, files[i])
    }
    
    Cov.Stack
    
    #add coordinate ref. 
    crs(Cov.Stack) <- "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # load in pts from memory 
    pts <- shapefile(paste0("./soil_pt_data/all_", site, "_pts.shp" ))
      
    # Extract raster stack values to points 
    pts.cov <- extract(Cov.Stack, pts , sp = 1,  method = "simple")
 
    # Write points with covar to disk 
    # geopackages allow for longer field names 
    writeOGR(pts.cov, layer= "pts_cov", dsn = paste0("./soil_pt_data/pt_w_covar/pt_w_covar_",res,"m.gpkg" ) , 
             driver = "GPKG", overwrite_layer = T)
    
    # write raster stack to disk
    stack.folder  <- paste0( "./covar/",res,"m/cov_stack/")
    dir.create(stack.folder )
    stackSave(Cov.Stack, filename = paste0(stack.folder, "stack.tif" ) )
    
  }
}




### splitting points out for the various datasets for model training and testing 

setwd(wd)

# create directory for each model 
dir.create("./models")


## STDE points
# loop through per resolution 

# create directory for each model
dir.create("./models/STDE_w_centers" )
dir.create("./models/STDE_no_centers")

# list of resolutions to loop through 
res.list <- c(2,5,10)
for(x in 1:length(res.list)){
  
  res <- res.list[x]
  
  # load all STDE points 
  all.STDE.pts <- st_read(paste0("./sites/STDE/soil_pt_data/pt_w_covar/pt_w_covar_" ,res,"m.gpkg" ))
  
  # subset training and testing points based on the training dataset 
  train_w_centers <- all.STDE.pts %>% filter(set == "STDE_LWBSF" | set == "STDE_CENTER"  )
  train_no_centers <- all.STDE.pts %>% filter(set == "STDE_LWBSF" )
  test <- all.STDE.pts %>% filter(set == "STDE_BIOCAP")
  
  # Export to model folders 
  dir.create(paste0("./models/STDE_w_centers/",res,"m" ) )
  dir.create(paste0("./models/STDE_no_centers/",res,"m" ) )
  
  # write points 
  # w centers 
  # train 
  st_write(train_w_centers, layer= "train", dsn = paste0("./models/STDE_w_centers/",res,"m/train.gpkg" ) , 
           driver = "GPKG", append = T)
  
  # test  
  st_write(test, layer= "test", dsn = paste0("./models/STDE_w_centers/",res,"m/test_1.gpkg" ) , 
           driver = "GPKG", append = T)
  
  # no centers 
  # train 
  st_write(train_no_centers, layer= "train", dsn = paste0("./models/STDE_no_centers/",res,"m/train.gpkg" ) , 
           driver = "GPKG", append = T)
  # test  
  st_write(test, layer= "test_1", dsn = paste0("./models/STDE_no_centers/",res,"m/test_1.gpkg" ) , 
           driver = "GPKG", append = T)
  
}




## SWCU points
# loop through per resolution 

dir.create("./models/SWCU" )

res.list <- c(2,5,10)
for(x in 1:length(res.list)){

  res <- res.list[x]
  
  # load all SWCU points 
  all.SWCU.pts <- st_read(paste0("./sites/SWCU/soil_pt_data/pt_w_covar/pt_w_covar_" ,res,"m.gpkg" ))

  
  # subset training and testing points based on the training dataset 
  train <- all.SWCU.pts %>% filter(set == "SWCU_LWBSF"   )
 
  # Export to model folders 
  dir.create(paste0("./models/SWCU/",res,"m" ) )

  # write points 
  # train 
  st_write(train, layer= "train", dsn = paste0("./models/SWCU/",res,"m/train.gpkg" ) , 
           driver = "GPKG", append = T)

}



## SMCR points
# loop through per resolution 

dir.create("./models/SMCR" )

res.list <- c(2,5,10)
for(x in 1:length(res.list)){
 
  res <- res.list[x]
  
  # load all SMCR points 
  all.SMCR.pts <- st_read(paste0("./sites/SMCR/soil_pt_data/pt_w_covar/pt_w_covar_" ,res,"m.gpkg" ))
  
  # subset training and testing points based on the training dataset 
  train <- all.SMCR.pts %>% filter(set == "SMCR_LWBSF"  )
  test_drained <- all.SMCR.pts %>% filter(set == "SMCR_BROWN_DRAINED")
  test_undrained <- all.SMCR.pts %>% filter(set == "SMCR_BROWN_UNDRAINED")
  
  # Export to model folders 
  dir.create(paste0("./models/SMCR/",res,"m" ) )

  # write points 
  # w centers 
  # train 
  st_write(train, layer= "train", dsn = paste0("./models/SMCR/",res,"m/train.gpkg" ) , 
           driver = "GPKG", append = T)
  
  #test  
  st_write(test_undrained, layer= "test_1", dsn = paste0("./models/SMCR/",res,"m/test_1.gpkg" ) , 
           driver = "GPKG", append = T)
  
  #test  
  st_write(test_drained, layer= "test_2", dsn = paste0("./models/SMCR/",res,"m/test_2.gpkg" ) , 
           driver = "GPKG", append = T)

}



## ALL site set 

# loop through per resolution 

dir.create("./models/All_sites_w_centers" )
dir.create("./models/All_sites_no_centers")


res.list <- c(2,5,10)
for(x in 1:length(res.list)){
  
  res <- res.list[x]
  
  #load all  points 
  all.STDE.pts <- st_read(paste0("./sites/STDE/soil_pt_data/pt_w_covar/pt_w_covar_" ,res,"m.gpkg" ))
  all.SWCU.pts <- st_read(paste0("./sites/SWCU/soil_pt_data/pt_w_covar/pt_w_covar_" ,res,"m.gpkg" ))
  all.SMCR.pts <- st_read(paste0("./sites/SMCR/soil_pt_data/pt_w_covar/pt_w_covar_" ,res,"m.gpkg" ))
  
  # combine  
  all.pts <- rbind(all.STDE.pts, all.SWCU.pts, all.SMCR.pts)
  
  # subset training and testing points based on the training dataset 
  train_w_centers <- all.pts %>% filter(set == "STDE_LWBSF" | 
                                               set == "STDE_CENTER" |
                                               set == "SWCU_LWBSF" |
                                               set == "SMCR_LWBSF")
                                               
  train_no_centers <- all.pts %>% filter(set == "STDE_LWBSF" | 
                                                set == "SWCU_LWBSF" |
                                                set == "SMCR_LWBSF" )
  
  test_BIOCAP <- all.pts %>% filter(set == "STDE_BIOCAP")
  
  test_Brown_undrained <- all.pts %>% filter(set == "SMCR_BROWN_UNDRAINED")
  
  test_Brown_drained <- all.pts %>% filter(set == "SMCR_BROWN_DRAINED")
  
  
  # Export to model folders 
  dir.create(paste0("./models/All_sites_w_centers/",res,"m" ) )
  dir.create(paste0("./models/All_sites_no_centers/",res,"m" ) )
  
  # write points to multiple folders 
  # w centers 
  # train 
  st_write(train_w_centers, layer= "train", dsn = paste0("./models/All_sites_w_centers/",res,"m/train.gpkg" ) , 
           driver = "GPKG", append = T)
  
  # test 1   
  st_write( test_BIOCAP, layer= "test_1", dsn = paste0("./models/All_sites_w_centers/",res,"m/test_1.gpkg" ) , 
           driver = "GPKG", append = T)
  
  
  # test 2   
  st_write(test_Brown_undrained, layer= "test_2", dsn = paste0("./models/All_sites_w_centers/",res,"m/test_2.gpkg" ) , 
           driver = "GPKG", append = T)
  
  # test 3  
  st_write(test_Brown_drained, layer= "test_3", dsn = paste0("./models/All_sites_w_centers/",res,"m/test_3.gpkg" ) , 
           driver = "GPKG", append = T)
  
  # no centers 
  # train 
  st_write(train_no_centers, layer= "train", dsn = paste0("./models/All_sites_no_centers/",res,"m/train.gpkg" ) , 
           driver = "GPKG", append = T)
  # test  
  st_write(test, layer= "test_1", dsn = paste0("./models/All_sites_no_centers/",res,"m/test_1.gpkg" ) , 
           driver = "GPKG", append = T)
  
  # test 1   
  st_write( test_BIOCAP, layer= "test_1", dsn = paste0("./models/All_sites_no_centers/",res,"m/test_1.gpkg" ) , 
            driver = "GPKG", append = T)
  
  # test 2   
  st_write(test_Brown_undrained, layer= "test_2", dsn = paste0("./models/All_sites_no_centers/",res,"m/test_2.gpkg" ) , 
           driver = "GPKG", append = T)
  
  # test 3  
  st_write(test_Brown_drained, layer= "test_3", dsn = paste0("./models/All_sites_no_centers/",res,"m/test_3.gpkg" ) , 
           driver = "GPKG", append = T)
  
}


