## MAPPING WITH RANDOM FOREST MODELS
## Author: Jeremy Kiss
## Date: 2022-01-25

## only mapping the best-performing models 

library(raster)

wd <- "D:/wd/"
setwd(wd)

#### GENERATING MAPS 

## separeted based on model is generalized or site specific 
beginCluster(n=12)
site.list <- c("STDE" , "SWCU", "SMCR")

## list of models to use for mapping 

### generalized models 
# list of models 
model.name.list <- c("2_supp-in_generalized_ranger",               "5_supp-in_generalized_ranger", "10_supp-in_generalized_ranger" )
model.fp.list <- c("./models/All_sites_w_centers/2m/ranger.rds", "./models/All_sites_w_centers/5m/ranger.rds" , "./models/All_sites_w_centers/10m/ranger.rds")
model.res.list <- c(2,5,10)

# loop through models
for(m in 1:length(model.name.list)){
  
  model.name <- model.name.list[m]
  
  model.fp <- model.fp.list[m]
  
  res <- model.res.list[m]

  # loop each model per site
  for(i in 1:length(site.list)){
    
    # specify  site
    site <- site.list[i]
    
    # create output map dir 
    dir.create(paste0("./sites/",site,"/pred_maps/" ))
    
    # output dir 
    out.dir <- paste0("./sites/",site,"/pred_maps/",res,"m/" )
    
    # output map dir per resolution 
    dir.create(out.dir )
    
    # load in raster stack to apply model to 
    cov.stack <- stackOpen(paste0("./sites/",site,"/covar/",res,"m/cov_stack/stack.tif" ) )
    
    # load in model 
    model <- readRDS(model.fp)
    
    # predict map 
    pr.map <- predict(object = cov.stack,   
                      model = model, 
                      filename = paste0(out.dir,model.name ) , 
                      format = "GTiff", overwrite = TRUE  )
    
  }
  
}


### site specific models 
# list of models 
model.name.list <- c("2_supp-in_site-specific_ranger",  "2_no-supp_site-specific_ranger", "5_no-supp_site-specific_ranger", "5_supp-in_site-specific_ranger", "10_supp-in_site-specific_ranger" ,"10_no-supp_site-specific_ranger" )
model.res.list <- c(2,2,5,5,10,10)
supp_used.list <- c("supp-in", "no-supp",  "no-supp","supp-in", "supp-in","no-supp" )

#model filepath will be different per site 
## site model will depend on if supplemental data was included 

# loop through models
for(m in 1:length(model.name.list)){
  
  model.name <- model.name.list[m]
  
  res <- model.res.list[m]
  
  supp_used <- supp_used.list[m]
  
  # loop each model per site
  for(i in 1:length(site.list)){
    
    # specify  site
    site <- site.list[i]
    
    # create output map dir 
    dir.create(paste0("./sites/",site,"/pred_maps/" ))
    
    # output dir 
    out.dir <- paste0("./sites/",site,"/pred_maps/",res,"m/" )
    
    # output map dir per resolution 
    dir.create(out.dir )
    
    # load in raster stack to apply model to 
    cov.stack <- stackOpen(paste0("./sites/",site,"/covar/",res,"m/cov_stack/stack.tif" ) )
    
    # specify model site training data 
    if(site != "STDE"){
      site_train <- site
    } else if(supp_used == "supp-in"){
      site_train <- "STDE_w_centers"
    } else site_train <- "STDE_no_centers"
    
    # specify model filepath 
    model.fp <- paste0("./models/", site_train, "/", res, "m/ranger.rds")
    
    # load in model 
    model <- readRDS(model.fp)
    
    # predict map 
    pr.map <- predict(object = cov.stack,   
                      model = model, 
                      filename = paste0(out.dir,model.name ) , 
                      format = "GTiff", overwrite = TRUE  )
    
  }
  
}


endCluster()
