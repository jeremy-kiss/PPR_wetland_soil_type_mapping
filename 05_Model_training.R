## MODEL TRAINING
## Author: Jeremy Kiss
## Date: 2022-01-25

# load libraries
library(caret )
library(raster)
library(ranger)
library(sf)
library(dplyr)


# parallel processing
library(parallel)
no_cores <- detectCores() -1
library(doParallel)
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)




wd <- "D:/wd/"
setwd(wd)

# set random seed 
set.seed(8888)


## models to build 
model.list <- c("STDE_w_centers", "STDE_no_centers", "SWCU", "SMCR", "All_sites_w_centers",  "All_sites_no_centers" )
res.list <- c(2,5,10)


# loop through each model 
for(i in 1:length(model.list)){
  
  model.name <- model.list[i]

  setwd(wd)
  model.wd <- paste0("./models/", model.name)
  setwd(model.wd)
  
  
  # loop through each model  
  for(x in 1:length(res.list)){
  
    # set working resolution    
    res <- res.list[x]
    
    # load in pts with covariates 
    pts.cov <- st_read(paste0("./", res, "m/train.gpkg"))
    
    #convert to df 
    pts.cov <- as.data.frame(pts.cov)
  
    #convert RDU to factor 
    pts.cov$class3 <- as.factor(pts.cov$class3)
    
    #remove all non covar fields 
    pts.cov <- pts.cov %>% dplyr::select( -CoreCode, -set, -site, -geom, -RDU )
    
    
    
    ## Recursive feature selection using the caret package 
    # following guidance provided here https://towardsdatascience.com/effective-feature-selection-recursive-feature-elimination-using-r-148ff998e4f7
    
    ctrl <- rfeControl( functions = rfFuncs, method= "repeatedcv", number = 5, repeats = 5 )
    
    rfe <- rfe(x = pts.cov[, 2:ncol(pts.cov)],  y = pts.cov$class3, 
               rfeControl = ctrl,
               sizes = c(1:(ncol(pts.cov)-1)),
               metric = "Kappa" )
               
    
    #print results
    rfe
    
    # Print the results visually
    ggplot(data = rfe, metric = "Kappa") + theme_bw()
    
    # save rfe results 
    saveRDS(rfe, file=paste0("./",res,"m/rfe.rds" ) )
    
    # print selected features
    predictors(rfe)
    sel_pred <- predictors(rfe)
    
    #save number of selected features 
    num_feats <- length(sel_pred)
    
    #save number of features
    saveRDS(num_feats, file=paste0("./",res,"m/num_feats.rds" ) )
    
    # keep only selected features 
    pts.select_covar <- pts.cov %>% select(class3, all_of(sel_pred)  )
    
    
  
    
    
    #create final models 
    # using caret to assess cross-validation accuracy 
    
    ##
    ## Random Forest 
    modelLookup(model="ranger")
    
    # ranger tuning grid 
      tgrid <- expand.grid(
      .mtry = 2:num_feats,
      .splitrule = "gini",
      .min.node.size=1)
    
    ranger <- caret::train(x = pts.select_covar[, 2:(num_feats+1)], y = pts.select_covar$class3, 
                          method = "ranger",  importance = 'impurity', num.trees = 1000,
                          tuneGrid = tgrid, 
                          metric = "Kappa",
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 20))
    
    ranger
    
    # final variable importance 
    varImp(ranger)

    # save model 
    saveRDS(ranger, file = paste0("./",res,"m/ranger.rds"))
    
    
    # subset 
    
    
    
    ##
    ## CART 
    modelLookup(model="rpart")
    
    rpart <- train(x = pts.select_covar[, 2:(num_feats+1)], y = pts.select_covar$class3, 
                                  method = "rpart", 
                                  tuneLength = 50,
                                  metric = "Kappa",
                                  trControl = trainControl(method = "repeatedcv", 
                                                           number = 5, repeats = 20))
    
    rpart
    
    # save model 
    saveRDS(rpart , file = paste0("./",res,"m/rpart.rds"))
    
    
    ##
    ## multinomial regression model 
    modelLookup(model="multinom")
    
    multinom <- train(x = pts.select_covar[, 2:(num_feats+1)], y = pts.select_covar$class3, 
                                method = "multinom", 
                                metric = "Kappa",
                                tuneLength = 20, trControl = trainControl(method = "repeatedcv", 
                                                                          number = 5, repeats = 20))

    # save model 
    saveRDS(multinom , file = paste0("./",res,"m/multinom.rds"))
    
    
  }}



stopCluster(cl)
