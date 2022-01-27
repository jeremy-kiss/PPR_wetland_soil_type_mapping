## MODEL TRAINING
## Author: Jeremy Kiss
## Date: 2022-01-25

# load libraries
library(caret )
library(raster)
library(ranger)
library(sf)
library(dplyr)

wd <- "D:/wd/"
setwd(wd)

# set random seed 
set.seed(88888)


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
    
    ## first perform forward feature selection (Sorenson et al., 2021)
    # build ranger rf model 
    # determine the importance of  each features
    feat.order.model <- ranger(class3 ~ .,   data= pts.cov, num.trees = 1000, importance = 'impurity' )
    feat.order.model 
    
    sort(importance(feat.order.model), decreasing = T)
    
    #forward feature selection
    feature.order <- names(sort(importance(feat.order.model), decreasing = T) )
    feature.order  <- c('class3', feature.order)
    val <- vector('list')
    
    
    # loop to create models using 1: all features, incoporating the most important first to determine how many variables to include 
    for (f in 2:length(feature.order)){
      
      temp <- pts.cov[,feature.order[1:f]]
      
      model <- ranger(class3~., data=temp, importance='impurity', num.trees= 500)
      
      val <- c(val, model$prediction.error)
      
      rm(model)
    }
    
    
    predict_val <- unlist(val)
    
    predict_val  <- data.frame(feature.order[-1], predict_val)
    
    a <- 1:nrow(predict_val)
    
    plot(predict_val$predict_val~a)
    
    predict_val
    
    which.min(predict_val$predict_val)
    
    # select number of features based on minimized error
    # or manually select if minimized error is similar with far fewer features 
    num.feats <- which.min(predict_val$predict_val)
    #save number of features
    saveRDS(num.feats, file=paste0("./",res,"m/num_feats.rds" ) )
    
    num.cols <- num.feats + 1
    
    #select final covar
    pts.select_covar <- pts.cov[,feature.order[1:num.cols]]
    
    
    #create final models 
    # using caret to assess cross-validation accuracy 
    
    ##
    ## Random Forest 
    modelLookup(model="rf")
    
    # ranger tuning grid 
      tgrid <- expand.grid(
      .mtry = 2:num.feats,
      .splitrule = "gini",
      .min.node.size=1)
    
    ranger <- caret::train(x = pts.select_covar[, 2:num.cols], y = pts.select_covar$class3, 
                          method = "ranger",  importance = 'impurity', num.trees = 1000,
                          tuneGrid = tgrid, 
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 20))
    
    ranger
    
    # final variable importance 
    varImp(ranger)

    # save model 
    saveRDS(ranger, file = paste0("./",res,"m/ranger.rds"))
    
    
    
    ##
    ## CART 
    modelLookup(model="rpart")
    
    rpart <- train(x = pts.select_covar[, 2:num.cols], y = pts.select_covar$class3, 
                                  method = "rpart", 
                                  tuneLength = 50,
                                  trControl = trainControl(method = "repeatedcv", 
                                                           number = 5, repeats = 20))
    
    rpart
    
    # save model 
    saveRDS(rpart , file = paste0("./",res,"m/rpart.rds"))
    
    
    ##
    ## multinomial regression model 
    modelLookup(model="multinom")
    
    multinom <- train(x = pts.select_covar[, 2:num.cols], y = pts.select_covar$class3, 
                                method = "multinom", 
                                tuneLength = 20, trControl = trainControl(method = "repeatedcv", 
                                                                          number = 5, repeats = 20))

    # save model 
    saveRDS(multinom , file = paste0("./",res,"m/multinom.rds"))
    
    
  }}
