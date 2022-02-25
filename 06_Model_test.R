## MODEL VALIDATION SUMMARY
## Author: Jeremy Kiss
## Date: 2022-01-25

# load libraries
library(ithir)
library(sf)
library(dplyr)
library(plyr)

wd <- "D:/wd/models/"
setwd(wd)

# create list of all models
model.list <- c("STDE_w_centers", "STDE_no_centers", "SWCU", "SMCR", "All_sites_w_centers",  "All_sites_no_centers" )

# list of model types
model.type.list <- c("ranger", "rpart", "multinom" )

# list of model resolutions 
res.list <- c(2,5,10)


### STDE_w_centers 
Train_data <- "STDE_w_centers"
Test_set_1 = "STDE_BIOCAP"

# create table for saving results 
metrics.table <- data.frame(Train_data = character(),
                            Model_type = character(),
                            Resolution = character(),
                            cross_val_acc = double(),
                            cross_val_kappa = double(),
                            num_features = integer(),
                            bestTune_param = character(),
                            bestTune_value = character(),
                            
                            
                            final_model_internal_acc_STDE = double(),
                            final_model_internal_kappa_STDE = double(),
                            final_model_internal_acc_SWCU = double(),
                            final_model_internal_kappa_SWCU = double(),
                            final_model_internal_acc_SMCR = double(),
                            final_model_internal_kappa_SMCR = double(),
                            
                            Test_set_1 = character(),
                            ind_val_1_acc = double(),
                            ind_val_1_kappa = double(),
                            
                            ind_val_1_class1 = character(),
                            ind_val_1_class1_prod_acc = double(),
                            ind_val_1_class1_user_acc = double(),
                            
                            ind_val_1_class2 = character(),
                            ind_val_1_class2_prod_acc = double(),
                            ind_val_1_class2_user_acc = double(),
                            
                            ind_val_1_class3 = character(),
                            ind_val_1_class3_prod_acc = double(),
                            ind_val_1_class3_user_acc = double())
                            

## loop per resolution 
for(x in 1:length(res.list)){
  
  # set working resolution    
  res <- res.list[x]
  
  # set wd to resolution 
  setwd(wd)
  setwd(paste0("./",Train_data,"/",res,"m/" ) )
  
  # load internal training points 
  train.pts <- st_read("./train.gpkg")
  
  # as df 
  train.pts <- as.data.frame( train.pts )
  
  #chr to factor 
  train.pts$class3 <- as.factor(train.pts$class3)
  
  ## load independent points (BIOCAP)
  test.pts <- st_read("./test_1.gpkg" )
  
  # as df 
  test.pts <- as.data.frame(test.pts)
  
  #chr to factor
  test.pts$class3 <-  as.factor(test.pts$class3)
  
    ## loop to test built models 
  for(t in 1:length(model.type.list)){
    
    #specify name of the model type 
    model.type <- model.type.list[t]
    
    #load model 
    caret.model <- readRDS(paste0("./",model.type, ".rds"))
    
    #get cross val metrics 
    caret.model.CV <-  caret::getTrainPerf(caret.model)
    
    #model tuning  parameters 
    caret.model.tune <- caret.model$bestTune
    
    #predict for train points 
    train_pred <- predict(caret.model, newdata = train.pts)
    
    #assess goodness of fit 
    train.goof <- goofcat( observed = train.pts$class3, predicted = train_pred)
    train.goof
    
    # save full goof
    saveRDS(train.goof, file =  paste0("./traingoof.rds"))
    
    # predict for test points
    test_pred <- predict(caret.model, newdata = test.pts)
    
    # assess goodness of fit 
    goof <- goofcat( observed = test.pts$class3, predicted = test_pred)
    goof
    
    # save full goof
    saveRDS(goof, file =  paste0("./",Test_set_1,".goof.rds"))
    
    ## RESULTS TABLE
    # get goof users and producers accuracy 
    goof.prod <- goof$producers_accuracy
    goof.user <- goof$users_accuracy
    
    # gather info to fill the table 
    Model_type <- model.type
    Resolution <- res
    
    cross_val_acc <- caret.model.CV[1]
    cross_val_kappa <- caret.model.CV[2]
    
    num_features <- readRDS("./num_feats.rds") 
    bestTune_param <- colnames(caret.model.tune[1])  
    bestTune_value <-  caret.model.tune[1,1] 

    final_model_internal_acc_STDE <- train.goof$overall_accuracy 
    final_model_internal_kappa_STDE <- train.goof$kappa 
    final_model_internal_acc_SWCU <- NA
    final_model_internal_kappa_SWCU <- NA
    final_model_internal_acc_SMCR <- NA
    final_model_internal_kappa_SMCR <- NA
    
    ind_val_1_acc <- goof$overall_accuracy
    ind_val_1_kappa <- goof$kappa
    
    ind_val_1_class1 <- names(goof.prod[1])
    ind_val_1_class1_prod_acc <- goof.prod[[1]]
    ind_val_1_class1_user_acc <- goof.user[[1]]

    ind_val_1_class2 <- names(goof.prod[2])
    ind_val_1_class2_prod_acc <- goof.prod[[2]]
    ind_val_1_class2_user_acc <- goof.user[[2]]

    ind_val_1_class3 <- names(goof.prod[3])
    ind_val_1_class3_prod_acc <- goof.prod[[3]]
    ind_val_1_class3_user_acc <- goof.user[[3]]
    
    # fill table with info and results 
    metrics.table[1,] <- c(Train_data , 
                                  Model_type, 
                                  Resolution ,    
                           
                                  cross_val_acc,
                                  cross_val_kappa,
                           
                                  
                                  num_features,
                                  bestTune_param,
                                  bestTune_value ,
                           
                                  final_model_internal_acc_STDE,
                                  final_model_internal_kappa_STDE ,
                                  final_model_internal_acc_SWCU ,
                                  final_model_internal_kappa_SWCU ,
                                  final_model_internal_acc_SMCR ,
                                  final_model_internal_kappa_SMCR ,

                                  Test_set_1,
                                  
                                  ind_val_1_acc,
                                  ind_val_1_kappa,
                                  
                                  ind_val_1_class1 ,
                                  ind_val_1_class1_prod_acc ,
                                  ind_val_1_class1_user_acc ,
                                  
                                  ind_val_1_class2 ,
                                  ind_val_1_class2_prod_acc ,
                                  ind_val_1_class2_user_acc ,
                                  
                                  ind_val_1_class3 ,
                                  ind_val_1_class3_prod_acc,
                                  ind_val_1_class3_user_acc  )
    
  
    write.csv(metrics.table, file= paste0( "./",Train_data,"_", model.type,"_", res, "m_metrics.csv" ) )
      
    assign(x = paste0( Train_data,"_", res,"m_", model.type, "_metrics" ), value = metrics.table )

  }}
  
  

# combine results from this model set 
STDE_w_centers_metrics_table  <- rbind( STDE_w_centers_2m_ranger_metrics,
                                        STDE_w_centers_2m_rpart_metrics,
                                        STDE_w_centers_2m_multinom_metrics,
                                        STDE_w_centers_5m_ranger_metrics,
                                        STDE_w_centers_5m_rpart_metrics,
                                        STDE_w_centers_5m_multinom_metrics,
                                        STDE_w_centers_10m_ranger_metrics,
                                        STDE_w_centers_10m_rpart_metrics,
                                        STDE_w_centers_10m_multinom_metrics)
                                        
rm( STDE_w_centers_2m_ranger_metrics,
    STDE_w_centers_2m_rpart_metrics,
    STDE_w_centers_2m_multinom_metrics,
    STDE_w_centers_5m_ranger_metrics,
    STDE_w_centers_5m_rpart_metrics,
    STDE_w_centers_5m_multinom_metrics,
    STDE_w_centers_10m_ranger_metrics,
    STDE_w_centers_10m_rpart_metrics,
    STDE_w_centers_10m_multinom_metrics )
  
# save 
setwd(wd)
write.csv(STDE_w_centers_metrics_table, file=paste0("./",Train_data,"/",Train_data,"_metrics_table.csv" ))

  






### STDE_no_centers 
Train_data <- "STDE_no_centers"
Test_set_1 = "STDE_BIOCAP"

# create table for saving results 
metrics.table <- data.frame(Train_data = character(),
                            Model_type = character(),
                            Resolution = character(),
                            cross_val_acc = double(),
                            cross_val_kappa = double(),
                            num_features = integer(),
                            bestTune_param = character(),
                            bestTune_value = character(),
                            
                            final_model_internal_acc_STDE = double(),
                            final_model_internal_kappa_STDE = double(),
                            final_model_internal_acc_SWCU = double(),
                            final_model_internal_kappa_SWCU = double(),
                            final_model_internal_acc_SMCR = double(),
                            final_model_internal_kappa_SMCR = double(),
                            
                            Test_set_1 = character(),
                            ind_val_1_acc = double(),
                            ind_val_1_kappa = double(),
                            ind_val_1_class1 = character(),
                            ind_val_1_class1_prod_acc = double(),
                            ind_val_1_class1_user_acc = double(),
                            
                            ind_val_1_class2 = character(),
                            ind_val_1_class2_prod_acc = double(),
                            ind_val_1_class2_user_acc = double(),
                            
                            ind_val_1_class3 = character(),
                            ind_val_1_class3_prod_acc = double(),
                            ind_val_1_class3_user_acc = double())



## loop per resolution 
for(x in 1:length(res.list)){
  
  # set working resolution    
  res <- res.list[x]
  
  # set wd to resolution 
  setwd(wd)
  setwd(paste0("./",Train_data,"/",res,"m/" ) )
  
  # load internal training points 
  train.pts <- st_read("./train.gpkg")
  
  # as df 
  train.pts <- as.data.frame( train.pts )
  
  # chr to factor 
  train.pts$class3 <- as.factor(train.pts$class3)
  
  #load independent points (BIOCAP)
  test.pts <- st_read("./test_1.gpkg" )
  
  #as df 
  test.pts <- as.data.frame(test.pts)
  
  #chr to factor
  test.pts$class3 <-  as.factor(test.pts$class3)
  
  
  ## loop to test built models 
  for(t in 1:length(model.type.list)){
    
    # specify name of the model type 
    model.type <- model.type.list[t]
    
    # load model 
    caret.model <- readRDS(paste0("./",model.type, ".rds"))
    
    # get cross val metrics 
    caret.model.CV <-  caret::getTrainPerf(caret.model)
    
    # model tuning  parameters 
    caret.model.tune <- caret.model$bestTune
    
    # predict for train points 
    train_pred <- predict(caret.model, newdata = train.pts)
    
    # assess goodness of fit 
    train.goof <- goofcat( observed = train.pts$class3, predicted = train_pred)
    train.goof
    
    # save full goof
    saveRDS(train.goof, file =  paste0("./traingoof.rds"))
    
    # predict for test points
    test_pred <- predict(caret.model, newdata = test.pts)
    
    # assess goodness of fit 
    goof <- goofcat( observed = test.pts$class3, predicted = test_pred)
    goof
    
    # save full goof
    saveRDS(goof, file =  paste0("./",Test_set_1,".goof.rds"))
    
    ## RESULTS TABLE
    # get goof users and producers accuracy 
    goof.prod <- goof$producers_accuracy
    goof.user <- goof$users_accuracy
    
    # gather info to fill the table 
    Model_type <- model.type
    Resolution <- res
    
    cross_val_acc <- caret.model.CV[1]
    cross_val_kappa <- caret.model.CV[2]
    
    num_features <- readRDS("./num_feats.rds") 
    bestTune_param <- colnames(caret.model.tune[1])  
    bestTune_value <-  caret.model.tune[1,1] 
    
    final_model_internal_acc_STDE <- train.goof$overall_accuracy 
    final_model_internal_kappa_STDE <- train.goof$kappa 
    final_model_internal_acc_SWCU <- NA
    final_model_internal_kappa_SWCU <- NA
    final_model_internal_acc_SMCR <- NA
    final_model_internal_kappa_SMCR <- NA
    
    ind_val_1_acc <- goof$overall_accuracy
    ind_val_1_kappa <- goof$kappa
    
    ind_val_1_class1 <- names(goof.prod[1])
    ind_val_1_class1_prod_acc <- goof.prod[[1]]
    ind_val_1_class1_user_acc <- goof.user[[1]]
    
    ind_val_1_class2 <- names(goof.prod[2])
    ind_val_1_class2_prod_acc <- goof.prod[[2]]
    ind_val_1_class2_user_acc <- goof.user[[2]]
    
    ind_val_1_class3 <- names(goof.prod[3])
    ind_val_1_class3_prod_acc <- goof.prod[[3]]
    ind_val_1_class3_user_acc <- goof.user[[3]]
    
    
    # fill table with info and results 
    metrics.table[1,] <- c(Train_data , 
                           Model_type, 
                           Resolution ,    
                           
                           cross_val_acc,
                           cross_val_kappa,
                           
                           
                           num_features,
                           bestTune_param,
                           bestTune_value ,
                           
                           
                           final_model_internal_acc_STDE,
                           final_model_internal_kappa_STDE ,
                           final_model_internal_acc_SWCU ,
                           final_model_internal_kappa_SWCU ,
                           final_model_internal_acc_SMCR ,
                           final_model_internal_kappa_SMCR ,
                           
                           Test_set_1,
                           
                           ind_val_1_acc,
                           ind_val_1_kappa,
                           
                           ind_val_1_class1 ,
                           ind_val_1_class1_prod_acc ,
                           ind_val_1_class1_user_acc ,
                           
                           ind_val_1_class2 ,
                           ind_val_1_class2_prod_acc ,
                           ind_val_1_class2_user_acc ,
                           
                           ind_val_1_class3 ,
                           ind_val_1_class3_prod_acc,
                           ind_val_1_class3_user_acc  )
    
    
    write.csv(metrics.table, file= paste0( "./",Train_data,"_", model.type,"_", res, "m_metrics.csv" ) )
    
    assign(x = paste0( Train_data,"_", res,"m_", model.type, "_metrics" ), value = metrics.table )
    
  }}

#combine results from this model set 
STDE_no_centers_metrics_table  <- rbind( STDE_no_centers_2m_ranger_metrics,
                                        STDE_no_centers_2m_rpart_metrics,
                                        STDE_no_centers_2m_multinom_metrics,
                                        STDE_no_centers_5m_ranger_metrics,
                                        STDE_no_centers_5m_rpart_metrics,
                                        STDE_no_centers_5m_multinom_metrics,
                                        STDE_no_centers_10m_ranger_metrics,
                                        STDE_no_centers_10m_rpart_metrics,
                                        STDE_no_centers_10m_multinom_metrics)


rm( STDE_no_centers_2m_ranger_metrics,
       STDE_no_centers_2m_rpart_metrics,
       STDE_no_centers_2m_multinom_metrics,
       STDE_no_centers_5m_ranger_metrics,
       STDE_no_centers_5m_rpart_metrics,
       STDE_no_centers_5m_multinom_metrics,
       STDE_no_centers_10m_ranger_metrics,
       STDE_no_centers_10m_rpart_metrics,
       STDE_no_centers_10m_multinom_metrics)

# save 
setwd(wd)
write.csv(STDE_no_centers_metrics_table, file=paste0("./",Train_data,"/",Train_data,"_metrics_table.csv" ))









### SWCU
Train_data <- "SWCU"

# create table for saving results 
metrics.table <- data.frame(Train_data = character(),
                            Model_type = character(),
                            Resolution = character(),
                            cross_val_acc = double(),
                            cross_val_kappa = double(),
                            num_features = integer(),
                            bestTune_param = character(),
                            bestTune_value = character(),
                            
                            final_model_internal_acc_STDE = double(),
                            final_model_internal_kappa_STDE = double(),
                            final_model_internal_acc_SWCU = double(),
                            final_model_internal_kappa_SWCU = double(),
                            final_model_internal_acc_SMCR = double(),
                            final_model_internal_kappa_SMCR = double()
                            # ,
                            # Test_set = character(),
                            # ind_val_acc = double(),
                            # ind_val_kappa = double(),
                            # ind_val_class1 = character(),
                            # ind_val_class1_prod_acc = double(),
                            # ind_val_class1_user_acc = double(),
                            # 
                            # ind_val_class2 = character(),
                            # ind_val_class2_prod_acc = double(),
                            # ind_val_class2_user_acc = double(),
                            # 
                            # ind_val_class3 = character(),
                            # ind_val_class3_prod_acc = double(),
                            # ind_val_class3_user_acc = double
                            )


## loop per resolution 
for(x in 1:length(res.list)){
  
  # set working resolution    
  res <- res.list[x]
  
  #set wd to resolution 
  setwd(wd)
  setwd(paste0("./",Train_data,"/",res,"m/" ) )
  
  # load internal training points 
  train.pts <- st_read("./train.gpkg")
  
  #as df 
  train.pts <- as.data.frame( train.pts )
  
  #chr to factor 
  train.pts$class3 <- as.factor(train.pts$class3)
  
  # 
  # #load independent points (BIOCAP)
  # test.pts <- st_read("./test_1.gpkg" )
  # 
  # #as df 
  # test.pts <- as.data.frame(test.pts)
  # 
  # #chr to factor
  # test.pts$class3 <-  as.factor(test.pts$class3)
  # 
  # 
  
  ## loop to test built models 
  for(t in 1:length(model.type.list)){
    
    #specify name of the model type 
    model.type <- model.type.list[t]
    
    #load model 
    caret.model <- readRDS(paste0("./",model.type, ".rds"))
    
    #get cross val metrics 
    caret.model.CV <-  caret::getTrainPerf(caret.model)
    
    #model tuning  parameters 
    caret.model.tune <- caret.model$bestTune
   
    #predict for train points 
    train_pred <- predict(caret.model, newdata = train.pts)
    
    #assess goodness of fit 
    train.goof <- goofcat( observed = train.pts$class3, predicted = train_pred)
    train.goof
    
    # save full goof
    saveRDS(train.goof, file =  paste0("./traingoof.rds"))
    
    # 
    # #predict for test points
    # test_pred <- predict(caret.model, newdata = test.pts)
    # 
    # #assess goodness of fit 
    # goof <- goofcat( observed = test.pts$class3, predicted = test_pred)
    # goof
    # 
    # # save full goof
    # saveRDS(goof, file =  paste0("./",Test_set,".goof.rds"))
    # 
    # 
    
    ## RESULTS TABLE
    
    # get goof users and producers accuracy 
    goof.prod <- goof$producers_accuracy
    goof.user <- goof$users_accuracy
    
    # gather info to fill the table 
    Model_type <- model.type
    Resolution <- res
    
    cross_val_acc <- caret.model.CV[1]
    cross_val_kappa <- caret.model.CV[2]
    
    num_features <- readRDS("./num_feats.rds") 
    bestTune_param <- colnames(caret.model.tune[1])  
    bestTune_value <-  caret.model.tune[1,1] 
    
    final_model_internal_acc_STDE <- NA
    final_model_internal_kappa_STDE <- NA
    final_model_internal_acc_SWCU <- train.goof$overall_accuracy 
    final_model_internal_kappa_SWCU <- train.goof$kappa 
    final_model_internal_acc_SMCR <- NA
    final_model_internal_kappa_SMCR <- NA
    
    # ind_val_acc <- goof$overall_accuracy
    # ind_val_kappa <- goof$kappa
    # 
    # ind_val_class1 <- names(goof.prod[1])
    # ind_val_class1_prod_acc <- goof.prod[[1]]
    # ind_val_class1_user_acc <- goof.user[[1]]
    # 
    # ind_val_class2 <- names(goof.prod[2])
    # ind_val_class2_prod_acc <- goof.prod[[2]]
    # ind_val_class2_user_acc <- goof.user[[2]]
    # 
    # ind_val_class3 <- names(goof.prod[3])
    # ind_val_class3_prod_acc <- goof.prod[[3]]
    # ind_val_class3_user_acc <- goof.user[[3]]
    
    #fill table with info and results 
    metrics.table[1,] <- c(Train_data , 
                           Model_type, 
                           Resolution ,    
                           
                           cross_val_acc,
                           cross_val_kappa,
                           
                           num_features,
                           bestTune_param,
                           bestTune_value ,
                           final_model_internal_acc_STDE,
                           final_model_internal_kappa_STDE ,
                           final_model_internal_acc_SWCU ,
                           final_model_internal_kappa_SWCU ,
                           final_model_internal_acc_SMCR ,
                           final_model_internal_kappa_SMCR 
                           # ,
                           # 
                           # Test_set,
                           # 
                           # ind_val_acc,
                           # ind_val_kappa,
                           # 
                           # ind_val_class1 ,
                           # ind_val_class1_prod_acc ,
                           # ind_val_class1_user_acc ,
                           # 
                           # ind_val_class2 ,
                           # ind_val_class2_prod_acc ,
                           # ind_val_class2_user_acc ,
                           # 
                           # ind_val_class3 ,
                           # ind_val_class3_prod_acc,
                           # ind_val_class3_user_acc  
                           )
    
    write.csv(metrics.table, file= paste0( "./",Train_data,"_", model.type,"_", res, "m_metrics.csv" ) )
    
    assign(x = paste0( Train_data,"_", res,"m_", model.type, "_metrics" ), value = metrics.table )
    
  }}



#combine results from this model set 
SWCU_metrics_table  <- rbind( SWCU_2m_ranger_metrics,
                                        SWCU_2m_rpart_metrics,
                                        SWCU_2m_multinom_metrics,
                                        SWCU_5m_ranger_metrics,
                                        SWCU_5m_rpart_metrics,
                                        SWCU_5m_multinom_metrics,
                                        SWCU_10m_ranger_metrics,
                                        SWCU_10m_rpart_metrics,
                                        SWCU_10m_multinom_metrics)

rm( SWCU_2m_ranger_metrics,
       SWCU_2m_rpart_metrics,
       SWCU_2m_multinom_metrics,
       SWCU_5m_ranger_metrics,
       SWCU_5m_rpart_metrics,
       SWCU_5m_multinom_metrics,
       SWCU_10m_ranger_metrics,
       SWCU_10m_rpart_metrics,
       SWCU_10m_multinom_metrics)


## save 
setwd(wd)
write.csv(SWCU_metrics_table, file=paste0("./",Train_data,"/",Train_data,"_metrics_table.csv" ))












### SMCR

# code was changed so that test set 2 in entire script refers to SMCR BROWN UNDRAINED and Test set 3 always refers to SMCR_BROWN_DRAINED 

Train_data <- "SMCR"
Test_set_2 <- "SMCR_BROWN_UNDRAINED"
Test_set_3 <- "SMCR_BROWN_DRAINED"

# create table for saving results 
metrics.table <- data.frame(Train_data = character(),
                            Model_type = character(),
                            Resolution = character(),
                            cross_val_acc = double(),
                            cross_val_kappa = double(),
                            num_features = integer(),
                            bestTune_param = character(),
                            bestTune_value = character(),
                            
                            final_model_internal_acc_STDE = double(),
                            final_model_internal_kappa_STDE = double(),
                            final_model_internal_acc_SWCU = double(),
                            final_model_internal_kappa_SWCU = double(),
                            final_model_internal_acc_SMCR = double(),
                            final_model_internal_kappa_SMCR = double(),
                            
                            #test set 2
                            Test_set_2 = character(),
                            ind_val_2_acc = double(),
                            ind_val_2_kappa = double(),
                            ind_val_2_class1 = character(),
                            ind_val_2_class1_prod_acc = double(),
                            ind_val_2_class1_user_acc = double(),
                            
                            ind_val_2_class2 = character(),
                            ind_val_2_class2_prod_acc = double(),
                            ind_val_2_class2_user_acc = double(),
                            
                            # No CW obersvations in test set 1 
                            # ind_val_1_class3 = character(),
                            # ind_val_1_class3_prod_acc = double(),
                            # ind_val_1_class3_user_acc = double(),

                            #test set 3
                            Test_set_3 = character(),
                            ind_val_3_acc = double(),
                            ind_val_3_kappa = double(),
                            ind_val_3_class1 = character(),
                            ind_val_3_class1_prod_acc = double(),
                            ind_val_3_class1_user_acc = double(),
                            
                            ind_val_3_class2 = character(),
                            ind_val_3_class2_prod_acc = double(),
                            ind_val_3_class2_user_acc = double(),
                            
                            ind_val_3_class3 = character(),
                            ind_val_3_class3_prod_acc = double(),
                            ind_val_3_class3_user_acc = double()
                            
                            )





## loop per resolution 
for(x in 1:length(res.list)){
  
  
  # set working resolution    
  res <- res.list[x]
  
  # set wd to resolution 
  setwd(wd)
  setwd(paste0("./",Train_data,"/",res,"m/" ) )
  
  # load internal training points 
  train.pts <- st_read("./train.gpkg")
  
  # as df 
  train.pts <- as.data.frame( train.pts )
  
  # chr to factor 
  train.pts$class3 <- as.factor(train.pts$class3)
  
  # load independent points 
  test1.pts <- st_read("./test_1.gpkg" )
  
  # as df 
  test1.pts <- as.data.frame(test1.pts)
  
  # chr to factor
  test1.pts$class3 <-  as.factor(test1.pts$class3)
  
  # load independent points 
  test2.pts <- st_read("./test_2.gpkg" )
  
  # as df 
  test2.pts <- as.data.frame(test2.pts)
  
  # chr to factor
  test2.pts$class3 <-  as.factor(test2.pts$class3)
  
  # loop to test built models 
  for(t in 1:length(model.type.list)){
    
    #specify name of the model type 
    model.type <- model.type.list[t]
    
    #load model 
    caret.model <- readRDS(paste0("./",model.type, ".rds"))
    
    #get cross val metrics 
    caret.model.CV <-  caret::getTrainPerf(caret.model)
    
    #model tuning  parameters 
    caret.model.tune <- caret.model$bestTune
  
    #predict for train points 
    train_pred <- predict(caret.model, newdata = train.pts)
    
    #assess goodness of fit 
    train.goof <- goofcat( observed = train.pts$class3, predicted = train_pred)
    train.goof
    
    # save full goof
    saveRDS(train.goof, file =  paste0("./traingoof.rds"))
    
    
    ### TEST SET 2 
    #predict for test points
    test1_pred <- predict(caret.model, newdata = test1.pts)
    
    #assess goodness of fit 
    goof1 <- goofcat( observed = test1.pts$class3, predicted = test1_pred)
    goof1
    
    # save full goof
    saveRDS(goof1, file =  paste0("./",Test_set_1,".goof.rds"))
    
    # get goof users and producers accuracy 
    goof1.prod <- goof1$producers_accuracy
    goof1.user <- goof1$users_accuracy
    
    
    ### TEST SET 3 
    #predict for test points
    test2_pred <- predict(caret.model, newdata = test2.pts)
    
    #assess goodness of fit 
    goof2 <- goofcat( observed = test2.pts$class3, predicted = test2_pred)
    goof2
    
    # save full goof
    saveRDS(goof2, file =  paste0("./",Test_set_2,".goof.rds"))
    
    # get goof users and producers accuracy 
    goof2.prod <- goof2$producers_accuracy
    goof2.user <- goof2$users_accuracy
    
    
    ## RESULTS TABLE
    # gather info to fill the table 
    
    Model_type <- model.type
    Resolution <- res
    
    cross_val_acc <- caret.model.CV[1]
    cross_val_kappa <- caret.model.CV[2]
    
    num_features <- readRDS("./num_feats.rds") 
    bestTune_param <- colnames(caret.model.tune[1])  
    bestTune_value <-  caret.model.tune[1,1] 
    
    final_model_internal_acc_STDE <- NA
    final_model_internal_kappa_STDE <- NA
    final_model_internal_acc_SWCU <- NA
    final_model_internal_kappa_SWCU <- NA
    final_model_internal_acc_SMCR <- train.goof$overall_accuracy 
    final_model_internal_kappa_SMCR <- train.goof$kappa 
    
    
    ## test 2 
    ind_val_2_acc <- goof1$overall_accuracy
    ind_val_2_kappa <- goof1$kappa
    
    ind_val_2_class1 <- names(goof1.prod[1])
    ind_val_2_class1_prod_acc <- goof1.prod[[1]]
    ind_val_2_class1_user_acc <- goof1.user[[1]]
    
    ind_val_2_class2 <- names(goof1.prod[2])
    ind_val_2_class2_prod_acc <- goof1.prod[[2]]
    ind_val_2_class2_user_acc <- goof1.user[[2]]
    # 
    # ## No CW observations in this test set 
    # ind_val_1_class3 <- names(goof1.prod[3])
    # ind_val_1_class3_prod_acc <- goof1.prod[[3]]
    # ind_val_1_class3_user_acc <- goof1.user[[3]]
    
    
    ## test 2
    ind_val_3_acc <- goof2$overall_accuracy
    ind_val_3_kappa <- goof2$kappa
    
    ind_val_3_class1 <- names(goof2.prod[1])
    ind_val_3_class1_prod_acc <- goof2.prod[[1]]
    ind_val_3_class1_user_acc <- goof2.user[[1]]
    
    ind_val_3_class2 <- names(goof2.prod[2])
    ind_val_3_class2_prod_acc <- goof2.prod[[2]]
    ind_val_3_class2_user_acc <- goof2.user[[2]]
    
    ind_val_3_class3 <- names(goof2.prod[3])
    ind_val_3_class3_prod_acc <- goof2.prod[[3]]
    ind_val_3_class3_user_acc <- goof2.user[[3]]
    
    
    #fill table with info and results 
    metrics.table[1,] <- c(Train_data , 
                           Model_type, 
                           Resolution ,    
                           
                           cross_val_acc,
                           cross_val_kappa,
                           
                           num_features,
                           bestTune_param,
                           bestTune_value ,
                           
                           
                           final_model_internal_acc_STDE,
                           final_model_internal_kappa_STDE ,
                           final_model_internal_acc_SWCU ,
                           final_model_internal_kappa_SWCU ,
                           final_model_internal_acc_SMCR ,
                           final_model_internal_kappa_SMCR ,
                           
                           #test set 2
                           
                           Test_set_2,
                           
                           ind_val_2_acc,
                           ind_val_2_kappa,
                           
                           ind_val_2_class1 ,
                           ind_val_2_class1_prod_acc ,
                           ind_val_2_class1_user_acc ,
                           
                           ind_val_2_class2 ,
                           ind_val_2_class2_prod_acc ,
                           ind_val_2_class2_user_acc ,
                           
                           # ind_val_1_class3 ,
                           # ind_val_1_class3_prod_acc,
                           # ind_val_1_class3_user_acc ,
                           
                           #test set 3
                           
                           Test_set_3,
                           
                           ind_val_3_acc,
                           ind_val_3_kappa,
                           
                           ind_val_3_class1 ,
                           ind_val_3_class1_prod_acc ,
                           ind_val_3_class1_user_acc ,
                           
                           ind_val_3_class2 ,
                           ind_val_3_class2_prod_acc ,
                           ind_val_3_class2_user_acc ,
                           
                           ind_val_3_class3 ,
                           ind_val_3_class3_prod_acc,
                           ind_val_3_class3_user_acc
                           )
    
    
    write.csv(metrics.table, file= paste0( "./",Train_data,"_", model.type,"_", res, "m_metrics.csv" ) )
    
    assign(x = paste0( Train_data,"_", res,"m_", model.type, "_metrics" ), value = metrics.table )
    
  }}



# combine results from this model set 
SMCR_metrics_table  <- rbind( SMCR_2m_ranger_metrics,
                                        SMCR_2m_rpart_metrics,
                                        SMCR_2m_multinom_metrics,
                                        SMCR_5m_ranger_metrics,
                                        SMCR_5m_rpart_metrics,
                                        SMCR_5m_multinom_metrics,
                                        SMCR_10m_ranger_metrics,
                                        SMCR_10m_rpart_metrics,
                                        SMCR_10m_multinom_metrics)


rm( SMCR_2m_ranger_metrics,
       SMCR_2m_rpart_metrics,
       SMCR_2m_multinom_metrics,
       SMCR_5m_ranger_metrics,
       SMCR_5m_rpart_metrics,
       SMCR_5m_multinom_metrics,
       SMCR_10m_ranger_metrics,
       SMCR_10m_rpart_metrics,
       SMCR_10m_multinom_metrics)


## save 
setwd(wd)
write.csv(SMCR_metrics_table, file=paste0("./",Train_data,"/",Train_data,"_metrics_table.csv" ))








### All Sites w centers 
Train_data <- "All_sites_w_centers"
Test_set_1 <- "STDE_BIOCAP"
Test_set_2 <- "SMCR_BROWN_UNDRAINED"
Test_set_3 <- "SMCR_BROWN_DRAINED"

# create table for saving results 
metrics.table <- data.frame(Train_data = character(),
                            Model_type = character(),
                            Resolution = character(),
                            cross_val_acc = double(),
                            cross_val_kappa = double(),
                            num_features = integer(),
                            bestTune_param = character(),
                            bestTune_value = character(),
                            
                            final_model_internal_acc_STDE = double(),
                            final_model_internal_kappa_STDE = double(),
                            final_model_internal_acc_SWCU = double(),
                            final_model_internal_kappa_SWCU = double(),
                            final_model_internal_acc_SMCR = double(),
                            final_model_internal_kappa_SMCR = double(),
                            
                            #test set 1
                            Test_set_1 = character(),
                            ind_val_1_acc = double(),
                            ind_val_1_kappa = double(),
                            ind_val_1_class1 = character(),
                            ind_val_1_class1_prod_acc = double(),
                            ind_val_1_class1_user_acc = double(),
                            
                            ind_val_1_class2 = character(),
                            ind_val_1_class2_prod_acc = double(),
                            ind_val_1_class2_user_acc = double(),
                            
                            
                            ind_val_1_class3 = character(),
                            ind_val_1_class3_prod_acc = double(),
                            ind_val_1_class3_user_acc = double(),

                            
                            #test set 2
                            Test_set_2 = character(),
                            ind_val_2_acc = double(),
                            ind_val_2_kappa = double(),
                            ind_val_2_class1 = character(),
                            ind_val_2_class1_prod_acc = double(),
                            ind_val_2_class1_user_acc = double(),
                            
                            ind_val_2_class2 = character(),
                            ind_val_2_class2_prod_acc = double(),
                            ind_val_2_class2_user_acc = double(),
                            
                            # #No CW observation in BROWN UNDRAINED 
                            # ind_val_2_class3 = character(),
                            # ind_val_2_class3_prod_acc = double(),
                            # ind_val_2_class3_user_acc = double(),
                            
                            
                            
                            #test set 3
                            Test_set_3 = character(),
                            ind_val_3_acc = double(),
                            ind_val_3_kappa = double(),
                            ind_val_3_class1 = character(),
                            ind_val_3_class1_prod_acc = double(),
                            ind_val_3_class1_user_acc = double(),
                            
                            ind_val_3_class2 = character(),
                            ind_val_3_class2_prod_acc = double(),
                            ind_val_3_class2_user_acc = double(),
                            
                            ind_val_3_class3 = character(),
                            ind_val_3_class3_prod_acc = double(),
                            ind_val_3_class3_user_acc = double()
                            
)





# loop per resolution 
for(x in 1:length(res.list)){
  
  # set working resolution    
  res <- res.list[x]
  
  #set wd to resolution 
  setwd(wd)
  setwd(paste0("./",Train_data,"/",res,"m/" ) )
  
  # load internal training points 
  train.pts <- st_read("./train.gpkg")
  
  #as df 
  train.pts <- as.data.frame( train.pts )
  
  #chr to factor 
  train.pts$class3 <- as.factor(train.pts$class3)
  
  #split into sites 
  STDE.train.pts <- train.pts %>% filter(set == "STDE_LWBSF" )
  SWCU.train.pts <- train.pts %>% filter(set == "SWCU_LWBSF" )
  SMCR.train.pts <- train.pts %>% filter(set == "SMCR_LWBSF" )
  
  #load independent points 
  test1.pts <- st_read("./test_1.gpkg" )
  
  #as df 
  test1.pts <- as.data.frame(test1.pts)
  
  #chr to factor
  test1.pts$class3 <-  as.factor(test1.pts$class3)
  
  #load independent points 
  test2.pts <- st_read("./test_2.gpkg" )
  
  #as df 
  test2.pts <- as.data.frame(test2.pts)
  
  #chr to factor
  test2.pts$class3 <-  as.factor(test2.pts$class3)
  
  #load independent points 
  test3.pts <- st_read("./test_3.gpkg" )
  
  #as df 
  test3.pts <- as.data.frame(test3.pts)
  
  #chr to factor
  test3.pts$class3 <-  as.factor(test3.pts$class3)
  
  
  
  ## loop to test built models 
  for(t in 1:length(model.type.list)){
    
    #specify name of the model type 
    model.type <- model.type.list[t]
    
    #load model 
    caret.model <- readRDS(paste0("./",model.type, ".rds"))
    
    #get cross val metrics 
    caret.model.CV <-  caret::getTrainPerf(caret.model)
    
    #model tuning  parameters 
    caret.model.tune <- caret.model$bestTune
    
        #predict for train points 
    #STDE
    STDE_train_pred <- predict(caret.model, newdata = STDE.train.pts)
    
    #assess goodness of fit 
    STDE.train.goof <- goofcat( observed = STDE.train.pts$class3, predicted = STDE_train_pred)
    STDE.train.goof
    
    # save full goof
    saveRDS(STDE.train.goof, file =  paste0("./STDEtraingoof.rds"))
    
    #SWCU
    SWCU_train_pred <- predict(caret.model, newdata = SWCU.train.pts)
    
    #assess goodness of fit 
    SWCU.train.goof <- goofcat( observed = SWCU.train.pts$class3, predicted = SWCU_train_pred)
    SWCU.train.goof
    
    # save full goof
    saveRDS(SWCU.train.goof, file =  paste0("./SWCUtraingoof.rds"))
    
    #SMCR
    SMCR_train_pred <- predict(caret.model, newdata = SMCR.train.pts)
    
    #assess goodness of fit 
    SMCR.train.goof <- goofcat( observed = SMCR.train.pts$class3, predicted = SMCR_train_pred)
    SMCR.train.goof
    
    # save full goof
    saveRDS(SMCR.train.goof, file =  paste0("./SMCRtraingoof.rds"))
    
    
    ### TEST SET 1 
    #predict for test points
    test1_pred <- predict(caret.model, newdata = test1.pts)
    
    #assess goodness of fit 
    goof1 <- goofcat( observed = test1.pts$class3, predicted = test1_pred)
    goof1
    
    # save full goof
    saveRDS(goof1, file =  paste0("./",Test_set_1,".goof.rds"))
    
    # get goof users and producers accuracy 
    goof1.prod <- goof1$producers_accuracy
    goof1.user <- goof1$users_accuracy
    
    
    ### TEST SET 2 
    #predict for test points
    test2_pred <- predict(caret.model, newdata = test2.pts)
    
    #assess goodness of fit 
    goof2 <- goofcat( observed = test2.pts$class3, predicted = test2_pred)
    goof2
    
    # save full goof
    saveRDS(goof2, file =  paste0("./",Test_set_2,".goof.rds"))
    
    # get goof users and producers accuracy 
    goof2.prod <- goof2$producers_accuracy
    goof2.user <- goof2$users_accuracy
    
    
    ### TEST SET 3
    #predict for test points
    test3_pred <- predict(caret.model, newdata = test3.pts)
    
    #assess goodness of fit 
    goof3 <- goofcat( observed = test3.pts$class3, predicted = test3_pred)
    goof3
    
    # save full goof
    saveRDS(goof3, file =  paste0("./",Test_set_3,".goof.rds"))
    
    # get goof users and producers accuracy 
    goof3.prod <- goof3$producers_accuracy
    goof3.user <- goof3$users_accuracy
    
    ## RESULTS TABLE
    # gather info to fill the table 
    Model_type <- model.type
    Resolution <- res
    
    cross_val_acc <- caret.model.CV[1]
    cross_val_kappa <- caret.model.CV[2]
    
    num_features <- readRDS("./num_feats.rds") 
    bestTune_param <- colnames(caret.model.tune[1])  
    bestTune_value <-  caret.model.tune[1,1] 
    
    final_model_internal_acc_STDE <- STDE.train.goof$overall_accuracy 
    final_model_internal_kappa_STDE <- STDE.train.goof$kappa 
    final_model_internal_acc_SWCU <- SWCU.train.goof$overall_accuracy 
    final_model_internal_kappa_SWCU <- SWCU.train.goof$kappa 
    final_model_internal_acc_SMCR <- SMCR.train.goof$overall_accuracy 
    final_model_internal_kappa_SMCR <- SMCR.train.goof$kappa 
    
    ## test 1 
    ind_val_1_acc <- goof1$overall_accuracy
    ind_val_1_kappa <- goof1$kappa
    
    ind_val_1_class1 <- names(goof1.prod[1])
    ind_val_1_class1_prod_acc <- goof1.prod[[1]]
    ind_val_1_class1_user_acc <- goof1.user[[1]]
    
    ind_val_1_class2 <- names(goof1.prod[2])
    ind_val_1_class2_prod_acc <- goof1.prod[[2]]
    ind_val_1_class2_user_acc <- goof1.user[[2]]

    ind_val_1_class3 <- names(goof1.prod[3])
    ind_val_1_class3_prod_acc <- goof1.prod[[3]]
    ind_val_1_class3_user_acc <- goof1.user[[3]]
    
    ## test 2
    ind_val_2_acc <- goof2$overall_accuracy
    ind_val_2_kappa <- goof2$kappa
    
    ind_val_2_class1 <- names(goof2.prod[1])
    ind_val_2_class1_prod_acc <- goof2.prod[[1]]
    ind_val_2_class1_user_acc <- goof2.user[[1]]
    
    ind_val_2_class2 <- names(goof2.prod[2])
    ind_val_2_class2_prod_acc <- goof2.prod[[2]]
    ind_val_2_class2_user_acc <- goof2.user[[2]]
    # 
    # ## No CW observations in this test set - BROWN UNDRAINED 
    # ind_val_2_class3 <- names(goof2.prod[3])
    # ind_val_2_class3_prod_acc <- goof2.prod[[3]]
    # ind_val_2_class3_user_acc <- goof2.user[[3]]
    # 
    
    ## test 3 
    ind_val_3_acc <- goof3$overall_accuracy
    ind_val_3_kappa <- goof3$kappa
    
    ind_val_3_class1 <- names(goof3.prod[1])
    ind_val_3_class1_prod_acc <- goof1.prod[[1]]
    ind_val_3_class1_user_acc <- goof1.user[[1]]
    
    ind_val_3_class2 <- names(goof1.prod[2])
    ind_val_3_class2_prod_acc <- goof1.prod[[2]]
    ind_val_3_class2_user_acc <- goof1.user[[2]]
    
    ind_val_3_class3 <- names(goof1.prod[3])
    ind_val_3_class3_prod_acc <- goof1.prod[[3]]
    ind_val_3_class3_user_acc <- goof1.user[[3]]
    
    #fill table with info and results 
    metrics.table[1,] <- c(Train_data , 
                           Model_type, 
                           Resolution ,    
                           
                           cross_val_acc,
                           cross_val_kappa,
                           
                           num_features,
                           bestTune_param,
                           bestTune_value ,
                           
                           final_model_internal_acc_STDE,
                           final_model_internal_kappa_STDE ,
                           final_model_internal_acc_SWCU ,
                           final_model_internal_kappa_SWCU ,
                           final_model_internal_acc_SMCR ,
                           final_model_internal_kappa_SMCR ,
                           
                           #test set 1 
                           
                           Test_set_1,
                           
                           ind_val_1_acc,
                           ind_val_1_kappa,
                           
                           ind_val_1_class1 ,
                           ind_val_1_class1_prod_acc ,
                           ind_val_1_class1_user_acc ,
                           
                           ind_val_1_class2 ,
                           ind_val_1_class2_prod_acc ,
                           ind_val_1_class2_user_acc ,
                           
                           ind_val_1_class3 ,
                           ind_val_1_class3_prod_acc,
                           ind_val_1_class3_user_acc ,
                           
                           #test set 2
                           
                           Test_set_2,
                           
                           ind_val_2_acc,
                           ind_val_2_kappa,
                           
                           ind_val_2_class1 ,
                           ind_val_2_class1_prod_acc ,
                           ind_val_2_class1_user_acc ,
                           
                           ind_val_2_class2 ,
                           ind_val_2_class2_prod_acc ,
                           ind_val_2_class2_user_acc ,
                           
                           # ind_val_2_class3 ,
                           # ind_val_2_class3_prod_acc,
                           # ind_val_2_class3_user_acc
                           
                           #test set 3
                           
                           Test_set_3,
                           
                           ind_val_3_acc,
                           ind_val_3_kappa,
                           
                           ind_val_3_class1 ,
                           ind_val_3_class1_prod_acc ,
                           ind_val_3_class1_user_acc ,
                           
                           ind_val_3_class2 ,
                           ind_val_3_class2_prod_acc ,
                           ind_val_3_class2_user_acc, 
                           
                           ind_val_3_class3 ,
                           ind_val_3_class3_prod_acc,
                           ind_val_3_class3_user_acc
    )
    
    write.csv(metrics.table, file= paste0( "./",Train_data,"_", model.type,"_", res, "m_metrics.csv" ) )
    
    assign(x = paste0( Train_data,"_", res,"m_", model.type, "_metrics" ), value = metrics.table )
    
  }}



# combine results from this model set 
All_sites_w_centers_metrics_table  <- rbind( All_sites_w_centers_2m_ranger_metrics,
                              All_sites_w_centers_2m_rpart_metrics,
                              All_sites_w_centers_2m_multinom_metrics,
                              All_sites_w_centers_5m_ranger_metrics,
                              All_sites_w_centers_5m_rpart_metrics,
                              All_sites_w_centers_5m_multinom_metrics,
                              All_sites_w_centers_10m_ranger_metrics,
                              All_sites_w_centers_10m_rpart_metrics,
                              All_sites_w_centers_10m_multinom_metrics)


rm( All_sites_w_centers_2m_ranger_metrics,
       All_sites_w_centers_2m_rpart_metrics,
       All_sites_w_centers_2m_multinom_metrics,
       All_sites_w_centers_5m_ranger_metrics,
       All_sites_w_centers_5m_rpart_metrics,
       All_sites_w_centers_5m_multinom_metrics,
       All_sites_w_centers_10m_ranger_metrics,
       All_sites_w_centers_10m_rpart_metrics,
       All_sites_w_centers_10m_multinom_metrics)



## save 
setwd(wd)
write.csv(All_sites_w_centers_metrics_table, file=paste0("./",Train_data,"/",Train_data,"_metrics_table.csv" ))





### All Sites no centers 
Train_data <- "All_sites_no_centers"
Test_set_1 <- "STDE_BIOCAP"
Test_set_2 <- "SMCR_BROWN_UNDRAINED"
Test_set_3 <- "SMCR_BROWN_DRAINED"

# create table for saving results 
metrics.table <- data.frame(Train_data = character(),
                            Model_type = character(),
                            Resolution = character(),
                            cross_val_acc = double(),
                            cross_val_kappa = double(),
                            num_features = integer(),
                            bestTune_param = character(),
                            bestTune_value = character(),
                            
                            final_model_internal_acc_STDE = double(),
                            final_model_internal_kappa_STDE = double(),
                            final_model_internal_acc_SWCU = double(),
                            final_model_internal_kappa_SWCU = double(),
                            final_model_internal_acc_SMCR = double(),
                            final_model_internal_kappa_SMCR = double(),
                            
                            #test set 1
                            Test_set_1 = character(),
                            ind_val_1_acc = double(),
                            ind_val_1_kappa = double(),
                            ind_val_1_class1 = character(),
                            ind_val_1_class1_prod_acc = double(),
                            ind_val_1_class1_user_acc = double(),
                            
                            ind_val_1_class2 = character(),
                            ind_val_1_class2_prod_acc = double(),
                            ind_val_1_class2_user_acc = double(),
                            
                            
                            ind_val_1_class3 = character(),
                            ind_val_1_class3_prod_acc = double(),
                            ind_val_1_class3_user_acc = double(),
                            
                            
                            #test set 2
                            Test_set_2 = character(),
                            ind_val_2_acc = double(),
                            ind_val_2_kappa = double(),
                            ind_val_2_class1 = character(),
                            ind_val_2_class1_prod_acc = double(),
                            ind_val_2_class1_user_acc = double(),
                            
                            ind_val_2_class2 = character(),
                            ind_val_2_class2_prod_acc = double(),
                            ind_val_2_class2_user_acc = double(),
                            
                            # #No CW observation in BROWN UNDRAINED 
                            # ind_val_2_class3 = character(),
                            # ind_val_2_class3_prod_acc = double(),
                            # ind_val_2_class3_user_acc = double(),
                            
                            #test set 3
                            Test_set_3 = character(),
                            ind_val_3_acc = double(),
                            ind_val_3_kappa = double(),
                            ind_val_3_class1 = character(),
                            ind_val_3_class1_prod_acc = double(),
                            ind_val_3_class1_user_acc = double(),
                            
                            ind_val_3_class2 = character(),
                            ind_val_3_class2_prod_acc = double(),
                            ind_val_3_class2_user_acc = double(),
                            
                            ind_val_3_class3 = character(),
                            ind_val_3_class3_prod_acc = double(),
                            ind_val_3_class3_user_acc = double()
                            
)





## loop per resolution 
for(x in 1:length(res.list)){
  
  
  # set working resolution    
  res <- res.list[x]
  
  # set wd to resolution 
  setwd(wd)
  setwd(paste0("./",Train_data,"/",res,"m/" ) )
  
  # load internal training points 
  train.pts <- st_read("./train.gpkg")
  
  # as df 
  train.pts <- as.data.frame( train.pts )
  
  # chr to factor 
  train.pts$class3 <- as.factor(train.pts$class3)
  
  # split into sites 
  STDE.train.pts <- train.pts %>% filter(set == "STDE_LWBSF" )
  SWCU.train.pts <- train.pts %>% filter(set == "SWCU_LWBSF" )
  SMCR.train.pts <- train.pts %>% filter(set == "SMCR_LWBSF" )
  
  # load independent points 
  test1.pts <- st_read("./test_1.gpkg" )
  
  # as df 
  test1.pts <- as.data.frame(test1.pts)
  
  # chr to factor
  test1.pts$class3 <-  as.factor(test1.pts$class3)
  
  # load independent points 
  test2.pts <- st_read("./test_2.gpkg" )
  
  # as df 
  test2.pts <- as.data.frame(test2.pts)
  
  # chr to factor
  test2.pts$class3 <-  as.factor(test2.pts$class3)
   
  # load independent points 
  test3.pts <- st_read("./test_3.gpkg" )
  
  # as df 
  test3.pts <- as.data.frame(test3.pts)
  
  # chr to factor
  test3.pts$class3 <-  as.factor(test3.pts$class3)
  
  
  
  ## loop to test built models 
  for(t in 1:length(model.type.list)){
    
    # specify name of the model type 
    model.type <- model.type.list[t]
    
    # load model 
    caret.model <- readRDS(paste0("./",model.type, ".rds"))
    
    # get cross val metrics 
    caret.model.CV <-  caret::getTrainPerf(caret.model)
    
    # model tuning  parameters 
    caret.model.tune <- caret.model$bestTune
    
    
    
    # predict for train points 
    # STDE
    STDE_train_pred <- predict(caret.model, newdata = STDE.train.pts)
    
    # assess goodness of fit 
    STDE.train.goof <- goofcat( observed = STDE.train.pts$class3, predicted = STDE_train_pred)
    STDE.train.goof
    
    # save full goof
    saveRDS(STDE.train.goof, file =  paste0("./STDEtraingoof.rds"))
    
    # SWCU
    SWCU_train_pred <- predict(caret.model, newdata = SWCU.train.pts)
    
    # assess goodness of fit 
    SWCU.train.goof <- goofcat( observed = SWCU.train.pts$class3, predicted = SWCU_train_pred)
    SWCU.train.goof
    
    # save full goof
    saveRDS(SWCU.train.goof, file =  paste0("./SWCUtraingoof.rds"))
    
    # SMCR
    SMCR_train_pred <- predict(caret.model, newdata = SMCR.train.pts)
    
    # assess goodness of fit 
    SMCR.train.goof <- goofcat( observed = SMCR.train.pts$class3, predicted = SMCR_train_pred)
    SMCR.train.goof
    
    # save full goof
    saveRDS(SMCR.train.goof, file =  paste0("./SMCRtraingoof.rds"))
    
    
    
    
    ### TEST SET 1 
    # predict for test points
    test1_pred <- predict(caret.model, newdata = test1.pts)
    
    # assess goodness of fit 
    goof1 <- goofcat( observed = test1.pts$class3, predicted = test1_pred)
    goof1
    
    # save full goof
    saveRDS(goof1, file =  paste0("./",Test_set_1,".goof.rds"))
    
    # get goof users and producers accuracy 
    goof1.prod <- goof1$producers_accuracy
    goof1.user <- goof1$users_accuracy
    
    
    ### TEST SET 2 
    # predict for test points
    test2_pred <- predict(caret.model, newdata = test2.pts)
    
    # assess goodness of fit 
    goof2 <- goofcat( observed = test2.pts$class3, predicted = test2_pred)
    goof2
    
    # save full goof
    saveRDS(goof2, file =  paste0("./",Test_set_2,".goof.rds"))
    
    # get goof users and producers accuracy 
    goof2.prod <- goof2$producers_accuracy
    goof2.user <- goof2$users_accuracy
    
    
    ### TEST SET 3
    # predict for test points
    test3_pred <- predict(caret.model, newdata = test3.pts)
    
    # assess goodness of fit 
    goof3 <- goofcat( observed = test3.pts$class3, predicted = test3_pred)
    goof3
    
    # save full goof
    saveRDS(goof3, file =  paste0("./",Test_set_3,".goof.rds"))
    
    # get goof users and producers accuracy 
    goof3.prod <- goof3$producers_accuracy
    goof3.user <- goof3$users_accuracy
    
    ## RESULTS TABLE
    
    # gather info to fill the table 
    Model_type <- model.type
    Resolution <- res
    
    cross_val_acc <- caret.model.CV[1]
    cross_val_kappa <- caret.model.CV[2]
    
    num_features <- readRDS("./num_feats.rds") 
    bestTune_param <- colnames(caret.model.tune[1])  
    bestTune_value <-  caret.model.tune[1,1] 
    
    final_model_internal_acc_STDE <- STDE.train.goof$overall_accuracy 
    final_model_internal_kappa_STDE <- STDE.train.goof$kappa 
    final_model_internal_acc_SWCU <- SWCU.train.goof$overall_accuracy 
    final_model_internal_kappa_SWCU <- SWCU.train.goof$kappa 
    final_model_internal_acc_SMCR <- SMCR.train.goof$overall_accuracy 
    final_model_internal_kappa_SMCR <- SMCR.train.goof$kappa 
    
    ## test 1 
    ind_val_1_acc <- goof1$overall_accuracy
    ind_val_1_kappa <- goof1$kappa
    
    ind_val_1_class1 <- names(goof1.prod[1])
    ind_val_1_class1_prod_acc <- goof1.prod[[1]]
    ind_val_1_class1_user_acc <- goof1.user[[1]]
    
    ind_val_1_class2 <- names(goof1.prod[2])
    ind_val_1_class2_prod_acc <- goof1.prod[[2]]
    ind_val_1_class2_user_acc <- goof1.user[[2]]
    
    ind_val_1_class3 <- names(goof1.prod[3])
    ind_val_1_class3_prod_acc <- goof1.prod[[3]]
    ind_val_1_class3_user_acc <- goof1.user[[3]]
    
    ## test 2
    ind_val_2_acc <- goof2$overall_accuracy
    ind_val_2_kappa <- goof2$kappa
    
    ind_val_2_class1 <- names(goof2.prod[1])
    ind_val_2_class1_prod_acc <- goof2.prod[[1]]
    ind_val_2_class1_user_acc <- goof2.user[[1]]
    
    ind_val_2_class2 <- names(goof2.prod[2])
    ind_val_2_class2_prod_acc <- goof2.prod[[2]]
    ind_val_2_class2_user_acc <- goof2.user[[2]]
    # 
    # ## No CW observations in this test set - BROWN UNDRAINED 
    # ind_val_2_class3 <- names(goof2.prod[3])
    # ind_val_2_class3_prod_acc <- goof2.prod[[3]]
    # ind_val_2_class3_user_acc <- goof2.user[[3]]
    # 
    
    ## test 3 
    ind_val_3_acc <- goof3$overall_accuracy
    ind_val_3_kappa <- goof3$kappa
    
    ind_val_3_class1 <- names(goof3.prod[1])
    ind_val_3_class1_prod_acc <- goof1.prod[[1]]
    ind_val_3_class1_user_acc <- goof1.user[[1]]
    
    ind_val_3_class2 <- names(goof1.prod[2])
    ind_val_3_class2_prod_acc <- goof1.prod[[2]]
    ind_val_3_class2_user_acc <- goof1.user[[2]]
    
    ind_val_3_class3 <- names(goof1.prod[3])
    ind_val_3_class3_prod_acc <- goof1.prod[[3]]
    ind_val_3_class3_user_acc <- goof1.user[[3]]
    
    
    # fill table with info and results 
    metrics.table[1,] <- c(Train_data , 
                           Model_type, 
                           Resolution ,    
                           
                           cross_val_acc,
                           cross_val_kappa,
                           
                           num_features,
                           bestTune_param,
                           bestTune_value ,
                           
                           final_model_internal_acc_STDE,
                           final_model_internal_kappa_STDE ,
                           final_model_internal_acc_SWCU ,
                           final_model_internal_kappa_SWCU ,
                           final_model_internal_acc_SMCR ,
                           final_model_internal_kappa_SMCR ,
                           
                           #test set 1 
                           Test_set_1,
                           
                           ind_val_1_acc,
                           ind_val_1_kappa,
                           
                           ind_val_1_class1 ,
                           ind_val_1_class1_prod_acc ,
                           ind_val_1_class1_user_acc ,
                           
                           ind_val_1_class2 ,
                           ind_val_1_class2_prod_acc ,
                           ind_val_1_class2_user_acc ,
                           
                           ind_val_1_class3 ,
                           ind_val_1_class3_prod_acc,
                           ind_val_1_class3_user_acc ,
                           
                           #test set 2
                           Test_set_2,
                           
                           ind_val_2_acc,
                           ind_val_2_kappa,
                           
                           ind_val_2_class1 ,
                           ind_val_2_class1_prod_acc ,
                           ind_val_2_class1_user_acc ,
                           
                           ind_val_2_class2 ,
                           ind_val_2_class2_prod_acc ,
                           ind_val_2_class2_user_acc ,
                           
                           # ind_val_2_class3 ,
                           # ind_val_2_class3_prod_acc,
                           # ind_val_2_class3_user_acc
                           
                           #test set 3
                           Test_set_3,
                           
                           ind_val_3_acc,
                           ind_val_3_kappa,
                           
                           ind_val_3_class1 ,
                           ind_val_3_class1_prod_acc ,
                           ind_val_3_class1_user_acc ,
                           
                           ind_val_3_class2 ,
                           ind_val_3_class2_prod_acc ,
                           ind_val_3_class2_user_acc, 
                           
                           ind_val_3_class3 ,
                           ind_val_3_class3_prod_acc,
                           ind_val_3_class3_user_acc
    )
    
    
    write.csv(metrics.table, file= paste0( "./",Train_data,"_", model.type,"_", res, "m_metrics.csv" ) )
    
    assign(x = paste0( Train_data,"_", res,"m_", model.type, "_metrics" ), value = metrics.table )
    
  }}



#combine results from this model set 
All_sites_no_centers_metrics_table  <- rbind( All_sites_no_centers_2m_ranger_metrics,
                                             All_sites_no_centers_2m_rpart_metrics,
                                             All_sites_no_centers_2m_multinom_metrics,
                                             All_sites_no_centers_5m_ranger_metrics,
                                             All_sites_no_centers_5m_rpart_metrics,
                                             All_sites_no_centers_5m_multinom_metrics,
                                             All_sites_no_centers_10m_ranger_metrics,
                                             All_sites_no_centers_10m_rpart_metrics,
                                             All_sites_no_centers_10m_multinom_metrics)


rm( All_sites_no_centers_2m_ranger_metrics,
       All_sites_no_centers_2m_rpart_metrics,
       All_sites_no_centers_2m_multinom_metrics,
       All_sites_no_centers_5m_ranger_metrics,
       All_sites_no_centers_5m_rpart_metrics,
       All_sites_no_centers_5m_multinom_metrics,
       All_sites_no_centers_10m_ranger_metrics,
       All_sites_no_centers_10m_rpart_metrics,
       All_sites_no_centers_10m_multinom_metrics)



## save 
setwd(wd)
write.csv(All_sites_no_centers_metrics_table, file=paste0("./",Train_data,"/",Train_data,"_metrics_table.csv" ))




## Create master result sheet 


master.result.table <- rbind.fill( STDE_w_centers_metrics_table,
                                   STDE_no_centers_metrics_table,
                                   SWCU_metrics_table,
                                   SMCR_metrics_table,
                                   All_sites_w_centers_metrics_table, 
                                   All_sites_no_centers_metrics_table)



# export 
write.csv(master.result.table, file=paste0("./master_results_table.csv" ),row.names = F)

# load 
master.result.table <- read.csv( file=paste0("./master_results_table.csv" ))


### add fields to describe model parameters 
# Is model generalized or site-specific
master.result.table <- master.result.table %>% mutate(general = ifelse(Train_data == "All_sites_w_centers" |
                                                                         Train_data == "All_sites_no_centers" , "generalized", "site-specific"  ) )


# need to group models to compare those that used the supplementary STDE_wetland_centers data set and those that did not
# The SMCR and SWCU models need to be included in both supp in and no supp models 
# make copies of those models 
suppIn.result.table <- master.result.table %>% filter(Train_data == "STDE_w_centers" |
                                                        Train_data == "SWCU" |
                                                        Train_data == "SMCR" |
                                                        Train_data =="All_sites_w_centers")

noSupp.result.table <- master.result.table %>% filter(Train_data == "STDE_no_centers" |
                                                               Train_data == "SWCU" |
                                                               Train_data == "SMCR" |
                                                               Train_data =="All_sites_no_centers")
                                                        
# specify if the model group had supplemenetal training data used 
suppIn.result.table$supp_data_in_train <- "supp-in"
noSupp.result.table$supp_data_in_train <- "no-supp"

# rbind to create master table 
master.result.table <- rbind( suppIn.result.table, noSupp.result.table)

# create unique code for each model 
master.result.table <- master.result.table %>% mutate(model_code = paste(Resolution, supp_data_in_train, general, Model_type  , sep = "_" )  )

# reorder col 
master.result.table <- master.result.table %>% select( model_code, Resolution, supp_data_in_train, general, Model_type, Train_data, num_features, bestTune_param, bestTune_value, everything() )

# separate generalized and site-specific models 
gen.results.table <- master.result.table %>% filter(general == "generalized" )
ss.results.table <- master.result.table %>% filter(general == "site-specific")  

# calculate average cross-validation error for site-specific results 
ss.mean.cross.val <- ss.results.table %>% dplyr::group_by(model_code) %>% dplyr::summarise(allsite_cross_val_acc = mean(cross_val_acc ),
                                                                            allsite_cross_val_kappa = mean(cross_val_kappa ))

# merge into site-specific table
ss.results.table <- merge(x = ss.results.table, y= ss.mean.cross.val, by= "model_code" )

# create columns for allsite cross validation in the generalized models 
gen.results.table$allsite_cross_val_acc <- gen.results.table$cross_val_acc
gen.results.table$allsite_cross_val_kappa <- gen.results.table$cross_val_kappa

# relocate mean cross val
ss.results.table <- ss.results.table %>% relocate(allsite_cross_val_acc, .before = Test_set_1 ) %>% relocate(allsite_cross_val_kappa, .before = Test_set_1 )

#merge back with generalized results 
master.result.table <- rbind(ss.results.table, gen.results.table )

### sort 
master.result.table <- master.result.table %>% arrange(Resolution, desc(supp_data_in_train), general,  desc(allsite_cross_val_acc), Train_data)

# export 
write.csv(master.result.table, file=paste0("./master_results_table.csv" ),row.names = F)

## export only the best performing model types for each combination of parameters 

## lazy way to get these by pulling specific rows with a repeating pattern. WILL NOT WORK WITH DIFFERING AMOUNT OF MODELS
best.model.type.table <- master.result.table[c(T,F,F,T,T,T,rep(F,6)),]

# export 
write.csv(best.model.type.table, file=paste0("./best_model_type_results_table.csv" ),row.names = F)

