## VARIABLE IMPORTANCE PLOT
## Author: Jeremy Kiss
## Date: 2022-01-25

### determine covariate importance across best random forest models 

wd <- "D:/wd/"
setwd(wd)

library(plyr)
library(dplyr)

# get all var names 
cov.path <- "./sites/STDE/covar/2m/TIFFS"
cov.list <- gsub("\\.tif$","", list.files(path = cov.path , pattern="\\.tif$"))

# build empty df
df <- data.frame(matrix(ncol = 32, nrow = 0) )
#add column for model name 
colnames(df) <- cov.list

# list of model training sets - not including models without supplemental data 
model.train.list <- c( "All_sites_w_centers",  "STDE_w_centers",  "SWCU", "SMCR")

#res list
res.list <- c(2,5,10)

#loop through models and resolutions 
for(m in 1:length(model.train.list)){
  
  model.train <- model.train.list[m]
  
  #loop through resolution
  for(x in 1:length(res.list)){
    
    res <- res.list[x]
    
    # load ranger model
    ranger.model <- readRDS(paste0("./models/", model.train, "/", res, "m/ranger.rds") )
    
    # get covar  importance 
    v.imp <- ranger.model$finalModel$variable.importance
    
    # convert to df 
    v.imp.df <- as.data.frame(v.imp)
    v.imp.df <- data.frame(as.list(v.imp))

    # rbind fill missing with NAs
    df <- rbind.fill(df, v.imp.df )
    
  }}

# sum across each model 
imp.sum  <- colSums(df, na.rm = T)
varnames <- names(imp.sum)
Importance <- unname(imp.sum)

#create df 
imp.df <- cbind(data.frame(varnames), data.frame(Importance))

# add long names    
#import lookup table 
name.lookup <- read.csv("./impdf_w_fullnames.csv")

#merge 
imp.df <- merge(x= imp.df, y = name.lookup, by= "varnames")

## create plot using code from https://stackoverflow.com/questions/52200095/how-to-customize-the-importance-plot-generated-by-package-randomforest/52200505

library(ggplot2) 

var.imp.plot <- ggplot(imp.df, aes(x=reorder(fullname, Importance), y=Importance)) + 
  geom_point() +
  geom_segment(aes(x=fullname,xend=fullname,y=0,yend=Importance)) +
  ylab("Importance (Gini Index summed)") +
  xlab("Variable Name") +
  coord_flip() +
  theme(text = element_text(family="arial"))

var.imp.plot 

# export eps
ggsave(var.imp.plot, 
       file= "./models/var_imp_plot_arial.eps", 
       device = "eps")

# export jpeg
ggsave(var.imp.plot, 
       file= "./models/var_imp_plot_arial.jpeg", 
       device = "jpeg")

