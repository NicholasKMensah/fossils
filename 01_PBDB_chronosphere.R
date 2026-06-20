library(chronosphere)

pbdb <- fetch("pbdb")
attributes(pbdb)$chronosphere$API


 # keep only relevant fields 
  sel <- c("occurrence_no", "collection_no", "collection_name", "cc", "identified_rank", "identified_name",  
          "accepted_rank", "accepted_name", "accepted_rank", 
          "early_interval", "late_interval", "max_ma", "min_ma", "reference_no", 
          "phylum", "class", "order", "family", "genus", 
          "lng", "lat", "geogscale", "cc", "paleomodel", "paleolng", "paleolat", "localsection",
           "formation", "zone", "lithology1", "lithification1", "environment",
          "ecospace_comments", "composition", "motility", "life_habit", "diet",
          "abund_value", "abund_unit")
  
  dat <- subset(pbdb, select=sel)
  
  
  # quick count of nominal species (and different taxa)
  length(unique(dat$identified_name))
  specs <- subset(dat, identified_rank=="species")
    length(unique(specs$identified_name))
  
  #### for divdyn analysis at the species level scroll down to "assign species" without filter ####
    
  # Only if focus is marine ###
  setwd("C:/Users/Acer/Desktop/Research implementation project - R")
     #f
    source("FilterMarine.R")
  
  dat <- subset(dat, accepted_rank!="")
  dat <- subset(dat, genus!="") # consider to also allow family-level ids
  dat <- subset(dat, life_habit!="amphibious") # remove toads and frogs
  
  # Consider to also remove questionable classification
  
  
  # assign stages
  library(divDyn)
  data(stages)
  data(tens)
  data(keys)
  
  # this for analyses across environments at species level
  dat <- specs
  
  
  # the 'stg' entries (lookup)
  stgMin <- categorize(dat[ ,"early_interval"], keys$stgInt)
  stgMax <- categorize(dat[ ,"late_interval"], keys$stgInt)
  # convert to numeric
  stgMin <- as.numeric(stgMin)
  stgMax <- as.numeric(stgMax)
  
  # empty container
  dat$stg <- rep(NA, nrow(dat)) 
  
  # select entries, where
  stgCondition <- c(
    # the early and late interval fields indicate the same stg
    which(stgMax==stgMin),
    # or the late_intervar field is empty
    which(stgMax==-1))
  # in these entries, use the stg indicated by the early_interval
  dat$stg[stgCondition] <- stgMin[stgCondition] 
  
  load(url(
    "https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))
  
  source(
    "https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")
  
  load(url(
    "https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))
  
  source(
    "https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R")
  
  
  
  ### do the ten million year resolution here ###
  # names of the bins
  colnames(tens)[colnames(tens)=="X10"]<-"name"
  data(stratkeys) 
  data(keys) 
  
  # Assign bins
  tenMin<-categorize(dat[,"early_interval"],keys$tenInt)
  tenMax<-categorize(dat[,"late_interval"],keys$tenInt) 
  
  tenMin<-as.numeric(tenMin)
  tenMax<-as.numeric(tenMax) 
  
  dat$ten <- rep(NA, nrow(dat)) 
  # select entries, where
  tenCondition <- c(
    # the early and late interval fields indicate the same stg
    which(tenMax==tenMin),
    # or the late_intervar field is empty
    which(tenMax==-1))
  # in these entries, use the ten indicated by the early_interval
  dat$ten[tenCondition] <- tenMin[tenCondition] 
  
  # table(dat$ten)
  
  
  # Quick and dirty diversity analysis
 res <- divDyn(dat, tax="identified_name", bin="stg")
  
 # genus level curves (range-through seems to be with singletons)
 #X(11)
 tsplot(stages, shading="system", boxes="sys", xlim=c(535,0), 
        ylab="range-through diversity (species)", ylim=c(0,15500))
 lines(stages$mid, res$divRT, col="red", lwd=2)

  res <- cbind(stages$mid, res)
  colnames(res)[1] <- "age"
 
 # fit an exponential curve
 mod <- lm(divRT ~ poly(age, 2), data=res)
 newdat <- seq(1, 540, length.out = 540)
 new_dat <- data.frame(age = newdat)
 pred1 <- predict(mod, newdata = new_dat)
  lines(pred1, lwd=3, col="red")
  
    # save as R Data file
  setwd("C:/Users/Acer/Desktop/Research implementation project - R")

  
# Assign habitats
 dat$env <- categorize(dat$environment,keys$reef)
 dat$bath <- categorize(dat$environment,keys$depenv)
 dat$lith <- categorize(dat$lithology1,keys$lith) 
  
  
  
  
  
  ######   Save the output ####
  
   save(dat, file="pdbd_Aug2025.RData")  

