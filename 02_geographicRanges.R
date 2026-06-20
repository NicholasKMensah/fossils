# Script to assess ecological changes in a quantitative fashion
# Similar to traits 

 setwd("C:/Users/Acer/Desktop/Research implementation project - R") # adjust the path

 library(divDyn)
 library(fields)
 library(chronosphere)
 
 data(stages)
 data(tens)
 age <- stages$mid
 age2 <- tens$mid
 
 
 # Get a recent PBDB download through chronosphere
   load("pdbd_Aug2025.RData") # This file is marine only
    table(dat$class)
   table(dat$stg)
 
      # Number of genera
    length(levels(factor(dat$gen))) # 73k genera all, 37k genera marine only

#####
    # Here would be the place to link to your mode data and such
    mode_data <- read.csv("Research implementation project data.csv", stringsAsFactors = FALSE)
### 
    # This code for computing range sizes
    # This is slow and may take up to 30 minutes
    pbd <- subset(dat, !is.na(dat$paleolat)) # exclude occurrences without paleocoordinates
    

    # Calculate great circle distances and number of occurrences for each species in each bin
    # Genera
    pbd <- subset(pbd, genus %in% mode_data$Genus)  # To ensure that both datasets are aligned
    gens <- levels(factor(pbd$genus))
    gc.dist <- numeric()
    
    for (i in 1:length(gens)) {
      g <- subset(pbd, genus== gens[i])

      coord <- subset(g, select = c(paleolng, paleolat))
      ifelse(nrow(coord)>1, gc.dist[i] <- max(rdist.earth(coord, miles = FALSE, R = NULL), na.rm=T), gc.dist[i] <- 0)
     
      }
      
 # gc.dist is maximum great circle distance of all occurrences of each genus
    results <- data.frame(genus = gens, gc.dist = gc.dist)
    head(results)
    
    
    
    #Wilcoxon rank-sum test (Mann–Whitney U test)
    results <- data.frame(Genus = gens, range_size = gc.dist)
    
    test_data <- merge(results, mode_data, by = "Genus")
    
    wilcox.test(range_size ~ Larval.mode, data = test_data)
    
    names(mode_data)
    