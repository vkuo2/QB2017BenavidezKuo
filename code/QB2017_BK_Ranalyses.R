############################################################
#                                                          #
# QB-group project                                         #
# Diversity of BCI in response to xxx                      #
# Written by Venus Kuo and Michelle Benavidez              #
#                                                          #
# 01/19/2017                                               #
#                                                          #
############################################################
#                                                          #
# Notes                                                    #
#                                                          #
#                                                          #
############################################################
## Setup work enviroment ##
rm(list = ls())
setwd("C:/Users/Venus/Github/QB2017BenavidezKuo/data")

## Load packages ##
require("vegan")  
require("tidyr")
require(dplyr)

## Load dataset ##
# From BCI, Panama from year 2000 #
BCI <- read.delim("BCI2010.txt", header=T)

## Transform census data into site by species matrix ##
BCI.SbyS <- group_by(BCI, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)

# Slim down BCI data frame #
BCI.dat <- subset(BCI, select = c(Latin, Quadrat, PX, PY))
# Manually subsetting # 
dat <- subset(BCI, Quadrat ==0 , select = c(Latin, Quadrat, PX, PY))

# Randomly sampling one individual or row #
# In <- dat[sample(nrow(dat), 1), ]

# Define List for Distance and Species # 
DistList <- list()
SpList <- list()

  # find x and y
  # use distance formula to get Euclidean distance
  # add the distance to a list of distances
  # add spID to another list (elements in SpList and DistList correspond)

# For loop to determine closest 4 species around sampled individual #


# Sample indivudals from BCI #
ilist <- dat[sample(nrow(dat), size = 100, replace = FALSE), ]

# For loop to sample 1 individual and nearest neighbors # 
main.dat <- NA
nrows.1 <- nrow(ilist)
for(i in 1:nrows.1) {
  x1 <- as.numeric(ilist[i,]["PX"])
  y1 <- as.numeric(ilist[i,]["PY"])
  spID <- ilist[i,]["Latin"]
  z <- cbind(spID, x1, y1)
  
  zz.dat <- NA
  
  nrows <- nrow(dat)
  for(j in 1:nrows) {
    
      x2 <- as.numeric(dat[j,]["PX"])
      y2 <- as.numeric(dat[j,]["PY"])
      spID2 <- dat[j,]["Latin"]
      
      #print(x2)
      #print(y2)
      #print(spID2)
      #break
      
      dist <- sqrt(((x1-x2)^2)+((y1-y2)^2))
      zz <- paste(spID2, x2, y2, dist)
      zz.dat <- rbind(zz)
    }
  
  print(zz.dat)
  break()
  
}

















