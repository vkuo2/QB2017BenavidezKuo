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
# Setup work enviroment #
rm(list = ls())
setwd("C:/Users/Venus/Github/QB2017BenavidezKuo/data")

# Load packages #
require("vegan")  #Already contains BCI data
require("tidyr")
require(dplyr)

# Load dataset #
BCI2010 <- read.delim("BCI2010.txt", header=T)
BCI1982 <- read.delim("BCI1982.txt", header=T)

# Transform 2010 census data into site by species matrix #
BCI.2010.SbyS <- group_by(BCI2010, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)

# Transform 1982 census data # 
BCI.1982.SbyS <- group_by(BCI1982, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)












