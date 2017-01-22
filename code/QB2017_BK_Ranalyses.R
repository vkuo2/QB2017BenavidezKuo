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
setwd("C:/Users/Venus/Github/QB2017BenavidezKuo/")

# Load packages #
require("vegan")  #Already contains BCI data
require("BiodiversityR") #Contains more information on BCI dataset: long/lat/etc

# Load BCI dataset #
data(BCI) # Only for 1 year it seems 
dim(BCI)
View(BCI)







