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
BCI <- read.delim("BCI2000.txt", header=T)
# From Huai Kha Khaeng, Thailand from year 1999 #
HKK <- read.delim(".txt", header=T)
# From Korup, Cameroon from year 1998 # 
Korup <- read.delim(".txt", header=T)
# From Lambir, Malaysia from year 1997 #
Lambir <- read.delim(".txt", header=T)


## Transform census data into site by species matrix ##
# BCI #
BCI.SbyS <- group_by(BCI, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
# HKK #
HKK.SbyS <- group_by(HKK, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
# Korup #
Korup.SbyS <- group_by(Korup, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
# Lambir #
Lambir.SbyS <- group_by(Lambir, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)


## Bind SbySs into one ##
SbS <- rbind(BCI.SbyS, HKK.SbyS, Korup.SbyS)


## Alpha Diversity ## 
# Species richness function #
S.obs <- function(x=""){
  rowSums(x>0) *1 
}

# Coverage function #
C <- function(x = ""){
  1 - (sum(x == 1) / rowSums(x)) }


# Rarefaction curve # 
# Calculating observed richness #
richness <- S.obs(P.SbyS)
# Smallest value #
min.N <- min(rowSums(BCI.2010.SbyS))
# Rarefy each sample #
S.rarefy <- rarefy(x = BCI.2010.SbyS, sample = min.N, se = TRUE) 
# Plot rarefaction results
rarecurve(x = BCI.2010.SbyS, step = 20, col = "blue", cex = 0.6, las=1) 
abline(0, 1, col = 'red')
# Add 1:1 line #
text(1500, 1500, "1:1", pos = 2, col = 'red')


# Rank abundance curves # 
# Cosntruct RAC that removes zero abundances and ordered and returned #
RAC <- function(x = ""){
  x = as.vector(x)
  x.ab = x[x > 0]
  x.ab.ranked = x.ab[order(x.ab, decreasing = TRUE)] 
  return(x.ab.ranked)
}

# plot the RAC with natural log transformed abundances #
plot.new()
site1 <- BCI[1, ]
rac <- RAC(x = site1)
ranks <- as.vector(seq(1, length(rac)))
opar <- par(no.readonly = TRUE) # Saves default plot parameters

par(mar = c(5.1, 5.1, 4.1, 2.1)) 
plot(ranks, log(rac), type = 'p', axes = F,
     xlab = "Rank in abundance", ylab = "log(Abundance)",
     las = 1, cex.lab = 1.4, cex.axis = 1.25)
box()
axis(side = 1, labels = T, cex.axis = 1.25)
axis(side = 2, las = 1, cex.axis = 1.25,
     labels = c(1, 2, 5, 10, 20), at = log(c(1, 2, 5, 10, 20)))


# Species abundance models #
# Use radfit() #
RACresults <- radfit(site1)
RACresults

# Plot results of the radfit #
plot.new()
plot(RACresults, las = 1, cex.lab = 1.4, cex.axis = 1.25)

## Beta diversity # 
# Ordination #
BCI.2000.db=vegdist(BCI.2000.SbyS,method="bray")
BCI.2000.pcoa=cmdscale(BCI.2000.db,eig=T,k=3)
explainvar1 <- round(BCI.2000.pcoa$eig[1] / sum(BCI.2000.pcoa$eig), 3) * 100
explainvar2 <- round(BCI.2000.pcoa$eig[2] / sum(BCI.2000.pcoa$eig), 3) * 100
explainvar3 <- round(BCI.2000.pcoa$eig[3] / sum(BCI.2000.pcoa$eig), 3) * 100
sum.eig <- sum(explainvar1, explainvar2, explainvar3)
par(mar = c(5, 5, 1, 2) + 0.1)
plot(BCI.2000.pcoa$points[ ,1], BCI.2000.pcoa$points[ ,2], ylim = c(-0.2, 0.3),
     xlab = paste("PCoA 1 (", explainvar1, "%)", sep = ""),
     ylab = paste("PCoA 2 (", explainvar2, "%)", sep = ""),
     pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(BCI.2000.pcoa$points[ ,1], BCI.2000.pcoa$points[ ,2],
       pch = 19, cex = 3, bg = "gray", col = "gray")
text(BCI.2000.pcoa$points[ ,1], BCI.2000.pcoa$points[ ,2],
     labels = row.names(BCI.2000.pcoa$points))



