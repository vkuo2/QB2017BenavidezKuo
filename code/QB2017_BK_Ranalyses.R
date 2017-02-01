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



# PCoA codes for HW3; still have not removed NAs from 1985 and 1990

BCI2010 <- read.delim("BCI2010.txt", header=T)
BCI2005 <- read.delim("BCI2005.txt", header=T)
BCI2000 <- read.delim("BCI2000.txt", header=T)
BCI1995 <- read.delim("BCI1995.txt", header=T)
BCI1990 <- read.delim("BCI1990.txt", header=T)
BCI1985 <- read.delim("BCI1985.txt", header=T)
BCI1982 <- read.delim("BCI1982.txt", header=T)

BCI.2010.SbyS <- group_by(BCI2010, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
BCI.2005.SbyS <- group_by(BCI2005, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
BCI.2000.SbyS <- group_by(BCI2000, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
BCI.1995.SbyS <- group_by(BCI1995, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
BCI.1990.SbyS <- group_by(BCI1990, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
BCI.1985.SbyS <- group_by(BCI1985, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
BCI.1982.SbyS <- group_by(BCI1982, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)

par(mfrow=c(4, 2))

BCI.1982.db=vegdist(BCI.1982.SbyS,method="bray")
BCI.1982.pcoa=cmdscale(BCI.1982.db,eig=T,k=3)
explainvar1 <- round(BCI.1982.pcoa$eig[1] / sum(BCI.1982.pcoa$eig), 3) * 100
explainvar2 <- round(BCI.1982.pcoa$eig[2] / sum(BCI.1982.pcoa$eig), 3) * 100
explainvar3 <- round(BCI.1982.pcoa$eig[3] / sum(BCI.1982.pcoa$eig), 3) * 100
sum.eig <- sum(explainvar1, explainvar2, explainvar3)
par(mar = c(5, 5, 1, 2) + 0.1)
plot(BCI.1982.pcoa$points[ ,1], BCI.1982.pcoa$points[ ,2], ylim = c(-0.2, 0.3),
     xlab = paste("PCoA 1 (", explainvar1, "%)", sep = ""),
     ylab = paste("PCoA 2 (", explainvar2, "%)", sep = ""),
     pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(BCI.1982.pcoa$points[ ,1], BCI.1982.pcoa$points[ ,2],
       pch = 19, cex = 3, bg = "gray", col = "gray")
text(BCI.1982.pcoa$points[ ,1], BCI.1982.pcoa$points[ ,2],
     labels = row.names(BCI.1982.pcoa$points))


BCI.1985.db=vegdist(BCI.1985.SbyS,method="bray")
BCI.1985.pcoa=cmdscale(BCI.1985.db,eig=T,k=3)
explainvar1 <- round(BCI.1985.pcoa$eig[1] / sum(BCI.1985.pcoa$eig), 3) * 100
explainvar2 <- round(BCI.1985.pcoa$eig[2] / sum(BCI.1985.pcoa$eig), 3) * 100
explainvar3 <- round(BCI.1985.pcoa$eig[3] / sum(BCI.1985.pcoa$eig), 3) * 100
sum.eig <- sum(explainvar1, explainvar2, explainvar3)
par(mar = c(5, 5, 1, 2) + 0.1)
plot(BCI.1985.pcoa$points[ ,1], BCI.1985.pcoa$points[ ,2], ylim = c(-0.2, 0.3),
     xlab = paste("PCoA 1 (", explainvar1, "%)", sep = ""),
     ylab = paste("PCoA 2 (", explainvar2, "%)", sep = ""),
     pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(BCI.1985.pcoa$points[ ,1], BCI.1985.pcoa$points[ ,2],
       pch = 19, cex = 3, bg = "gray", col = "gray")
text(BCI.1985.pcoa$points[ ,1], BCI.1985.pcoa$points[ ,2],
     labels = row.names(BCI.1985.pcoa$points))


BCI.1990.db=vegdist(BCI.1990.SbyS,method="bray")
BCI.1990.pcoa=cmdscale(BCI.1990.db,eig=T,k=3)
explainvar1 <- round(BCI.1990.pcoa$eig[1] / sum(BCI.1990.pcoa$eig), 3) * 100
explainvar2 <- round(BCI.1990.pcoa$eig[2] / sum(BCI.1990.pcoa$eig), 3) * 100
explainvar3 <- round(BCI.1990.pcoa$eig[3] / sum(BCI.1990.pcoa$eig), 3) * 100
sum.eig <- sum(explainvar1, explainvar2, explainvar3)
par(mar = c(5, 5, 1, 2) + 0.1)
plot(BCI.1990.pcoa$points[ ,1], BCI.1990.pcoa$points[ ,2], ylim = c(-0.2, 0.3),
     xlab = paste("PCoA 1 (", explainvar1, "%)", sep = ""),
     ylab = paste("PCoA 2 (", explainvar2, "%)", sep = ""),
     pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(BCI.1990.pcoa$points[ ,1], BCI.1990.pcoa$points[ ,2],
       pch = 19, cex = 3, bg = "gray", col = "gray")
text(BCI.1990.pcoa$points[ ,1], BCI.1990.pcoa$points[ ,2],
     labels = row.names(BCI.1990.pcoa$points))


BCI.1995.db=vegdist(BCI.1995.SbyS,method="bray")
BCI.1995.pcoa=cmdscale(BCI.1995.db,eig=T,k=3)
explainvar1 <- round(BCI.1995.pcoa$eig[1] / sum(BCI.1995.pcoa$eig), 3) * 100
explainvar2 <- round(BCI.1995.pcoa$eig[2] / sum(BCI.1995.pcoa$eig), 3) * 100
explainvar3 <- round(BCI.1995.pcoa$eig[3] / sum(BCI.1995.pcoa$eig), 3) * 100
sum.eig <- sum(explainvar1, explainvar2, explainvar3)
par(mar = c(5, 5, 1, 2) + 0.1)
plot(BCI.1995.pcoa$points[ ,1], BCI.1995.pcoa$points[ ,2], ylim = c(-0.2, 0.3),
     xlab = paste("PCoA 1 (", explainvar1, "%)", sep = ""),
     ylab = paste("PCoA 2 (", explainvar2, "%)", sep = ""),
     pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(BCI.1995.pcoa$points[ ,1], BCI.1995.pcoa$points[ ,2],
       pch = 19, cex = 3, bg = "gray", col = "gray")
text(BCI.1995.pcoa$points[ ,1], BCI.1995.pcoa$points[ ,2],
     labels = row.names(BCI.1995.pcoa$points))


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


BCI.2005.db=vegdist(BCI.2005.SbyS,method="bray")
BCI.2005.pcoa=cmdscale(BCI.2005.db,eig=T,k=3)
explainvar1 <- round(BCI.2005.pcoa$eig[1] / sum(BCI.2005.pcoa$eig), 3) * 100
explainvar2 <- round(BCI.2005.pcoa$eig[2] / sum(BCI.2005.pcoa$eig), 3) * 100
explainvar3 <- round(BCI.2005.pcoa$eig[3] / sum(BCI.2005.pcoa$eig), 3) * 100
sum.eig <- sum(explainvar1, explainvar2, explainvar3)
par(mar = c(5, 5, 1, 2) + 0.1)
plot(BCI.2005.pcoa$points[ ,1], BCI.2005.pcoa$points[ ,2], ylim = c(-0.2, 0.3),
     xlab = paste("PCoA 1 (", explainvar1, "%)", sep = ""),
     ylab = paste("PCoA 2 (", explainvar2, "%)", sep = ""),
     pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(BCI.2005.pcoa$points[ ,1], BCI.2005.pcoa$points[ ,2],
       pch = 19, cex = 3, bg = "gray", col = "gray")
text(BCI.2005.pcoa$points[ ,1], BCI.2005.pcoa$points[ ,2],
     labels = row.names(BCI.2005.pcoa$points))


BCI.2010.db=vegdist(BCI.2010.SbyS,method="bray")
BCI.2010.pcoa=cmdscale(BCI.2010.db,eig=T,k=3)
explainvar1 <- round(BCI.2010.pcoa$eig[1] / sum(BCI.2010.pcoa$eig), 3) * 100
explainvar2 <- round(BCI.2010.pcoa$eig[2] / sum(BCI.2010.pcoa$eig), 3) * 100
explainvar3 <- round(BCI.2010.pcoa$eig[3] / sum(BCI.2010.pcoa$eig), 3) * 100
sum.eig <- sum(explainvar1, explainvar2, explainvar3)
par(mar = c(5, 5, 1, 2) + 0.1)
plot(BCI.2010.pcoa$points[ ,1], BCI.2010.pcoa$points[ ,2], ylim = c(-0.2, 0.3),
     xlab = paste("PCoA 1 (", explainvar1, "%)", sep = ""),
     ylab = paste("PCoA 2 (", explainvar2, "%)", sep = ""),
     pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(BCI.2010.pcoa$points[ ,1], BCI.2010.pcoa$points[ ,2],
       pch = 19, cex = 3, bg = "gray", col = "gray")
text(BCI.2010.pcoa$points[ ,1], BCI.2010.pcoa$points[ ,2],
     labels = row.names(BCI.2010.pcoa$points))








