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
require("vegan")  
require("tidyr")
require(dplyr)

# Load dataset #
BCI2010 <- read.delim("BCI2010.txt", header=T)
BCI1982 <- read.delim("BCI1982.txt", header=T)

# Transform 2010 census data into site by species matrix #
BCI.2010.SbyS <- group_by(BCI2010, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)
dim(BCI.2010.SbyS)

# Transform 1982 census data # 
BCI.1982.SbyS <- group_by(BCI1982, Quadrat) %>% count(Latin) %>% spread(key=Latin, value=n , fill=0)

# Alpha Diversity # 
# Species richness function #
S.obs <- function(x=""){
  rowSums(x>0) *1 
}

# Coverage function #
C <- function(x = ""){
  1 - (sum(x == 1) / rowSums(x)) }

# Chao1 # 
S.chao1 <- function(x = ""){
  S.obs(x) + (sum(x == 1)^2) / (2 * sum(x == 2)) }

# Chao2 # 
S.chao2 <- function(site = "", SbyS = ""){ 
  SbyS = as.data.frame(SbyS)
  x = SbyS[site, ]
  SbyS.pa <- (SbyS > 0) * 1
  Q1 = sum(colSums(SbyS.pa) == 1)
  Q2 = sum(colSums(SbyS.pa) == 2) 
  S.chao2 = S.obs(x) + (Q1^2)/(2 * Q2) 
  return(S.chao2)
}

# ACE #
S.ace <- function(x = "", thresh=10){
  x <- x[x>0]
  S.abund <- length(which(x>thresh))
  S.rare <- length(which(x <= thresh))
  singlt <- length(which(x == 1))
  N.rare <- sum(x[which(x <= thresh)])
  C.ace <- 1-(singlt/N.rare)
  i <- c(1:thresh)
  count <- function(i,y){
    length(y[y == i])
  }
  a.1 <- sapply(i, count, x)
  f.1 <- (i*(i-1))*a.1
  G.ace <- (S.rare/C.ace)*(sum(f.1)/(N.rare*(N.rare-1)))
  S.ace <- S.abund + (S.rare/C.ace) + (singlt/C.ace) * max(G.ace, 0)
  return(S.ace)
}

# Rarefaction curve # 
# Calculating observed richness #
soilbac.S <- S.obs(soilbac.t)
# Smallest value #
min.N <- min(rowSums(soilbac.t))
# Rarefy each sample #
S.rarefy <- rarefy(x = soilbac.t, sample = min.N, se = TRUE) 
# Plot rarefaction results
rarecurve(x = soilbac.t, step = 20, col = "blue", cex = 0.6, las=1) 
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


# Simpson's eveness index #
SimpE <- function(x = ""){
  S <- S.obs(x)
  x = as.data.frame(x)
  D <- diversity(x, "inv")
  E <- (D)/S
  return(E)
}

# Smith and Wilson's evenness index #
Evar <- function(x){
  x <- as.vector(x[x > 0])
  1 - (2/pi)*atan(var(log(x)))
}


# Shannon's diversity #
ShanH <- function(x = ""){
  H = 0
  for (n_i in x){
    if(n_i > 0) {
      p = n_i / sum(x)
      H = H - p*log(p)
    }
  }
  return(H)
}

# Simpson's Diversity #
SimpD <- function(x = ""){
  D = 0
  N = sum(x)
  for (n_i in x){
    D = D + (n_i^2)/(N^2)
  }
  return(D)
}

# Species abundance models #
# Use radfit() #
RACresults <- radfit(site1)
RACresults

# Plot results of the radfit #
plot.new()
plot(RACresults, las = 1, cex.lab = 1.4, cex.axis = 1.25)













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








