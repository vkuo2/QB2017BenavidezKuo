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






















