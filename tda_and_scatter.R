library(ggplot2)
library(MASS)
library(gridExtra)

install.packages(pkgs = "FNN")
install.packages(pkgs = "igraph")
install.packages(pkgs = "scales")

library("FNN")
library("igraph")
library("scales")

#install.packages("readtext")
library(readtext)

#install.packages(pkgs = "TDA")

library(package = "TDA")

########################################################################## 
# uniform sample on the circle, and grid of points 
########################################################################## 
setwd("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data")
# X1: Tmax(2),Tmin(4)
# X2: Ta (6), Qa (8)
# X3: FWI(12),BUI(16)
# ISI (15) BUI(16)
# Wind speed (10) DC(14)
# DC(14) Tmax(2)

#/Users/hk/Desktop/School/MRHS/11th\ Grade/R/NN-ML/Wildfire-NN-ML/ML_Data/Old\ Data/
X1 <- read.csv("merra2_active_calfire_jja.csv")[,c(2,8)]
Y1 <- read.csv("merra2_inactive_calfire_jja.csv")[,c(2,8)]
plot(X1)

######
require(gridExtra)
commonTheme=list(labs(color="Density",fill="Density",
                      x="Tmax",
                      y="Qa"),
                 theme_bw(),
                 theme(legend.position=c(0.01,0.99),
                       legend.justification=c(0,1)))
plot1 <- ggplot(data=X1, aes(X1[,1], X1[,2])) +
  stat_density2d(aes(fill=..level..,alpha=..level..), geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") +  
  guides(alpha="none") + xlim(round(min(X1[,1])-1),round(max(X1[,1]))+1)+
  ylim(round(min(X1[,2])-1),round(max(X1[,2]))+1)+
  geom_point(shape=1, colour = "black", size =2) +
  commonTheme
plot1  + labs(title="Active Summer",
              x ="Tmax", y = "Qa") + theme(
                axis.text = element_text(color="black", size=16),
                plot.title = element_text(color="black", size=26, hjust=0.5),
                axis.title.x = element_text(color="black", size=24),
                axis.title.y = element_text(color="black", size=24)
              )
dev.off()

plot2 <- ggplot(data=Y1, aes(Y1[,1], Y1[,2])) +
  stat_density2d(aes(fill=..level..,alpha=..level..), geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") + 
  guides(alpha="none") + xlim(round(min(Y1[,1])-1),round(max(Y1[,1]))+1)+
  ylim(round(min(Y1[,2])-1),round(max(Y1[,2]))+1)+
  geom_point(shape=1, colour = "black", size = 2) +
  commonTheme
plot2  + labs(title="Inactive Summer",
              x ="Tmax", y = "Qa") + theme(
                axis.text = element_text(color="black", size=16),
                plot.title = element_text(color="black", size=30, hjust=0.5),
                axis.title.x = element_text(color="black", size=24),
                axis.title.y = element_text(color="black", size=24)
              ) 
dev.off()

###


###
Xlim <- c(-5.2, 5.1) 
Ylim <- c(-5.2, 5.1) 
by <- 0.1
Xseq <- seq(from = Xlim[1], to = Xlim[2], by = by) 
Yseq <- seq(from = Ylim[1], to = Ylim[2], by = by) 
Grid <- expand.grid(Xseq, Yseq)
########################################################################## 
# distance function 
########################################################################## 
Xlim <- c(-2.6, 2.55) 
Ylim <- c(-2.6, 2.55) 
by <- 0.05
Xseq <- seq(from = Xlim[1], to = Xlim[2], by = by) 
Yseq <- seq(from = Ylim[1], to = Ylim[2], by = by) 
Grid <- expand.grid(Xseq, Yseq)

distance <- distFct(X1, Grid = Grid)
par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(distance, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "Distance", theta =45, phi = 50, scale = FALSE, 
      expand = 2, col = "red", border = NA, ltheta = 40, shade = 0.5, 
      main = "Distance Function (Active Summer)")
dev.off()

distance <- distFct(Y1, Grid = Grid)
par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(distance, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "Distance", theta =45, phi = 50, scale = FALSE, 
      expand = 2, col = "red", border = NA, ltheta = 40, shade = 0.5, 
      main = "Distance Function (Inactive Summer)")
dev.off()
########################################################################## 
# distance to measure 
########################################################################## 
m0 <- 0.5 
m0 <- 0.05

DTM <- dtm(X1, Grid = Grid, m0 = m0, r=5)
par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(DTM, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "DTM", theta = 45, phi = 50, scale = FALSE, 
      expand = 2, col = "red", border = NA, ltheta = 40, shade = 0.5, 
      main = "DTM(Active Summer)")
dev.off()

DTM <- dtm(Y1, Grid = Grid, m0 = m0, r=5)
par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(DTM, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "DTM", theta = 45, phi = 50, scale = FALSE, 
      expand = 2, col = "red", border = NA, ltheta = 40, shade = 0.5, 
      main = "DTM(Inactive Summer)")
dev.off()
########################################################################## 
# k nearest neighbor density estimator 
########################################################################## 
k <- 20 

kNN <- knnDE(X = X1, Grid = Grid, k = k)

par(mfrow = c(1,1)) 

persp(x = Xseq, y = Yseq, 
      z = matrix(kNN, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "kNN", theta = -20, phi = 35, scale = FALSE, 
      expand = 10, col = "red", border = NA, ltheta = 50, shade = 0.5, 
      main = "kNN (active Summer)")
dev.off()

kNN <- knnDE(X = Y1, Grid = Grid, k = k)

par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(kNN, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "kNN", theta = -20, phi = 35, scale = FALSE, 
      expand = 10, col = "red", border = NA, ltheta = 50, shade = 0.5, 
      main = "kNN (inactive Summer)")
dev.off()
##########################################################################
# kernel density estimator 
########################################################################## 
h <- 0.3 
h <- 0.1

KDE <- kde(X1, Grid = Grid, h = h)

par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(KDE, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "KDE", theta = -20, phi = 35, scale = FALSE, 
      expand = 10, col = "red", border = NA, ltheta = 50, shade = 0.5, 
      main = "KDE (active Summer)")
dev.off()

KDE <- kde(Y1, Grid = Grid, h = h)

par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(KDE, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "KDE", theta = -20, phi = 35, scale = FALSE, 
      expand = 10, col = "red", border = NA, ltheta = 50, shade = 0.5, 
      main = "KDE (inactive Summer)")
dev.off()
########################################################################## 
# kernel distance 
########################################################################## 
h <- 0.3 
h <- 0.1

Kdist <- kernelDist(X1, Grid = Grid, h = h)

par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(Kdist, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "Kdist", theta =45, phi = 50, scale = FALSE, 
      expand = 500, col = "red", border = NA, ltheta = 40, shade = 0.5, 
      main = "Kernel Distance (active Summer)")
dev.off()

Kdist <- kernelDist(Y1, Grid = Grid, h = h)

par(mfrow = c(1,1)) 
persp(x = Xseq, y = Yseq, 
      z = matrix(Kdist, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "Tmax", ylab = "Qa", zlab = "Kdist", theta =45, phi = 50, scale = FALSE, 
      expand = 500, col = "red", border = NA, ltheta = 40, shade = 0.5, 
      main = "Kernel Distance (Inactive Summer)")
dev.off()
########################################################################## 
# bootstrap confidence band 
########################################################################## 
band <- bootstrapBand(Y1, FUN = kde, Grid = Grid, B = 100, 
                      parallel = FALSE, alpha = 0.1, h = h)
print(band[["width"]]) 
## 90% 
## 0.06426617

########################################################################## 
# persistent homology of a function over a grid 
########################################################################## 

Diag1 <- gridDiag(X1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.1)
Diag2 <- gridDiag(X1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.2)
par(mfrow = c(1,2)) 
plot(Diag1[["diagram"]], main = "FUN=kde (h=0.1)")
plot(Diag2[["diagram"]], main = "FUN=kde (h=0.2)")
dev.off()


Diag1 <- gridDiag(X1, FUN = knnDE, k = 20, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
Diag2 <- gridDiag(X1, FUN = knnDE, k = 30, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
par(mfrow = c(1,2)) 
plot(Diag1[["diagram"]], main = "FUN=knnDE (k=20)")
plot(Diag2[["diagram"]], main = "FUN=knnDE (k=30)")
dev.off()

Diag1 <- gridDiag(Y1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.1)
Diag2 <- gridDiag(Y1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.2)
par(mfrow = c(1,2)) 
plot(Diag1[["diagram"]], main = "FUN=kde (h=0.1)")
plot(Diag2[["diagram"]], main = "FUN=kde (h=0.2)")
dev.off()


Diag1 <- gridDiag(Y1, FUN = knnDE, k = 20, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
Diag2 <- gridDiag(Y1, FUN = knnDE, k = 30, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
par(mfrow = c(1,2)) 
plot(Diag1[["diagram"]], main = "FUN=knnDE (k=20)")
plot(Diag2[["diagram"]], main = "FUN=knnDE (k=30)")
dev.off()

Diag1 <- gridDiag(X1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.1)
####
Diag2 <- gridDiag(X1, FUN = knnDE, k = 20, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
par(mfrow = c(1,2)) 
plot(Diag1[["diagram"]])
plot(Diag2[["diagram"]])
###
Xlim <- c(-2.6, 2.55) 
Ylim <- c(-2.6, 2.55) 
by <- 0.05
# h <- .1  #bandwidth for the function kde
#Kernel Density Diagram of the superlevel sets

Diag1 <- gridDiag(X1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.1)
Diag2 <- gridDiag(Y1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                  sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.1)

# confidence set
B <- 50       ## B <- 10 the number of bootstrap iterations should be higher!
## this is just an example
alpha <- 0.05
alpha <- 0.05

cc1 <- bootstrapDiagram(X1, kde, lim = cbind(Xlim, Ylim), by = by, sublevel = FALSE, B = B,
                        alpha = alpha, dimension = 1, printProgress = TRUE, h = 0.1)
cc2 <- bootstrapDiagram(Y1, kde, lim = cbind(Xlim, Ylim), by = by, sublevel = FALSE, B = B,
                        alpha = alpha, dimension = 1, printProgress = TRUE, h = 0.1)

par(mfrow = c(1,2)) 
plot(Diag1[["diagram"]], band = 2 * cc1, main = "Active Summer")
plot(Diag2[["diagram"]], band = 2 * cc2, main = "Inactive Summer")
dev.off()

# Silhouette (Not working)

tseq <- seq(0, 1, length = 1000)
Sil <- silhouette(Diag1[["diagram"]], p = 1, dimension = 1, tseq = tseq) 
par(mfrow=c(1,1)) 
plot(tseq, Sil, type = "l", main="Silhouette (p = 1), dim = 1", ylab = "", 
     asp = 1, col = "red", lwd = 3)

# Ref) Statistical Topological Data Analysis using Persistence Landscapes by Bubenik
Land1 <- landscape(Diag1[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
Land2 <- landscape(Diag2[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
par(mfrow=c(2,1)) 
plot(tseq, Land1, type = "l", main = "1st Landscape, dim = 1", ylab = "", 
     asp = 1, col = "red", lwd = 3)
plot(tseq, Land2, type = "l", main = "1st Landscape, dim = 1", ylab = "", 
     asp = 1, col = "red", lwd = 3)
########################################################################## 
# plotting persistence diagram 
########################################################################## 
par(mfrow = c(1,1)) 
plot(x = Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram")
dev.off()
########################################################################## 
# other options for plotting persistence diagram 
########################################################################## 
par(mfrow = c(1,2)) 
plot(Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram") 
plot(Diag[["diagram"]], rotated = TRUE, band = band[["width"]], 
     main = "Rotated Diagram")
plot(Diag[["diagram"]], barcode = TRUE, main = "Barcode")

########################################################################## 
# Rips persistence diagram 
########################################################################## 
Diag1 <- ripsDiag(X1, maxdimension = 2, maxscale = 1.0, 
                  library = "GUDHI", printProgress = FALSE)
Diag2 <- ripsDiag(Y1, maxdimension = 2, maxscale = 1.0, 
                  library = "GUDHI", printProgress = FALSE)

par(mfrow=c(2,2)) 
plot(Diag1[["diagram"]], main = "Active Summer (Tmax vs Qa)")
plot(Diag2[["diagram"]], main = "Inactive Summer (Tmax vs Qa)")
plot(Diag1[["diagram"]], barcode =TRUE)
plot(Diag2[["diagram"]], barcode =TRUE)
dev.off()


par(mfrow=c(1,2)) 
plot(Diag1[["diagram"]], main = "Rips (Tmax vs Qa)")
plot(Diag1[["diagram"]], barcode =TRUE, main = "barcode (Active)")
dev.off()


par(mfrow=c(2,2)) 
plot(Diag1[["diagram"]], main = "Active Summer")
plot(Diag1[["diagram"]], rotated =TRUE, main = "Rotated")
plot(Diag2[["diagram"]], main = "Inactive Summer")
plot(Diag2[["diagram"]], rotated =TRUE, main = "Rotated")
dev.off()


par(mfrow=c(1,2)) 
plot(Diag2[["diagram"]], main = "Rips (Tmax vs Qa)")
plot(Diag2[["diagram"]], barcode =TRUE, main = "barcode (Inactive)")
dev.off()

print(bottleneck(Diag1 = Diag1[["diagram"]], Diag2 = Diag2[["diagram"]], 
                 dimension = 1))
## https://www.rdocumentation.org/packages/TDA/versions/1.6.2/topics/wasserstein
wasserstein(Diag1[["diagram"]], Diag2[["diagram"]], p = 1, dimension = 1)
#RIPS with

##
Diag3 <- alphaComplexDiag(
  X2, maxdimension = NCOL(X2) - 1, library = "GUDHI",
  location = FALSE, printProgress = FALSE)
##
#clusterTree https://www.rdocumentation.org/packages/TDA/versions/1.6.2/topics/plot.clusterTree
V1 <- X1[,1]
V2 <- X1[,2]

VV <- rbind(V1, V2, V3)

k <- 100   #parameter of knn

## Density clustering using knn and kde
k <- 20
Tree <- clusterTree(X1, k, density = "knn")
TreeKDE <- clusterTree(X1,k, h = 0.1, density = "kde")
par(mfrow = c(1, 2))
# plot lambda trees
plot(Tree, type = "lambda", main = "lambda Tree (knn)")
plot(TreeKDE, type = "lambda", main = "lambda Tree (kde)")

# plot lambda trees for active season
Tree <- clusterTree(X1, k = 20, density = "knn")
TreeKDE <- clusterTree(X1,k, h = 0.1, density = "kde")


par(mfrow = c(1, 1))
plot(Tree, type = "lambda", main = "lambda Tree (knn) Inactive Summer")
dev.off()

par(mfrow = c(1, 1))
plot(TreeKDE, type = "lambda", main = "lambda Tree (kde) Inactive Summer")
dev.off()
# plot lambda trees for inactive season
Tree <- clusterTree(Y1, k = 20, density = "knn")
TreeKDE <- clusterTree(Y1,k, h = 0.1, density = "kde")

plot(Tree, type = "lambda", main = "lambda Tree (knn) Inactive Summer")

plot(TreeKDE, type = "lambda", main = "lambda Tree (kde) Inactive Summer")

##

Diag1 <- ripsDiag(X1, maxdimension = 2, maxscale = 1.0, 
                  library = "Dionysus", printProgress = FALSE)
Diag2 <- ripsDiag(Y1, maxdimension = 2, maxscale = 1.0, 
                  library = "Dionysus", printProgress = FALSE)


par(mfrow=c(2,2)) 
plot(Diag1[["diagram"]], main = "Active Summer (Tmax vs Qa)")
plot(Diag2[["diagram"]], main = "Inactive Summer (Tmax vs Qa)")
plot(Diag1[["diagram"]], barcode =TRUE)
plot(Diag2[["diagram"]], barcode =TRUE)


par(mfrow=c(1,2)) 
plot(Diag1[["diagram"]], main = "Rips (Tmax vs Qa)")
plot(Diag1[["diagram"]], barcode =TRUE, main = "barcode (Active)")


par(mfrow=c(2,2)) 
plot(Diag1[["diagram"]], main = "Active Summer")
plot(Diag1[["diagram"]], rotated =TRUE, main = "Rotated")
plot(Diag2[["diagram"]], main = "Inactive Summer")
plot(Diag2[["diagram"]], rotated =TRUE, main = "Rotated")


par(mfrow=c(1,2)) 
plot(Diag2[["diagram"]], main = "Rips (Tmax vs Qa)")
plot(Diag2[["diagram"]], barcode =TRUE, main = "barcode (Inactive)")

# Silhouette
tseq <- seq(0, 1, length = 1000)
Sil <- silhouette(Diag1[["diagram"]], p = 1, dimension = 1, tseq = tseq) 
par(mfrow=c(1,1)) 
plot(tseq, Sil, type = "l", main="Silhouette (p = 1), dim = 1", ylab = "", 
     asp = 1, col = "red", lwd = 3)

# Ref Statistical Topological Data Analysis using Persistence Landscapes by Bubenik

Land1 <- landscape(Diag1[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
Land2 <- landscape(Diag2[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
par(mfrow=c(2,1)) 
plot(tseq, Land1, type = "l", main = "1st Landscape, dim = 1 (Active)", ylab = "", 
     asp = 1, col = "red", lwd = 3)
plot(tseq, Land2, type = "l", main = "1st Landscape, dim = 1 (Inactive)", ylab = "", 
     asp = 1, col = "red", lwd = 3)

## From https://www.rdocumentation.org/packages/TDA/versions/1.6.2/topics/ripsDiag
## EXAMPLE 1: rips diagram for circles (euclidean distance)
X <- circleUnif(30)
maxscale <- 5
maxdimension <- 1
## input X is a point cloud
DiagRips <- ripsDiag(
  X = X, maxdimension = maxdimension, maxscale = maxscale,
  library = "Dionysus", location = TRUE, printProgress = TRUE)

# plot
layout(matrix(c(1, 3, 2, 2), 2, 2))
plot(X, cex = 0.5, pch = 19)
title(main = "Data")
plot(DiagRips[["diagram"]])
title(main = "rips Diagram")
one <- which(
  DiagRips[["diagram"]][, 1] == 1 &
    DiagRips[["diagram"]][, 3] - DiagRips[["diagram"]][, 2] > 0.5)
plot(X, col = 2, main = "Representative loop of data points")
for (i in seq(along = one)) {
  for (j in seq_len(dim(DiagRips[["cycleLocation"]][[one[i]]])[1])) {
    lines(
      DiagRips[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1,
      col = i)
  }
}

## EXAMPLE 2: rips diagram with arbitrary distance
## distance matrix for triangle with edges of length: 1,2,4
distX <- matrix(c(0, 1, 2, 1, 0, 4, 2, 4, 0), ncol = 3)
maxscale <- 5
maxdimension <- 1
## note that the input distXX is a distance matrix
DiagTri <- ripsDiag(distX, maxdimension, maxscale, dist = "arbitrary",
                    printProgress = TRUE)
#points with lifetime = 0 are not shown. e.g. the loop of the triangle.
print(DiagTri[["diagram"]])
# }
Diag <- ripsDiag(X = X2, maxdimension = 1, maxscale = 5, 
                 library = "GUDHI", printProgress = FALSE)
par(mfrow=c(1,2)) 
plot(Circles, xlab="", ylab="") 
plot(Diag[["diagram"]])

Diag1 <- ripsDiag(X = X1, maxdimension = 1, maxscale = 5) 
Diag2 <- ripsDiag(X = Y1, maxdimension = 1, maxscale = 5) 
print(bottleneck(Diag1 = Diag1[["diagram"]], Diag2 = Diag2[["diagram"]], 
                 dimension = 1))

## [1] 1.389126

print(wasserstein(Diag1 = Diag1[["diagram"]], Diag2 = Diag2[["diagram"]], 
                  p = 2, dimension = 1))

## [1] 2.184218


tseq <- seq(from = 0, to = 5, length = 1000) #domain 
Land <- landscape(Diag = Diag[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
par(mfrow=c(1,1)) 
plot(tseq, Land, type = "l", main = "1st Landscape, dim = 1", ylab = "", 
     asp = 1, col = "red", lwd = 3)

tseq <- seq(from = 0, to = 5, length = 1000) #domain 
Sil <- silhouette(Diag = Diag[["diagram"]], p = 1, dimension = 1, tseq = tseq) 
par(mfrow=c(1,1)) 
plot(tseq, Sil, type = "l", main="Silhouette (p = 1), dim = 1", ylab = "", 
     asp = 1, col = "red", lwd = 3)
