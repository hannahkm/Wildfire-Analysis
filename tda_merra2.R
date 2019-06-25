library(ggplot2)
library(MASS)
library(gridExtra)

if (!require(package = "FNN")) { 
  install.packages(pkgs = "FNN")
} 
if (!require(package = "igraph")) { 
  install.packages(pkgs = "igraph")
} 
if (!require(package = "scales")) { 
  install.packages(pkgs = "scales")
}

install.packages("readtext")
library(readtext)

if (!require(package = "TDA")) { 
  install.packages(pkgs = "TDA")
}

library(package = "TDA")

setwd("\\Users\\kimh2\\Desktop")
# X1: Tmax,Tmin
# X2: Ta, Qa
# X3: FWI(12),BUI(16)
X1 <- read.csv("merra2_active_calfire_jja.csv")[,c(2,8,12)]
X2 <- read.csv("merra2_active_calfire_jja.csv")[,c(6,8)]
X3 <- read.csv("merra2_active_calfire_jja.csv")[,c(12,16)]
Y1 <- read.csv("merra2_inactive_calfire_jja.csv")[,c(2,4)]
Y2 <- read.csv("merra2_inactive_calfire_jja.csv")[,c(6,8)]
Y3 <- read.csv("merra2_inactive_calfire_jja.csv")[,c(12,16)]
plot(X1)

Xlim <- c(-4, 4) 
Ylim <- c(-4, 4) 
by <- 0.1
Xseq <- seq(from = Xlim[1], to = Xlim[2], by = by) 
Yseq <- seq(from = Ylim[1], to = Ylim[2], by = by) 
Grid <- expand.grid(Xseq, Yseq)
##
x_values <- X1[,1]
y_values <- X1[,2]

#plot density graphs
commonTheme=list(labs(color="Density",fill="Density",
                      x="RMM1",
                      y="RMM2"),
                 theme_bw(),
                 theme(legend.position=c(0,1),
                       legend.justification=c(0,1)))
ggplot(data=X1, aes(x_values, y_values)) + 
  geom_point(shape=1, colour = "black", size = 0.3)+
  geom_density2d(aes(color=..level..))+
  scale_colour_gradient(low="green",high="red")+
  commonTheme

#plot density again except with shading and line of best fit now
ggplot(data=X1, aes(X1[,1], X1[,2])) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),
                 geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm, linetype=2, colour="red", se=F) +
  guides(alpha="none") +
  geom_point(shape=1, colour = "black", size = 0.1) +
  commonTheme

plot <- ggplot(X1, aes(X1[,1], X1[,2])) 

#blue graph with white dots??? more density ig
plot + stat_density2d(geom="tile", aes(fill = ..density..), 
                      contour = FALSE) + geom_point(colour = "white")

par(mfrow = c(1,2)) 
ggplot(data=X1, aes(X1[,1], X1[,2])) + 
  geom_point(shape=1, colour = "black", size = 0.3)+
  geom_density2d(aes(color=..level..))+
  scale_colour_gradient(low="green",high="red")+
  commonTheme

ggplot(data=Y1, aes(Y1[,1], Y1[,2])) + 
  geom_point(shape=1, colour = "black", size = 0.3)+
  geom_density2d(aes(color=..level..))+
  scale_colour_gradient(low="green",high="red")+
  commonTheme

require(gridExtra)
plot1 <- ggplot(data=X1, aes(X1[,1], X1[,2])) + 
  geom_point(shape=1, colour = "black", size = 0.3)+
  geom_density2d(aes(color=..level..))+
  scale_colour_gradient(low="green",high="red")+
  commonTheme

plot2 <- ggplot(data=Y1, aes(Y1[,1], Y1[,2])) + 
  geom_point(shape=1, colour = "black", size = 0.3)+
  geom_density2d(aes(color=..level..))+
  scale_colour_gradient(low="green",high="red")+
  commonTheme
grid.arrange(plot1, plot2, ncol=2)

#color = ~continent, size = ~size, colors = colors,
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
plot_ly(X1, x = X1[,1], y = X1[,2], z = X1[,3], 
        marker = list(symbol = 'circle'), colors = colors,
        sizes = c(5, 10)) %>%
  layout(scene = list(gridcolor = 'rgb(255, 255, 255)',
                      range = c(2.003297660701705, 5.191505530708712),
                      type = 'log', zerolinewidth = 1, ticklen = 5, gridwidth = 2))

plot_ly(z = as.matrix(as.data.frame(lapply(X1, as.numeric)))) %>% add_surface(  
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )) %>% layout(
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=-0.64)
      )
    )
  )

######
commonTheme=list(labs(color="Density",fill="Density",
                      x="Tmax",
                      y="Tmin"),
                 theme_bw(),
                 theme(legend.position=c(0,1),
                       legend.justification=c(0,1)))
require(gridExtra)
plot1 <- ggplot(data=X1, aes(X1[,1], X1[,2])) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),
                 geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") +
  guides(alpha="none") +
  xlim(-4,4)+ylim(-4,4)+
  geom_point(shape=1, colour = "black", size = 0.1) +
  commonTheme

plot2 <- ggplot(data=Y1, aes(Y1[,1], Y1[,2])) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),
                 geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") +
  guides(alpha="none") + xlim(-4,4)+ylim(-4,4)+
  geom_point(shape=1, colour = "black", size = 0.1) +
  commonTheme
grid.arrange(plot1, plot2, ncol=2)
##
commonTheme=list(labs(color="Density",fill="Density",
                      x="Ta",
                      y="Qa"),
                 theme_bw(),
                 theme(legend.position=c(0,1),
                       legend.justification=c(0,1)))
require(gridExtra)
plot1 <- ggplot(data=X2, aes(X2[,1], X2[,2])) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),
                 geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") +
  guides(alpha="none") +
  xlim(-4,4)+ylim(-4,4)+
  geom_point(shape=1, colour = "black", size = 0.1) +
  commonTheme

plot2 <- ggplot(data=Y2, aes(Y2[,1], Y2[,2])) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),
                 geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") +
  guides(alpha="none") + xlim(-4,4)+ylim(-4,4)+
  geom_point(shape=1, colour = "black", size = 0.1) +
  commonTheme
grid.arrange(plot1, plot2, ncol=2)
##
commonTheme=list(labs(color="Density",fill="Density",
                      x="FWI",
                      y="BUI"),
                 theme_bw(),
                 theme(legend.position=c(0,1),
                       legend.justification=c(0,1)))
require(gridExtra)
plot1 <- ggplot(data=X3, aes(X3[,1], X3[,2])) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),
                 geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") +
  guides(alpha="none") +
  xlim(-4,4)+ylim(-4,4)+
  geom_point(shape=1, colour = "black", size = 0.1) +
  commonTheme

plot2 <- ggplot(data=Y3, aes(Y3[,1], Y3[,2])) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),
                 geom='polygon',colour='black') +
  scale_fill_continuous(low="green",high="red") +
  guides(alpha="none") + xlim(-4,4)+ylim(-4,4)+
  geom_point(shape=1, colour = "black", size = 0.1) +
  commonTheme
grid.arrange(plot1, plot2, ncol=2)
###
Xlim <- c(-4, 4) 
Ylim <- c(-4, 4) 
by <- 0.1
Xseq <- seq(from = Xlim[1], to = Xlim[2], by = by) 
Yseq <- seq(from = Ylim[1], to = Ylim[2], by = by) 
Grid <- expand.grid(Xseq, Yseq)

#distances ###############################################################

distance <- distFct(Y2 = Y2, Grid = Grid)

par(mfrow = c(1,2)) 
plot(Y2, xlab = "RMM1", ylab = "RMM2", main = "MJO Index") 
persp(x = Xseq, y = Yseq, 
      z = matrix(distance, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, 
      scale = FALSE, expand = 3, col = "red", border = NA, 
      ltheta = 50, shade = 0.5, main = "Distance Function")

m0 <- 0.5 
DTM <- dtm(X2 = X2, Grid = Grid, m0 = m0)

par(mfrow = c(1,2)) 
plot(X2, xlab = "", ylab = "", main = "Sample X") 
persp(x = Xseq, y = Yseq, 
      z = matrix(DTM, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE, 
      expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5, 
      main = "DTM")

# k nearest neighbor density estimator #################################
k <- 20 
kNN <- knnDE(X = X2, Grid = Grid, k = k)

par(mfrow = c(1,2)) 
plot(X2, xlab = "", ylab = "", main = "Sample X") 
persp(x = Xseq, y = Yseq, 
      z = matrix(kNN, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE, 
      expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5, 
      main = "kNN")

# kernel density estimator ###############################################
h <- 0.3 
KDE <- kde(X = X2, Grid = Grid, h = h)

par(mfrow = c(1,2)) 
plot(X2, xlab = "", ylab = "", main = "Sample X") 
persp(x = Xseq, y = Yseq, 
      z = matrix(kNN, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE, 
      expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5, 
      main = "KDE")

# kernel distance ######################################################## 
h <- 0.3 
Kdist <- kernelDist(X = X2, Grid = Grid, h = h)

par(mfrow = c(1,2)) 
plot(X2, xlab = "", ylab = "", main = "Sample X") 
persp(x = Xseq, y = Yseq, 
      z = matrix(Kdist, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE, 
      expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5, 
      main = "Kernel Distance")


# bootstrap confidence band ############################################## 
band <- bootstrapBand(X = X2, FUN = kde, Grid = Grid, B = 100, 
                      parallel = FALSE, alpha = 0.1, h = h)
print(band[["width"]]) 
## 90% 
## 0.06426617

# persistent homology of a function over a grid ########################## 
Diag <- gridDiag(X = X2, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                 sublevel = FALSE, library = "Dionysus", 
                 printProgress = FALSE, h = 0.3)
####
Diag <- gridDiag(X = X2, FUN = knnDE, k = 30, 
                 lim = cbind(Xlim, Ylim), by = by, 
                 sublevel = FALSE, library = "Dionysus", 
                 printProgress = TRUE)
plot(Diag[["diagram"]])

# plotting persistence diagram ########################################### 
par(mfrow = c(1,3)) 
plot(X2, main = "Sample X") 
persp(x = Xseq, y = Yseq, 
      z = matrix(KDE, nrow = length(Xseq), ncol = length(Yseq)), 
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE, 
      expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.9, 
      main = "KDE")
plot(x = Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram")
dev.off()

# other options for plotting persistence diagram ######################### 
par(mfrow = c(1,2)) 
plot(Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram") 
plot(Diag[["diagram"]], rotated = TRUE, band = band[["width"]], 
     main = "Rotated Diagram")
plot(Diag[["diagram"]], barcode = TRUE, main = "Barcode")


# Rips persistence diagram ############################################### 
Diag <- ripsDiag(X = X2, maxdimension = 1, maxscale = 5, 
                 library = "GUDHI", printProgress = FALSE)
par(mfrow=c(1,2)) 
#plot(Circles, xlab="", ylab="") 
plot(Diag[["diagram"]])

Diag1 <- ripsDiag(X = X2, maxdimension = 1, maxscale = 5) 
Diag2 <- ripsDiag(X = X3, maxdimension = 1, maxscale = 5) 
print(bottleneck(Diag1 = Diag1[["diagram"]], Diag2 = Diag2[["diagram"]], 
                 dimension = 1))

## [1] 1.389126

print(wasserstein(Diag1 = Diag1[["diagram"]], Diag2 = Diag2[["diagram"]], 
                  p = 2, dimension = 1))

## [1] 2.184218
# note to self: figure out what all these numbers mean

#landscapes and silhouettes ###############################################
tseq <- seq(from = 0, to = 5, length = 1000) #domain 
Land <- landscape(Diag = Diag[["diagram"]], dimension = 1,
                  KK = 1, tseq = tseq) 
par(mfrow=c(1,1)) 
plot(tseq, Land, type = "l", main = "1st Landscape, dim = 1", ylab = "", 
     asp = 1, col = "red", lwd = 3)

tseq <- seq(from = 0, to = 5, length = 1000) #domain 
Sil <- silhouette(Diag = Diag[["diagram"]], p = 1, dimension = 1,
                  tseq = tseq) 
par(mfrow=c(1,1)) 
plot(tseq, Sil, type = "l", main="Silhouette (p = 1), dim = 1", ylab = "", 
     asp = 1, col = "red", lwd = 3)
