library(ggplot2)
library(MASS)
library(gridExtra)
library("FNN")
library("igraph")
library("scales")
#install.packages("readtext")
library(readtext)
#install.packages(pkgs = "TDA")
library(package = "TDA")

work_dir <- "/Users/hk/Desktop/School/MRHS/11th\ Grade/R/NN-ML/Wildfire-NN-ML/ML_Data/Old\ Data"
setwd(work_dir)
# X1: Tmax(2),Tmin(4)
# X2: Ta (6), Qa (8)
# X3: FWI(12),BUI(16)
# ISI (15) BUI(16)
# Wind speed (10) DC(14)
# DC(14) Tmax(2)

X1 <- read.csv("merra2_active_calfire_jja.csv")[,c(2,8)]
Y1 <- read.csv("merra2_inactive_calfire_jja.csv")[,c(2,8)]
#plot(X1, family = "serif")
density_plot(X1, Y1)

#density plots with contour lines
#X1: active summer/year
#Y1: inactive summer/year
density_plot <- function (X1, Y1){
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
    geom_point(shape=1, colour = "black", size =1) +
    commonTheme
  plot1 <- plot1  + labs(title="Active Summer",
                         x ="Tmax", y = "Qa") + theme(
                           axis.text = element_text(color="black", size=16, family = "serif"),
                           plot.title = element_text(color="black", size=26, hjust=0.5, family = "serif"),
                           axis.title.x = element_text(color="black", size=24, family = "serif"),
                           axis.title.y = element_text(color="black", size=24, family = "serif")
                         )
  
  plot2 <- ggplot(data=Y1, aes(Y1[,1], Y1[,2])) +
    stat_density2d(aes(fill=..level..,alpha=..level..), geom='polygon',colour='black') +
    scale_fill_continuous(low="green",high="red") + 
    guides(alpha="none") + xlim(round(min(Y1[,1])-1),round(max(Y1[,1]))+1)+
    ylim(round(min(Y1[,2])-1),round(max(Y1[,2]))+1)+
    geom_point(shape=1, colour = "black", size = 1) +
    commonTheme
  plot2 <- plot2  + labs(title="Inactive Summer",
                         x ="Tmax", y = "Qa") + theme(
                           axis.text = element_text(color="black", size=16, family = "serif"),
                           plot.title = element_text(color="black", size=26, hjust=0.5, family = "serif"),
                           axis.title.x = element_text(color="black", size=24, family = "serif"),
                           axis.title.y = element_text(color="black", size=24, family = "serif")
                         ) 
  grid.arrange(plot1, plot2, ncol=2)
}

#comparison of the distribution of points for active/inactive 
  #AKA: significance of the differences btn active/inactive plots
#X1: active
#Y1: inactive
density_comparison <- function (X1, Y1){
  commonTheme=list(labs(color="Density",fill="Density",
                        x="Tmax",
                        y="Qa"),
                   theme_bw(),
                   theme(legend.position=c(0.01,0.99),
                         legend.justification=c(0,1)))
  
  hist_top <- ggplot()+geom_density(aes(x=X1[,1], color="red"))+
    geom_density(aes(x=Y1[,1], color="darkblue")) +
    theme(legend.position = "none",          
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = 'white', colour = 'white'))
  
  hist_right <- ggplot()+geom_density(aes(x=X1[,2], color="red"))+
    geom_density(aes(x=Y1[,2], color="darkblue"))+
    coord_flip()+
    theme(legend.position = "none",          
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = 'white', colour = 'white'))
  
  empty <- ggplot() + 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'))
  
  plot3 <- ggplot() +
    geom_point(data=X1, aes(X1[,1], X1[,2]), shape=1, colour = "red", size=0.2) + 
    geom_point(data=Y1, aes(Y1[,1], Y1[,2]), shape=1, colour = "blue", size=0.2) + 
    guides(alpha="none") + xlim(round(min(Y1[,1], X1[,1])-1),round(max(Y1[,1],X1[,1]))+1)+
    ylim(round(min(Y1[,2],X1[,2])-1),round(max(Y1[,2],X1[,2]))+1)+
    commonTheme
  
  grid.arrange(hist_top, empty, plot3, hist_right, 
               ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
}

learning_TDA(){
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
  plot(Diag1[["diagram"]], main = "FUN=kde (h=0.1)", family = "serif")
  plot(Diag2[["diagram"]], main = "FUN=kde (h=0.2)", family = "serif")
  dev.off()
  
  
  Diag1 <- gridDiag(X1, FUN = knnDE, k = 20, lim = cbind(Xlim, Ylim), by = by, 
                    sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
  Diag2 <- gridDiag(X1, FUN = knnDE, k = 30, lim = cbind(Xlim, Ylim), by = by, 
                    sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
  par(mfrow = c(1,2)) 
  plot(Diag1[["diagram"]], main = "FUN=knnDE (k=20)", family = "serif")
  plot(Diag2[["diagram"]], main = "FUN=knnDE (k=30)", family = "serif")
  dev.off()
  
  Diag1 <- gridDiag(Y1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                    sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.1)
  Diag2 <- gridDiag(Y1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                    sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.2)
  par(mfrow = c(1,2)) 
  plot(Diag1[["diagram"]], main = "FUN=kde (h=0.1)", family = "serif")
  plot(Diag2[["diagram"]], main = "FUN=kde (h=0.2)", family = "serif")
  dev.off()
  
  
  Diag1 <- gridDiag(Y1, FUN = knnDE, k = 20, lim = cbind(Xlim, Ylim), by = by, 
                    sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
  Diag2 <- gridDiag(Y1, FUN = knnDE, k = 30, lim = cbind(Xlim, Ylim), by = by, 
                    sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
  par(mfrow = c(1,2)) 
  plot(Diag1[["diagram"]], main = "FUN=knnDE (k=20)", family = "serif")
  plot(Diag2[["diagram"]], main = "FUN=knnDE (k=30)", family = "serif")
  dev.off()
  
  Diag1 <- gridDiag(X1, FUN = kde, lim = cbind(Xlim, Ylim), by = by, 
                    sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.1)
  ####
  Diag2 <- gridDiag(X1, FUN = knnDE, k = 20, lim = cbind(Xlim, Ylim), by = by, 
                    sublevel = FALSE, library = "Dionysus", printProgress = TRUE)
  par(mfrow = c(1,2)) 
  plot(Diag1[["diagram"]], family = "serif")
  plot(Diag2[["diagram"]], family = "serif")
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
  plot(Diag1[["diagram"]], band = 2 * cc1, main = "Active Summer", family = "serif")
  plot(Diag2[["diagram"]], band = 2 * cc2, main = "Inactive Summer", family = "serif")
  dev.off()
  
  # Silhouette (Not working)
  
  tseq <- seq(0, 1, length = 1000)
  Sil <- silhouette(Diag1[["diagram"]], p = 1, dimension = 1, tseq = tseq) 
  par(mfrow=c(1,1)) 
  plot(tseq, Sil, type = "l", main="Silhouette (p = 1), dim = 1", ylab = "", 
       asp = 1, col = "red", lwd = 3, family = "serif")
  
  # Ref) Statistical Topological Data Analysis using Persistence Landscapes by Bubenik
  Land1 <- landscape(Diag1[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
  Land2 <- landscape(Diag2[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
  par(mfrow=c(2,1)) 
  plot(tseq, Land1, type = "l", main = "1st Landscape, dim = 1", ylab = "", 
       asp = 1, col = "red", lwd = 3, family = "serif")
  plot(tseq, Land2, type = "l", main = "1st Landscape, dim = 1", ylab = "", 
       asp = 1, col = "red", lwd = 3, family = "serif")
  ########################################################################## 
  # plotting persistence diagram 
  ########################################################################## 
  par(mfrow = c(1,1)) 
  plot(x = Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram"
       , family = "serif")
  dev.off()
  ########################################################################## 
  # other options for plotting persistence diagram 
  ########################################################################## 
  par(mfrow = c(1,2)) 
  plot(Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram"
       , family = "serif") 
  plot(Diag[["diagram"]], rotated = TRUE, band = band[["width"]], 
       main = "Rotated Diagram", family = "serif")
  plot(Diag[["diagram"]], barcode = TRUE, main = "Barcode", family = "serif")
  
  ########################################################################## 
  # Rips persistence diagram 
  ########################################################################## 
  Diag1 <- ripsDiag(X1, maxdimension = 2, maxscale = 1.0, 
                    library = "GUDHI", printProgress = FALSE)
  Diag2 <- ripsDiag(Y1, maxdimension = 2, maxscale = 1.0, 
                    library = "GUDHI", printProgress = FALSE)
  
  par(mfrow=c(2,2)) 
  plot(Diag1[["diagram"]], main = "Active Summer (Tmax vs Qa)", family = "serif")
  plot(Diag2[["diagram"]], main = "Inactive Summer (Tmax vs Qa)", family = "serif")
  plot(Diag1[["diagram"]], barcode =TRUE, family = "serif")
  plot(Diag2[["diagram"]], barcode =TRUE, family = "serif")
  dev.off()
  
  
  par(mfrow=c(1,2)) 
  plot(Diag1[["diagram"]], main = "Rips (Tmax vs Qa)", family = "serif")
  plot(Diag1[["diagram"]], barcode =TRUE, main = "barcode (Active)", family = "serif")
  dev.off()
  
  
  par(mfrow=c(2,2)) 
  plot(Diag1[["diagram"]], main = "Active Summer", family = "serif")
  plot(Diag1[["diagram"]], rotated =TRUE, main = "Rotated", family = "serif")
  plot(Diag2[["diagram"]], main = "Inactive Summer", family = "serif")
  plot(Diag2[["diagram"]], rotated =TRUE, main = "Rotated", family = "serif")
  dev.off()
  
  
  par(mfrow=c(1,2)) 
  plot(Diag2[["diagram"]], main = "Rips (Tmax vs Qa)", family = "serif")
  plot(Diag2[["diagram"]], barcode =TRUE, main = "barcode (Inactive)", family = "serif")
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
  plot(Tree, type = "lambda", main = "lambda Tree (knn)", family = "serif")
  plot(TreeKDE, type = "lambda", main = "lambda Tree (kde)", family = "serif")
  
  # plot lambda trees for active season
  Tree <- clusterTree(X1, k = 20, density = "knn")
  TreeKDE <- clusterTree(X1,k, h = 0.1, density = "kde")
  
  
  par(mfrow = c(1, 1))
  plot(Tree, type = "lambda", main = "lambda Tree (knn) Inactive Summer", family = "serif")
  dev.off()
  
  par(mfrow = c(1, 1))
  plot(TreeKDE, type = "lambda", main = "lambda Tree (kde) Inactive Summer", family = "serif")
  dev.off()
  # plot lambda trees for inactive season
  Tree <- clusterTree(Y1, k = 20, density = "knn")
  TreeKDE <- clusterTree(Y1,k, h = 0.1, density = "kde")
  
  plot(Tree, type = "lambda", main = "lambda Tree (knn) Inactive Summer",
       family = "serif")
  
  plot(TreeKDE, type = "lambda", main = "lambda Tree (kde) Inactive Summer",
       family = "serif")
  
  ##
  
  Diag1 <- ripsDiag(X1, maxdimension = 2, maxscale = 1.0, 
                    library = "Dionysus", printProgress = FALSE)
  Diag2 <- ripsDiag(Y1, maxdimension = 2, maxscale = 1.0, 
                    library = "Dionysus", printProgress = FALSE)
  
  
  par(mfrow=c(2,2)) 
  plot(Diag1[["diagram"]], main = "Active Summer (Tmax vs Qa)", family = "serif")
  plot(Diag2[["diagram"]], main = "Inactive Summer (Tmax vs Qa)", family = "serif")
  plot(Diag1[["diagram"]], barcode =TRUE, family = "serif")
  plot(Diag2[["diagram"]], barcode =TRUE, family = "serif")
  
  
  par(mfrow=c(1,2)) 
  plot(Diag1[["diagram"]], main = "Rips (Tmax vs Qa)", family = "serif")
  plot(Diag1[["diagram"]], barcode =TRUE, main = "barcode (Active)", family = "serif")
  
  
  par(mfrow=c(2,2)) 
  plot(Diag1[["diagram"]], main = "Active Summer", family = "serif")
  plot(Diag1[["diagram"]], rotated =TRUE, main = "Rotated", family = "serif")
  plot(Diag2[["diagram"]], main = "Inactive Summer", family = "serif")
  plot(Diag2[["diagram"]], rotated =TRUE, main = "Rotated", family = "serif")
  
  
  par(mfrow=c(1,2)) 
  plot(Diag2[["diagram"]], main = "Rips (Tmax vs Qa)", family = "serif")
  plot(Diag2[["diagram"]], barcode =TRUE, main = "barcode (Inactive)", family = "serif")
  
  # Silhouette
  tseq <- seq(0, 1, length = 1000)
  Sil <- silhouette(Diag1[["diagram"]], p = 1, dimension = 1, tseq = tseq) 
  par(mfrow=c(1,1)) 
  plot(tseq, Sil, type = "l", main="Silhouette (p = 1), dim = 1", ylab = "", 
       asp = 1, col = "red", lwd = 3, family = "serif")
  
  # Ref Statistical Topological Data Analysis using Persistence Landscapes by Bubenik
  
  Land1 <- landscape(Diag1[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
  Land2 <- landscape(Diag2[["diagram"]], dimension = 1, KK = 1, tseq = tseq) 
  par(mfrow=c(2,1)) 
  plot(tseq, Land1, type = "l", main = "1st Landscape, dim = 1 (Active)", ylab = "", 
       asp = 1, col = "red", lwd = 3, family = "serif")
  plot(tseq, Land2, type = "l", main = "1st Landscape, dim = 1 (Inactive)", ylab = "", 
       asp = 1, col = "red", lwd = 3, family = "serif")
  
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
  plot(X, cex = 0.5, pch = 19, family = "serif")
  title(main = "Data")
  plot(DiagRips[["diagram"]], family = "serif")
  title(main = "rips Diagram")
  one <- which(
    DiagRips[["diagram"]][, 1] == 1 &
      DiagRips[["diagram"]][, 3] - DiagRips[["diagram"]][, 2] > 0.5)
  plot(X, col = 2, main = "Representative loop of data points", family = "serif")
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
  plot(Circles, xlab="", ylab="", family = "serif") 
  plot(Diag[["diagram"]], family = "serif")
  
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
       asp = 1, col = "red", lwd = 3, family = "serif")
  
  tseq <- seq(from = 0, to = 5, length = 1000) #domain 
  Sil <- silhouette(Diag = Diag[["diagram"]], p = 1, dimension = 1, tseq = tseq) 
  par(mfrow=c(1,1)) 
  plot(tseq, Sil, type = "l", main="Silhouette (p = 1), dim = 1", ylab = "", 
       asp = 1, col = "red", lwd = 3, family = "serif")
}
