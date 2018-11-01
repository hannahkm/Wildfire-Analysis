library("TDA")

charge_data <- file.choose()
#Read in the file
inputted.file <- data.frame(read.csv(charge_data), header = TRUE)

x.grid <- data.frame(inputted.file[1:100,c(1,4,5)], stringsAsFactors = FALSE)

#Creates categorical variables for nonnumerical columns
if (is.factor(x.grid)){
  task.data <- x.grid[,1]
  task.data.f <- factor(task.data, levels = as.character(unique(task.data)), 
                        labels=c(1:length(unique(task.data))))
  task.data.f <- as.numeric(task.data.f)
  
  group.data <- x.grid[,2]
  group.data.f <- factor(group.data, levels = as.character(unique(group.data)), 
                         labels=c(1:length(unique(group.data))))
  group.data.f <- as.numeric(group.data.f)
  
  x.grid[,1] <- task.data.f
  x.grid[,2] <- group.data.f
} 

Xlim <- c(-1.5, 1.45)
Ylim <- c(-1.5, 1.45)
by <- 0.06
Xseq <- seq(from = Xlim[1], to = Xlim[2], by = by)
Yseq <- seq(from = Ylim[1], to = Ylim[2], by = by)
total.grid <- expand.grid(Xseq, Yseq)

#distance <- distFct(X = x.grid, Grid = total.grid)
distance <- dist(x.grid)

#DTM (distance to measure) is measured by this complicated math formula
#calculate DTM for every point in Grid:
m0 <- 0.1
DTM <- dtm(X = x.grid, Grid = x.grid, m0 = m0)

#calculates nearest neighbor
k <- 60
kNN <- knnDE(X = x.grid, Grid = x.grid, k = k)

#estimates density?
h <- 0.3
KDE <- kde(X = x.grid, Grid = x.grid, h = h)

#estimates distance
h <- 0.3
Kdist <- kernelDist(X = x.grid, Grid = x.grid, h = h)


#NOTE: fix persp3d?
persp3d(Xseq, Yseq, matrix(DTM, ncol = length(Yseq), nrow = length(Xseq)),
        xlim = NULL, ylim = NULL, zlim = NULL,
        xlab = NULL, ylab = NULL, zlab = NULL, add = FALSE, aspect = !add, 
        forceClipregion = FALSE)

persp3d(Xseq, Yseq, matrix(DTM, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = 90, phi = -90, ltheta = 20,
      col = 2, border = NA, main = "Kdist", d = 0.5, scale = FALSE,
      expand = 3, shade = 0.9)

band <- bootstrapBand(X = x.grid, FUN = kde, Grid = total.grid, B = 100, 
                      parallel = FALSE, alpha = 0.1, h = h)

#computes the persistent homology of the superlevel sets
#if trying other functions, FUN       and k/h/m0 must line up
DiagGrid <- gridDiag(X = x.grid, FUN = knnDE, k = 60, lim = cbind(Xlim, Ylim), by = 0.1,
                     sublevel = FALSE, library = "Dionysus", location = TRUE,
                     printProgress = FALSE)

plot(DiagGrid[["diagram"]], band = 2 * band[["width"]],
     main = "KDE Diagram")


par(mfrow = c(1, 2), mai = c(0.8, 0.8, 0.3, 0.1))
plot(DiagGrid[["diagram"]], rotated = TRUE, band = band[["width"]],
     main = "Rotated Diagram")
plot(DiagGrid[["diagram"]], barcode = TRUE, main = "Barcode")

max.scale <- 3 # limit of the filtration
max.dimension <- 1 # components and loops
#0 for components, 1 for loops, 2 for voids, etc.

DiagRips <- ripsDiag(X = total.grid, max.dimension, max.scale,
                     library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = FALSE)

plot(DiagRips[["diagram"]], rotated = TRUE, band = band[["width"]],
     main = "Rotated Diagram")
plot(DiagRips[["diagram"]], barcode = TRUE, main = "Barcode")

# persistence diagram of alpha complex 
DiagAlphaCmplx <- alphaComplexDiag(X = total.grid, library = c("GUDHI", "Dionysus"),
                                   location = TRUE, printProgress = TRUE)

# plot
par(mfrow = c(1, 2))
plot(DiagAlphaCmplx[["diagram"]], main = "Alpha complex persistence diagram")
one <- which(DiagAlphaCmplx[["diagram"]][, 1] == 1)
one <- one[which.max( + DiagAlphaCmplx[["diagram"]][one, 3] - 
                        DiagAlphaCmplx[["diagram"]][one, 2])]
plot(total.grid, col = 1, main = "Representative loop")
for (i in seq(along = one)) { 
  for (j in seq_len(dim(DiagAlphaCmplx[["cycleLocation"]][[one[i]]])[1])) {
    lines(DiagAlphaCmplx[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1, col = i + 1)
  }
} 
par(mfrow = c(1, 1))

n <- 30
x.grid <- cbind(circleUnif(n = n), runif(n = n, min = -0.1, max = 0.1))

DiagAlphaShape <- alphaShapeDiag(X = total.grid, maxdimension = 1, 
                                 library = c("GUDHI", "Dionysus"), 
                                 location = TRUE, printProgress = TRUE)

par(mfrow = c(1, 2))
plot(DiagAlphaShape[["diagram"]], main = "Alpha complex persistence diagram")
one <- which(DiagAlphaShape[["diagram"]][, 1] == 1)
one <- one[which.max( + DiagAlphaShape[["diagram"]][one, 3] - 
                        DiagAlphaShape[["diagram"]][one, 2])]
plot(total.grid, col = 1, main = "Representative loop")
for (i in seq(along = one)) { 
  for (j in seq_len(dim(DiagAlphaShape[["cycleLocation"]][[one[i]]])[1])) {
    lines(DiagAlphaShape[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1, col = i + 1)
  }
} 

max.scale <- 0.4
# limit of the filtration
max.dimension <- 1 
# components and loops
FltRips <- ripsFiltration(X = total.grid, maxdimension = max.dimension,
                          maxscale = max.scale, dist = "euclidean", library = "GUDHI",
                          printProgress = TRUE)

#another alpha persistance diagram?
DiagAlphaShape <- alphaShapeDiag(X = x.grid, printProgress = FALSE)
plot(DiagAlphaShape[["diagram"]], main = "Persistance Diagram")

#bottleneck and wasserstein distances
Diag1 <- ripsDiag(x.grid[,1], maxdimension = 1, maxscale = 5)
Diag2 <- ripsDiag(x.grid[,2], maxdimension = 1, maxscale = 5)

print (bottleneck(Diag1[["diagram"]], Diag2[["diagram"]],dimension = 1))
print (wasserstein(Diag1[["diagram"]], Diag2[["diagram"]], p = 2, dimension = 1))

#landscape and silhouettes 
tseq <- seq(0, maxscale, length = 1000) #domain
Diag <- ripsDiag(X = x.grid, maxdimension, maxscale,library = "GUDHI", printProgress = FALSE)
Land <- landscape(Diag[["diagram"]], dimension = 1, KK = 1, tseq)
Sil <- silhouette(Diag[["diagram"]], p = 1, dimension = 1, tseq)


