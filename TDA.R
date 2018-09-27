library("TDA")

charge_data <- file.choose()
#Read in the file
Grid_Me <- unique(read.csv(charge_data))[,c(1,5)]
colnames(Grid_Me) <- c("Var1","Var2")

v <- c()
for (i in 1:(nrow(Grid_Me)/2)){
  v <- append(v, i*2)
}

Grid_Me <- Grid_Me[-v,]

Xlim <- c(0,100)
Ylim <- c(0,100)

Xseq <- sort(unique(Grid_Me[,1]))
Yseq <- sort(unique(Grid_Me[,2]))

X_Me <- circleUnif(900)

distance <- distFct(X = X_Me, Grid = Grid_Me)

#DTM (distance to measure) is measured by this complicated math formula
#calculate DTM for every point in Grid:
m0 <- 0.1
DTM <- dtm(X = X_Me, Grid = Grid_Me, m0 = m0)

#calculates nearest neighbor
k <- 60
kNN <- knnDE(X = X_Me, Grid = Grid_Me, k = k)

#estimates density?
h <- 0.3
KDE <- kde(X = X_Me, Grid = Grid_Me, h = h)

#estimates distance
h <- 0.3
Kdist <- kernelDist(X = X_Me, Grid = Grid_Me, h = h)

persp(Xseq, Yseq, matrix(DTM, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = 0, phi = -90, ltheta = 20,
      col = 2, border = NA, main = "Kdist", d = 0.5, scale = FALSE,
      expand = 3, shade = 0.9)

band <- bootstrapBand(X = X_Me, FUN = kde, Grid = Grid_Me, B = 100, 
                      parallel = FALSE, alpha = 0.1, h = h)

#computes the persistent homology of the superlevel sets
#if trying other functions, FUN       and k/h/m0 must line up
DiagGrid <- gridDiag(X = X_Me, FUN = knnDE, k = 60, lim = cbind(Xlim, Ylim), by = 0.1,
                     sublevel = FALSE, library = "Dionysus", location = TRUE,
                     printProgress = FALSE)

plot(DiagGrid[["diagram"]], band = 2 * band[["width"]],
     main = "KDE Diagram")


par(mfrow = c(1, 2), mai = c(0.8, 0.8, 0.3, 0.1))
plot(DiagGrid[["diagram"]], rotated = TRUE, band = band[["width"]],
     main = "Rotated Diagram")
plot(DiagGrid[["diagram"]], barcode = TRUE, main = "Barcode")

maxscale <- 3 # limit of the filtration
maxdimension <- 1 # components and loops
#0 for components, 1 for loops, 2 for voids, etc.

DiagRips <- ripsDiag(X = Grid_Me, maxdimension, maxscale,
                     library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = FALSE)

plot(DiagRips[["diagram"]], rotated = TRUE, band = band[["width"]],
     main = "Rotated Diagram")
plot(DiagRips[["diagram"]], barcode = TRUE, main = "Barcode")

X_Me <- circleUnif(n = 30)
# persistence diagram of alpha complex 
DiagAlphaCmplx <- alphaComplexDiag(X = Grid_Me, library = c("GUDHI", "Dionysus"),
                                   location = TRUE, printProgress = TRUE)

# plot
par(mfrow = c(1, 2))
plot(DiagAlphaCmplx[["diagram"]], main = "Alpha complex persistence diagram")
one <- which(DiagAlphaCmplx[["diagram"]][, 1] == 1)
one <- one[which.max( + DiagAlphaCmplx[["diagram"]][one, 3] - 
                        DiagAlphaCmplx[["diagram"]][one, 2])]
plot(Grid_Me, col = 1, main = "Representative loop")
for (i in seq(along = one)) { 
  for (j in seq_len(dim(DiagAlphaCmplx[["cycleLocation"]][[one[i]]])[1])) {
    lines(DiagAlphaCmplx[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1, col = i + 1)
  }
} 
par(mfrow = c(1, 1))

n <- 30
X_Me <- cbind(circleUnif(n = n), runif(n = n, min = -0.1, max = 0.1))

DiagAlphaShape <- alphaShapeDiag(X = Grid_Me, maxdimension = 1, library = c("GUDHI", "Dionysus"), 
                                 location = TRUE, printProgress = TRUE)

par(mfrow = c(1, 2))
plot(DiagAlphaShape[["diagram"]], main = "Alpha complex persistence diagram")
one <- which(DiagAlphaShape[["diagram"]][, 1] == 1)
one <- one[which.max( + DiagAlphaShape[["diagram"]][one, 3] - 
                        DiagAlphaShape[["diagram"]][one, 2])]
plot(Grid_Me, col = 1, main = "Representative loop")
for (i in seq(along = one)) { 
  for (j in seq_len(dim(DiagAlphaShape[["cycleLocation"]][[one[i]]])[1])) {
    lines(DiagAlphaShape[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1, col = i + 1)
  }
} 

maxscale <- 0.4
# limit of the filtration
maxdimension <- 1 
# components and loops
FltRips <- ripsFiltration(X = Grid_Me, maxdimension = maxdimension,
                          maxscale = maxscale, dist = "euclidean", library = "GUDHI",
                          printProgress = TRUE)

# #generates 400 points and puts stuff in grid to be analyzed
# X <- circleUnif(400)
# Xlim <- c(-1.6, 1.6); Ylim <- c(-1.7, 1.7); by <- 0.065
# Xseq <- seq(Xlim[1], Xlim[2], by = by)
# Yseq <- seq(Ylim[1], Ylim[2], by = by)
# Grid <- expand.grid(Xseq, Yseq)
# 
# #calculates distance for every point in Grid
# distance <- distFct(X = X, Grid = Grid)
# 
# #DTM (distance to measure) is measured by this complicated math formula
# #calculate DTM for every point in Grid:
# m0 <- 0.1
# DTM <- dtm(X = X, Grid = Grid, m0 = m0)
# 
# #calculates nearest neighbor
# k <- 60
# kNN <- knnDE(X = X, Grid = Grid, k = k)
# 
# #estimates density?
# h <- 0.3
# KDE <- kde(X = X, Grid = Grid, h = h)
# 
# #estimates distance
# h <- 0.3
# Kdist <- kernelDist(X = X, Grid = Grid, h = h)
# 
# #plot it!
# persp(Xseq, Yseq, matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
#     ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
#     col = 2, border = NA, main = "Kdist", d = 0.5, scale = FALSE,
#     expand = 3, shade = 0.9)
# 
# #calculates bootstrap confidence
# #possible to parallelize it??
# band <- bootstrapBand(X = X, FUN = kde, Grid = Grid, B = 100, 
#                       parallel = FALSE, alpha = 0.1, h = h)
# 
# #computes the persistent homology of the superlevel sets
# #if trying other functions, FUN and k/h/m0 must line up
# DiagGrid <- gridDiag(X = X, FUN = knnDE, k = 60, lim = cbind(Xlim, Ylim), by = by,
#   sublevel = FALSE, library = "Dionysus", location = TRUE,
#   printProgress = FALSE)
# 
# plot(DiagGrid[["diagram"]], band = 2 * band[["width"]],
#      main = "KDE Diagram")
# 
# #The Vietoris-Rips complex R(X, ε) consists of simplices with vertices
# #       and diameter at most ε. In other words, a simplex σ is included
# #       in the complex if each pair of vertices in σ is at most ε apart.
# 
# par(mfrow = c(1, 2), mai = c(0.8, 0.8, 0.3, 0.1))
# plot(DiagGrid[["diagram"]], rotated = TRUE, band = band[["width"]],
#         main = "Rotated Diagram")
# plot(DiagGrid[["diagram"]], barcode = TRUE, main = "Barcode")
# 
# Circle1 <- circleUnif(60)
# Circle2 <- circleUnif(60, r = 2) + 3
# Circles <- rbind(Circle1, Circle2)
# 
# maxscale <- 5 # limit of the filtration
# maxdimension <- 1 # components and loops
# #0 for components, 1 for loops, 2 for voids, etc.
# 
# DiagRips <- ripsDiag(X = Circles, maxdimension, maxscale,
#                      library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = FALSE)
# 
# plot(DiagRips[["diagram"]], rotated = TRUE, band = band[["width"]],
#      main = "Rotated Diagram")
# plot(DiagRips[["diagram"]], barcode = TRUE, main = "Barcode")














