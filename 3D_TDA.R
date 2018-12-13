library("TDA")
library("rgl")

charge_data <- file.choose()
#Read in the file
inputted.file <- data.frame(read.csv(charge_data), header = TRUE)

x.grid <- data.frame(inputted.file[2:nrow(inputted.file),c(1,3,5)], stringsAsFactors = FALSE)
#x.grid <- as.numeric(unlist(x.grid))

#Creates categorical variables for nonnumerical columns
for (i in 1:ncol(x.grid)){
  if (is.factor(x.grid[,i])){
    this.data <- x.grid[,i]
    this.data.f <- factor(this.data, levels = as.character(unique(this.data)), 
                          labels=c(1:length(unique(this.data))))
    this.data.f <- as.numeric(this.data.f)
    
    x.grid[,i] <- this.data.f
  } 
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


#color changes based on z value
z<-matrix(DTM, ncol = length(Yseq), nrow = length(Xseq))
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3d(Xseq, Yseq, z, xlim = NULL, ylim = NULL, zlim = NULL, col = color[zcol],
        xlab = NULL, ylab = NULL, zlab = NULL, add = FALSE,
        forceClipregion = FALSE)

tree <- clusterTree(x.grid, k, density = "knn")
tree.KDE <- clusterTree(x.grid, k, h = 0.3, density = "kde")

#plots lambda trees
plot(tree, type = "lambda", color = NULL, add = FALSE, main = "knn lambda tree")
plot(tree.KDE, type = "lambda", main = "lambda Tree (kde)")

# plot clusters
plot(x.grid, pch = 19, cex = 0.6, main = "cluster labels")
for (i in tree[["id"]]){
  points(matrix(x.grid[tree[["DataPoints"]][[i]]], ncol = 2), col = i, pch = 19,
         cex = 0.6)
}


