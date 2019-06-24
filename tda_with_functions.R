library(ggplot2)
library(package = "TDA")
#devtools::install_github("paultpearson/TDAmapper")
library(TDAmapper)
library(igraph)
#install.packages("networkD3")
library("networkD3")
#install.packages("netdiffuseR")
library("netdiffuseR")
#install.packages("plotrix")
library("plotrix")

# X1: Max Temp(2), Specific Humidity (8) Tmin(4)
# X2: Max Temp(2), Wind Speed (m/s) (10)
# X3: Fire Weather Index (6), Specific Humidity (8)
# X4: Fire Weather Index (6), Fire Weather Index(12)
# ISI (15) BUI(16)
# Wind Speed (m/s) (10) DC(14)
# DC(14) Max Temp(2)

setwd("C:/Users/kimh2/Desktop/Wildfire-Analysis")
tmax_humidity <- read.csv("merra2_active_calfire_jja.csv")[,c("t2mmax", "qv2m", 
                                                              "fcount_aqua")] #2,8
tmax_humidity_y <- read.csv("merra2_inactive_calfire_jja.csv")[,c("t2mmax", "qv2m",
                                                                  "fcount_aqua")]
tmax_speed <- read.csv("merra2_active_calfire_jja.csv")[,c("t2mmax", "speed",
                                                           "fcount_aqua")] #2,10
tmax_speed_y <- read.csv("merra2_inactive_calfire_jja.csv")[,c("t2mmax", "speed",
                                                               "fcount_aqua")]

tmean_humidity <- read.csv("merra2_active_calfire_jja.csv")[,c("t2m","qv2m",
                                                               "fcount_aqua")] #6,8
tmean_humidity_y <- read.csv("merra2_inactive_calfire_jja.csv")[,c("t2m","qv2m",
                                                                   "fcount_aqua")]
tmean_fwi <- read.csv(
  "merra2_active_calfire_jja.csv")[,c("t2m","fwi","fcount_aqua")] #6,12
tmean_fwi_y <- read.csv(
  "merra2_inactive_calfire_jja.csv")[,c("t2m","fwi","fcount_aqua")]

rips_persistence(tmax_humidity)
barcode_diag(tmax_humidity)
scatterplot_diag(tmax_humidity)
lambda_trees(tmax_humidity)
landscape_diag(tmax_humidity)
radius_plots(tmax_humidity, 2)
mapper_graph(tmax_humidity)

mapper_graph(tmax_humidity)
mapper_graph(tmax_humidity_y)
mapper_graph(tmax_speed)
mapper_graph(tmax_speed_y)
mapper_graph(tmean_humidity)
mapper_graph(tmean_humidity_y)
mapper_graph(tmean_fwi)
mapper_graph(tmean_fwi_y)


#notes: generally, the y plots (inactive seasons) don't have voids (blue squares)
#       but the x plots (active seasons) do have voids
#       One exception: tmax_humidity_y DOES have voids?
rips_persistence(tmax_humidity)
rips_persistence(tmax_humidity_y)
rips_persistence(tmax_speed)
rips_persistence(tmax_speed_y)
rips_persistence(tmean_humidity)
rips_persistence(tmean_humidity_y)
rips_persistence(tmean_fwi)
rips_persistence(tmean_fwi_y) 

radius_plots(tmax_humidity, 0.1)

returnData(tmax_humidity)

## FUNCTIONS ####################################################################

radius_plots <- function(data, radius){
  connected<-0
  #segment.count<-0
  #edge.count<-0
  x1<-data[,1]
  y1<-data[,2]
  
  par(mfrow=c(1,1))
  plot(x1, y1, xlim=range(x1), ylim=range(y1), xlab = colnames(data)[1], 
       ylab=colnames(data)[2], pch=16,
       main = paste("plot between", colnames(data)[1], "and\n", 
                    colnames(data)[2],"with radius", radius)) 
  points(x1, y1, col='black', cex=0.5, pch=16) 
  for (i in 1:nrow(data)) {
    xc=x1[[i]]
    yc=y1[[i]]
    draw.circle(xc,yc,radius=radius,nv=100,border=NULL,col="red",lty=1,density=80,
                angle=45,lwd=1) 
    points(xc, yc, type = "p", cex=0.3, pch=19)
    
  }
  
  for (i in 1:length(x1)){
    for (j in i:length(y1)){
      if (sqrt((x1[i]-x1[j])^2 + (y1[i]-y1[j])^2)<radius*2){
        segments(x1[i],y1[i],x1[j],y1[j], lwd=0.5)
        connected <- connected+1
      } 
    }
    #point.count <- point.count+1
  }
  
}

mapper_graph <- function(data){
  data.dist <- dist(data[,c(1,2)])
  par(mfrow=c(1,1), mar=c(2,3,2,2))
  
  # data.mapper <- mapper(dist_object = data.dist,
  #                       filter_values = data[,1],
  #                       num_intervals = 6,
  #                       percent_overlap = 50,
  #                       num_bins_when_clustering = 50) #was 10
  # 
  # data.graph <- graph.adjacency(data.mapper$adjacency, mode="undirected")
  # 
  # y.mean.vertex <- rep(0,data.mapper$num_vertices)
  # vertex.size <- rep(0,data.mapper$num_vertices)
  # for (i in 1:data.mapper$num_vertices){
  #   points.in.vertex <- data.mapper$points_in_vertex[[i]]
  #   y.mean.vertex[i] <-mean((data[,2][points.in.vertex]))
  #   vertex.size[i] <- length((data.mapper$points_in_vertex[[i]]))/4
  # }
  # y.mean.vertex.grey <- grey(1-(y.mean.vertex - min(y.mean.vertex))
  #                            /(max(y.mean.vertex) - min(y.mean.vertex) ))
  # 
  # V(data.graph)$color <- y.mean.vertex.grey
  # V(data.graph)$size <- vertex.size
  # plot(data.graph, main = paste("adjacency between", colnames(data)[1], "and\n",
  #                               colnames(data)[2], ": size based on clustering"), 
  #      cex.main=0.5, horizontal=TRUE)
  
  data.mapper2 <- mapper2D(
    distance_matrix = dist(data.frame( x=data[,1], y=data[,2] )),
    num_intervals = c(5,5),
    percent_overlap = 60,
    num_bins_when_clustering = 60)
  
  #changes color based on if there are fires or not
  # data$Color <- cut(data$fcount_aqua, breaks = c(-0.1, 50, Inf), 
  #                   labels = c("black", "red"))
  
  vertex.size <- rep(0,data.mapper2$num_vertices)

  # for (i in 1:data.mapper2$num_vertices){ OLD
  #   if ((data[i,3]) >= 50){
  #     vertex.size[i] <- (data[i,3])/5
  #   } else{
  #     vertex.size[i] <- 8
  #   }
  # }
  
  for (i in 1:data.mapper2$num_vertices){
    for (j in 1:length(data.mapper2$points_in_vertex[[i]])){
      vertex.size[i] <- vertex.size[i] + data[data.mapper2$points_in_vertex[[i]][j],3]
    }
  }

  vertex.size <- (scale(vertex.size)+1)*5
  
  d<-data[,3]
  cols<-setNames(colorRampPalette(c("blue", "red"))(length(unique(vertex.size)))
                 , sort(unique(vertex.size)))
  
  # cols <- as.data.frame(cols)
  # row.names(cols)<-1:nrow(cols)
  
  mapper.graph <- graph.adjacency(data.mapper2$adjacency, mode="undirected")
  plot(mapper.graph, vertex.color = cols[as.character(vertex.size)], cex.main=0.5, 
       horizontal=TRUE,
       vertex.label = NA, vertex.size = vertex.size, 
       main=paste("adjacency between", colnames(data)[1], "and\n", colnames(data)[2],
                  ": size based on fire count"))
}


#rips persistence using kde, knnDE, and dtm for confidence band
rips_persistence <- function(data){
  Diag <- ripsDiag(data[,c(1,2)], maxdimension = 2, maxscale = 1.0, location = TRUE,
                    library = c("GUDHI", "Dionysus"), printProgress = FALSE)$diagram
  
  printList(Diag)
  
  #par(mfrow = c(1,1))
  band <- bootstrapBand(X=data[,c(1,2)], FUN=kde, Grid=data[,c(1,2)], B=100,
                        parallel=FALSE, alpha = 0.1, h=0.3)
  plot(Diag, diagLim=NULL, dimension=NULL, col=NULL, rotated=FALSE, barcode=FALSE,
       band=2*band[["width"]], lab.line=2.2, colorBand="pink",
       colorBorder=NA, add = FALSE,
       main= paste("persistence diagram with ", colnames(data)[1], "and \n", colnames(data)[2],
                                                      ": kde band"))
  
  # band <- bootstrapBand(X=data[,c(1,2)], FUN=knnDE, Grid=data[,c(1,2)], B=100,
  #                       parallel=FALSE, alpha = 0.1, k=60)
  # plot(Diag, diagLim=NULL, dimension=NULL, col=NULL, rotated=FALSE, barcode=FALSE,
  #      band=2*band[["width"]], lab.line=2.2, colorBand="pink",
  #      colorBorder=NA, add = FALSE,
  #      main=paste("persistence diagram with ", colnames(data)[1], "and", colnames(data)[2],
  #                 ": knnDE band"))
  # 
  # band <- bootstrapBand(X=data[,c(1,2)], FUN=dtm, Grid=data[,c(1,2)], B=100,
  #                       parallel=FALSE, alpha = 0.1, m0=0.1)
  # plot(Diag, diagLim=NULL, dimension=NULL, col=NULL, rotated=FALSE, barcode=FALSE,
  #      band=2*band[["width"]], lab.line=2.2, colorBand="pink",
  #      colorBorder=NA, add = FALSE,
  #      main=paste("persistence diagram with ", colnames(data)[1], "and", colnames(data)[2],
  #                 ": dtm band"))
}

#barcode diagram for data
barcode_diag <- function(data){
  Diag <- ripsDiag(data[,c(1,2)], maxdimension = 2, maxscale = 1.0,
                   library = "GUDHI", printProgress = FALSE)$diagram
  
  par(mfrow = c(1,1), mar=c(3,3,3,3))
  plot(Diag, diagLim=NULL, dimension=NULL, col=NULL, rotated=FALSE, barcode=TRUE, 
       band=NULL, lab.line=2.2, colorBand="pink", colorBorder=NA, add = FALSE, 
       main=paste("barcode with ", colnames(data)[1], "and", colnames(data)[2]))
}

#scatterplot of data
scatterplot_diag <-function(data){
  data$Color <- cut(data$fcount_aqua, breaks = c(0, 50, Inf), 
                     labels = c("black", "red"))
  
  par(mfrow = c(1,1), mar=c(3,3,3,3))
  plot(data[,1], data[,2], col=data$Color, xlab=colnames(data)[1], 
       ylab=colnames(data)[2], main=paste("scatterplot with", colnames(data)[1], "and", 
                                          colnames(data)[2]))
  
  #plot(data, main="Active Summer", xlab=colnames(data)[1], ylab=colnames(data)[2])
  
}

#lambda trees using knn and kde
lambda_trees <- function(data){
  Tree <- clusterTree(data[,c(1,2)], k = 20, density = "knn")
  TreeKDE <- clusterTree(data[,c(1,2)], 20, h = 0.1, density = "kde")
  

  par(mfrow = c(2,1), mar=c(2,3,2,2))
  plot(Tree, type = "lambda", main = paste("lambda Tree (knn) ", colnames(data)[1], 
                                           "and", colnames(data)[2]))
  plot(TreeKDE, type = "lambda", main = paste("lambda Tree (kde) ", colnames(data)[1], 
                                              "and", colnames(data)[2]))
  
}

#landscape diagrams with 0, 1, and 2 dimensions
landscape_diag <- function(data){
  tseq <- seq(from = 0, to = 1.5, length = 1000) #domain 
  
  Diag=ripsDiag(data[,c(1,2)], maxdimension = 2, maxscale =1.0 , 
                printProgress=FALSE)$diagram
  Land1 <- landscape(Diag, dimension = 0, KK = 1, tseq = tseq)
  Land2 <- landscape(Diag, dimension = 1, KK = 1, tseq = tseq)
  Land3 <- landscape(Diag, dimension = 2, KK = 1, tseq = tseq)
  
  par(mfrow = c(3,1), mar=c(2,3,2,2))
  plot(tseq, Land1, type = "l", main = paste("Landscape, dim = 0, with", colnames(data)[1], 
      "and", colnames(data)[2]), ylab = "", asp = 1, col = "red", lwd = 3)
  plot(tseq, Land2, type = "l", main = paste("Landscape, dim = 1, with", colnames(data)[1], 
      "and", colnames(data)[2]), ylab = "", asp = 1, col = "red", lwd = 3)
  plot(tseq, Land3, type = "l", main = paste("Landscape, dim = 2, with", colnames(data)[1], 
      "and", colnames(data)[2]), ylab = "", asp = 1, col = "red", lwd = 3)
}







# #"ggplot2 doesn't know how to deal with data of class numeric"
# density_plots <- function(data){
#   tmax_humidity<-data[,1]
#   tmax_speed<-data[,2]
#   commonTheme=list(labs(color="Density",fill="Density",x="x",y="y"),theme_bw(),
#                    theme(legend.position=c(0,1), 3legend.justification=c(0,1)))
#   
#   ggplot(data=data, aes(tmax_humidity, tmax_speed)) + 
#       stat_densittmax_speed_yd(aes(fill=..level..,alpha=..level..), 
#       geom='polygon',colour='black') +
#       scale_fill_continuous(low="green",high="red") +
#       geom_smooth(method=lm, linetype=2, colour="red", se=FALSE) +
#       guides(alpha="none") + geom_point(shape=1, colour = 
#       "black", size = 0.1) + commonTheme
#   
#   plot <- ggplot(tmax_humidity, aes(tmax_humidity[,1], 
#       tmax_humidity[,2])) #here is the error!
#   
#   # a different type of density plot?
#   plot + stat_densittmax_speed_yd(geom="tile", aes(fill = ..density..), 
#                         contour = FALSE) + geom_point(colour = "white")
# }
# 

printList <- function(list) {
  
  c1 <- as.data.frame(as.matrix(list[,1]))
  c2 <- as.data.frame(as.matrix(list[,2]))
  c3 <- as.data.frame(as.matrix(list[,3]))
  df <- data.frame(matrix(ncol = 3, nrow = nrow(c1)))
  df$dimension <- c1
  df$birth <- c2
  df$death <- c3
  
  View(df)
}

returnData <- function(data){
  data.dist <- dist(data[,c(1,2)])
  par(mfrow=c(1,1), mar=c(2,3,2,2))
  
  data.mapper2 <- mapper2D(
    distance_matrix = dist(data.frame( x=data[,1], y=data[,2] )),
    num_intervals = c(5,5),
    percent_overlap = 60,
    num_bins_when_clustering = 60)
  
  df <- data.frame(matrix())
  
  for (i in 1:data.mapper2$num_vertices){
    df <- rbind.fill(df,as.data.frame(t(data.mapper2$points_in_vertex[[i]])))
  }
  df <- df[-1,]
  
  vertex.size <- rep(0,data.mapper2$num_vertices)
  
  for (i in 1:data.mapper2$num_vertices){
    for (j in 1:length(data.mapper2$points_in_vertex[[i]])){
      vertex.size[i] <- vertex.size[i] + data[j,3]
    }
  }
  
  vertex.size <- (scale(vertex.size)+1)*5
  df[,1] <- vertex.size
  
  View(df)
}





