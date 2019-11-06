library(ggplot2)
library(package = "TDA")
#devtools::install_github("paultpearson/TDAmapper")
library(TDAmapper)
library(igraph)
#install.packages("BBmisc")
library(BBmisc)
#install.packages("plotrix")
library("plotrix")
library("circlize")
library("plyr")

# X1: Max Temp(2), Specific Humidity (8) Tmin(4)
# X2: Max Temp(2), Wind Speed (m/s) (10)
# X3: Fire Weather Index (6), Specific Humidity (8)
# X4: Fire Weather Index (6), Fire Weather Index(12)
# ISI (15) BUI(16)
# Wind Speed (m/s) (10) DC(14)
# DC(14) Max Temp(2)

setwd("/Volumes/HKIM/TDA/")
#setwd("E:\\TDA")
setwd("/Users/hk/Desktop/School/MRHS/11th\ Grade/R/NN-ML/Wildfire-NN-ML/ML_Data/Old\ Data")

tmax_humidity <- read.csv("merra2_active_calfire_jja.csv")
if (true){
  tmax_humidity_y <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_inactive_calfire_jja.csv")[,c("t2mmax", "qv2m",
                                                                    "fcount_aqua")]
  tmax_speed <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_active_calfire_jja.csv")[,c("t2mmax", "speed",
                                                             "fcount_aqua")] #2,10
  tmax_speed_y <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_inactive_calfire_jja.csv")[,c("t2mmax", "speed",
                                                                 "fcount_aqua")]
  
  tmean_humidity <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_active_calfire_jja.csv")[,c("dc","qv2m",
                                                                 "fcount_aqua")] #6,8
  tmean_humidity_y <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_inactive_calfire_jja.csv")[,c("dc","qv2m",
                                                                     "fcount_aqua")]
  tmean_fwi <- read.csv(
    "C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_active_calfire_jja.csv")[,c("t2m","fwi","fcount_aqua")] #6,12
  tmean_fwi_y <- read.csv(
    "C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_inactive_calfire_jja.csv")[,c("t2m","fwi","fcount_aqua")]
  
  bui_dc <- read.csv(
    "C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_active_calfire_jja.csv")[,c("bui","dc","fcount_aqua")]
  bui_dc_y <- read.csv(
    "C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_inactive_calfire_jja.csv")[,c("bui","dc","fcount_aqua")]
}
rips_persistence(tmax_humidity)
mapper_graph(tmax_humidity, "active")
if (true){
  mapper_graph(tmax_humidity_y, "inactive")
  mapper_graph(tmax_speed, "active")
  mapper_graph(tmax_speed_y, "inactive")
  mapper_graph(tmean_humidity, "active")
  mapper_graph(tmean_humidity_y, "inactive")
  mapper_graph(tmean_fwi, "active")
  mapper_graph(tmean_fwi_y, "inactive")
  mapper_graph(bui_dc, "active")
  mapper_graph(bui_dc_y, "inactive")
}


#notes: generally, the y plots (inactive seasons) don't have voids (blue squares)
#       but the x plots (active seasons) do have voids
#       One exception: tmax_humidity_y DOES have voids?
if (true){
  rips_persistence(tmax_humidity)
  rips_persistence(tmax_humidity_y)
  persistence_comparison(tmax_speed, tmax_speed_y)
  #persistence_comparison(tmean_humidity, tmean_humidity_y)
  persistence_comparison(tmean_fwi, tmean_fwi_y)
  #persistence_comparison(tmax_humidity, tmax_humidity_y)
  
  rips_persistence(tmax_speed)
  rips_persistence(tmax_speed_y)
  rips_persistence(tmean_humidity)
  rips_persistence(tmean_humidity_y)
  rips_persistence(tmean_fwi)
  rips_persistence(tmean_fwi_y) 
  persistence_comparison(tmax_speed, tmax_speed_y)
  persistence_comparison(tmean_humidity, tmean_humidity_y)
  persistence_comparison(tmean_fwi, tmean_fwi_y)
  
  
  radius_plots(tmax_humidity, 0, "active")
  radius_plots(tmax_humidity, 0.1, "active")
  radius_plots(tmax_humidity, 0.3, "active")
  radius_plots(tmax_humidity, 0.6, "active")
  radius_plots(tmax_humidity, 0.8, "active")
  radius_plots(tmax_humidity_y, 0.3, "inactive")
  persistence_comparison(tmax_humidity, tmax_humidity_y)
  persistence_comparison(tmax_speed, tmax_speed_y)
  persistence_comparison(tmean_humidity, tmean_humidity_y)
  persistence_comparison(tmean_fwi, tmean_fwi_y)
  
  # plot_clusters(tmax_humidity,"active")
  # plot_clusters(tmax_humidity_y,"active")
  # plot_clusters(tmax_speed,"active")
  # plot_clusters(tmax_speed_y,"active")
  # plot_clusters(tmean_humidity,"active")
  # plot_clusters(tmean_humidity_y,"active")
  # plot_clusters(tmean_fwi,"active")
  # plot_clusters(tmean_fwi_y,"active")
  
  returnData(tmax_humidity)
}
## FUNCTIONS ####################################################################


plot_clusters <- function(data,type){
  data.dist <- dist(data[,c(1,2)])
  par(mfrow=c(1,1))
  
  data.mapper2 <- mapper2D(
    distance_matrix = dist(data.frame( x=data[,1], y=data[,2] )),
    filter_values = list(data[,1],data[,2]),
    num_intervals = c(5,5),
    percent_overlap = 60,
    num_bins_when_clustering = 60)
  
  color.ramp <- rand_color(data.mapper2$num_vertices, hue = NULL, luminosity = "dark", transparency = 0.3)
  
  #colorRampPalette(c('red','purple'))(data.mapper2$num_vertices)
  data$color <- 0
  for (i in 1:data.mapper2$num_vertices){
    points.in.vertex <- data.mapper2$points_in_vertex[[i]]
    len <- length(points.in.vertex)
    for (j in 1:len){
      if (!is.na(points.in.vertex[[j]])){
        data$color[points.in.vertex[[j]]] <- color.ramp[i]
      }
    }
  }
  x1 <- data[,1]
  y1 <- data[,2]
  View(data)
  plot(x1, y1, xlim=range(x1), ylim=range(y1), xlab = colnames(data)[1], 
       ylab=colnames(data)[2], pch=16, col=ifelse(data$color==0, "black", data$color),
       cex=ifelse(data$color==0, 0.2,1.5)) 
  
  # for (i in 1:length(x1)){
  #   for (j in i:length(y1)){
  #     if (data[i,4]!=0 && data[j,4]!=0 && data[i,4]==data[j,4]){
  #       segments(x1[i],y1[i],x1[j],y1[j], lwd=0.5)
  #     } 
  #   }
  # }
  
}

persistence_comparison <- function(data, data1){
  Diag <- ripsDiag(data[,c(1,2)], maxdimension = 2, maxscale = 1.0, location = TRUE,
                   library = c("GUDHI", "Dionysus"), printProgress = FALSE)$diagram
  Diag1 <- ripsDiag(data1[,c(1,2)], maxdimension = 2, maxscale = 1.0, location = TRUE,
                    library = c("GUDHI", "Dionysus"), printProgress = FALSE)$diagram
  
  df1 <- printList(Diag)
  df2 <- printList(Diag1)
  
  count <- 1
  while (as.numeric(df1[count,1])==0){
    count <- count + 1
  }
  
  df1 <- df1[count:nrow(df1),]
  df2 <- df2[count:nrow(df2),]
  
  active.x1<-df1[,2]
  active.y1<-df1[,3]
  inactive.x1 <- df2[,2]
  inactive.y1 <- df2[,3] 
  
  # par(mfrow=c(1,1), mar=c(7,7,8,7))
  # plot(active.x1, active.y1, xlim=range(active.x1), ylim=range(active.y1), xlab = "birth", 
  #      ylab="death", pch = 16, col = 'red', cex.main=2.5, cex.lab=2.5, cex = 2,
  #      main=paste("comparison of loops/voids in \n active and inactive seasons -",
  #                 colnames(data)[1], " and ", colnames(data1)[2]))
  # #points(active.x1, active.y1, col='red', cex=0.5, pch=16) 
  # points(inactive.x1, inactive.y1, col='blue', pch=16, cex = 2) 
  # segments(0,0,1,1, lwd=0.5)
  # legend("bottomright", legend=c("Active", "Inactive"),
  #        col=c("red", "blue"), pch=c(16, 16), cex = 1.5, bty="n")

  plot(active.x1, active.y1, xlim=range(active.x1), ylim=range(active.y1), xlab = "birth",
       ylab="death", pch = ifelse(df1[,1] == 1,18,19), col = 'red',
       main=paste("comparison of loops/voids in \n active and inactive seasons\n",
                  colnames(data)[1], " and ", colnames(data1)[2]))
  #points(active.x1, active.y1, col='red', cex=0.5, pch=16)
  points(inactive.x1, inactive.y1, col='blue', pch=ifelse(df2[,1] == 1,18,19))
  segments(0,0,1,1, lwd=0.5)
  legend("bottomright", inset = 0.01, legend=c("Active", "Inactive", "loops", "voids"),
         col=c("red", "blue", "black", "black"), pch=c(16, 16, 18, 19))
  
}

radius_plots <- function(data, radius, type){
  connected<-0
  #segment.count<-0
  #edge.count<-0
  x1<-data[,1]
  y1<-data[,2]
  
  par(mfrow=c(1,1))
  plot(x1, y1, xlim=range(x1), ylim=range(y1), xlab = colnames(data)[1], 
       ylab=colnames(data)[2], pch=16,
       main = paste("plot between", colnames(data)[1], "and\n", 
                    colnames(data)[2],"with radius", radius, "\n - ", type, "summer")) 
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

cluster_overlap <- function(data){
  data.dist <- dist(data[,c("t2mmax","qv2m")])
  par(mfrow=c(1,1))
  
  data.mapper2 <- mapper2D(
    distance_matrix = dist(data.frame( x=data$t2mmax, y=data$qv2m )),
    filter_values = list(data$t2mmax,data$qv2m),
    num_intervals = c(5,5),
    percent_overlap = 60,
    num_bins_when_clustering = 60)
  
  data.graph <- graph.adjacency(data.mapper2$adjacency, mode="undirected")
  
  num.verx <- data.mapper2$num_vertices
  print(num.verx)
  adj <- data.frame(matrix(nrow=num.verx, ncol=num.verx))
  print(dim(adj))
  for (i in 1:num.verx){
    for (j in i:num.verx){
      x1 <- data.mapper2$points_in_vertex[[i]]
      x2 <- data.mapper2$points_in_vertex[[j]]
      overlap <- length(intersect(x1, x2))
      total <- length(union(x1, x2))
      adj[i,j] <- (overlap/total)*100
    }
  }
  
  View(adj)
}

mapper_graph <- function(data, type){
  data.dist <- dist(data[,c("t2mmax","qv2m")])
  par(mfrow=c(1,1))
  
  data.mapper2 <- mapper2D(
    distance_matrix = dist(data.frame( x=data$t2mmax, y=data$qv2m )),
    filter_values = list(data$t2mmax,data$qv2m),
    num_intervals = c(5,5),
    percent_overlap = 60,
    num_bins_when_clustering = 60)
  
  data.graph <- graph.adjacency(data.mapper2$adjacency, mode="undirected")
  l <- layout.auto(data.graph)
  
  column.list <- c("day", "t2mmax", "qv2m", "speed", "fwi", "isi", "bui", "dc")
  for (k in 1:8){
    column = column.list[k]
    vertex.size <- rep(0,data.mapper2$num_vertices)
    
    #i <- which(colnames(data) == column)
    for (i in 1:data.mapper2$num_vertices){
      points.in.vertex <- data.mapper2$points_in_vertex[[i]]
      len <- length(points.in.vertex)
      count <- 0
      
      for (j in 1:len) {
        if (!is.na(data[points.in.vertex[[j]], column])) {
          count <- count + data[points.in.vertex[[j]], column]
          # if (data[points.in.vertex[[j]], column] >= 50) {
          #   count <- count + abs(data[points.in.vertex[[j]], column])
          # }
        }
      }
      if (column == "day"){
        if (count <= 5) {
          count <- 5
        } else{
          count <- count / 20
        }
        vertex.size[i] <- count * 0.1
      } else {
        vertex.size[i] <- count * 0.3
      }
      
    }
    
    
    
    # plot(data.graph, main = paste(type, "summers - size based \n on",column), 
    #      vertex.label = NA,
    #      cex.main=0.5, horizontal=TRUE, vertex.size = vertex.size, layout = l)
    if (k == 1){
      vertex.labels <- rep(NA,data.mapper2$num_vertices)
      for (j in 1:data.mapper2$num_vertices){
        if (vertex.size[j] %in% rev(sort(vertex.size))[1:10]){
          vertex.labels[j] <- j
        }
      }
      # print(vertex.size)
      # print(sort(vertex.size))
    }
    
    
    plot(data.graph, main = paste(type, "summers - size based \n on", 
                                  column), 
         vertex.label = vertex.labels, cex.main=0.5, horizontal=TRUE, 
         vertex.label.color = "white", vertex.size = (abs(vertex.size)+2)*2, layout = l,
         vertex.color = ifelse(vertex.size > 0, "red", 
                               ifelse(vertex.size < 0,"blue", "black")))
  }

}

#rips persistence using kde, knnDE, and dtm for confidence band
rips_persistence <- function(data){
  Diag <- ripsDiag(data[,c(2,8)], maxdimension = 2, maxscale = 1.0, location = TRUE,
                   library = c("GUDHI", "Dionysus"), printProgress = FALSE)$diagram
  
  
  #par(mfrow = c(2,1))
  band <- bootstrapBand(X=data[,c(1,2)], FUN=kde, Grid=data[,c(1,2)], B=100,
                        parallel=FALSE, alpha = 0.1, h=0.3)
  plot(Diag, diagLim=NULL, dimension=NULL, col=NULL, rotated=FALSE, barcode=FALSE,
       band=2*band[["width"]], lab.line=2.2, colorBand="pink",
       colorBorder=NA, add = FALSE, cex.lab = 2.5, cex = 2.5,
       main = "Active")
       
  #main= paste("persistence diagram with ", colnames(data)[1], 
      #"and \n", colnames(data)[2],": kde band"))
  plot(Diag, diagLim=NULL, dimension=NULL, col=NULL, rotated=FALSE, barcode=FALSE,
       band=NULL, lab.line=2.2, colorBand="pink",
       colorBorder=NA, add = FALSE, cex.lab = 2.5, cex = 2.5,
       main= "Inactive")
  # legend("bottomright", legend=c("connection", "loops", "voids"), pch=c(16, 2, 5), 
  #        col=c("black", "red", "blue"),
  #        title = "Legend")
  
  # df <- printList(Diag)
  # 
  # df
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

printList <- function(list) {
  
  c1 <- as.data.frame(as.matrix(list[,1]))
  c2 <- as.data.frame(as.matrix(list[,2]))
  c3 <- as.data.frame(as.matrix(list[,3]))
  df <- data.frame(matrix(ncol = 3, nrow = nrow(c1)))
  df[,1] <- c1
  df[,2] <- c2
  df[,3] <- c3
  colnames(df) <- c("dimension", "birth", "death")
  
  df
}

returnData <- function(data){
  par(mfrow = c(1, 1))
  
  data.mapper2 <- mapper2D( 
    distance_matrix = dist(data.frame(x = data[, 1], y = data[, 2])),
    filter_values = list(data[,1],data[,2]),
    num_intervals = c(5, 5),
    percent_overlap = 60,
    num_bins_when_clustering = 60
  )
  
  data.graph <-
    graph.adjacency(data.mapper2$adjacency, mode = "undirected")
  
  vertex.size <- rep(0, data.mapper2$num_vertices)
  df <- as.data.frame(matrix(ncol = 3))
  for (i in 1:data.mapper2$num_vertices) {
    points.in.vertex <- data.mapper2$points_in_vertex[[i]]
    len <- length(points.in.vertex)
    count <- 0
    for (j in 1:len) {
      if (!is.na(data[points.in.vertex[[j]], 3])) {
        count <- count + abs(data[points.in.vertex[[j]], 3])
        cat("vertex (i): ", i, " point # (j): ", j, " y-m-d: ", data[points.in.vertex[[j]],4],"-",
            data[points.in.vertex[[j]],5], "-", data[points.in.vertex[[j]],6], "\n")
        df_add <-
          as.data.frame(cbind(data[points.in.vertex[[j]], 4], data[points.in.vertex[[j]], 5],
                              data[points.in.vertex[[j]], 6]))
        df <- rbind(df, df_add)
      }
      else if (is.na(data[points.in.vertex[[j]], 3]) && j == 1) {
        cat("vertex: ", i, " empty", "\n")
      }
      
    }
    
    vertex.size[i] <- count
    
  }
  
  df <- unique(df[order(df[, 1], df[, 2], df[, 3]), ])
  df <- df[-1,]
  df
  
}







