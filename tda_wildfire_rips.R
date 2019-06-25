library(ggplot2)
library(package = "TDA")
#devtools::install_github("paultpearson/TDAmapper")
library(TDAmapper)
library(igraph)
#install.packages("networkD3")
library("networkD3")
#install.packages("netdiffuseR")
library("netdiffuseR")

# X1: Max Temp(2), Specific Humidity (8) Tmin(4)
# X2: Max Temp(2), Wind Speed (m/s) (10)
# X3: Fire Weather Index (6), Specific Humidity (8)
# X4: Fire Weather Index (6), Fire Weather Index(12)
# ISI (15) BUI(16)
# Wind Speed (m/s) (10) DC(14)
# DC(14) Max Temp(2)

setwd("/Volumes/HKIM/TDA/")
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

mapper_graph(tmax_humidity)
mapper_graph(tmax_humidity_y)
mapper_graph(tmax_speed)
mapper_graph(tmax_speed_y)
mapper_graph(tmean_humidity)
mapper_graph(tmean_humidity_y)
mapper_graph(tmean_fwi)
mapper_graph(tmean_fwi_y)


#using tdamapper
mapper_graph <- function(data){
  data.dist <- dist(data[,c(1,2)])
  data.mapper <- mapper(dist_object = data.dist,
                        filter_values = data[,1],
                        num_intervals = 6,
                        percent_overlap = 50,
                        num_bins_when_clustering = 50) #was 10
  
  data.graph <- graph.adjacency(data.mapper$adjacency, mode="undirected")
  par(mfrow=c(2,1), mar=c(2,3,2,2))
  
  y.mean.vertex <- rep(0,data.mapper$num_vertices)
  vertex.size <- rep(0,data.mapper$num_vertices)
  for (i in 1:data.mapper$num_vertices){
    points.in.vertex <- data.mapper$points_in_vertex[[i]]
    y.mean.vertex[i] <-mean((data[,2][points.in.vertex]))
    vertex.size[i] <- length((data.mapper$points_in_vertex[[i]]))/4
  }
  y.mean.vertex.grey <- grey(1-(y.mean.vertex - min(y.mean.vertex))
                             /(max(y.mean.vertex) - min(y.mean.vertex) ))
  
  V(data.graph)$color <- y.mean.vertex.grey
  V(data.graph)$size <- vertex.size
  plot(data.graph, main = paste("adjacency between", colnames(data)[1], "and\n", colnames(data)[2],
                                ": size based on clustering"), cex.main=0.5, 
       horizontal=TRUE)
  
  data.mapper2 <- mapper2D(
    distance_matrix = dist(data.frame( x=data[,1], y=data[,2] )),
    num_intervals = c(5,5,5),
    percent_overlap = 50,
    num_bins_when_clustering = 50)
  
  #changes color based on if there are fires or not
  # data$Color <- cut(data$fcount_aqua, breaks = c(-0.1, 50, Inf), 
  #                   labels = c("black", "red"))
  
  d<-data[,3]
  cols<-setNames(colorRampPalette(c("blue", "red"))(length(unique(d)))
                 , sort(unique(d)))
  for (i in 1:data.mapper2$num_vertices){
    if ((data[i,3]) >= 50){
      vertex.size[i] <- (data[i,3])/5
    } else{
      vertex.size[i] <- 8
    }
  }
  mapper.graph <- graph.adjacency(data.mapper2$adjacency, mode="undirected")
  plot(mapper.graph, vertex.color = cols[as.character(data[,3])], cex.main=0.5, 
       horizontal=TRUE,
       vertex.label = NA, vertex.size = vertex.size, 
       main=paste("adjacency between", colnames(data)[1], "and\n", colnames(data)[2],
                  ": size based on fire count"))
}
