setwd(
  "/Users/hk/Desktop/School/MRHS/11th\ Grade/R/NN-ML/Wildfire-NN-ML/ML_Data/Old\ Data"
)
data <- read.csv("merra2_active_calfire_jja.csv")[, c("t2mmax", "qv2m",
                                                "fcount_aqua", "year", "month", "day")] #2,8

data.dist <- dist(data[, c(1, 2)])
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
print(nrow(df))
