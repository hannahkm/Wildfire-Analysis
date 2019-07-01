#' @author Paul Pearson, \email{pearsonp@@hope.edu}
#' @references \url{https://github.com/paultpearson/TDAmapper}
#' @seealso \code{\link{mapper1D}}
#' @keywords mapper2D

data <- read.csv("merra2_active_calfire_jja.csv")[, c("t2mmax", "qv2m",
                                                      "fcount_aqua", "year", "month", "day")]

distance_matrix = dist(data.frame( x=data[,1], y=data[,2] ))
filter_values = list(data[,1],data[,2])
num_intervals = c(5,5)
percent_overlap = 60
num_bins_when_clustering = 60

# initialize variables
vertex_index <- 0

# indexed from 1 to the number of vertices
level_of_vertex <- c()
points_in_vertex <- list()
# filter_values_in_vertex <- list() # just use filter_values[points_in_vertex[[i]]]

# indexed from 1 to the number of levels
points_in_level <- list()
vertices_in_level <- list()
# filter_values_in_level <- list() # just use filter_values[points_in_level[[i]]]

filter_min_1 <- min(filter_values[[1]])
filter_max_1 <- max(filter_values[[1]])
filter_min_2 <- min(filter_values[[2]])
filter_max_2 <- max(filter_values[[2]])

interval_length_1 <-
  (filter_max_1 - filter_min_1)/(num_intervals[1]-(num_intervals[1] - 1)*percent_overlap/100)
interval_length_2 <-
  (filter_max_2 - filter_min_2)/(num_intervals[2]-(num_intervals[2]-1)*percent_overlap/100)

step_size_1 <- interval_length_1 * (1 - percent_overlap / 100)
step_size_2 <- interval_length_2 * (1 - percent_overlap / 100)

num_levels <- num_intervals[1] * num_intervals[2]

# level_index_matrix <- matrix(1:num_levels, num_intervals[1], num_intervals[2])
#
# Given any sequential index i in 1:num_levels,
# you can get the ordered pair index (row_index,col_index) by
# row_index <- which(level_index_matrix == i, arr.ind=TRUE)[1]
# col_index <- which(level_index_matrix == i, arr.ind=TRUE)[2]
#
# Given any ordered pair index (row_index,col_index),
# you can get the sequential index i in 1:num_levels by
# i <- level_index_matrix[row_index,col_index]

# Given any sequential index i in 1:num_levels,
# "row_index" = level_indices_1[i]
# "col_index" = level_indices_2[i]
level_indices_1 <- rep(1:num_intervals[1], num_intervals[2])
level_indices_2 <- rep(1:num_intervals[2], each = num_intervals[1])

# begin mapper main loop
for (level in 1:num_levels) {
  level_1 <- level_indices_1[level]
  level_2 <- level_indices_2[level]
  
  min_value_in_level_1 <-
    filter_min_1 + (level_1 - 1) * step_size_1
  min_value_in_level_2 <-
    filter_min_2 + (level_2 - 1) * step_size_2
  max_value_in_level_1 <- min_value_in_level_1 + interval_length_1
  max_value_in_level_2 <- min_value_in_level_2 + interval_length_2
  
  points_in_level_logical <-
    (min_value_in_level_1 <= filter_values[[1]]) &
    (min_value_in_level_2 <= filter_values[[2]]) &
    (filter_values[[1]] <= max_value_in_level_1) &
    (filter_values[[2]] <= max_value_in_level_2)
  
  num_points_in_level <- sum(points_in_level_logical)
  points_in_level[[level]] <- which(points_in_level_logical == TRUE)
  
  if (num_points_in_level == 0) {
    print('Level set is empty')
    vertices_in_level[[level]] <-
      -1 #  Bertrand Michel's suggestion for empty levels: use flag -1
    next
  }
  
  if (num_points_in_level == 1) {
    print('Level set has only one point')
    num_vertices_in_level <- 1
    cluster_indices_within_level <- c(1)
    vertices_in_level[[level]] <-
      vertex_index + (1:num_vertices_in_level)
  }
  
  if (num_points_in_level > 1) {
    # use as.matrix() to put the distance matrix in square form,
    # and as.dist() to put it in vector form
    # This could probably use some optimization...
    level_distance_matrix <-
      as.dist(as.matrix(distance_matrix)[points_in_level_logical, points_in_level_logical])
    level_max_distance <- max(level_distance_matrix)
    # use R's hclust (provided by stats or fastcluster)
    # in place of Matlab's linkage function.
    level_hcluster_ouput <-
      hclust(level_distance_matrix, method = "single")
    heights <- level_hcluster_ouput$height
    cutoff <-
      cluster_cutoff_at_first_empty_bin(heights, level_max_distance, num_bins_when_clustering)
    
    # use as.vector() to get rid fo the names for the vector entries
    cluster_indices_within_level <-
      as.vector(cutree(level_hcluster_ouput, h = cutoff))
    num_vertices_in_level <- max(cluster_indices_within_level)
    
    vertices_in_level[[level]] <-
      vertex_index + (1:num_vertices_in_level)
    
    # points_in_level[[level]] and cluster_indices_within_level have the same length.
    # heights has length 1 less than points_in_level[[level]] and cluster_indices_within_level
    # print(heights)
    # print(points_in_level[[level]])
    # print(cluster_indices_within_level)
  }
  
  
  for (j in 1:num_vertices_in_level) {
    vertex_index <- vertex_index + 1
    
    # points_in_level_logical is a logical vector, so use which(points_in_level_logical==TRUE)
            #to convert it to a numerical vector of indices
    #nodeset <- which(points_in_level_logical==TRUE)[cluster_indices_within_level == j]
    
    level_of_vertex[vertex_index] <- level
    points_in_vertex[[vertex_index]] <-
      which(points_in_level_logical == TRUE)[cluster_indices_within_level == j]
    #points_in_vertex[[vertex_index]] <- nodeset
    #filter_values_in_vertex[[vertex_index]] <- filter_values[nodeset]
    
  }
  
} # end mapper main loop


# Note: num_vertices = vertex index.
# Create the adjacency matrix for the graph, starting with a matrix of zeros
adja <- mat.or.vec(vertex_index, vertex_index)

for (i in 1:num_intervals[1]) {
  for (j in 2:num_intervals[2]) {
    # For adjacent level sets L_{i,j} and L_{i,j-1}, get the sequential index values k1 and k2
    k1 <- which((level_indices_1 == i) & (level_indices_2 == j))
    k2 <-
      which((level_indices_1 == i) & (level_indices_2 == j - 1))
    
    # check that both level sets are nonemtpy
    if ((vertices_in_level[[k1]][1] != -1) &
        (vertices_in_level[[k2]][1] != -1)) {
      for (v1 in vertices_in_level[[k1]]) {
        for (v2 in vertices_in_level[[k2]]) {
          # return 1 if the intersection is nonempty
          adja[v1, v2] <-
            (length(intersect(
              points_in_vertex[[v1]],
              points_in_vertex[[v2]]
            )) > 0)
          adja[v2, v1] <- adja[v1, v2]
        }
      }
      
    }
  } # end part 1 of constructing adjacency matrix
}
for (j in 1:num_intervals[2]) {
  for (i in 2:num_intervals[1]) {
    # For adjacent level sets L_{i,j} and L_{i-1,j}, get the sequential index values k1 and k2
    k1 <- which((level_indices_1 == i) & (level_indices_2 == j))
    k2 <-
      which((level_indices_1 == i - 1) & (level_indices_2 == j))
    
    # check that both level sets are nonemtpy
    if ((vertices_in_level[[k1]][1] != -1) &
        (vertices_in_level[[k2]][1] != -1)) {
      for (v1 in vertices_in_level[[k1]]) {
        for (v2 in vertices_in_level[[k2]]) {
          # return 1 if the intersection is nonempty
          adja[v1, v2] <-
            (length(intersect(
              points_in_vertex[[v1]],
              points_in_vertex[[v2]]
            )) > 0)
          adja[v2, v1] <- adja[v1, v2]
        }
      }
      
    }
  } # end part 2 of constructing adjacency matrix
}

mapperoutput <- list(
  adjacency = adja,
  num_vertices = vertex_index,
  level_of_vertex = level_of_vertex,
  points_in_vertex = points_in_vertex,
  #filter_values_in_vertex = filter_values_in_vertex,
  points_in_level = points_in_level,
  vertices_in_level = vertices_in_level
)

class(mapperoutput) <- "TDAmapper"





cluster_cutoff_at_first_empty_bin <- function(heights, diam, num_bins_when_clustering) {
  
  # if there are only two points (one height value), then we have a single cluster
  if (length(heights) == 1) {
    if (heights == diam) {
      cutoff <- Inf
      return(cutoff)
    }
  }
  
  bin_breaks <- seq(from=min(heights), to=diam, 
                    by=(diam - min(heights))/num_bins_when_clustering)
  if (length(bin_breaks) == 1) { bin_breaks <- 1 }
  
  myhist <- hist(c(heights,diam), breaks=bin_breaks, plot=FALSE)
  z <- (myhist$counts == 0)
  if (sum(z) == 0) {
    cutoff <- Inf
    return(cutoff)
  } else {
    #which returns the indices of the logical vector (z == TRUE), min gives the smallest index
    cutoff <- myhist$mids[ min(which(z == TRUE)) ]
    return(cutoff)
  }
  
}

