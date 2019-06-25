library("stringr")
setwd("/Volumes/HANNAH/TDA/Threat Hunt")
og_file <- file.choose()
bro_file <- data.frame(readLines(og_file), stringsAsFactors = FALSE, drop = FALSE)

#deletes all the headers at the top
to_delete <- c()
for (i in 1:nrow(bro_file)){
  if (substring(bro_file[i,1],1,1)=="#" && substring(bro_file[i,1],1,8) != "#fields\t"){
    to_delete <- append(to_delete, i)
  }
}
bro_file <- bro_file[-to_delete,]

#starts creating new data frame
num.rows <- nrow(bro_file)
csv.df <- data.frame()

#splits data by whitespace and enters each term into column
for (i in 2:num.rows){
  new.row <- str_split(bro_file[i,1], "\t")
  new.row <- as.character(unlist(new.row, use.names=FALSE))
  x <- f4(new.row)
  new.df <- t(data.frame(x, stringsAsFactors=FALSE))
  csv.df <- rbind(csv.df, new.df)
}

#creates column rows from #fields row in bro file
bro_file[1,1] <- toString(gsub("#fields\t", "", bro_file[1,1]))
new.row <- str_split(bro_file[1,1], "\t")
new.row <- as.character(unlist(new.row, use.names=FALSE))
x <- f4(new.row)
colnames(csv.df) <- as.character(x)

write.csv(csv.df, "csv data.csv", row.names = FALSE)

#convert into list
f4 <- function(n) {
  x <- character(length(n))
  for (i in 1:length(n)) {
    x[i] <- n[[i]]
  }
  x
}


