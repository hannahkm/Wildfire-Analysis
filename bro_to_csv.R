library("stringr")
og_file <- file.choose()
bro_file <- data.frame(readLines(og_file), stringsAsFactors = FALSE, drop = FALSE)

#deletes all the headers at the top
to_delete <- c()
for (i in 1:nrow(bro_file)){
  if (substring(bro_file[i,1],1,1)=="#" && substring(bro_file[i,1],1,7) != "#fields"){
    to_delete <- append(to_delete, i)
  }
}
bro_file <- bro_file[-to_delete,]

bro_file[1,1] <- gsub("#fields", "", bro_file[1,1])

#to fix: x is correct but doesn't transfer into data frame correctly. 
new.row <- gsub("\t", " ", bro_file[1,1])
x <- f4(new.row)
data.frame(x, stringsAsFactors=FALSE)

f4 <- function(n) {
  x <- character(length(n))
  for (i in 1:length(n)) {
    x[i] <- n[[i]]
  }
  x
}


