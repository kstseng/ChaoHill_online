saveList2csv <- function(out, file) {
  for (i in seq_along(out)){
    write.table(names(out)[i], file=file, sep=",", dec=".", 
                quote=FALSE, col.names=FALSE, row.names=FALSE, append=TRUE)  #writes the name of the list elements
    write.table(out[[i]], file=file, sep=",", dec=".", quote=FALSE, 
                col.names=FALSE, row.names=TRUE, append=T)  #writes the data.frames
  }
}
