fname <- paste(path_to_course,"MVData2.jpeg",sep="/")
try(dev.off(),silent=TRUE)
plot.new()
plotArea=par('fig')
rasterImage(readJPEG(fname),plotArea[1],plotArea[3],plotArea[2],plotArea[4],interpolate=FALSE)
