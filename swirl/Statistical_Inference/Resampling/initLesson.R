# Put initialization code in this file.
library(ggplot2)
data(InsectSprays)

# For compatibility with 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

#get correct paths for plotting
pathtofile <- function(fileName){
  mypath <- file.path(.get_course_path(),
  	"Statistical_Inference","Resampling",
                      fileName)
}
fxfer <- function(fileName){
  mypath <- pathtofile(fileName)
  file.copy(mypath,fileName)
  fileName
}
#fname <- paste(getwd(),"Statistical_Inference/Hypothesis_Testing/father_son.csv",sep="/")
fname <- pathtofile("father_son.csv")
fs <- read.csv(fname)
sh <- fs$sheight
fh <- fs$fheight
nh <- length(sh)
B <- 1000
