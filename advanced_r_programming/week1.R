#install.packages("swirl")
#packageVersion("swirl")
library(swirl)
#swirl::install_course("Advanced R Programming")
swirl()


########## Control Structures

##### if-else

my.name <- readline(prompt="Enter name: ")
my.age <- readline(prompt="Enter age: ")
my.age <- as.integer(my.age)
print(paste("Hi,", my.name, "next year you will be", my.age+1, "years old."))

if(substring(my.name, 1, 1) == "A") {
  print("Your name starts with A.")
  } else if(substring(my.name, 1, 1) == "B") {
    print("Your name starts with A.")
  } else {
    print("Your name starts with C-Z.")
  }
if(my.age == 28) {
  print("You're 28.")
} else if(my.age < 28) {
  print("You're young.")
} else {
  print("You're old.")
}

## Generate a uniform random number
x <- runif(1, 0, 10)
if(x > 3) {
  y <- 10
} else {
  y <- 0
}
print(x)
print(y)

if(x <= 5) {
  print("x < 5")
  }
if(y >= 5) {
  print("y > 5")
  }


##### for loops

numbers <- rnorm(10)
for(i in 1:10) {
  print(numbers[i])
}

x <- c("a", "b", "c", "d")
for(i in 1:4) {
  print(x[i])  
}

## Generate a sequence based on length of 'x'
for(i in seq_along(x)) {   
  print(x[i])
}
for(i in 1:length(x)) {   
  print(x[i])
}

for(letter in x) {
  print(letter)
}

# curly braces are not strictly necessary. Useful for multiple lines
for(i in 1:4) print(x[i])


##### nested for loops

# matrix(numbers, rows, cols)
x <- matrix(1:6, 2, 3)

for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }   
}

# next is used to skip an iteration of a loop.
for(i in 1:100) {
  if(i <= 20) {
    print(i)  #prints 1:20
    ## Skip the first 20 iterations
    next
  }
  print(i)  #prints 21:100
}

# break is used to exit a loop immediately, regardless of what iteration the loop may be on.
for(i in 1:100) {
  print(i)
  if(i == 20) {
    ## Stop loop after 20 iterations
    break  
  }     
}


########## Functions

library(readr)
library(dplyr)

getwd()
setwd("H:/learning/R")
## Download data from RStudio (if we haven't already)  
if(!file.exists("data/2016-07-20.csv.gz")) {
  download.file("http://cran-logs.rstudio.com/2016/2016-07-20.csv.gz", 
                "data/2016-07-20.csv.gz")
}

# col_type to avoid difficulty identifying the type of each column
cran <- read_csv("data/2016-07-20.csv.gz")
cran <- read_csv("data/2016-07-20.csv.gz", col_types = "ccicccccci")
cran
cran %>% filter(package == "filehash") %>% nrow


##### function interface

library(dplyr)
library(readr)

## pkgname: package name (character)
## date: YYYY-MM-DD format (character)
num_download <- function(pkgname, date) {
  ## Construct web URL
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  
  ## Construct path for storing local file
  dest <- file.path("data", basename(src))
  
  ## Don't download if the file is already there!
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)
  
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

# Now we can call our function using whatever date or package name we choose.
num_download("filehash", "2016-07-20")
num_download("Rcpp", "2016-07-19")


##### default values

num_download <- function(pkgname, date = "2016-07-21") {
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  dest <- file.path("data", basename(src))
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

num_download("Rcpp")


##### re-factoring code

check_for_logfile <- function(date) {
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  dest <- file.path("data", basename(src))
  if(!file.exists(dest)) {
    val <- download.file(src, dest, quiet = TRUE)
    if(!val)
      stop("unable to download file ", src)
  }
  dest
}

# takes the original download code from num_download() and adds a bit of error 
# checking to see if download.file() was successful 
# if not, an error is thrown with stop()

num_download <- function(pkgname, date = "2016-07-20") {
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}    

num_download("filehash")  #############################


##### dependency checking

check_pkg_deps <- function() {
  if(!require(readr)) {
    message("installing the 'readr' package")
    install.packages("readr")
  }
  if(!require(dplyr))
    stop("the 'dplyr' package needs to be installed first")
}

# require() function is similar to library(), however library() stops with 
# error if package cannot be loaded whereas require() returns TRUE or FALSE 
# depending on whether the package can be loaded or not

# if we cannot load the dplyr package we throw an error

num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

num_download("filehash")


##### vectorization

## 'pkgname' can now be a character vector of names
num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package %in% pkgname) %>% 
    group_by(package) %>%
    summarize(n = n())
}    

# filter() to grab rows of the data frame that fall within a vector of package names
# group_by() %>% summarize() combination to count the downloads for each package

num_download(c("filehash", "weathermetrics"))


##### argument checking

num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  
  ## Check arguments
  if(!is.character(pkgname))
    stop("'pkgname' should be character")
  if(!is.character(date))
    stop("'date' should be character")
  if(length(date) != 1)
    stop("'date' should be length 1")
  
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", 
                   progress = FALSE)
  cran %>% filter(package %in% pkgname) %>% 
    group_by(package) %>%
    summarize(n = n())
}    

num_download("filehash", c("2016-07-20", "2016-0-21"))
#Error in num_download("filehash", c("2016-07-20", "2016-0-21")): 'date' should be length 1


##### removing first character of string

PopStringFactory <- setRefClass(
  "PopString",
  fields = list(
    x = "character"  
  ),
  methods = list(
    initialize = function(x)
    {
      x <<- x
    },
    pop = function(n = 1)
    {
      if(nchar(x) == 0)
      {
        warning("Nothing to pop.")
        return("")
      }
      first <- substring(x, 1, n)
      x <<- substring(x, n + 1)
      first
    }
  )
)

x <- PopStringFactory$new("hello stackoverflow")
x

replicate(nchar(x$x), x$pop())
