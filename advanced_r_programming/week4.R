########## OOP

##### S3

# system governs how R handles objects of different classes

class(2)

class("is in session.")

class(class)

special_num_1 <- structure(1, class = "special_number")
class(special_num_1)

special_num_2 <- 2
class(special_num_2)

class(special_num_2) <- "special_number"
class(special_num_2)

shape_s3 <- function(side_lengths){
  structure(list(side_lengths = side_lengths), class = "shape_S3")
}

square_4 <- shape_s3(c(4, 4, 4, 4))
class(square_4)

triangle_3 <- shape_s3(c(3, 3, 3))
class(triangle_3)

mean(c(2, 3, 7))

mean(c(as.Date("2016-09-01"), as.Date("2016-09-23")))


is_square <- function(x) UseMethod("is_square")

is_square.shape_S3 <- function(x){
  length(x$side_lengths) == 4 &&
    x$side_lengths[1] == x$side_lengths[2] &&
    x$side_lengths[2] == x$side_lengths[3] &&
    x$side_lengths[3] == x$side_lengths[4]
}

is_square(square_4)
is_square(triangle_3)

is_square.default <- function(x){
  NA
}

is_square("square")

is_square(c(1, 1, 1, 1))

print(square_4)


print.shape_S3 <- function(x){
  if(length(x$side_lengths) == 3){
    paste("A triangle with side lengths of", x$side_lengths[1], 
          x$side_lengths[2], "and", x$side_lengths[3])
  } else if(length(x$side_lengths) == 4) {
    if(is_square(x)){
      paste("A square with four sides of length", x$side_lengths[1])
    } else {
      paste("A quadrilateral with side lengths of", x$side_lengths[1],
            x$side_lengths[2], x$side_lengths[3], "and", x$side_lengths[4])
    }
  } else {
    paste("A shape with", length(x$side_lengths), "slides.")
  }
}

print(square_4)
print(triangle_3)
print(shape_s3(c(10, 10, 20, 20, 15)))
print(shape_s3(c(2, 3, 4, 5)))

methods(print)
head(methods(print), 10)

class(square_4) <- c("shape_S3", "square")
class(square_4)

# if an object is a sub-class of a specified class
inherits(square_4, "square")


##### S4

# new class in S4 you need to use thesetClass() function
setClass("bus_S4",
         slots = list(n_seats = "numeric", 
                      top_speed = "numeric",
                      current_speed = "numeric",
                      brand = "character"))
setClass("party_bus_S4",
         slots = list(n_subwoofers = "numeric",
                      smoke_machine_on = "logical"),
         contains = "bus_S4")

my_bus <- new("bus_S4", n_seats = 20, top_speed = 80, 
              current_speed = 0, brand = "Volvo")
my_bus

my_party_bus <- new("party_bus_S4", n_seats = 10, top_speed = 100,
                    current_speed = 0, brand = "Mercedes-Benz", 
                    n_subwoofers = 2, smoke_machine_on = FALSE)
my_party_bus

my_bus@n_seats
my_party_bus@top_speed

# to implement a new generic method you need to use thesetGeneric() function 
# and the standardGeneric() function
setGeneric("new_generic", function(x){
  standardGeneric("new_generic")
})

setGeneric("is_bus_moving", function(x){
  standardGeneric("is_bus_moving")
})

setMethod("is_bus_moving",
          c(x = "bus_S4"),
          function(x){
            x@current_speed > 0
          })

is_bus_moving(my_bus)

my_bus@current_speed <- 1
is_bus_moving(my_bus)


setGeneric("print")

setMethod("print",
          c(x = "bus_S4"),
          function(x){
            paste("This", x@brand, "bus is traveling at a speed of", x@current_speed)
          })

print(my_bus)

print(my_party_bus)


##### reference classes

# class definition called Student which defines the student class. 
# This class has five fields and three methods. 
# To create a Student object use the new() method

Student <- setRefClass("Student",
                       fields = list(name = "character",
                                     grad_year = "numeric",
                                     credits = "numeric",
                                     id = "character",
                                     courses = "list"),
                       methods = list(
                         hello = function(){
                           paste("Hi! My name is", name)
                         },
                         add_credits = function(n){
                           credits <<- credits + n
                         },
                         get_email = function(){
                           paste0(id, "@jhu.edu")
                         }
                       ))
Student

brooke <- Student$new(name = "Brooke", grad_year = 2019, credits = 40,
                      id = "ba123", courses = list("Ecology", "Calculus III"))
roger <- Student$new(name = "Roger", grad_year = 2020, credits = 10,
                     id = "rp456", courses = list("Puppetry", "Elementary Algebra"))

brooke$credits

roger$hello()

roger$get_email()

brooke$credits

brooke$add_credits(4)
brooke$credits

Grad_Student <- setRefClass("Grad_Student",
                            contains = "Student",
                            fields = list(thesis_topic = "character"),
                            methods = list(
                              defend = function(){
                                paste0(thesis_topic, ". QED.")
                              }
                            ))

jeff <- Grad_Student$new(name = "Jeff", grad_year = 2021, credits = 8,
                         id = "jl55", courses = list("Fitbit Repair", 
                                                     "Advanced Base Graphics"),
                         thesis_topic = "Batch Effects")

jeff$defend()


########## Summarize Assessment
########## Functional and Object-Oriented Programming


##### Part 1: Factorial Function

## Factorial_loop

factorial_loop <- function(x){
  y <- 1
  if(x == 0) return(1)
  for (i in seq(x,1,-1)){
    y <- y*i
  }
  return(y)
}
factorial_loop(7)


## Factorial_reduce

factorial_reduce <- function(x){
  if(x == 0) return(1)
  else{ 
    reduce(c(1:x), function(x, y){
      x * y
    })
  }
}
factorial_reduce(6)


## Factorial_func

factorial_func <- function(x) {
  recursive <- function(x) {
    if(x <= 0) return(1)
    if(x <= 1) return(1)
    else return(x * recursive(x-1))
  }
  return(recursive(x))
}
factorial_func(5)


## Factorial_mem

mem_tbl <- c(1, rep(NA, 100))
factorial_mem <- function(x){
  if (x == 0) return(1)
  if(!is.na(mem_tbl[x])) return(mem_tbl[x])
  else{
    mem_tbl[x] <<- x * factorial_mem(x-1)
    return(mem_tbl[x])
  }
}
factorial_mem(4)


## Demonstration Microbenchmark

library(microbenchmark)

Proof <- microbenchmark(a <- 10,
                        b <- factorial_loop(10),
                        c <- factorial_mem(10),
                      #  d <- factorial_reduce(10),
                        e <- factorial_func(10))
Proof

autoplot(Proof)


Proof2 <- microbenchmark(a <- 50,
                         b <- factorial_loop(50),
                         c <- factorial_mem(50),
                         d <- factorial_reduce(50),
                         e <- factorial_func(50))
Proof2

autoplot(Proof2)


Proof3 <- microbenchmark(a <- 100,
                         b <- factorial_loop(100),
                         c <- factorial_mem(100),
                         d <- factorial_reduce(100),
                         e <- factorial_func(100))
Proof3

autoplot(Proof3)


##### Part 2: Longitudinal Data Class and Methods

## Read in the data
library(readr)
library(magrittr)
#source("week4.R")

longdata <- read_csv("data/MIE.csv")
class(longdata)
summary(longdata)
lapply(longdata, class)


## Transformation in object

# Define functions
subject <- function(loadlongdata, id) UseMethod("subject")

visit <- function(subject, visitid) UseMethod("visit")

room <- function(visit, roomid) UseMethod("room")

# Define methods for LongitudinalData objects 
make_LD <- function(longdata) {
  loadlongdata <- longdata %>% nest(-id)
  structure(loadlongdata, class = c("LongitudinalData"))
}
# oop_output.txt: command: print(class(x)) result: [1] "LongitudinalData"
print.LongitudinalData <- function(variable) {
  cat("Longitudinal dataset with", length(variable[["id"]]), "subjects")
  invisible(variable)
}

subject.LongitudinalData <- function(loadlongdata, id) {
  index <- which(loadlongdata[["id"]] == id)
  if (length(index) == 0)
    return(NULL)
  structure(list(id = id, data = loadlongdata[["data"]][[index]]), class = "Subject")
}

# Define methods for Subject objects

# oop_output.txt: command: out <- subject(x, 10) (Longitudinal dataset with 10 subjects) 
# result: Subject 10 doesn't exist
# oop_output.txt: command: out <- subject(x, 14), result: Subject ID: 14

print.Subject <- function(variable) {
  cat("Subject ID:", variable[["id"]])
  invisible(variable)
}

# oop_output.txt: command: out <- subject(x, 54) %>% summary, result: 
# ID: 54 
#   visit  bedroom       den living room    office
# 1     0       NA        NA    2.792601 13.255475
# 2     1       NA 13.450946          NA  4.533921
# 3     2 4.193721  3.779225          NA        NA


summary.Subject <- function(object) {
  output <- object[["data"]] %>% 
    group_by(visit, room) %>%
    summarise(value = mean(value)) %>% 
    spread(room, value) %>% 
    as.data.frame
  structure(list(id = object[["id"]],
                 output = output), class = "Summary")
}

visit.Subject <- function(subject, visitid) {
  data <- subject[["data"]] %>% 
    filter(visit == visitid) %>% 
    select(-visit)
  structure(list(id = subject[["id"]],
                 visitid = visitid,
                 data = data), class = "Visit")
}

# oop_output.txt: command: out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
# as an example. The results are printed at the room methods

# Define methods for Visit objects
room.Visit <- function(visit, roomid) {
  data <- visit[["data"]] %>% 
    filter(room == roomid) %>% 
    select(-room)
  structure(list(id = visit[["id"]],
                 visitid = visit[["visitid"]],
                 room = roomid,
                 data = data), class = "Room")
}

# Define methods for Room objects 
# oop_output.txt: command: out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
# result: 
# ID: 44 
# Visit: 0 
# Room: bedroom 

print.Room <- function(variable) {
  cat("ID:", variable[["id"]], "\n")
  cat("Visit:", variable[["visitid"]], "\n")
  cat("Room:", variable[["room"]])
  invisible(variable)
}

# Show a summary of the pollutant values
# oop_output.tvariablet: command:
# out <- subject(variable, 44) %>% visit(0) %>% room("bedroom") %>% summary
# result: 
summary.Room <- function(object) {
  output <- summary(object[["data"]][["value"]])
  structure(list(id = object[["id"]],
                 output = output), class = "Summary")
}

# Define methods for Summary objects
# out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
# ID: 44 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    2.75   14.00   24.00   41.37   37.00 1607.00 

print.Summary <- function(variable) {
  cat("ID:", variable[[1]], "\n")
  print(variable[[2]])
  invisible(variable)
}

x <- make_LD(longdata)
print(class(x))

print(x)

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)







