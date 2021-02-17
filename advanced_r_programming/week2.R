########## Functional Programming

adder_maker <- function(n){
  function(x){
    n + x
  }
}

add2 <- adder_maker(2)
add3 <- adder_maker(3)

add2(5)
add3(5)


##### map

# In the purrr package the map() function returns a list, 
# while the map_lgl(), map_chr(), and map_dbl()functions return 
# vectors of logical values, strings, or numbers respectively.

library(purrr)

map_chr(c(5, 4, 3, 2, 1), function(x){
  c("one", "two", "three", "four", "five")[x]
})

map_lgl(c(1, 2, 3, 4, 5), function(x){
  x > 3
})


# map_if() function takes as its arguments a list or vector containing data, 
# a predicate function, and then a function to be applied
map_if(1:5, function(x){
  x %% 2 == 0
},
function(y){
  y^2
}) %>% unlist()


# map_at() function only applies the provided function to elements of a vector specified by their indexes
map_at(seq(100, 500, 100), c(1, 3, 5), function(x){
  x - 10
}) %>% unlist()


# map a function over two data structures with the map2() family of functions
map2_chr(letters, 1:26, paste)

# map multiple
pmap_chr(list(
  list(1, 2, 3),
  list("one", "two", "three"),
  list("uno", "dos", "tres")
), paste)


###### reduce

reduce(c(1, 3, 5, 7, 9, 11), function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  x + y
})


reduce(letters[1:4], function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  paste0(x, y)
})


########## search

letters <- c("a", "b", "c")
#install.packages("tidyverse")
library(tidyverse)
contains(letters, "a")

'b' %in% letters

detect(20:40, function(x){
  x > 22 && x %% 2 == 0
})

detect_index(20:40, function(x){
  x > 22 && x %% 2 == 0
})


##### filter

keep(1:20, function(x){
  x %% 2 == 0
})

discard(1:20, function(x){
  x %% 2 == 0
})

every(1:20, function(x){
  x %% 2 == 0
})

some(1:20, function(x){
  x %% 2 == 0
})


##### compose

n_unique <- compose(length, unique)
# The composition above is the same as:
# n_unique <- function(x){
#   length(unique(x))
# }

rep(1:5, 1:5)

n_unique(rep(1:5, 1:5))


##### partial application

library(purrr)

mult_three_n <- function(x, y, z){
  x * y * z
}

mult_by_15 <- partial(mult_three_n, x = 3, y = 5)

mult_by_15(z = 4)


##### side effects

walk(c("Friends, Romans, countrymen,",
       "lend me your ears;",
       "I come to bury Caesar,", 
       "not to praise him."), message)


##### recursion

# function called inside of itself
vector_sum_loop <- function(v){
  result <- 0
  for(i in v){
    result <- result + i
  }
  result
}

vector_sum_loop(c(5, 40, 91))

vector_sum_rec <- function(v){
  if(length(v) == 1){
    v
  } else {
    v[1] + vector_sum_rec(v[-1])
  }
}

vector_sum_rec(c(5, 40, 91))

fib <- function(n){
  stopifnot(n > 0)
  if(n == 1){
    0
  } else if(n == 2){
    1
  } else {
    fib(n - 1) + fib(n - 2)
  }
}
fib(1)
fib(2)
fib(5)

map_dbl(1:12, fib)

fib_tbl <- c(0, 1, rep(NA, 23))
fib_tbl

fib_mem <- function(n){
  stopifnot(n > 0)
  
  if(!is.na(fib_tbl[n])){
    fib_tbl[n]
  } else {
    fib_tbl[n - 1] <<- fib_mem(n - 1)
    fib_tbl[n - 2] <<- fib_mem(n - 2)
    fib_tbl[n - 1] + fib_tbl[n - 2]
  }
}

map_dbl(1:12, fib_mem)

# paste0 prints strings
paste0(letters[1:10], 1:10)

library(purrr)
#install.packages("microbenchmark")
library(microbenchmark)
library(tidyr)
library(magrittr)
library(dplyr)

fib_data <- map(1:10, function(x){microbenchmark(fib(x), times = 100)$time})
names(fib_data) <- paste0(letters[1:10], 1:10)
fib_data <- as.data.frame(fib_data)
fib_data

fib_data %<>%                                   ######### 
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))
fib_data

memo_data <- map(1:10, function(x){microbenchmark(fib_mem(x))$time})
names(memo_data) <- paste0(letters[1:10], 1:10)
memo_data <- as.data.frame(memo_data)
memo_data

memo_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))
memo_data

plot(1:10, fib_data$med_time, xlab = "Fibonacci Number", ylab = "Median Time (Nanoseconds)",
     pch = 18, bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = 1:10)
axis(2, at = seq(0, 350000, by = 50000))
points(1:10 + .1, memo_data$med_time, col = "blue", pch = 18)
legend(1, 300000, c("Not Memorized", "Memoized"), pch = 18, 
       col = c("black", "blue"), bty = "n", cex = 1, y.intersp = 1.5)


########## Expressions & Environments

##### expressions

two_plus_two <- quote(2 + 2)
two_plus_two

eval(two_plus_two)

tpt_string <- "2 + 2"

tpt_expression <- parse(text = tpt_string)
tpt_expression
eval(tpt_expression)

deparse(two_plus_two)

sum_expr <- quote(sum(1, 5))
eval(sum_expr)

sum_expr[[1]]  #sum
sum_expr[[2]]  #1
sum_expr[[3]]  #5

sum_expr[[1]] <- quote(paste0)
sum_expr[[2]] <- quote(4)
sum_expr[[3]] <- quote(6)
eval(sum_expr)

sum_40_50_expr <- call("sum", 40, 50)
sum_40_50_expr
sum(40, 50)
eval(sum_40_50_expr)

# executed a function by including match.call() 
return_expression <- function(...){
  match.call()
}

return_expression(2, col = "blue", FALSE)
return_expression(2, col = "blue", FALSE)

first_arg <- function(...){
  expr <- match.call()
  first_arg_expr <- expr[[2]]
  first_arg <- eval(first_arg_expr)
  if(is.numeric(first_arg)){
    paste("The first argument is", first_arg)
  } else {
    "The first argument is not numeric."
  }
}

first_arg(2, 4, "seven", FALSE)
first_arg("seven", 2, 4, "seven", FALSE)


##### environments

my_new_env <- new.env()
my_new_env
my_new_env$x <- 4
my_new_env
my_new_env$x

assign("y", 9, envir = my_new_env)
my_new_env$y
get("y", envir = my_new_env)

ls(my_new_env)
rm(y, envir = my_new_env)
exists("y", envir = my_new_env)
exists("x", envir = my_new_env)
my_new_env$x
my_new_env$y

search()

library(ggplot2)
search()


##### execution environments

x <- 10
my_func <- function(){
  x <- 5
  return(x)
}
my_func()

x <- 10
another_func <- function(){
  return(x)
}
another_func()

x <- 10
x
assign1 <- function(){
  x <<- "Wow!"
}
assign1()
x

exists("a_variable_name")

assign2 <- function(){
  a_variable_name <<- "Magic!"
}

assign2()
exists("a_variable_name")
a_variable_name

a = 1
exists("a")


########## Error Handling and Generation

##### what is an error?

"hello" + "world"

as.numeric(c("5", "6", "seven"))
as.numeric(c("5", "6", "7"))

f <- function(){
  message("This is a message.")
}
f()

name_of_function <- function(){
  stop("Something bad happened.")
}
name_of_function()

error_if_n_is_greater_than_zero <- function(n){
  stopifnot(n <= 0)
  n
}
error_if_n_is_greater_than_zero(5)
error_if_n_is_greater_than_zero(-1)

warning("Consider yourself warned!")

make_NA <- function(x){
  warning("Generating an NA.")
  NA
}
make_NA("Sodium")

message("In a bottle.")


##### how should errors be handled?

beera <- function(expr){
  tryCatch(expr,
           error = function(e){
             message("An error occurred:\n", e)
           },
           warning = function(w){
             message("A warning occured:\n", w)
           },
           finally = {
             message("Finally done!")
           })
}

beera(2+2)
beera({
  2 + 2
})

beera({
  "two" + 2
})

beera({
  as.numeric(c(1, "two", 3))
})

is_even <- function(n){
  n %% 2 == 0
}

is_even(768)
is_even("two")

is_even_error <- function(n){
  tryCatch(n %% 2 == 0,
           error = function(e){
             FALSE
           })
}

is_even_error(714)
is_even_error("eight")

is_even_check <- function(n){
  is.numeric(n) && n %% 2 == 0
}

is_even_check(1876)
is_even_check("twelve")

# measure how long it takes for each function to be applied to the same data.
library(microbenchmark)
microbenchmark(sapply(letters, is_even_check))

microbenchmark(sapply(letters, is_even_error))

