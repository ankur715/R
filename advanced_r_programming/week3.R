########## Debugging

# browser(): an interactive debugging environment that allows you to step through code one expression at a time
# debug() / debugonce(): a function that initiates the browser within a function
# trace(): this function allows you to temporarily insert pieces of code into other functions to modify their behavior
# recover(): a function for navigating the function call stack after a function has thrown an error
# traceback(): prints out the function call stack after an error occurs; does nothing if there's no error

##### traceback()

check_n_value <- function(n) {
  if(n > 0) {
    stop("n should be <= 0")
  }
}
error_if_n_is_greater_than_zero <- function(n){
  check_n_value(n)
  n
}
error_if_n_is_greater_than_zero(5)
traceback()


##### browsing a function environment

check_n_value <- function(n) {
  if(n > 0) {
    browser()  ## Error occurs around here
    stop("n should be <= 0")
  }
}

error_if_n_is_greater_than_zero(5)


##### tracing functions

trace("check_n_value")

error_if_n_is_greater_than_zero(5)

as.list(body(check_n_value))

as.list(body(check_n_value)[[2]])

trace("check_n_value", browser, at = list(c(2, 3)))

check_n_value

body(check_n_value)

trace("check_n_value", quote({
  if(n == 5) {
    message("invoking the browser")
    browser()
  }
}), at = 2)

body(check_n_value)

trace("glm", browser, at = 4, where = asNamespace("stats"))

body(stats::glm)[1:5]


##### using debug() and debugonce()

## Turn on debugging state for 'lm' function
debug(lm)

options(error = recover)

error_if_n_is_greater_than_zero(5)


########## Profiling 

##### microbenchmark

library(microbenchmark)
microbenchmark(a <- rnorm(1000), 
               b <- mean(rnorm(1000)))

# Function that uses a loop 
find_records_1 <- function(datafr, threshold){
  highest_temp <- c()
  record_temp <- c()
  for(i in 1:nrow(datafr)){
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold & 
      datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
  return(datafr)
}

# Function that uses tidyverse functions
find_records_2 <- function(datafr, threshold){
  datafr <- datafr %>%
    mutate_(over_threshold = ~ temp >= threshold,
            cummax_temp = ~ temp == cummax(temp),
            record_temp = ~ over_threshold & cummax_temp) %>%
    select_(.dots = c("-over_threshold", "-cummax_temp"))
  return(as.data.frame(datafr))
}

example_data <- data.frame(date = c("2015-07-01", "2015-07-02",
                                    "2015-07-03", "2015-07-04",
                                    "2015-07-05", "2015-07-06",
                                    "2015-07-07", "2015-07-08"),
                           temp = c(26.5, 27.2, 28.0, 26.9, 
                                    27.5, 25.9, 28.0, 28.2))
example_data

find_records_1(example_data, 27)
highest_temp <- c()
threshold <- 27
highest_temp <- max(highest_temp, example_data$temp[1])
example_data$temp[3] >= threshold & example_data$temp[3] >= highest_temp

(test_1 <- find_records_1(example_data, 27))

(test_2 <- find_records_2(example_data, 27))

all.equal(test_1, test_2)

record_temp_perf <- microbenchmark(find_records_1(example_data, 27), 
                                   find_records_2(example_data, 27))
record_temp_perf

library(dlnm)
data("chicagoNMMAPS")

record_temp_perf_2 <- microbenchmark(find_records_1(chicagoNMMAPS, 27), 
                                     find_records_2(chicagoNMMAPS, 27))
record_temp_perf_2

library(ggplot2)

# For larger data set
autoplot(record_temp_perf_2)


##### profvis

# install.packages("profvis")
library(profvis)
datafr <- chicagoNMMAPS
threshold <- 27

profvis({
  highest_temp <- c()
  record_temp <- c()
  for(i in 1:nrow(datafr)){
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold & 
      datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
})


########## Non-standard evaluation

# Non-standard evaluation version	Standard evaluation version
# filter(fips %in% counties)	filter_(~ fips %in% counties)
# mutate(max_rain = max(tot_precip)	mutate_(max_rain = ~ max(tot_precip)
# summarize(tot_precip = sum(precip))	summarize_(tot_precip = ~ sum(precip))
# group_by(storm_id, fips)	group_by_(~ storm_id, ~ fips)
# aes(x = long, y = lat)	aes_(x = ~ long, y = ~ lat)
# select(-start_date, -end_date)	select_(.dots = c('start_date', 'end_date'))
# select(-start_date, -end_date)	select_(.dots = c('-start_date', '-end_date'))
# spread(key, mean)	spread_(key_col = 'key', value_col = 'mean')
# gather(key, mean)	gather_(key_col = 'key', value_col = 'mean')
                                          
x <- seq(0, 2 * pi, length = 100)
sinx <- sin(x)
plot(x, sinx, type = "l")


##### capturing expressions

f <- function(x) {
  substitute(x)
}
f(1:10)

x <- 10
f(x)

y <- 13
f(x + y^2)

x
substitute(2:20)
g <- function(x) deparse(substitute(x))
g(1:10)
g(x)
g(x + y^2)

x <- 1:4
y <- letters[1:4]
names(data.frame(x, y))
x
y
names

f <- function(x) substitute(x)
g <- function(x) deparse(f(x))
g(1:10)
g(x)
g(x + y ^ 2 / z + exp(a * sin(b)))


##### non-standard evaluation in subset

sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))
sample_df

subset(sample_df, a >= 4)

subset(sample_df, b == c)

quote(1:10)
quote(x)
quote(x + y^2)

eval(quote(x <- 1))
x
eval(quote(x))

eval(quote(y))

e <- new.env()
e
e$x <- 20
e$x
eval(quote(x), e)

eval(quote(x), list(x = 30))
eval(quote(x), data.frame(x = 40))

eval(quote(a >= 4), sample_df)
eval(quote(b == c), sample_df)

a <- 10
eval(quote(a), sample_df)
eval(a, sample_df)
eval(quote(b), sample_df)
eval(b, sample_df)

subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  x[r, ]
}
subset2(sample_df, a >= 4)


