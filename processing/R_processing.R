# Declare variables of different types
# Numeric
x <- 28
class(x)

# String
y <- "R is Fantastic"
class(y)

# Boolean
z <- TRUE
class(z)

# is k an integer? 
is.integer(x)

# First way to declare a variable:  use the `<-`
name_of_variable <- value
# Second way to declare a variable:  use the `=`
name_of_variable = value

# Print variable x
x <- 5
x

y  <- 2
y

# operations
x-y  #subtract
x*y  #multiply
x**y  #exponent
x%%y  #modulus
x%/%y #integer division

# logical operators
x<y	#less than
x<=y	#less than or equal to
x>y	#greater than
x>=y	#greater than or equal to
x==y	#exactly equal to
x!=y	#not equal to
x|y == 7	#x OR y
x&y == 2	#x AND y
isTRUE(x<y)	#test if X is TRUE

# An example
x <- c(1:10)
x[(x>8) | (x<5)]
# yields 1 2 3 4 9 10

# How it works
x <- c(1:10)
x
x > 8
x < 5
x > 8 | x < 5
x[c(T,T,T,T,F,F,F,F,T,T)]

# Construct a matrix with 5 rows that contain the numbers 1 up to 10 and byrow =  TRUE 
matrix_a <-matrix(1:10, byrow = TRUE, nrow = 5)
matrix_a

# Print dimension of the matrix with dim()
dim(matrix_a)

# Construct a matrix with 5 rows that contain the numbers 1 up to 10 and byrow =  FALSE
matrix_b <-matrix(1:10, byrow = FALSE, nrow = 5)
matrix_b

# Print dimension of the matrix with dim()
dim(matrix_b)

matrix_c <-matrix(1:12, byrow = FALSE, ncol = 3)
print(matrix_c)
dim(matrix_c)

# Add a Column to a Matrix with the cbind()
# concatenate c(1:5) to the matrix_a
matrix_a1 <- cbind(matrix_a, c(1:5))
# Check the dimension
dim(matrix_a1)

matrix_a1

matrix_a2 <-matrix(13:24, byrow = FALSE, ncol = 3)
matrix_a2

matrix_c <-matrix(1:12, byrow = FALSE, ncol = 3)		
matrix_d <- cbind(matrix_a2, matrix_c)
dim(matrix_d)

matrix_c
matrix_c[1,2] #selects the element at the first row and second column.
matrix_c[1:3,2:3] #results in a matrix with the data on the rows 1, 2, 3 and columns 2, 3,
matrix_c[,1] #selects all elements of the first column.
matrix_c[1,] #selects all elements of the first row.

## Categorical Variables

# Create gender vector
gender_vector <- c("Male", "Female", "Female", "Male", "Male")
class(gender_vector)
# Convert gender_vector to a factor
factor_gender_vector <-factor(gender_vector)
class(factor_gender_vector)

# Create a color vector
color_vector <- c('blue', 'red', 'green', 'white', 'black', 'yellow')
# Convert the vector to factor
factor_color <- factor(color_vector)
factor_color  #From the factor_color, we can't tell any order.

# Create Ordinal categorical vector 
day_vector <- c('evening', 'morning', 'afternoon', 'midday', 'midnight', 'evening')
# Convert `day_vector` to a factor with ordered level
factor_day <- factor(day_vector, order = TRUE, levels =c('morning', 'midday', 'afternoon', 'evening', 'midnight'))
# Print the new variable
factor_day

## Levels: morning < midday < afternoon < evening < midnight
# Append the line to above code
# Count the number of occurence of each level
summary(factor_day)

# continuous variables
dataset <- mtcars
class(dataset$mpg)

# How to Create a Data Frame
# Create a, b, c, d variables
a <- c(10,20,30,40)
b <- c('book', 'pen', 'textbook', 'pencil_case')
c <- c(TRUE,FALSE,TRUE,FALSE)
d <- c(2.5, 8, 10, 7)
# Join the variables to create a data frame
df <- data.frame(a,b,c,d)
df

# Name the data frame
names(df) <- c('ID', 'items', 'store', 'price')
df

# Print the structure
str(df)

# Slice
df
# Select row 1 in column 2
df[1,2]
# Select Rows 1 to 2
df[1:2,]
# Select Columns 1
df[,1]
# Select Rows 1 to 3 and columns 3 to 4
df[1:3, 3:4]
# Slice with columns name
df[, c('ID', 'store')]

# Append a column to dataframe

# Create a new vector
quantity <- c(10, 35, 40, 5)
# Add `quantity` to the `df` data frame
df$quantity <- quantity
df

quantity <- c(10, 35, 40)
# Add `quantity` to the `df` data frame
df$quantity <- quantity

# Select the column ID
df$ID

# Select price above 5
subset(df, subset = price > 5)

# Vector with numeric from 1 up to 5
vect  <- 1:5

# A 2x 5 matrix
mat  <- matrix(1:9, ncol = 5)
dim(mat)

# select the 10th row of the built-in R data set EuStockMarkets
df <- EuStockMarkets[1:10,]

# Construct list with these vec, mat, and df:
my_list <- list(vect, mat, df)
my_list

# Print second element of the list
my_list[[2]]

## Built-in Data Frame
PATH <-'C:/Users/ankur/Documents/Data Science/Projects/R/R/prison.csv'
PATH <-'C:\\Users\\ankur\\Documents\\Data Science\\Projects\\R\\R\\prison.csv'
df <- read.csv(PATH)[1:5]
head(df, 5)
PATHH <- file.choose()

# Structure of the data
str(df)

# https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html
library(dplyr)
set.seed(1234)
data_frame <- tibble(  
  c1 = rnorm(50, 5, 1.5),   
  c2 = rnorm(50, 5, 1.5),  
  c3 = rnorm(50, 5, 1.5),
  c4 = rnorm(50, 5, 1.5), 	
  c5 = rnorm(50, 5, 1.5)
)
# Sort by c1
df <-data_frame[order(data_frame$c1),]
head(df)

# Sort by c3 and c4
df <-data_frame[order(data_frame$c3, data_frame$c4),]
head(df)

# Sort by c3(descending) and c4(acending)
df <-data_frame[order(-data_frame$c3, data_frame$c4),]
head(df)

# Merge with dplyr()
library(dplyr)
df_primary <- tribble(
  ~ID, ~y,
  "A", 5,
  "B", 5,
  "C", 8,
  "D", 0,
  "F", 9)
df_secondary <- tribble(
  ~ID, ~y,
  "A", 30,
  "B", 21,
  "C", 22,
  "D", 25,
  "E", 29)

left_join(df_primary, df_secondary, by ='ID')

right_join(df_primary, df_secondary, by = 'ID')

inner_join(df_primary, df_secondary, by ='ID')

full_join(df_primary, df_secondary, by = 'ID')

df_primary <- tribble(
  ~ID, ~year, ~items,
  "A", 2015,3,
  "A", 2016,7,
  "A", 2017,6,
  "B", 2015,4,
  "B", 2016,8,
  "B", 2017,7,
  "C", 2015,4,
  "C", 2016,6,
  "C", 2017,6)
df_secondary <- tribble(
  ~ID, ~year, ~prices,
  "A", 2015,9,
  "A", 2016,8,
  "A", 2017,12,
  "B", 2015,13,
  "B", 2016,14,
  "B", 2017,6,
  "C", 2015,15,
  "C", 2016,15,
  "C", 2017,13)
left_join(df_primary, df_secondary, by = c('ID', 'year'))

# Data Cleaning functions
library(tidyr)
# Create a messy dataset
messy <- data.frame(
  country = c("A", "B", "C"),
  q1_2017 = c(0.03, 0.05, 0.01),
  q2_2017 = c(0.05, 0.07, 0.02),
  q3_2017 = c(0.04, 0.05, 0.01),
  q4_2017 = c(0.03, 0.02, 0.04))
messy

# Reshape the data
tidier <- messy %>%
  gather(quarter, growth, q1_2017:q4_2017)
tidier

# Reshape the data
messy_1 <- tidier %>%
  spread(quarter, growth) 
messy_1

# The separate() function splits a column into two according to a separator.
separate_tidier <-tidier %>%
  separate(quarter, c("Qrt", "year"), sep ="_")
head(separate_tidier)

# The unite() function concanates two columns into one.
unit_tidier <- separate_tidier %>%
  unite(Quarter, Qrt, year, sep ="_")
head(unit_tidier)

## Merge

# Create origin dataframe(
producers <- data.frame(   
  surname =  c("Spielberg","Scorsese","Hitchcock","Tarantino","Polanski"),    
  nationality = c("US","US","UK","US","Poland"),    
  stringsAsFactors=FALSE)

# Create destination dataframe
movies <- data.frame(    
  surname = c("Spielberg",
              "Scorsese",
              "Hitchcock",
              "Hitchcock",
              "Spielberg",
              "Tarantino",
              "Polanski"),    
  title = c("Super 8",
            "Taxi Driver",
            "Psycho",
            "North by Northwest",
            "Catch Me If You Can",
            "Reservoir Dogs","Chinatown"),                
  stringsAsFactors=FALSE)

# Merge two datasets
m1 <- merge(producers, movies, by.x = "surname")
m1
dim(m1)

# Change name of ` movies ` dataframe
colnames(movies)[colnames(movies) == 'surname'] <- 'name'
# Merge with different key value
m2 <- merge(producers, movies, by.x = "surname", by.y = "name")
# Print head of the data
head(m2)

# Check if data are identical
identical(m1, m2)

# Create a new producer
add_producer <-  c('Lucas', 'US')
# Append it to the ` producer` dataframe
producers <- rbind(producers, add_producer)
# Use a partial merge 
m3 <-merge(producers, movies, by.x = "surname", by.y = "name", all.x = TRUE)
m3

# Compare the dimension of each data frame
dim(m1)

dim(m2)

dim(m3)

set.seed(123)
# Create the data
x = rnorm(1000)
ts <- cumsum(x)
# Stationary the serie
diff_ts <- diff(ts)
par(mfrow=c(1,2))
# Plot the series
plot(ts, type='l')
plot(diff(ts), type='l')

dt <- cars
# number columns
length(dt)

# number rows
length(dt[,1])

# sequence of number from 44 to 55 both including incremented by 1
x_vector <- seq(45,55, by = 1)
#logarithm
log(x_vector)

#exponential
exp(x_vector)

#squared root
sqrt(x_vector)

#factorial
factorial(x_vector)

speed <- dt$speed
speed
# Mean speed of cars dataset
mean(speed)

# Median speed of cars dataset
median(speed)

# Variance speed of cars dataset
var(speed)

# Standard deviation speed of cars dataset
sd(speed)

# Standardize vector speed of cars dataset		
head(scale(speed), 5)

# Quantile speed of cars dataset
quantile(speed)

# Summary speed of cars dataset
summary(speed)

# Write functions
square_function <- function(n) 
{
  # compute the square of integer `n`
  n^2
}  
# calling the function and passing value 4
square_function(4)

rm(square_function)
square_function

# Environment Scoping
ls(environment())

# Multi arguments
times <- function(x,y) {
  x*y
}
times(2,4)

library(tibble)
# Create a data frame
data_frame <- tibble(  
  c1 = rnorm(50, 5, 1.5), 
  c2 = rnorm(50, 5, 1.5),    
  c3 = rnorm(50, 5, 1.5),    
)

# Create c1_norm: rescaling of c1		
data_frame$c1_norm <- (data_frame$c1 -min(data_frame$c1))/(max(data_frame$c1)-min(data_frame$c1))
# show the first five values
head(data_frame$c1_norm, 5)

data_frame$c1_norm <- (data_frame$c1 -min(data_frame$c1))/(max(data_frame$c1)-min(data_frame$c1))
data_frame$c2_norm <- (data_frame$c2 - min(data_frame$c2))/(max(data_frame$c2)-min(data_frame$c2))
data_frame$c3_norm <- (data_frame$c3 - min(data_frame$c3))/(max(data_frame$c3)-min(data_frame$c3))

normalize <- function(x){
  # step 1: create the nominator
  nominator <- x-min(x)
  # step 2: create the denominator
  denominator <- max(x)-min(x)
  # step 3: divide nominator by denominator
  normalize <- nominator/denominator
  # return the value
  return(normalize)
}

normalize(data_frame$c1)

data_frame$c1_norm_function <- normalize (data_frame$c1)
data_frame$c2_norm_function <- normalize	(data_frame$c2)
data_frame$c3_norm_function <- normalize	(data_frame$c3)

nrow(airquality)

length<- nrow(airquality)
length

total_row <- length*0.8
total_row

split <- 1:total_row
split[1:5] 

train_df <- airquality[split, ] 
head(train_df)

test_df <- airquality[-split, ] 
head(test_df)

split_data <- function(df, train = TRUE){
  length<- nrow(df)
  total_row <- length *0.8
  split <- 1:total_row
  if (train ==TRUE){ 
    train_df <- df[split, ] 
    return(train_df)		
  } else {
    test_df <- df[-split, ] 
    return(test_df)		
  }
}

train <- split_data(airquality, train = TRUE)
dim(train)

test <- split_data(airquality, train = FALSE)
dim(test)

# Create vector quantity
quantity <-  25
# Set the is-else statement
if (quantity > 20) {
  print('You sold a lot!')
} else {
  print('Not enough for today')  
}

# Create vector quantiy
quantity <-  10
# Create multiple condition statement
if (quantity <20) {
  print('Not enough for today')
} else if (quantity > 20  &quantity <= 30) {
  print('Average day')
} else {
  print('What a great day!')
}

category <- 'A'
price <- 10
if (category =='A'){
  cat('A vat rate of 8% is applied.','The total price is',price *1.08)  
} else if (category =='B'){
  cat('A vat rate of 10% is applied.','The total price is',price *1.10)  
} else {
  cat('A vat rate of 20% is applied.','The total price is',price *1.20)  
}

# Create fruit vector
fruit <- c('Apple', 'Orange', 'Passion fruit', 'Banana')
# Create the for statement
for ( i in fruit){ 
  print(i)
}

# Create an empty list
list <- c()
# Create a for statement to populate the list
for (i in seq(1, 4, by=1)) {
  list[[i]] <- i*i
}
print(list)

# Create a list with three vectors
fruit <- list(Basket = c('Apple', 'Orange', 'Passion fruit', 'Banana'), 
              Money = c(10, 12, 15), purchase = FALSE)
for (p  in fruit) 
{ 
  print(p)
}

# Create a matrix
mat <- matrix(data = seq(10, 20, by=1), nrow = 6, ncol =2)
# Create the loop with r and c to iterate over the matrix
for (r in 1:nrow(mat))   
  for (c in 1:ncol(mat))  
    print(paste("Row", r, "and column",c, "have values of", mat[r,c]))  

#Create a variable with value 1
begin <- 1

#Create the loop
while (begin <= 10){
  
  #See which we are  
  cat('This is loop number',begin)
  
  #add 1 to the variable begin after each loop
  begin <- begin+1
  print(begin)
}

set.seed(123)
# Set variable stock and price
stock <- 50
price <- 50

# Loop variable counts the number of loops 
loop <- 1

m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 2, sum)
a_m1

movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <-lapply(movies, tolower) #list structure
str(movies_lower)

movies_lower <-unlist(lapply(movies,tolower))
str(movies_lower)

dt <- cars
lmn_cars <- lapply(dt, min)
smn_cars <- sapply(dt, min)  #unlist
lmn_cars

smn_cars

lmxcars <- lapply(dt, max)
smxcars <- sapply(dt, max)
lmxcars

smxcars

avg <- function(x) {  
  ( min(x) + max(x) ) / 2}
fcars <- sapply(dt, avg)
fcars

below_ave <- function(x) {  
  ave <- mean(x) 
  return(x[x > ave])
}
dt_s<- sapply(dt, below_ave)
dt_l<- lapply(dt, below_ave)
identical(dt_s, dt_l)

data(iris)
tapply(iris$Sepal.Width, iris$Species, median)

install.packages("readxl")
library(readxl)  # install.packages("readxl") or install.packages("tidyverse")
excel_sheets(readxl_example("datasets.xls"))

# To load all sheets in a workbook, use lapply
path <- readxl_example("datasets.xls")
lapply(excel_sheets(path), read_excel, path = path)

# Store the path of `datasets.xlsx`
example <- readxl_example("datasets.xlsx")
# Import the spreadsheet
df <- read_excel(example)
# Count the number of columns
length(df)

example <- readxl_example("datasets.xlsx")
quake <- read_excel(example, sheet = "quakes")
quake_1 <-read_excel(example, sheet = 4)
identical(quake, quake_1)

# Read the first five row: with header
iris <-read_excel(example, n_max =5, col_names =TRUE)
iris

# Read the first five row: without header
iris_no_header <-read_excel(example, n_max =5, col_names =FALSE)
iris_no_header

# Read rows A1 to B5
example_1 <-read_excel(example, range = "A1:B5", col_names =TRUE)
dim(example_1)

# Read rows 1 to 5
example_2 <-read_excel(example, range =cell_rows(1:5),col_names =TRUE)			
dim(example_2)

iris_na <-read_excel(example, na ="setosa")
sum(is.na(iris_na))

# Titanic
setwd("C:/Users/ankur/Documents/Data Science/Projects/R/R")
PATH <- "titanic.csv"
df_titanic <- read.csv(PATH, sep = ",")
# Return the column names containing missing observations
list_na <- colnames(df_titanic)[ apply(df_titanic, 2, anyNA) ]
list_na

library(dplyr)
# Exclude the missing observations
df_titanic_drop <-df_titanic %>%
  na.omit()		
dim(df_titanic_drop)
dim(df_titanic)

# Create mean
average_missing <- apply(df_titanic[,colnames(df_titanic) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing# Create a new variable with the mean and median
df_titanic_replace <- df_titanic %>%  #create new datasets
  mutate(replace_mean_age  = ifelse(is.na(age), average_missing[1], age),
         replace_mean_fare = ifelse(is.na(fare), average_missing[2], fare))

df_titanic

sum(is.na(df_titanic_replace$Age))

sum(is.na(df_titanic_replace$replace_mean_age))

median_missing <- apply(df_titanic[,colnames(df_titanic) %in% list_na],
                        2,
                        median,
                        na.rm =  TRUE)

df_titanic_replace <- df_titanic %>%
  mutate(replace_median_age  = ifelse(is.na(Age), median_missing[1], Age), 
         replace_median_fare = ifelse(is.na(Fare), median_missing[2], Fare))
head(df_titanic_replace)


directory <-getwd()
directory

# Create data frame
library(dplyr)
df <-mtcars %>%
  select(mpg, disp, gear) %>%
  group_by(gear) %>%
  summarize(mean_mpg = mean(mpg), mean_disp = mean(disp))
df

write.csv(df, "table_car.csv")
write.csv2(df, "table_car2.csv")  #separate the rows with a semicolon.

# conda install -c r r-xlsx
library(xlsx)
write.xlsx(df, "table_car.xlsx")

install.packages("googledrive")	
library(googledrive)
# drive_upload(file, path = NULL, name = NULL)
drive_upload("table_car.csv", name ="table_car")

drive_browse("table_car")

x <-drive_get("table_car")
as_id(x)
x

install.packages('rdrop2')
library(rdrop2)
drop_auth()
drop_create('my_first_drop')
drop_upload('table_car.csv', path = "my_first_drop")

# Pearson Correlation
# Spearman Rank Correlation
library(dplyr)
PATH <-"C:/Users/ankur/Documents/Data Science/Projects/R/R/british_household.csv"
data <-read.csv(PATH)
str(data)

data <-read.csv(PATH)
  filter(income < 500)
  mutate(log_income = log(income),
         log_totexp = log(totexp),
         children_fac = factor(children, order = TRUE, labels = c("No", "Yes")))
  select(-c(X,X.1, children, totexp, income))
glimpse(data)

library("Hmisc")
data_rcorr <-as.matrix(data[, 1: 9])

mat_2 <-rcorr(data_rcorr)  #p-value
# mat_2 <-rcorr(as.matrix(data)) returns the same output
mat_2

p_value <-round(mat_2[["P"]], 3)
p_value

install.packages("GGally")
library(GGally)
ggcorr(data)

ggcorr(data,
       nbreaks = 6,
       low = "steelblue",
       mid = "white",
       high = "darkred",
       geom = "circle")

ggcorr(data,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")

ggpair(df, columns = 1: ncol(df), title = NULL,
       upper = list(continuous = "cor"),
       lower = list(continuous = "smooth"),
       mapping = NULL)

library(ggplot2)
ggpairs(data, columns = c("log_totexp", "log_income", "age", "wtrans"), title = "Bivariate analysis of revenue expenditure by the British household", upper = list(continuous = wrap("cor",
                                                                                                                                                                                     size = 3)),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.3,
                                       size = 0.1)),
        mapping = aes(color = children_fac))

# Basic scatter plot
ggplot(mtcars, aes(x = drat, y = mpg)) +
  geom_point()

# scatter plot with groups
ggplot(mtcars, aes(x = mpg, y = drat)) +
  geom_point(aes(color = factor(gear)))

# change axis
ggplot(mtcars, aes(x = log(mpg), y = log(drat))) +
  geom_point(aes(color = factor(gear)))

# Scatter plot with fitted values
my_graph <- ggplot(mtcars, aes(x = log(mpg), y = log(drat))) +
  geom_point(aes(color = factor(gear))) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)
my_graph

my_graph +
  labs(
    title = "Plot Mile per hours and drat, in log"
  )

A <-2010
paste("The first year is", A)

B <-2018
paste("The first year is", A, "and the last year is", B)

mean_mpg <- mean(mtcars$mpg)
my_graph + labs(
  title = paste("Plot Mile per hours and drat, in log. Average mpg is", mean_mpg)
)

my_graph +
  labs(
    title =
      "Relation between Mile per hours and drat",
    subtitle =
      "Relationship break down by gear class",
    caption = "Authors own computation"
  )

my_graph +
  labs(
    x = "Drat definition",
    y = "Mile per hours",
    color = "Gear",
    title = "Relation between Mile per hours and drat",
    subtitle = "Relationship break down by gear class",
    caption = "Authors own computation"
  )

seq(0, 12,4)

my_graph +
  scale_x_continuous(breaks = seq(1, 3.6, by = 0.2)) +
  scale_y_continuous(breaks = seq(1, 1.6, by = 0.1)) +
  labs(
    x = "Drat definition",
    y = "Mile per hours",
    color = "Gear",
    title = "Relation between Mile per hours and drat",
    subtitle = "Relationship break down by gear class",
    caption = "Authors own computation"
  )

my_graph +
  theme_dark() +
  labs(
    x = "Drat definition, in log",
    y = "Mile per hours, in log",
    color = "Gear",
    title = "Relation between Mile per hours and drat",
    subtitle = "Relationship break down by gear class",
    caption = "Authors own computation"
  )

directory <-getwd()
directory

my_graph +
  theme_dark() +
  labs(
    x = "Drat definition, in log",
    y = "Mile per hours, in log",
    color = "Gear",
    title = "Relation between Mile per hours and drat",
    subtitle = "Relationship break down by gear class",
    caption = "Authors own computation"
  )

ggsave("my_fantastic_plot.png")

# Run this code to create the function
open_folder <- function(dir) {
  if (.Platform['OS.type'] == "windows") {
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

open_folder("C:/Users/ankur/Documents/Data Science/Projects/R/R")

# create box plot
library(dplyr)
library(ggplot2)
# Step 1
data_air <- airquality %>%
  
  #Step 2
  select(-c(Solar.R, Temp)) %>%
  
  #Step 3
  mutate(Month = factor(Month, order = TRUE, labels = c("May", "June", "July", "August", "September")), 
         
         #Step 4 
         day_cat = factor(ifelse(Day < 10, "Begin", ifelse(Day < 20, "Middle", "End"))))
data_air
glimpse(data_air)

data_air_nona <-data_air %>% na.omit()	

# Store the graph
box_plot <- ggplot(data_air_nona, aes(x = Month, y = Ozone))
# Add the geometric object box plot
box_plot +
  geom_boxplot()

box_plot +
  geom_boxplot()+
  coord_flip()

box_plot +
  geom_boxplot(outlier.colour = "red",
               outlier.shape = 2,
               outlier.size = 3) +
  theme_classic()

box_plot +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

box_plot +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',
               dotsize = 1,
               stackdir = 'center') +
  theme_classic()

ggplot(data_air_nona, aes(x = Month, y = Ozone, color = Month)) +
  geom_boxplot() +
  theme_classic()

ggplot(data_air_nona, aes(Month, Ozone)) +
  geom_boxplot(aes(fill = day_cat)) +
  theme_classic()

box_plot +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(width = 0.21)) +
  theme_classic()

box_plot +
  geom_boxplot() +
  geom_point(shape = 5,
             color = "steelblue") +
  theme_classic()

box_plot +
  geom_boxplot(notch = TRUE) +
  theme_classic()

# Bar chart
library(ggplot2)
# Most basic bar chart
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar()

# Change the color of the bars
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(fill = "coral") +
  theme_classic()

grDevices::colors()		

# Change intensity
ggplot(mtcars,
       aes(factor(cyl))) +
  geom_bar(fill = "coral",
           alpha = 0.5) +
  theme_classic()

# Color by group
ggplot(mtcars, aes(factor(cyl),
                   fill = factor(cyl))) +
  geom_bar()

library(dplyr)
# Step 1
data <- mtcars %>% 
  #Step 2
  mutate(am = factor(am, labels = c("auto", "man")),
         cyl = factor(cyl))
ggplot(data, aes(x = cyl, fill = am)) +
  geom_bar() +
  theme_classic()

ggplot(data, aes(x = cyl, fill = am)) +
  geom_bar(position = "fill") +
  theme_classic()

# Bar chart side by side
ggplot(data, aes(x = cyl, fill = am)) +
  geom_bar(position = position_dodge()) +
  theme_classic()

# T-test
set.seed(123) 
sugar_cookie <- rnorm(30, mean = 9.99, sd = 0.04)
head(sugar_cookie)

# sales before the program
sales_before <- rnorm(7, mean = 50000, sd = 50)
# sales after the program.This has higher mean
sales_after <- rnorm(7, mean = 50075, sd = 50)
# draw the distribution
t.test(sales_before, sales_after,var.equal = TRUE)

PATH <- "C:/Users/ankur/Documents/Data Science/Projects/R/R/poisons.csv"
df <- read.csv(PATH) %>%
  select(-X) %>% 
  mutate(poison = factor(poison, ordered = TRUE))
glimpse(df)

levels(df$poison)

df %>%
  group_by(poison) %>%
  summarise(
    #count_poison = n(),
    mean_time = mean(time, na.rm = TRUE),
    sd_time = sd(time, na.rm = TRUE)
  )

ggplot(df, aes(x = poison, y = time, fill = poison)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()

anova_one_way <- aov(time~poison, data = df)
summary(anova_one_way)

# pairwise comparison
TukeyHSD(anova_one_way)

# two-way anove
anova_two_way <- aov(time~poison + treat, data = df)
summary(anova_two_way)

# scatterplot
library(ggplot2)
path <- 'C:\\Users\\ankur\\Documents\\Data Science\\Projects\\R\\R\\women.csv'
df <-read.csv(path)
ggplot(df,aes(x=height, y =  weight))+
  geom_point()

# least squares estimate
beta <- cov(df$height, df$weight) / var (df$height)
beta

alpha <- mean(df$weight) - beta * mean(df$height)
alpha

df <- mtcars %>%
  select(-c(am, vs, cyl, gear, carb))
glimpse(df)

model <- mpg~disp + hp + drat + wt
fit <- lm(model, df)
fit

summary(fit)

anova(fit)

plot(fit)

# factor regression
df <- mtcars %>%
  mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))
summary(lm(model, df))

# stepwise regression
library(GGally)
df <- mtcars %>%
  select(-c(am, vs, cyl, gear, carb))
ggscatmat(df, columns = 1: ncol(df))

library(olsrr)
install.packages("olsrr")
model <- mpg~.
fit <- lm(model, df)
test <- ols_all_subset(fit)
test = ols_step_all_possible(fit)
plot(test)

# compare results
stp_s <- ols_stepwise(fit, details=TRUE)
stp_s <- ols_step_both_p(fit, details=TRUE)

## Machine Learning
getwd()
setwd('C:/Users/ankur/Documents/Data Science/Projects/R/R')
titanic <-read.csv('titanic_data.csv')
head(titanic)
tail(titanic)

shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)

titanic <- titanic[shuffle_index, ]
head(titanic)

library(dplyr)
# Drop variables
clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  #Convert to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()
glimpse(clean_titanic)

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample < - 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

prop.table(table(data_train$survived))

prop.table(table(data_test$survived))

# Logistic Regression
library(dplyr)
data_adult <-read.csv("data/adult.csv")
glimpse(data_adult)

continuous <-select_if(data_adult, is.numeric)
summary(continuous)

# Histogram with kernel density curve
library(ggplot2)
ggplot(continuous, aes(x = hours.per.week)) +
  geom_density(alpha = .2, fill = "#FF6666")

top_one_percent <- quantile(data_adult$hours.per.week, .99)
top_one_percent

data_adult_drop <-data_adult %>%
  filter(hours.per.week<top_one_percent)
dim(data_adult_drop)

data_adult_rescale <- data_adult_drop %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(data_adult_rescale)

#  Select categorical column
factor <- data.frame(select_if(data_adult_rescale, is.factor))
ncol(factor)

# Create graph for each column
graph <- lapply(names(factor),
                function(x) 
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))

# Print the graph
graph

# Feature engineering
recast_data <- data_adult_rescale %>%
  select(-x) %>%
  mutate(education = factor(ifelse(education == "Preschool" | education == "10th" | education == "11th" | education == "12th" | education == "1st-4th" | education == "5th-6th" | education == "7th-8th" | education == "9th", "dropout", ifelse(education == "HS-grad", "HighGrad", ifelse(education == "Some-college" | education == "Assoc-acdm" | education == "Assoc-voc", "Community",
                                                                                                                                                                                                                                                                                            ifelse(education == "Bachelors", "Bachelors",
                                                                                                                                                                                                                                                                                                   ifelse(education == "Masters" | education == "Prof-school", "Master", "PhD")))))))
str(x)

table(recast_data$marital.status)

# Plot gender income
ggplot(recast_data, aes(x = gender, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic()

# Plot origin income
ggplot(recast_data, aes(x = race, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

# box plot gender working time
ggplot(recast_data, aes(x = gender, y = hours.per.week)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

# Plot distribution working time by education
ggplot(recast_data, aes(x = hours.per.week)) +
  geom_density(aes(color = education), alpha = 0.5) +
  theme_classic()

anova <- aov(hours.per.week~education, recast_data)
summary(anova)

# non-linearity
library(ggplot2)
ggplot(recast_data, aes(x = age, y = hours.per.week)) +
  geom_point(aes(color = income),
             size = 0.5) +
  stat_smooth(method = 'lm',
              formula = y~poly(x, 2),
              se = TRUE,
              aes(color = income)) +
  theme_classic()

# correlation
library(GGally)
# Convert data to numeric
corr <- data.frame(lapply(recast_data, as.integer))
# Plot the graph
ggcorr(corr,
  method = c("pairwise", "spearman"),
  nbreaks = 6,
  hjust = 0.8,
  label = TRUE,
  label_size = 3,
  color = "grey50")

set.seed(1234)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(recast_data, 0.8, train = TRUE)
data_test <- create_train_test(recast_data, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

formula <- income~.
logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)

lapply(logit, class)[1:3]

logit$aic

predict <- predict(logit, data_test, type = 'response')
# confusion matrix
table_mat <- table(data_test$income, predict > 0.5)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

# precision, recall
precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}
recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}
prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec

# harmonic mean of these two metrics, meaning it gives more weight to the lower values.
f1 <- 2 * ((prec * rec) / (prec + rec))
f1

# Receiver Operating Characteristic curve
install.packages('ROCR')
library(ROCR)
ROCRpred <- prediction(predict, data_test$income)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

formula_2 <- income~age: hours.per.week + gender: hours.per.week + .
logit_2 <- glm(formula_2, data = data_train, family = 'binomial')
predict_2 <- predict(logit_2, data_test, type = 'response')
table_mat_2 <- table(data_test$income, predict_2 > 0.5)
precision_2 <- precision(table_mat_2)
recall_2 <- recall(table_mat_2)
f1_2 <- 2 * ((precision_2 * recall_2) / (precision_2 + recall_2))
f1_2

# K-Means
library(ggplot2)
df <- data.frame(age = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54),
                 spend = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 27, 29, 20, 28, 21, 30, 31, 23, 24)
)
ggplot(df, aes(x = age, y = spend)) +
  geom_point()

df <- read.csv("data/computers.csv") %>%
  select(-c(X, cd, multi, premium))
glimpse(df)

summary(df)
str(df)

rescale_df <- df %>%
  mutate(price_scal = scale(price),
         hd_scal = scale(hd),
         ram_scal = scale(ram),
         screen_scal = scale(screen),
         ads_scal = scale(ads),
         trend_scal = scale(trend)) %>%
  select(-c(price, speed, hd, ram, screen, ads, trend))

